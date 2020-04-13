use crate::call_frame::CallFrame;
use crate::compiler::{Compiler, CompilerResult, Parser};
use crate::constants::{DEFAULT_STACK_MAX, FRAME_MAX, INIT, PLACEHOLDER_NAME, SCRIPT};
use fnv::FnvHashMap;
use spacelox_core::chunk::{ByteCode, UpvalueIndex};
use spacelox_core::io::{Io, NativeIo, StdIo};
use spacelox_core::managed::{Managed, Trace};
use spacelox_core::memory::{Gc, NO_GC};
use spacelox_core::native::{NativeFun, NativeMethod, NativeResult};
use spacelox_core::{
  utils::use_sentinel_nan,
  value::{ArityKind, BuiltInClasses, Class, Closure, Fun, Instance, Method, Upvalue, Value},
};
use spacelox_lib::assert::assert_funs;
use spacelox_lib::{builtin::make_builtin_classes, time::clock_funs};
use std::convert::TryInto;
use std::mem;
use std::ptr;
use std::ptr::NonNull;

#[cfg(feature = "debug")]
use crate::debug::disassemble_instruction;

pub type InterpretResult = Result<u32, Interpret>;

#[derive(Debug, Clone, PartialEq)]
pub enum Interpret {
  Ok,
  CompileError,
  RuntimeError,
}

pub fn default_native_vm() -> Vm<NativeIo> {
  let io = NativeIo::new();
  Vm::new(io)
}

/// A set of dependencies needed by the virtual machine
pub struct VmDependencies<I: Io + 'static> {
  /// access to the environments io
  io: I,

  /// The garbage collector
  gc: Gc,

  /// The value stack
  stack: Vec<Value>,

  /// The callframe stack
  frames: Vec<CallFrame>,

  /// The native functions
  natives: Vec<Box<dyn NativeFun>>,
}

/// The virtual machine for the spacelox programming language
pub struct Vm<I: Io + 'static> {
  /// A stack holding all local variable currently in use
  stack: Vec<Value>,

  /// A stack holding call frames currently in use
  frames: Vec<CallFrame>,

  /// A collection of built in classes
  builtin: BuiltInClasses,

  /// The vm's garbage collector
  gc: Gc,

  /// The environments io access
  io: I,

  /// A persisted set of globals most for a repl context
  globals: FnvHashMap<Managed<String>, Value>,
}

impl<I: Io> Vm<I> {
  pub fn new(io: I) -> Vm<I> {
    let gc = Gc::new(Box::new(io.stdio()));
    let fun = Fun::new(gc.manage_str(String::from(PLACEHOLDER_NAME), &NO_GC));

    let managed_fun = gc.manage(fun, &NO_GC);
    let closure = gc.manage(Closure::new(managed_fun), &NO_GC);

    let frames = vec![CallFrame::new(closure); FRAME_MAX];
    let stack = vec![Value::Nil; DEFAULT_STACK_MAX];

    let mut natives = Vec::new();
    natives.extend(assert_funs().into_iter());
    natives.extend(clock_funs().into_iter());

    let builtin = make_builtin_classes(&gc, &NO_GC);
    let globals = define_globals(&gc, natives);

    Vm {
      io,
      stack,
      frames,
      builtin,
      gc,
      globals,
    }
  }

  pub fn repl(&mut self) {
    let stdio = self.io.stdio();
    loop {
      let mut buffer = String::new();

      stdio.print("> ");
      stdio.flush().expect("Could not write to stdout");

      match stdio.read_line(&mut buffer) {
        Ok(_) => {
          self.interpret(&buffer);
        }
        Err(error) => panic!(error),
      }
    }
  }

  pub fn run(&mut self, source: &str) -> Interpret {
    self.interpret(source)
  }

  fn compile(&mut self, source: &str) -> CompilerResult {
    let mut parser = Parser::new(self.io.stdio(), source);

    let compiler = Compiler::new(self.io, &mut parser, &mut self.gc);
    compiler.compile()
  }

  fn interpret(&mut self, source: &str) -> Interpret {
    let result = self.compile(source);

    if !result.success {
      return Interpret::CompileError;
    }

    let script_closure = self.gc.manage(Closure::new(result.fun), &NO_GC);
    let script = Value::Closure(script_closure);
    let mut executor = VmExecutor::new(self, script);
    executor.run()
  }
}

impl<I: Io> From<VmDependencies<I>> for Vm<I> {
  fn from(dependencies: VmDependencies<I>) -> Self {
    let gc = dependencies.gc;
    let builtin = make_builtin_classes(&gc, &NO_GC);
    let globals = define_globals(&gc, dependencies.natives);

    Vm {
      io: dependencies.io,
      stack: dependencies.stack,
      frames: dependencies.frames,
      gc,
      globals,
      builtin,
    }
  }
}

fn define_globals(gc: &Gc, natives: Vec<Box<dyn NativeFun>>) -> FnvHashMap<Managed<String>, Value> {
  let mut globals = FnvHashMap::with_capacity_and_hasher(natives.len(), Default::default());

  natives.into_iter().for_each(|native| {
    let name = gc.manage_str(native.meta().name.to_string(), &NO_GC);
    let native_value = Value::NativeFun(gc.manage(native, &NO_GC));

    globals.insert(name, native_value);
  });

  globals
}

struct VmExecutor<'a, I: Io + 'static> {
  /// A stack of call frames for the current execution
  frames: &'a mut Vec<CallFrame>,

  /// A stack holding all local variable currently in use
  stack: &'a mut Vec<Value>,

  /// global variable present in the vm
  globals: &'a mut FnvHashMap<Managed<String>, Value>,

  /// A collection of built in classes
  builtin: &'a BuiltInClasses,

  /// A reference to a object currently in the vm
  gc: &'a mut Gc,

  /// The environments io access
  io: &'a I,

  /// A collection of currently available upvalues
  open_upvalues: Vec<Managed<Upvalue>>,

  /// the main script level function
  script: Value,

  /// The current frame's function
  current_fun: Managed<Fun>,

  /// The current frame's closure
  current_frame: CallFrame,

  /// index to the top of the value stack
  stack_top: usize,

  /// The current frame depth of the program
  frame_count: usize,
}

impl<'a, I: Io> VmExecutor<'a, I> {
  /// Create an instance of the vm executor that can execute the provided script.
  pub fn new(vm: &'a mut Vm<I>, script: Value) -> VmExecutor<'a, I> {
    let current_frame = vm.frames[0];
    let current_fun = current_frame.closure.fun;

    let mut executor = VmExecutor {
      frames: &mut vm.frames,
      frame_count: 0,
      stack: &mut vm.stack,
      script,
      current_fun,
      current_frame,
      builtin: &vm.builtin,
      gc: &mut vm.gc,
      io: &mut vm.io,
      stack_top: 1,
      globals: &mut vm.globals,
      open_upvalues: Vec::with_capacity(100),
    };

    let result = executor.call(executor.script.to_closure(), 0, 0);
    match result {
      Ok(_) => executor,
      _ => panic!("Script call failed"),
    }
  }

  /// Main virtual machine execution loop. This will run the until the program interrupts
  /// from a normal exit or from a runtime error.
  pub fn run(&mut self) -> Interpret {
    let mut ip: u32 = 0;

    #[cfg(feature = "debug")]
    let mut last_ip: u32 = 0;

    loop {
      // get the current instruction
      let op_code: ByteCode = self.frame_instruction(ip);

      #[cfg(feature = "debug")]
      {
        self.print_debug(ip as usize, last_ip as usize);
        last_ip = ip;
      }

      // execute the decoded instruction
      let result = match op_code {
        ByteCode::Negate => self.op_negate(ip),
        ByteCode::Add => self.op_add(ip),
        ByteCode::Subtract => self.op_sub(ip),
        ByteCode::Multiply => self.op_mul(ip),
        ByteCode::Divide => self.op_div(ip),
        ByteCode::Not => self.op_not(ip),
        ByteCode::Equal => self.op_equal(ip),
        ByteCode::Greater => self.op_greater(ip),
        ByteCode::Less => self.op_less(ip),
        ByteCode::JumpIfFalse => self.op_jump_if_not_false(ip),
        ByteCode::Jump => self.op_jump(ip),
        ByteCode::Loop => self.op_loop(ip),
        ByteCode::DefineGlobal => self.op_define_global(ip),
        ByteCode::GetIndex => self.op_get_index(ip),
        ByteCode::SetIndex => self.op_set_index(ip),
        ByteCode::GetGlobal => self.op_get_global(ip),
        ByteCode::SetGlobal => self.op_set_global(ip),
        ByteCode::GetLocal => self.op_get_local(ip),
        ByteCode::SetLocal => self.op_set_local(ip),
        ByteCode::GetUpvalue => self.op_get_upvalue(ip),
        ByteCode::SetUpvalue => self.op_set_upvalue(ip),
        ByteCode::GetProperty => self.op_get_property(ip),
        ByteCode::SetProperty => self.op_set_property(ip),
        ByteCode::Pop => self.op_pop(ip),
        ByteCode::Nil => self.op_literal(ip, Value::Nil),
        ByteCode::True => self.op_literal(ip, Value::Bool(true)),
        ByteCode::False => self.op_literal(ip, Value::Bool(false)),
        ByteCode::List => self.op_literal(ip, Value::List(self.gc.manage(Vec::new(), self))),
        ByteCode::ListInit => self.op_list(ip),
        ByteCode::Map => {
          self.op_literal(ip, Value::Map(self.gc.manage(FnvHashMap::default(), self)))
        }
        ByteCode::MapInit => self.op_map(ip),
        ByteCode::Constant => self.op_constant(ip),
        ByteCode::Print => self.op_print(ip),
        ByteCode::Call => self.op_call(ip),
        ByteCode::Invoke => self.op_invoke(ip),
        ByteCode::SuperInvoke => self.op_super_invoke(ip),
        ByteCode::Closure => self.op_closure(ip),
        ByteCode::Method => self.op_method(ip),
        ByteCode::Class => self.op_class(ip),
        ByteCode::Inherit => self.op_inherit(ip),
        ByteCode::GetSuper => self.op_get_super(ip),
        ByteCode::CloseUpvalue => self.op_close_upvalue(ip),
        ByteCode::Return => self.op_return(ip),
      };

      match result {
        Ok(new_ip) => ip = new_ip,
        Err(interrupt) => {
          return interrupt;
        }
      }
    }
  }

  /// Get an immutable reference to the current callframe
  #[inline]
  fn current_frame(&self) -> &CallFrame {
    unsafe { self.frames.get_unchecked(self.frame_count - 1) }
  }

  /// Get a mutable reference to the current callframe
  #[inline]
  fn current_mut_frame(&mut self) -> &mut CallFrame {
    unsafe { self.frames.get_unchecked_mut(self.frame_count - 1) }
  }

  /// Get an immutable reference to value on the stack
  #[inline]
  fn get_val(&self, index: usize) -> Value {
    unsafe { *self.stack.get_unchecked(index as usize) }
  }

  /// Set a value on the stack
  #[inline]
  fn set_val(&mut self, index: usize, val: Value) {
    unsafe {
      *self.stack.get_unchecked_mut(index as usize) = val;
    }
  }

  /// read a u8 out of the bytecode
  #[inline]
  fn read_byte(&self, ip: u32) -> u8 {
    unsafe {
      *self
        .current_fun
        .chunk
        .instructions
        .get_unchecked(ip as usize)
    }
  }

  /// read a u16 out of the bytecode
  #[inline]
  fn read_short(&self, ip: u32) -> u16 {
    let slice: &[u8] = unsafe {
      self
        .current_fun
        .chunk
        .instructions
        .get_unchecked(ip as usize..ip as usize + 2)
    };
    let buffer = slice.try_into().expect("slice of incorrect length.");
    u16::from_ne_bytes(buffer)
  }

  /// Get the current instruction from the present call frame
  #[inline]
  fn frame_instruction(&self, ip: u32) -> ByteCode {
    ByteCode::from(unsafe {
      *self
        .current_fun
        .chunk
        .instructions
        .get_unchecked(ip as usize)
    })
  }

  /// push a value onto the stack
  #[inline]
  fn push(&mut self, value: Value) {
    self.set_val(self.stack_top, value);
    self.stack_top += 1;
  }

  /// pop a value off the stack
  #[inline]
  fn pop(&mut self) -> Value {
    self.stack_top -= 1;
    unsafe { *self.stack.get_unchecked(self.stack_top as usize) }
  }

  /// reference a value n slots from the stack head
  #[inline]
  fn peek(&self, distance: u32) -> Value {
    self.get_val(self.stack_top - (distance as usize + 1))
  }

  /// read a constant from the current chunk
  #[inline]
  fn read_constant(&self, index: u8) -> Value {
    unsafe {
      *self
        .current_fun
        .chunk
        .constants
        .get_unchecked(index as usize)
    }
  }

  /// read a constant as a string from the current chunk
  #[inline]
  fn read_string(&self, index: u8) -> Managed<String> {
    self.read_constant(index).to_str()
  }

  /// reset the stack in case of interrupt
  fn reset_stack(&mut self) {
    self.stack_top = 1;
    self.frame_count = 0;
    self.open_upvalues.clear();
  }

  /// push a literal value onto the stack
  fn op_literal(&mut self, ip: u32, value: Value) -> InterpretResult {
    self.push(value);
    Ok(ip + 1)
  }

  /// pop a value off the stack
  fn op_pop(&mut self, ip: u32) -> InterpretResult {
    self.pop();
    Ok(ip + 1)
  }

  /// create a list from a list literal
  fn op_list(&mut self, ip: u32) -> InterpretResult {
    let arg_count = self.read_short(ip + 1);
    let args = unsafe {
      self
        .stack
        .get_unchecked(self.stack_top as usize - arg_count as usize..self.stack_top as usize)
    };
    let mut list = self.peek(arg_count as u32).to_list();
    list.extend(args);
    self.stack_top -= arg_count as usize;

    Ok(ip + 3)
  }

  /// create a list from a list literal
  fn op_map(&mut self, ip: u32) -> InterpretResult {
    let arg_count = self.read_short(ip + 1);
    let mut map = self.peek(arg_count as u32 * 2).to_map();
    map.reserve(arg_count as usize);

    for i in 0..arg_count {
      let key = unsafe {
        self
          .stack
          .get_unchecked(self.stack_top - (i as usize * 2) - 2)
      };
      let value = unsafe {
        self
          .stack
          .get_unchecked(self.stack_top - (i as usize * 2) - 1)
      };

      map.insert(*key, *value);
    }
    self.stack_top -= arg_count as usize * 2;

    Ok(ip + 3)
  }

  /// call a function or method
  fn op_call(&mut self, ip: u32) -> InterpretResult {
    let arg_count = self.read_byte(ip + 1);
    let callee = self.peek(arg_count as u32);

    self.resolve_call(callee, arg_count, ip + 2)
  }

  /// invoke a method on an instance's class
  fn op_invoke(&mut self, ip: u32) -> InterpretResult {
    let constant = self.read_byte(ip + 1);
    let arg_count = self.read_byte(ip + 2);

    let method_name = self.read_string(constant);
    let receiver = self.peek(arg_count as u32);

    match receiver {
      Value::Instance(instance) => match instance.fields.get(&method_name) {
        Some(field) => {
          self.set_val(self.stack_top - (arg_count as usize) - 1, *field);
          self.resolve_call(*field, arg_count, ip + 3)
        }
        None => self.invoke_from_class(instance.class, method_name, arg_count, ip + 3),
      },
      Value::Bool(_) => self.invoke_from_class(self.builtin.bool, method_name, arg_count, ip + 3),
      Value::Number(_) => {
        self.invoke_from_class(self.builtin.number, method_name, arg_count, ip + 3)
      }
      Value::Nil => self.invoke_from_class(self.builtin.nil, method_name, arg_count, ip + 3),
      Value::String(_) => {
        self.invoke_from_class(self.builtin.string, method_name, arg_count, ip + 3)
      }
      Value::Fun(_) => self.invoke_from_class(self.builtin.fun, method_name, arg_count, ip + 3),
      Value::Closure(closure) => {
        self.set_val(self.stack_top - 1, Value::Fun(closure.fun));
        self.invoke_from_class(self.builtin.fun, method_name, arg_count, ip + 3)
      }
      Value::List(_) => self.invoke_from_class(self.builtin.list, method_name, arg_count, ip + 3),
      Value::Map(_) => self.invoke_from_class(self.builtin.map, method_name, arg_count, ip + 3),
      Value::NativeFun(_) => {
        self.invoke_from_class(self.builtin.native, method_name, arg_count, ip + 3)
      }
      Value::NativeMethod(_) => {
        self.invoke_from_class(self.builtin.native, method_name, arg_count, ip + 3)
      }
      _ => self.runtime_error(&format!("{} does not have methods.", receiver.value_type())),
    }
  }

  /// Invoke a method on a instance's super class
  fn op_super_invoke(&mut self, ip: u32) -> InterpretResult {
    let constant = self.read_byte(ip + 1);
    let arg_count = self.read_byte(ip + 2);

    let method_name = self.read_string(constant);
    let super_class = self.pop().to_class();

    self.invoke_from_class(super_class, method_name, arg_count, ip + 3)
  }

  /// Generate a new class
  fn op_class(&mut self, ip: u32) -> InterpretResult {
    let slot = self.read_byte(ip + 1);
    let name = self.read_string(slot);
    let class = Value::Class(self.gc.manage(Class::new(name), self));
    self.push(class);
    Ok(ip + 2)
  }

  /// Get this classes super class
  fn op_get_super(&mut self, ip: u32) -> InterpretResult {
    let slot = self.read_byte(ip + 1);
    let name = self.read_string(slot);
    let super_class = self.pop().to_class();

    self.bind_method(super_class, name, ip + 2)
  }

  fn op_inherit(&mut self, ip: u32) -> InterpretResult {
    let mut class = self.peek(0).to_class();

    match self.peek(1) {
      Value::Class(super_class) => {
        super_class.methods.for_each(|(key, value)| {
          match class.methods.get(&*key) {
            None => class.methods.insert(*key, *value),
            _ => None,
          };
        });

        class.init = class.init.or(super_class.init);

        self.pop();
        Ok(ip + 1)
      }
      _ => self.runtime_error("Superclass must be a class."),
    }
  }

  fn op_loop(&mut self, ip: u32) -> InterpretResult {
    Ok(ip + 3 - self.read_short(ip + 1) as u32)
  }

  fn op_jump_if_not_false(&mut self, ip: u32) -> InterpretResult {
    let jump = self.read_short(ip + 1);
    if is_falsey(self.peek(0)) {
      return Ok(ip + 3 + jump as u32);
    }

    Ok(ip + 3)
  }

  fn op_jump(&mut self, ip: u32) -> InterpretResult {
    let jump = self.read_short(ip + 1);
    Ok(ip + 3 + jump as u32)
  }

  fn op_define_global(&mut self, ip: u32) -> InterpretResult {
    let slot = self.read_byte(ip + 1);
    let name = self.read_string(slot);
    let global = self.pop();
    self.globals.insert(name, global);
    Ok(ip + 2)
  }

  fn op_set_index(&mut self, ip: u32) -> InterpretResult {
    let mut target = self.peek(2);
    let index = self.peek(1);

    match (&mut target, index) {
      (Value::List(list), Value::Number(num)) => {
        let rounded = num as usize;
        if rounded >= list.len() {
          return self.runtime_error(&format!(
            "Index out of bounds. list was length {} but attempted to index with {}.",
            list.len(),
            rounded
          ));
        }

        list[rounded] = self.pop();
        self.pop();
        Ok(ip + 1)
      }
      (Value::Map(map), Value::Number(num)) => {
        map.insert(Value::Number(use_sentinel_nan(num)), self.pop());
        self.pop();
        Ok(ip + 1)
      }
      (Value::Map(map), _) => {
        map.insert(index, self.pop());
        self.pop();
        Ok(ip + 1)
      }
      _ => self.runtime_error(&format!("{} cannot be indexed", target.value_type())),
    }
  }

  fn op_set_global(&mut self, ip: u32) -> InterpretResult {
    let slot = self.read_byte(ip + 1);
    let string = self.read_string(slot);

    if self.globals.insert(string, self.peek(0)).is_none() {
      self.globals.remove_entry(&string);
      return self.runtime_error(&format!("Undefined variable {}", string.as_str()));
    }

    Ok(ip + 2)
  }

  fn op_set_local(&mut self, ip: u32) -> InterpretResult {
    let slot = self.read_byte(ip + 1) as usize;
    let copy = self.peek(0);
    let slots = self.current_frame.slots as usize;
    self.set_val(slots + slot, copy);

    Ok(ip + 2)
  }

  fn op_set_property(&mut self, ip: u32) -> InterpretResult {
    let slot = self.read_byte(ip + 1);
    let mut value = self.peek(1);
    let name = self.read_string(slot);

    if let Value::Instance(ref mut instance) = value {
      instance.fields.insert(name, self.peek(0));

      let popped = self.pop();
      self.pop();
      self.push(popped);

      return Ok(ip + 2);
    }

    self.runtime_error("Only instances have fields.")
  }

  fn op_set_upvalue(&mut self, ip: u32) -> InterpretResult {
    let slot = self.read_byte(ip + 1);
    let value = self.peek(0);
    let upvalue = &mut self.current_frame.closure.upvalues[slot as usize];

    let open_index = match &mut *upvalue.to_upvalue() {
      Upvalue::Open(stack_ptr) => Some(*stack_ptr),
      Upvalue::Closed(store) => {
        **store = value;
        None
      }
    };

    if let Some(stack_ptr) = open_index {
      unsafe { ptr::write(stack_ptr.as_ptr(), value) }
    }

    Ok(ip + 2)
  }

  fn op_get_index(&mut self, ip: u32) -> InterpretResult {
    let index = self.pop();
    let target = self.pop();

    match (target, index) {
      (Value::List(list), Value::Number(num)) => {
        let rounded = num as usize;
        if rounded >= list.len() {
          return self.runtime_error(&format!(
            "Index out of bounds. list was length 0 but attempted to index with {}.",
            rounded
          ));
        }

        self.push(list[rounded]);
        Ok(ip + 1)
      }
      (Value::Map(map), Value::Number(num)) => {
        match map.get(&Value::Number(use_sentinel_nan(num))) {
          Some(value) => {
            self.push(*value);
            Ok(ip + 1)
          }
          None => self.runtime_error(&format!("Key {} does not exist in map", index)),
        }
      }
      (Value::Map(map), _) => match map.get(&index) {
        Some(value) => {
          self.push(*value);
          Ok(ip + 1)
        }
        None => self.runtime_error(&format!("Key {} does not exist in map", index)),
      },
      _ => self.runtime_error(&format!("{} cannot be indexed", target.value_type())),
    }
  }

  fn op_get_global(&mut self, ip: u32) -> InterpretResult {
    let store_index = self.read_byte(ip + 1);
    let string = self.read_string(store_index);

    match self.globals.get(&string) {
      Some(gbl) => {
        let copy = *gbl;
        self.push(copy);
        Ok(ip + 2)
      }
      None => self.runtime_error(&format!("Undefined variable {}", string.as_str())),
    }
  }

  fn op_get_local(&mut self, ip: u32) -> InterpretResult {
    let slot = self.read_byte(ip + 1) as usize;
    let slots = self.current_frame.slots as usize;
    let copy = self.get_val(slots + slot);
    self.push(copy);
    Ok(ip + 2)
  }

  fn op_get_upvalue(&mut self, ip: u32) -> InterpretResult {
    let slot = self.read_byte(ip + 1);
    let upvalue_value = &self.current_frame.closure.upvalues[slot as usize];

    let value = match &*upvalue_value.to_upvalue() {
      Upvalue::Open(stack_ptr) => *unsafe { stack_ptr.as_ref() },
      Upvalue::Closed(store) => **store,
    };

    self.push(value);
    Ok(ip + 2)
  }

  fn op_get_property(&mut self, ip: u32) -> InterpretResult {
    let slot = self.read_byte(ip + 1);
    let value = self.peek(0);
    let name = self.read_string(slot);

    match value {
      Value::Instance(instance) => match instance.fields.get(&name) {
        Some(value) => {
          self.set_val(self.stack_top - 1, *value);
          Ok(ip + 2)
        }
        None => self.bind_method(instance.class, name, ip + 2),
      },
      Value::Bool(_) => self.bind_method(self.builtin.bool, name, ip),
      Value::Number(_) => self.bind_method(self.builtin.number, name, ip),
      Value::Nil => self.bind_method(self.builtin.nil, name, ip),
      Value::String(_) => self.bind_method(self.builtin.string, name, ip),
      Value::Fun(_) => self.bind_method(self.builtin.fun, name, ip),
      Value::Closure(closure) => {
        self.set_val(self.stack_top - 1, Value::Fun(closure.fun));
        self.bind_method(self.builtin.fun, name, ip)
      }
      Value::List(_) => self.bind_method(self.builtin.list, name, ip),
      Value::NativeFun(_) => self.bind_method(self.builtin.native, name, ip),
      _ => self.runtime_error(&format!("{} does not have properties.", value.value_type())),
    }
  }

  /// return from a spacelox function placing the result on top of the stack
  fn op_return(&mut self, _: u32) -> InterpretResult {
    // get the function result close upvalues and pop frame
    let result = self.pop();
    self.close_upvalues(NonNull::from(
      &self.stack[self.current_frame.slots as usize],
    ));
    self.frame_count -= 1;

    // if the frame was the whole script signal an ok interrupt
    if self.frame_count == 0 {
      self.pop();
      return Err(Interpret::Ok);
    }

    // pull the current frame out of the stack and set the cached frame
    self.stack_top = self.frames[self.frame_count].slots as usize;
    self.current_frame = *self.current_frame();
    self.current_fun = self.current_frame.closure.fun;

    // push the result onto the stack
    self.push(result);
    Ok(self.current_frame.ip)
  }

  fn op_negate(&mut self, ip: u32) -> InterpretResult {
    match self.pop() {
      Value::Number(num) => {
        self.push(Value::Number(-num));
        Ok(ip + 1)
      }
      _ => self.runtime_error("Operand must be a number."),
    }
  }

  fn op_not(&mut self, ip: u32) -> InterpretResult {
    let value = self.pop();
    self.push(Value::Bool(is_falsey(value)));
    Ok(ip + 1)
  }

  fn op_add(&mut self, ip: u32) -> InterpretResult {
    match (self.pop(), self.pop()) {
      (Value::String(right), Value::String(left)) => {
        let result = format!("{}{}", left.as_str(), right.as_str());
        let string = self.gc.manage_str(result, self);
        self.push(Value::String(string));
        Ok(ip + 1)
      }
      (Value::Number(right), Value::Number(left)) => {
        self.push(Value::Number(left + right));
        Ok(ip + 1)
      }
      _ => self.runtime_error("Operands must be two numbers or two strings."),
    }
  }

  fn op_sub(&mut self, ip: u32) -> InterpretResult {
    match (self.pop(), self.pop()) {
      (Value::Number(right), Value::Number(left)) => {
        self.push(Value::Number(left - right));
        Ok(ip + 1)
      }
      _ => self.runtime_error("Operands must be numbers."),
    }
  }

  fn op_mul(&mut self, ip: u32) -> InterpretResult {
    match (self.pop(), self.pop()) {
      (Value::Number(right), Value::Number(left)) => {
        self.push(Value::Number(left * right));
        Ok(ip + 1)
      }
      _ => self.runtime_error("Operands must be numbers."),
    }
  }

  fn op_div(&mut self, ip: u32) -> InterpretResult {
    match (self.pop(), self.pop()) {
      (Value::Number(right), Value::Number(left)) => {
        self.push(Value::Number(left / right));
        Ok(ip + 1)
      }
      _ => self.runtime_error("Operands must be numbers."),
    }
  }

  fn op_less(&mut self, ip: u32) -> InterpretResult {
    match (self.pop(), self.pop()) {
      (Value::Number(right), Value::Number(left)) => {
        self.push(Value::Bool(left < right));
        Ok(ip + 1)
      }
      _ => self.runtime_error("Operands must be numbers."),
    }
  }

  fn op_greater(&mut self, ip: u32) -> InterpretResult {
    match (self.pop(), self.pop()) {
      (Value::Number(right), Value::Number(left)) => {
        self.push(Value::Bool(left > right));
        Ok(ip + 1)
      }
      _ => self.runtime_error("Operands must be numbers."),
    }
  }

  fn op_equal(&mut self, ip: u32) -> InterpretResult {
    let right = self.pop();
    let left = self.pop();

    self.push(Value::Bool(left == right));
    Ok(ip + 1)
  }

  fn op_method(&mut self, ip: u32) -> InterpretResult {
    let slot = self.read_byte(ip + 1);
    let name = self.read_string(slot);

    match (self.peek(1), self.peek(0)) {
      (Value::Class(ref mut class), Value::Closure(_)) => {
        let method = self.peek(0);
        if *name == INIT {
          class.init = Some(method);
        }
        class.methods.insert(name, method);
      }
      _ => panic!("Internal spacelox error. stack invalid for op_method"),
    }

    self.pop();
    Ok(ip + 2)
  }

  fn op_closure(&mut self, ip: u32) -> InterpretResult {
    let slot = self.read_byte(ip + 1);
    let fun = self.read_constant(slot).to_fun();
    let mut closure = Closure::new(fun);
    let mut current_ip = ip + 2;

    for _ in 0..fun.upvalue_count {
      let upvalue_index: UpvalueIndex = unsafe { mem::transmute(self.read_short(current_ip)) };

      match upvalue_index {
        UpvalueIndex::Local(index) => {
          let total_index = self.current_frame.slots as usize + index as usize;
          closure
            .upvalues
            .push(self.capture_upvalue(NonNull::from(&self.stack[total_index])));
        }
        UpvalueIndex::Upvalue(upvalue) => {
          let upvalue = self.current_frame.closure.upvalues[upvalue as usize];
          closure.upvalues.push(upvalue);
        }
      }

      current_ip += 2;
    }

    let closure = Value::Closure(self.gc.manage(closure, self));
    self.push(closure);
    Ok(current_ip)
  }

  fn op_close_upvalue(&mut self, ip: u32) -> InterpretResult {
    self.close_upvalues(NonNull::from(&self.stack[self.stack_top as usize - 1]));
    self.pop();
    Ok(ip + 1)
  }

  fn op_print(&mut self, ip: u32) -> InterpretResult {
    self.io.stdio().println(&format!("{}", self.pop()));
    Ok(ip + 1)
  }

  fn op_constant(&mut self, ip: u32) -> InterpretResult {
    let slot = self.read_byte(ip + 1);
    let constant = self.read_constant(slot);
    self.push(constant);
    Ok(ip + 2)
  }

  fn resolve_call(&mut self, callee: Value, arg_count: u8, ip: u32) -> InterpretResult {
    match callee {
      Value::Closure(closure) => self.call(closure, arg_count, ip),
      Value::Method(method) => self.call_method(method, arg_count, ip),
      Value::NativeFun(native_fun) => self.call_native_fun(native_fun, arg_count, ip),
      Value::NativeMethod(native_method) => self.call_native_method(native_method, arg_count, ip),
      Value::Class(class) => self.call_class(class, arg_count, ip),
      Value::Fun(fun) => panic!("function {} was not wrapped in a closure", fun.name),
      _ => self.runtime_error("Can only call functions and classes."),
    }
  }

  fn call_class(&mut self, class: Managed<Class>, arg_count: u8, ip: u32) -> InterpretResult {
    let value = Value::Instance(self.gc.manage(Instance::new(class), self));
    self.set_val(self.stack_top - (arg_count as usize) - 1, value);

    match class.init {
      Some(init) => self.resolve_call(init, arg_count, ip),
      None => {
        if arg_count != 0 {
          self.runtime_error(&format!("Expected 0 arguments but got {}", arg_count))
        } else {
          Ok(ip)
        }
      }
    }
  }

  /// call a native function immediately returning the result
  fn call_native_fun(
    &mut self,
    native: Managed<Box<dyn NativeFun>>,
    arg_count: u8,
    ip: u32,
  ) -> InterpretResult {
    let meta = native.meta();

    // check that the current function is called with the right number of args
    if let Some(error) = self.check_arity(meta.arity, arg_count, || String::from(meta.name)) {
      return error;
    }

    let args = unsafe {
      self
        .stack
        .get_unchecked((self.stack_top - arg_count as usize) as usize..self.stack_top as usize)
    };

    match native.call(&self.gc, self, args) {
      NativeResult::Success(value) => {
        self.stack_top -= arg_count as usize + 1;
        self.push(value);
        Ok(ip)
      }
      NativeResult::RuntimeError(message) => self.runtime_error(&message),
    }
  }

  /// call a native method immediately returning the result
  fn call_native_method(
    &mut self,
    native: Managed<Box<dyn NativeMethod>>,
    arg_count: u8,
    ip: u32,
  ) -> InterpretResult {
    let meta = native.meta();

    // check that the current function is called with the right number of args
    if let Some(error) = self.check_arity(meta.arity, arg_count, || String::from(meta.name)) {
      return error;
    }

    let args = unsafe {
      self
        .stack
        .get_unchecked((self.stack_top - arg_count as usize) as usize..self.stack_top as usize)
    };

    match native.call(
      &self.gc,
      self,
      self.get_val(self.stack_top - arg_count as usize - 1),
      args,
    ) {
      NativeResult::Success(value) => {
        self.stack_top -= arg_count as usize + 1;
        self.push(value);
        Ok(ip)
      }
      NativeResult::RuntimeError(message) => self.runtime_error(&message),
    }
  }

  /// call a spacelox function setting it as the new call frame
  fn call(&mut self, closure: Managed<Closure>, arg_count: u8, ip: u32) -> InterpretResult {
    // check that the current function is called with the right number of args
    if let Some(error) = self.check_arity(closure.fun.arity, arg_count, || {
      closure.fun.name.to_string()
    }) {
      return error;
    }

    // set the current current instruction pointer. check for overflow
    match self.frame_count {
      0 => (),
      FRAME_MAX => {
        return self.runtime_error("Stack overflow.");
      }
      _ => self.current_mut_frame().ip = ip,
    }

    let frame = &mut self.frames[self.frame_count];
    frame.closure = closure;
    frame.ip = 0;
    frame.slots = self.stack_top as u32 - (arg_count as u32 + 1);

    self.current_frame = *frame;
    self.current_fun = closure.fun;
    self.frame_count += 1;
    Ok(0)
  }

  /// check that the number of args is valid for the function arity
  fn check_arity<F>(&mut self, arity: ArityKind, arg_count: u8, name: F) -> Option<InterpretResult>
  where
    F: Fn() -> String,
  {
    match arity {
      // if fixed we need exactly the correct amount
      ArityKind::Fixed(arity) => {
        if arg_count != arity {
          return Some(self.runtime_error(&format!(
            "Function {} expected {} argument(s) but got {}.",
            name(),
            arity,
            arg_count,
          )));
        }
      }
      // if variadic and ending with ... take arity +
      ArityKind::Variadic(arity) => {
        if arg_count < arity {
          return Some(self.runtime_error(&format!(
            "Function {} expected at least {} argument(s) but got {}.",
            name(),
            arity,
            arg_count,
          )));
        }
      }
    }

    None
  }

  /// Call a bound method
  fn call_method(&mut self, bound: Managed<Method>, arg_count: u8, ip: u32) -> InterpretResult {
    self.set_val(self.stack_top - (arg_count as usize) - 1, bound.receiver);
    self.resolve_call(bound.method, arg_count, ip)
  }

  /// bind a method to an instance
  fn bind_method(
    &mut self,
    class: Managed<Class>,
    name: Managed<String>,
    ip: u32,
  ) -> InterpretResult {
    match class.methods.get(&name) {
      Some(method) => {
        let bound = self.gc.manage(Method::new(self.peek(0), *method), self);
        self.set_val(self.stack_top - 1, Value::Method(bound));
        Ok(ip)
      }
      None => self.runtime_error(&format!("Undefined property {}", name.as_str())),
    }
  }

  /// invoke a method from the provided class
  fn invoke_from_class(
    &mut self,
    class: Managed<Class>,
    method_name: Managed<String>,
    arg_count: u8,
    ip: u32,
  ) -> InterpretResult {
    match class.methods.get(&method_name) {
      Some(method) => self.resolve_call(*method, arg_count, ip),
      None => self.runtime_error(&format!("Undefined property {}.", method_name.as_str())),
    }
  }

  /// capture an upvalue return an existing upvalue if already captured
  fn capture_upvalue(&mut self, local_index: NonNull<Value>) -> Value {
    let closest_upvalue = self
      .open_upvalues
      .iter()
      .rev()
      .find(|upvalue| match ***upvalue {
        Upvalue::Open(index) => index <= local_index,
        Upvalue::Closed(_) => panic!("Encountered closed upvalue!"),
      });

    if let Some(upvalue) = closest_upvalue {
      if let Upvalue::Open(index) = **upvalue {
        if index == local_index {
          return Value::Upvalue(*upvalue);
        }
      }
    }

    let created_upvalue = self.gc.manage(Upvalue::Open(local_index), self);
    self.open_upvalues.push(created_upvalue);

    Value::Upvalue(created_upvalue)
  }

  /// hoist all open upvalue above the last index
  fn close_upvalues(&mut self, last_index: NonNull<Value>) {
    for upvalue in self.open_upvalues.iter_mut().rev() {
      let index = match **upvalue {
        Upvalue::Open(index) => index,
        Upvalue::Closed(_) => panic!("Unexpected closed upvalue"),
      };

      if index < last_index {
        break;
      }

      upvalue.hoist();
    }

    self.open_upvalues.retain(|upvalue| upvalue.is_open())
  }

  #[cfg(feature = "debug")]
  fn print_debug(&self, ip: usize, last_ip: usize) {
    let stdio = self.io.stdio();
    stdio.print("Stack:        ");

    for i in 1..self.stack_top {
      stdio.print(&format!("[ {} ]", self.get_val(i)));
    }
    stdio.println("");

    #[cfg(feature = "debug_upvalue")]
    {
      stdio.print("Open UpVal:   ");
      self
        .open_upvalues
        .iter()
        .for_each(|upvalue| match &**upvalue {
          Upvalue::Open(stack_ptr) => {
            stdio.print(&format!("[ stack {} ]", unsafe { *stack_ptr.as_ref() }));
          }
          Upvalue::Closed(closed) => {
            stdio.print(&format!("[ heap {} ]", closed));
          }
        });
      stdio.println("");
    }

    disassemble_instruction(&stdio, &self.current_fun.chunk, ip, last_ip);
  }

  /// Report a known spacelox runtime error to the user
  fn runtime_error(&mut self, message: &str) -> InterpretResult {
    self.error(message);
    Err(Interpret::RuntimeError)
  }

  /// Print an error message and the current call stack to the user
  fn error(&mut self, message: &str) {
    let stdio = self.io.stdio();
    stdio.eprintln(message);
    stdio.eprintln(&format!(""));

    for frame in self.frames[0..self.frame_count].iter().rev() {
      let closure = &frame.closure;
      let location: String = match &**closure.fun.name {
        SCRIPT => SCRIPT.to_owned(),
        _ => format!("{}()", closure.fun.name),
      };

      stdio.eprintln(&format!(
        "[line {}] in {}",
        closure.fun.chunk.get_line(frame.ip as usize),
        location
      ));
    }

    self.reset_stack();
  }
}

impl<'a, I: Io> Trace for VmExecutor<'a, I> {
  fn trace(&self) -> bool {
    self.stack[0..self.stack_top as usize]
      .iter()
      .for_each(|value| {
        value.trace();
      });

    self.frames[0..self.frame_count].iter().for_each(|frame| {
      frame.closure.trace();
    });

    self.open_upvalues.iter().for_each(|upvalue| {
      upvalue.trace();
    });

    self.globals.iter().for_each(|(key, val)| {
      key.trace();
      val.trace();
    });

    self.builtin.trace();

    true
  }

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    self.stack[0..self.stack_top as usize]
      .iter()
      .for_each(|value| {
        value.trace_debug(stdio);
      });

    self.frames[0..self.frame_count].iter().for_each(|frame| {
      frame.closure.trace_debug(stdio);
    });

    self.open_upvalues.iter().for_each(|upvalue| {
      upvalue.trace_debug(stdio);
    });

    self.globals.iter().for_each(|(key, val)| {
      key.trace_debug(stdio);
      val.trace_debug(stdio);
    });

    self.builtin.trace_debug(stdio);
    true
  }
}

/// Is the provided `value` falsey according to spacelox rules
#[inline]
fn is_falsey(value: Value) -> bool {
  match value {
    Value::Nil => true,
    Value::Bool(b) => !b,
    _ => false,
  }
}
