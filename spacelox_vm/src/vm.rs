use crate::call_frame::CallFrame;
use crate::compiler::{Compiler, CompilerResult, Parser};
use crate::constants::{DEFAULT_STACK_MAX, FRAME_MAX};
use spacelox_core::hooks::NoContext;
use spacelox_core::hooks::{HookContext, Hooks};
use spacelox_core::{
  arity::{ArityError, ArityKind},
  chunk::{ByteCode, UpvalueIndex},
  constants::{PLACEHOLDER_NAME, SCRIPT},
  io::{Io, NativeIo, StdIo},
  managed::{Managed, Trace},
  memory::{Gc, NO_GC},
  native::{NativeFun, NativeMethod},
  utils::{is_falsey, ptr_len, use_sentinel_nan},
  value::{VALUE_NIL, Value, ValueVariant},
  object::{BuiltInClasses, Class, Closure, Fun, Instance, Method, Upvalue},
  CallResult, SlError, SlHashMap,
};
use spacelox_lib::{assert::assert_funs, builtin::make_builtin_classes, time::clock_funs};
use std::convert::TryInto;
use std::mem;
use std::ptr;
use std::ptr::NonNull;

#[cfg(feature = "debug")]
use crate::debug::disassemble_instruction;

#[derive(Debug, Clone, PartialEq)]
enum Signal {
  Ok,
  OkReturn,
  Exit,
  RuntimeError,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExecuteResult {
  Ok,
  FunResult(Value),
  RuntimeError,
  CompileError,
}

pub enum RunMode {
  Normal,
  CallFunction(usize),
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
  globals: SlHashMap<Managed<String>, Value>,
}

impl<I: Io> Vm<I> {
  pub fn new(io: I) -> Vm<I> {
    let gc = Gc::new(Box::new(io.stdio()));
    let fun = Fun::new(gc.manage_str(String::from(PLACEHOLDER_NAME), &NO_GC));

    let managed_fun = gc.manage(fun, &NO_GC);
    let closure = gc.manage(Closure::new(managed_fun), &NO_GC);

    let frames = vec![CallFrame::new(closure); FRAME_MAX];
    let stack = vec![VALUE_NIL; DEFAULT_STACK_MAX];

    let mut no_gc_context = NoContext::new(&gc);
    let mut hooks = Hooks::new(&mut no_gc_context);
    let mut natives = Vec::new();
    natives.extend(assert_funs(&mut hooks).into_iter());
    natives.extend(clock_funs().into_iter());

    let builtin = make_builtin_classes(&mut hooks).expect("Could not make built in classes.");
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

  pub fn run(&mut self, source: &str) -> ExecuteResult {
    self.interpret(source)
  }

  /// Interpret the provided spacelox script returning the execution result
  fn interpret(&mut self, source: &str) -> ExecuteResult {
    let result = self.compile(source);

    if !result.success {
      return ExecuteResult::CompileError;
    }

    let script_closure = self.gc.manage(Closure::new(result.fun), &NO_GC);
    let script = Value::from(script_closure);
    let mut executor = VmExecutor::new(self, script);
    executor.run(RunMode::Normal)
  }

  /// Compile the provided spacelox source into the virtual machine's bytecode
  fn compile(&mut self, source: &str) -> CompilerResult {
    let mut parser = Parser::new(self.io.stdio(), source);

    let mut compiler_context = NoContext::new(&self.gc);
    let hooks = Hooks::new(&mut compiler_context);

    let compiler = Compiler::new(self.io, &mut parser, &hooks);
    compiler.compile()
  }
}

impl<I: Io> From<VmDependencies<I>> for Vm<I> {
  /// Construct a vm from a set of dependencies. This is meant to be used when targeting
  /// different environments
  fn from(dependencies: VmDependencies<I>) -> Self {
    let gc = dependencies.gc;
    let builtin = make_builtin_classes(&Hooks::new(&mut NoContext::new(&gc)))
      .expect("Could not create builtin classes.");
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

fn define_globals(gc: &Gc, natives: Vec<Box<dyn NativeFun>>) -> SlHashMap<Managed<String>, Value> {
  let mut globals = SlHashMap::with_capacity_and_hasher(natives.len(), Default::default());

  natives.into_iter().for_each(|native| {
    let name = gc.manage_str(native.meta().name.to_string(), &NO_GC);
    let native_value = Value::from(gc.manage(native, &NO_GC));

    globals.insert(name, native_value);
  });

  globals
}

struct VmExecutor<'a, I: Io + 'static> {
  /// A stack of call frames for the current execution
  frames: &'a mut Vec<CallFrame>,

  /// A stack holding all local variable currently in use
  stack: &'a mut [Value],

  /// global variable present in the vm
  globals: &'a mut SlHashMap<Managed<String>, Value>,

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

  /// The current error if one is active
  current_error: Option<SlError>,

  /// index to the top of the value stack
  stack_top: *mut Value,

  /// The current frame depth of the program
  frame_count: usize,
}

impl<'a, I: Io> VmExecutor<'a, I> {
  /// Create an instance of the vm executor that can execute the provided script.
  pub fn new(vm: &'a mut Vm<I>, script: Value) -> VmExecutor<'a, I> {
    let current_frame = vm.frames[0];
    let current_fun = current_frame.closure.fun;
    let stack_top = &mut vm.stack[1] as *mut Value;

    let mut executor = VmExecutor {
      frames: &mut vm.frames,
      frame_count: 1,
      stack: &mut vm.stack,
      script,
      current_fun,
      current_frame,
      current_error: None,
      builtin: &vm.builtin,
      gc: &mut vm.gc,
      io: &mut vm.io,
      stack_top,
      globals: &mut vm.globals,
      open_upvalues: Vec::with_capacity(100),
    };

    executor.current_frame.ip = &executor.script.to_closure().fun.chunk().instructions[0];
    let result = executor.call(executor.script.to_closure(), 0);
    match result {
      Signal::Ok => executor,
      _ => panic!("Script call failed"),
    }
  }

  /// Run a spacelox function on top of the current stack.
  /// This acts as a hook for native functions to execute spacelox function
  fn run_fun(&mut self, callable: Value, args: &[Value]) -> ExecuteResult {
    for arg in args {
      self.push(*arg);
    }

    let mode = RunMode::CallFunction(self.frame_count);
    match self.resolve_call(callable, args.len() as u8) {
      Signal::Ok => self.run(mode),
      Signal::OkReturn => ExecuteResult::FunResult(self.pop()),
      Signal::RuntimeError => ExecuteResult::RuntimeError,
      _ => panic!("TODO"),
    }
  }

  /// Run a spacelox method on top of the current stack.
  /// This acts as a hook for native functions to execute spacelox function
  fn run_method(&mut self, this: Value, method: Value, args: &[Value]) -> ExecuteResult {
    self.push(this);
    for arg in args {
      self.push(*arg);
    }

    let mode = RunMode::CallFunction(self.frame_count);
    match self.resolve_call(method, args.len() as u8) {
      Signal::Ok => self.run(mode),
      Signal::OkReturn => ExecuteResult::FunResult(self.pop()),
      Signal::RuntimeError => ExecuteResult::RuntimeError,
      _ => panic!("TODO"),
    }
  }

  /// Run a method on a spacelox value on top of the current stack.
  /// This acts as a hook for native function to execute spacelox methods
  pub fn run_method_by_name(
    &mut self,
    this: Value,
    method_name: Managed<String>,
    args: &[Value],
  ) -> ExecuteResult {
    let class = this.value_class(self.builtin);

    self.push(this);
    for arg in args {
      self.push(*arg);
    }

    let mode = RunMode::CallFunction(self.frame_count);
    match self.invoke_from_class(class, method_name, args.len() as u8) {
      Signal::Ok => self.run(mode),
      Signal::OkReturn => ExecuteResult::FunResult(self.pop()),
      Signal::RuntimeError => ExecuteResult::RuntimeError,
      _ => panic!("TODO"),
    }
  }

  /// Main virtual machine execution loop. This will run the until the program interrupts
  /// from a normal exit or from a runtime error.
  pub fn run(&mut self, mode: RunMode) -> ExecuteResult {
    #[cfg(feature = "debug")]
    let mut last_ip: *const u8 = &self.current_fun.chunk().instructions[0] as *const u8;

    loop {
      // get the current instruction
      let op_code: ByteCode = ByteCode::from(self.read_byte());

      #[cfg(feature = "debug")]
      {
        let ip = unsafe { self.current_frame.ip.offset(-1) };
        self.print_debug(ip, last_ip);
        last_ip = ip;
      }

      // execute the decoded instruction
      let result = match op_code {
        ByteCode::Negate => self.op_negate(),
        ByteCode::Add => self.op_add(),
        ByteCode::Subtract => self.op_sub(),
        ByteCode::Multiply => self.op_mul(),
        ByteCode::Divide => self.op_div(),
        ByteCode::Not => self.op_not(),
        ByteCode::Equal => self.op_equal(),
        ByteCode::Greater => self.op_greater(),
        ByteCode::Less => self.op_less(),
        ByteCode::JumpIfFalse => self.op_jump_if_not_false(),
        ByteCode::Jump => self.op_jump(),
        ByteCode::Loop => self.op_loop(),
        ByteCode::DefineGlobal => self.op_define_global(),
        ByteCode::GetIndex => self.op_get_index(),
        ByteCode::SetIndex => self.op_set_index(),
        ByteCode::GetGlobal => self.op_get_global(),
        ByteCode::SetGlobal => self.op_set_global(),
        ByteCode::GetLocal => self.op_get_local(),
        ByteCode::SetLocal => self.op_set_local(),
        ByteCode::GetUpvalue => self.op_get_upvalue(),
        ByteCode::SetUpvalue => self.op_set_upvalue(),
        ByteCode::GetProperty => self.op_get_property(),
        ByteCode::SetProperty => self.op_set_property(),
        ByteCode::Pop => self.op_pop(),
        ByteCode::Nil => self.op_literal(VALUE_NIL),
        ByteCode::True => self.op_literal(Value::from(true)),
        ByteCode::False => self.op_literal(Value::from(false)),
        ByteCode::List => self.op_literal(Value::from(self.gc.manage(Vec::new(), self))),
        ByteCode::ListInit => self.op_list(),
        ByteCode::Map => self.op_literal(Value::from(self.gc.manage(SlHashMap::default(), self))),
        ByteCode::MapInit => self.op_map(),
        ByteCode::IterNext => self.op_iter_next(),
        ByteCode::IterCurrent => self.op_iter_current(),
        ByteCode::Constant => self.op_constant(),
        ByteCode::Print => self.op_print(),
        ByteCode::Call => self.op_call(),
        ByteCode::Invoke => self.op_invoke(),
        ByteCode::SuperInvoke => self.op_super_invoke(),
        ByteCode::Closure => self.op_closure(),
        ByteCode::Method => self.op_method(),
        ByteCode::Class => self.op_class(),
        ByteCode::Inherit => self.op_inherit(),
        ByteCode::GetSuper => self.op_get_super(),
        ByteCode::CloseUpvalue => self.op_close_upvalue(),
        ByteCode::Return => self.op_return(),
      };

      match result {
        Signal::OkReturn => {
          if let RunMode::CallFunction(depth) = mode {
            if depth == self.frame_count {
              return ExecuteResult::FunResult(self.get_val(-1));
            }
          }
        }
        Signal::Ok => (),
        Signal::RuntimeError => {
          let current_error = self.current_error.clone();
          match &current_error {
            Some(error) => {
              self.error(error);
              return ExecuteResult::RuntimeError;
            }
            None => panic!("Runtime Error was not set."),
          }
        }
        Signal::Exit => {
          return ExecuteResult::Ok;
        }
      }
    }
  }

  /// Get an immutable reference to the current callframe
  #[inline]
  fn current_frame(&self) -> &CallFrame {
    unsafe { self.frames.get_unchecked(self.frame_count - 1) }
  }

  /// Get an immutable reference to value on the stack
  #[inline]
  fn get_val(&self, offset: isize) -> Value {
    unsafe { *self.stack_top.offset(offset) }
  }

  /// Set a value on the stack
  #[inline]
  fn set_val(&mut self, offset: isize, val: Value) {
    unsafe { *self.stack_top.offset(offset) = val }
  }

  /// Update the current instruction pointer
  #[inline]
  fn update_ip(&mut self, offset: isize) {
    unsafe { self.current_frame.ip = self.current_frame.ip.offset(offset) };
  }

  /// read a u8 out of the bytecode
  #[inline]
  fn read_byte(&mut self) -> u8 {
    let byte = unsafe { *self.current_frame.ip };
    self.update_ip(1);
    byte
  }

  /// read a u16 out of the bytecode
  #[inline]
  fn read_short(&mut self) -> u16 {
    let slice = unsafe { std::slice::from_raw_parts(self.current_frame.ip, 2) };
    let buffer = slice.try_into().expect("slice of incorrect length.");
    let short = u16::from_ne_bytes(buffer);
    self.update_ip(2);

    short
  }

  /// push a value onto the stack
  #[inline]
  fn push(&mut self, value: Value) {
    self.set_val(0, value);
    self.stack_top = unsafe { self.stack_top.offset(1) };
  }

  /// pop a value off the stack
  #[inline]
  fn pop(&mut self) -> Value {
    self.stack_top = unsafe { self.stack_top.offset(-1) };
    self.get_val(0)
  }

  /// reference a value n slots from the stack head
  #[inline]
  fn peek(&self, distance: isize) -> Value {
    self.get_val(-(distance as isize) - 1)
  }

  /// read a constant from the current chunk
  #[inline]
  fn read_constant(&self, index: u8) -> Value {
    unsafe {
      *self
        .current_fun
        .chunk()
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
    self.stack_top = &mut self.stack[1] as *mut Value;
    self.frame_count = 1;
    self.open_upvalues.clear();
  }

  /// push a literal value onto the stack
  fn op_literal(&mut self, value: Value) -> Signal {
    self.push(value);
    Signal::Ok
  }

  /// pop a value off the stack
  fn op_pop(&mut self) -> Signal {
    self.pop();
    Signal::Ok
  }

  /// create a list from a list literal
  fn op_list(&mut self) -> Signal {
    let arg_count = self.read_short();
    let args = unsafe {
      std::slice::from_raw_parts(
        self.stack_top.offset(-(arg_count as isize)),
        arg_count as usize,
      )
    };
    let mut list = self.peek(arg_count as isize).to_list();
    self.gc.grow(&mut list, self, |list| list.extend(args));
    self.stack_top = unsafe { self.stack_top.offset(-(arg_count as isize)) };

    Signal::Ok
  }

  /// create a list from a list literal
  fn op_map(&mut self) -> Signal {
    let arg_count = self.read_short();
    let mut map = self.peek(arg_count as isize * 2).to_map();
    self
      .gc
      .grow(&mut map, self, |map| map.reserve(arg_count as usize));

    for i in 0..arg_count {
      let key = self.get_val(-(i as isize * 2) - 2);
      let value = self.get_val(-(i as isize * 2) - 1);

      map.insert(key, value);
    }
    self.stack_top = unsafe { self.stack_top.offset(-(arg_count as isize * 2)) };
    Signal::Ok
  }

  /// move an iterator to the next element
  fn op_iter_next(&mut self) -> Signal {
    let receiver = self.peek(0);

    if receiver.is_iter() {
      self.update_ip(1);
      match receiver.to_iter().next(&mut Hooks::new(self)) {
        Ok(value) => {
          self.set_val(-1, value);
          Signal::Ok
        }
        Err(error) => self.set_error(error),
      }
    } else {
      let constant = self.read_byte();
      let method_name = self.read_string(constant);
      self.invoke(receiver, method_name, 0)
    }
  }

  /// get the current value from an iterator
  fn op_iter_current(&mut self) -> Signal {
    let value = self.peek(0);

    if value.is_iter() {
      self.update_ip(1);
      let result = value.to_iter().current();
      self.set_val(-1, result);
      Signal::Ok
    } else {
      let slot = self.read_byte();
      let name = self.read_string(slot);
      self.get_property(value, name)
    }
  }

  /// call a function or method
  fn op_call(&mut self) -> Signal {
    let arg_count = self.read_byte();
    let callee = self.peek(arg_count as isize);

    self.resolve_call(callee, arg_count)
  }

  /// invoke a method on an instance's class
  fn op_invoke(&mut self) -> Signal {
    let constant = self.read_byte();
    let arg_count = self.read_byte();

    let method_name = self.read_string(constant);
    let receiver = self.peek(arg_count as isize);

    self.invoke(receiver, method_name, arg_count)
  }

  /// invoke a method
  fn invoke(&mut self, receiver: Value, method_name: Managed<String>, arg_count: u8) -> Signal {
    let class = receiver.value_class(&self.builtin);
    if receiver.is_instance() {
      let instance = receiver.to_instance();
      if let Some(field) = instance.get_field(&method_name) {
        self.set_val(-(arg_count as isize) - 1, *field);
        return self.resolve_call(*field, arg_count);
      }
    }

    self.invoke_from_class(class, method_name, arg_count)
  }

  /// Invoke a method on a instance's super class
  fn op_super_invoke(&mut self) -> Signal {
    let constant = self.read_byte();
    let arg_count = self.read_byte();

    let method_name = self.read_string(constant);
    let super_class = self.pop().to_class();

    self.invoke_from_class(super_class, method_name, arg_count)
  }

  /// Generate a new class
  fn op_class(&mut self) -> Signal {
    let slot = self.read_byte();
    let name = self.read_string(slot);
    let class = Value::from(self.gc.manage(Class::new(name), self));
    self.push(class);
    Signal::Ok
  }

  /// Get this classes super class
  fn op_get_super(&mut self) -> Signal {
    let slot = self.read_byte();
    let name = self.read_string(slot);
    let super_class = self.pop().to_class();

    self.bind_method(super_class, name)
  }

  /// Inherit a class by directly copying methods onto the subclass.
  fn op_inherit(&mut self) -> Signal {
    let mut class = self.peek(0).to_class();

    let peek = self.peek(1);
    if peek.is_class() {
      class.inherit(&Hooks::new(self), peek.to_class());

      self.pop();
      Signal::Ok
    } else {
      self.runtime_error("Superclass must be a class.")
    }
  }

  /// Loop by performing an unconditional jump to a new instruction
  fn op_loop(&mut self) -> Signal {
    let jump = self.read_byte() as isize;
    self.update_ip(1 - jump);
    Signal::Ok
  }

  fn op_jump_if_not_false(&mut self) -> Signal {
    let jump = self.read_short();
    if is_falsey(self.peek(0)) {
      self.update_ip(jump as isize);
      return Signal::Ok;
    }

    Signal::Ok
  }

  fn op_jump(&mut self) -> Signal {
    let jump = self.read_short();
    self.update_ip(jump as isize);
    Signal::Ok
  }

  fn op_define_global(&mut self) -> Signal {
    let slot = self.read_byte();
    let name = self.read_string(slot);
    let global = self.pop();
    self.globals.insert(name, global);
    Signal::Ok
  }

  fn op_set_index(&mut self) -> Signal {
    let target = self.peek(2);
    let index = self.peek(1);

    if target.is_list() && index.is_num() {
      let mut list = target.to_list();
      let num = index.to_num();

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
      return Signal::Ok
    } else if target.is_map() {
      let mut map = target.to_map();

      if index.is_num() {
        let num = index.to_num();
        let value = self.pop();
        self.gc.grow(&mut map, self, |map| {
          map.insert(Value::from(use_sentinel_nan(num)), value)
        });
        self.pop();
        return Signal::Ok
      } else {
        let value = self.pop();
        self.gc.grow(&mut map, self, |map| map.insert(index, value));
        self.pop();
        return Signal::Ok
      }
    }
    
    self.runtime_error(&format!("{} cannot be indexed by {}.", target.value_type(), index.value_type()))
  }

  fn op_set_global(&mut self) -> Signal {
    let slot = self.read_byte();
    let string = self.read_string(slot);

    if self.globals.insert(string, self.peek(0)).is_none() {
      self.globals.remove_entry(&string);
      return self.runtime_error(&format!("Undefined variable {}", string.as_str()));
    }

    Signal::Ok
  }

  fn op_set_local(&mut self) -> Signal {
    let slot = self.read_byte() as isize;
    let copy = self.peek(0);
    let slots = self.current_frame.slots;

    unsafe { *slots.offset(slot) = copy }

    Signal::Ok
  }

  fn op_set_property(&mut self) -> Signal {
    let slot = self.read_byte();
    let value = self.peek(1);
    let name = self.read_string(slot);

    if value.is_instance() {
      let mut instance = value.to_instance();
      let value = self.peek(0);
      instance.set_field(&Hooks::new(self), name, value);

      let popped = self.pop();
      self.pop();
      self.push(popped);

      return Signal::Ok;
    }

    self.runtime_error("Only instances have settable fields.")
  }

  fn op_set_upvalue(&mut self) -> Signal {
    let slot = self.read_byte();
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

    Signal::Ok
  }

  fn op_get_index(&mut self) -> Signal {
    let index = self.pop();
    let target = self.pop();


    if target.is_list() && index.is_num() {
      let list = target.to_list();
      let num = index.to_num();

      let rounded = num as usize;
      if rounded >= list.len() {
        return self.runtime_error(&format!(
          "Index out of bounds. list was length 0 but attempted to index with {}.",
          rounded
        ));
      }

      self.push(list[rounded]);
      return Signal::Ok;
    } else if target.is_map() {
      let map = target.to_map();

      if index.is_num() {
        let num = index.to_num();
        return match map.get(&Value::from(use_sentinel_nan(num))) {
          Some(value) => {
            self.push(*value);
            Signal::Ok
          }
          None => self.runtime_error(&format!("Key {} does not exist in map", index)),
        }
      } else {
        return match map.get(&index) {
          Some(value) => {
            self.push(*value);
            Signal::Ok
          }
          None => self.runtime_error(&format!("Key {} does not exist in map", index)),
        }
      }
    }

    self.runtime_error(&format!(
      "{} cannot be indexed with {}.",
      target.value_type(),
      index.value_type()
    ))
  }

  fn op_get_global(&mut self) -> Signal {
    let store_index = self.read_byte();
    let string = self.read_string(store_index);

    match self.globals.get(&string) {
      Some(gbl) => {
        let copy = *gbl;
        self.push(copy);
        Signal::Ok
      }
      None => self.runtime_error(&format!("Undefined variable {}", string.as_str())),
    }
  }

  fn op_get_local(&mut self) -> Signal {
    let slot = self.read_byte() as isize;
    let slots = self.current_frame.slots;

    let copy = unsafe { *slots.offset(slot) };

    self.push(copy);
    Signal::Ok
  }

  fn op_get_upvalue(&mut self) -> Signal {
    let slot = self.read_byte();
    let upvalue_value = &self.current_frame.closure.upvalues[slot as usize];

    let value = match &*upvalue_value.to_upvalue() {
      Upvalue::Open(stack_ptr) => *unsafe { stack_ptr.as_ref() },
      Upvalue::Closed(store) => **store,
    };

    self.push(value);

    Signal::Ok
  }

  fn op_get_property(&mut self) -> Signal {
    let slot = self.read_byte();
    let value = self.peek(0);
    let name = self.read_string(slot);

    self.get_property(value, name)
  }

  fn get_property(&mut self, value: Value, name: Managed<String>) -> Signal {
    let class = value.value_class(&self.builtin);

    if value.is_instance() {
      let instance = value.to_instance();
      
      if let Some(value) = instance.get_field(&name) {
        self.set_val(-1, *value);
        return Signal::Ok
      }
    } else if value.is_iter() {
      let iter = value.to_iter();

      if let Some(value) = iter.get_field(&name) {
        self.set_val(-1, *value);
        return Signal::Ok
      }
    }

    self.bind_method(class, name)
  }

  /// return from a spacelox function placing the result on top of the stack
  fn op_return(&mut self) -> Signal {
    // get the function result close upvalues and pop frame
    let result = self.pop();
    self.close_upvalues(NonNull::from(unsafe { &*self.current_frame.slots }));
    self.frame_count -= 1;

    // if the frame was the whole script signal an ok interrupt
    if self.frame_count == 1 {
      self.pop();
      return Signal::Exit;
    }

    // pull the current frame out of the stack and set the cached frame
    self.stack_top = self.frames[self.frame_count].slots;
    self.current_frame = *self.current_frame();
    self.current_fun = self.current_frame.closure.fun;

    // push the result onto the stack
    self.push(result);
    Signal::OkReturn
  }

  fn op_negate(&mut self) -> Signal {
    let pop = self.pop();

    if pop.is_num() {
      self.push(Value::from(-pop.to_num()));
      Signal::Ok
    } else {
      self.runtime_error("Operand must be a number.")
    }
  }

  fn op_not(&mut self) -> Signal {
    let value = self.pop();
    self.push(Value::from(is_falsey(value)));
    Signal::Ok
  }

  fn op_add(&mut self) -> Signal {
    let (right, left) = (self.pop(), self.pop());

    if right.is_str() && left.is_str() {
      let result = format!("{}{}", left.to_str(), right.to_str());
      let string = self.gc.manage_str(result, self);
      self.push(Value::from(string));
      Signal::Ok
    } else if right.is_num() && left.is_num() {
      self.push(Value::from(left.to_num() + right.to_num()));
      Signal::Ok
    } else {
      self.runtime_error("Operands must be two numbers or two strings.")
    }
  }

  fn op_sub(&mut self) -> Signal {
    let (right, left) = (self.pop(), self.pop());

    if right.is_num() && left.is_num() {
      self.push(Value::from(left.to_num() - right.to_num()));
      return Signal::Ok
    }

    self.runtime_error("Operands must be numbers.")
  }

  fn op_mul(&mut self) -> Signal {
    let (right, left) = (self.pop(), self.pop());

    if right.is_num() && left.is_num() {
      self.push(Value::from(left.to_num() * right.to_num()));
      return Signal::Ok
    }

    self.runtime_error("Operands must be numbers.")
  }

  fn op_div(&mut self) -> Signal {
    let (right, left) = (self.pop(), self.pop());

    if right.is_num() && left.is_num() {
      self.push(Value::from(left.to_num() / right.to_num()));
      return Signal::Ok
    }

    self.runtime_error("Operands must be numbers.")
  }

  fn op_less(&mut self) -> Signal {
    let (right, left) = (self.pop(), self.pop());

    if right.is_num() && left.is_num() {
      self.push(Value::from(left.to_num() < right.to_num()));
      return Signal::Ok
    }

    self.runtime_error("Operands must be numbers.")
  }

  fn op_greater(&mut self) -> Signal {
    let (right, left) = (self.pop(), self.pop());

    if right.is_num() && left.is_num() {
      self.push(Value::from(left.to_num() > right.to_num()));
      return Signal::Ok
    }

    self.runtime_error("Operands must be numbers.")
  }

  fn op_equal(&mut self) -> Signal {
    let right = self.pop();
    let left = self.pop();

    self.push(Value::from(left == right));
    Signal::Ok
  }

  fn op_method(&mut self) -> Signal {
    let slot = self.read_byte();
    let name = self.read_string(slot);

    let class = self.peek(1);
    let method = self.peek(0);

    if class.is_class() && method.is_closure() {
      class.to_class().add_method(&Hooks::new(self), name, method);
    } else {
      panic!("Internal spacelox error. stack invalid for op_method.");
    }

    self.pop();
    Signal::Ok
  }

  fn op_closure(&mut self) -> Signal {
    let slot = self.read_byte();
    let fun = self.read_constant(slot).to_fun();
    let mut closure = Closure::new(fun);

    for _ in 0..fun.upvalue_count {
      let upvalue_index: UpvalueIndex = unsafe { mem::transmute(self.read_short()) };

      match upvalue_index {
        UpvalueIndex::Local(index) => {
          let value = unsafe { &*self.current_frame.slots.offset(index as isize) };
          closure
            .upvalues
            .push(self.capture_upvalue(NonNull::from(value)));
        }
        UpvalueIndex::Upvalue(upvalue) => {
          let upvalue = self.current_frame.closure.upvalues[upvalue as usize];
          closure.upvalues.push(upvalue);
        }
      }
    }

    let closure = Value::from(self.gc.manage(closure, self));
    self.push(closure);
    Signal::Ok
  }

  fn op_close_upvalue(&mut self) -> Signal {
    self.close_upvalues(NonNull::from(unsafe { &*self.stack_top.offset(-1) }));
    self.pop();
    Signal::Ok
  }

  fn op_print(&mut self) -> Signal {
    self.io.stdio().println(&format!("{}", self.pop()));
    Signal::Ok
  }

  fn op_constant(&mut self) -> Signal {
    let slot = self.read_byte();
    let constant = self.read_constant(slot);
    self.push(constant);

    Signal::Ok
  }

  fn resolve_call(&mut self, callee: Value, arg_count: u8) -> Signal {
    match callee.kind() {
      ValueVariant::Closure => self.call(callee.to_closure(), arg_count),
      ValueVariant::Method => self.call_method(callee.to_method(), arg_count),
      ValueVariant::NativeFun => self.call_native_fun(callee.to_native_fun(), arg_count),
      ValueVariant::NativeMethod => self.call_native_method(callee.to_native_method(), arg_count),
      ValueVariant::Class => self.call_class(callee.to_class(), arg_count),
      ValueVariant::Fun => panic!("function {} was not wrapped in a closure", callee.to_fun().name),
      _ => self.runtime_error("Can only call functions and classes."),
    }
  }

  fn call_class(&mut self, class: Managed<Class>, arg_count: u8) -> Signal {
    let value = Value::from(self.gc.manage(Instance::new(class), self));
    self.set_val(-(arg_count as isize) - 1, value);

    match class.init {
      Some(init) => self.resolve_call(init, arg_count),
      None => {
        if arg_count != 0 {
          self.runtime_error(&format!("Expected 0 arguments but got {}", arg_count))
        } else {
          Signal::Ok
        }
      }
    }
  }

  /// call a native function immediately returning the result
  fn call_native_fun(&mut self, native: Managed<Box<dyn NativeFun>>, arg_count: u8) -> Signal {
    let meta = native.meta();

    // check that the current function is called with the right number of args
    if let Some(error) = self.check_arity(meta.arity, arg_count, || String::from(meta.name)) {
      return error;
    }

    let args = unsafe {
      std::slice::from_raw_parts(
        self.stack_top.offset(-(arg_count as isize)),
        arg_count as usize,
      )
    };

    match native.call(&mut Hooks::new(self), args) {
      Ok(value) => {
        self.stack_top = unsafe { self.stack_top.offset(-(arg_count as isize) - 1) };
        self.push(value);

        Signal::OkReturn
      }
      Err(error) => self.set_error(error),
    }
  }

  /// call a native method immediately returning the result
  fn call_native_method(
    &mut self,
    native: Managed<Box<dyn NativeMethod>>,
    arg_count: u8,
  ) -> Signal {
    let meta = native.meta();

    // check that the current function is called with the right number of args
    if let Some(error) = self.check_arity(meta.arity, arg_count, || String::from(meta.name)) {
      return error;
    }

    let args = unsafe {
      std::slice::from_raw_parts(
        self.stack_top.offset(-(arg_count as isize)),
        arg_count as usize,
      )
    };
    let this = self.get_val(-(arg_count as isize) - 1);

    match native.call(&mut Hooks::new(self), this, &args) {
      Ok(value) => {
        self.stack_top = unsafe { self.stack_top.offset(-(arg_count as isize) - 1) };
        self.push(value);

        Signal::OkReturn
      }
      Err(error) => self.set_error(error),
    }
  }

  /// call a spacelox function setting it as the new call frame
  fn call(&mut self, closure: Managed<Closure>, arg_count: u8) -> Signal {
    // check that the current function is called with the right number of args
    if let Some(error) = self.check_arity(closure.fun.arity, arg_count, || {
      closure.fun.name.to_string()
    }) {
      return error;
    }

    // set the current current instruction pointer. check for overflow
    if self.frame_count == FRAME_MAX {
      return self.runtime_error("Stack overflow.");
    }

    // let frame_ptr = &mut self.frames[self.frame_count - 1] as *mut CallFrame;
    self.frames[self.frame_count - 1] = self.current_frame;
    let frame = &mut self.frames[self.frame_count];
    frame.closure = closure;
    frame.ip = &closure.fun.chunk().instructions[0] as *const u8;
    frame.slots = unsafe { self.stack_top.offset(-(arg_count as isize) - 1) };

    self.current_frame = *frame;
    self.current_fun = closure.fun;
    self.frame_count += 1;

    Signal::Ok
  }

  /// check that the number of args is valid for the function arity
  fn check_arity<F>(&mut self, arity: ArityKind, arg_count: u8, name: F) -> Option<Signal>
  where
    F: Fn() -> String,
  {
    match arity.check(arg_count) {
      Ok(_) => None,
      Err(error) => match error {
        ArityError::Fixed(arity) => Some(self.runtime_error(&format!(
          "{} expected {} argument(s) but got {}.",
          name(),
          arity,
          arg_count,
        ))),
        ArityError::Variadic(arity) => Some(self.runtime_error(&format!(
          "{} expected at least {} argument(s) but got {}.",
          name(),
          arity,
          arg_count,
        ))),
        ArityError::DefaultLow(arity) => Some(self.runtime_error(&format!(
          "{} expected at least {} argument(s) but got {}.",
          name(),
          arity,
          arg_count,
        ))),
        ArityError::DefaultHigh(arity) => Some(self.runtime_error(&format!(
          "{} expected at most {} argument(s) but got {}.",
          name(),
          arity,
          arg_count,
        ))),
      },
    }
  }

  /// Call a bound method
  fn call_method(&mut self, bound: Managed<Method>, arg_count: u8) -> Signal {
    self.set_val(-(arg_count as isize) - 1, bound.receiver);
    self.resolve_call(bound.method, arg_count)
  }

  /// bind a method to an instance
  fn bind_method(&mut self, class: Managed<Class>, name: Managed<String>) -> Signal {
    match class.get_method(&name) {
      Some(method) => {
        let bound = self.gc.manage(Method::new(self.peek(0), *method), self);
        self.set_val(-1, Value::from(bound));
        Signal::Ok
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
  ) -> Signal {
    match class.get_method(&method_name) {
      Some(method) => self.resolve_call(*method, arg_count),
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
          return Value::from(*upvalue);
        }
      }
    }

    let created_upvalue = self.gc.manage(Upvalue::Open(local_index), self);
    self.open_upvalues.push(created_upvalue);

    Value::from(created_upvalue)
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

  /// Print debugging information for the current instruction
  #[cfg(feature = "debug")]
  fn print_debug(&self, ip: *const u8, last_ip: *const u8) {
    let stdio = self.io.stdio();

    self.print_stack_debug(&stdio);

    #[cfg(feature = "debug_upvalue")]
    self.print_upvalue_debug(&stdio);

    let start = &self.current_fun.chunk().instructions[0] as *const u8;
    let offset = ptr_len(start, ip);
    let last_offset = ptr_len(start, last_ip);
    disassemble_instruction(&stdio, &self.current_fun.chunk(), offset, last_offset);
  }

  /// Print the current stack
  #[cfg(feature = "debug")]
  fn print_stack_debug(&self, stdio: &I::StdIo) {
    stdio.print("Stack:        ");

    unsafe {
      let start = &self.stack[1] as *const Value;
      let len = ptr_len(start, self.stack_top);
      let slice = std::slice::from_raw_parts(start, len);

      for value in slice {
        stdio.print(&format!("[ {} ]", *value));
      }
    }

    stdio.println("");
  }

  /// Print the current upvalues
  #[cfg(feature = "debug_upvalue")]
  fn print_upvalue_debug(&self, stdio: &I::StdIo) {
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

  /// Convert an execute result to a call result
  fn to_call_result(&self, execute_result: ExecuteResult) -> CallResult {
    match execute_result {
      ExecuteResult::FunResult(value) => Ok(value),
      ExecuteResult::Ok => panic!("Accidental early exit in hook call."),
      ExecuteResult::CompileError => panic!("Compiler error should occur before code is executed."),
      ExecuteResult::RuntimeError => match self.current_error.clone() {
        Some(error) => Err(error),
        None => panic!("Error not set on vm executor."),
      },
    }
  }

  /// Report a known spacelox runtime error to the user
  fn runtime_error(&mut self, message: &str) -> Signal {
    self.set_error(SlError::new(
      self.gc.manage_str(String::from(message), self),
    ));
    Signal::RuntimeError
  }

  fn set_error(&mut self, error: SlError) -> Signal {
    self.current_error = Some(error);
    Signal::RuntimeError
  }

  /// Print an error message and the current call stack to the user
  fn error(&mut self, error: &SlError) {
    let message = error.message;

    let stdio = self.io.stdio();
    stdio.eprintln(&*message);
    stdio.eprintln("");

    for frame in self.frames[1..self.frame_count].iter().rev() {
      let closure = &frame.closure;
      let location: String = match &**closure.fun.name {
        SCRIPT => SCRIPT.to_owned(),
        _ => format!("{}()", closure.fun.name),
      };

      stdio.eprintln(&format!(
        "[line {}] in {}",
        closure.fun.chunk().get_line(frame.ip as usize),
        location
      ));
    }

    self.reset_stack();
  }
}

impl<'a, I: Io> Trace for VmExecutor<'a, I> {
  fn trace(&self) -> bool {
    self.script.trace();

    unsafe {
      let start = &self.stack[0] as *const Value;
      let len = ptr_len(start, self.stack_top);
      let slice = std::slice::from_raw_parts(start, len);

      slice.iter().for_each(|value| {
        value.trace();
      });
    }

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
    self.script.trace_debug(stdio);

    unsafe {
      let start = &self.stack[0] as *const Value;
      let len = ptr_len(start, self.stack_top) + 1;
      let slice = std::slice::from_raw_parts(start, len);

      slice.iter().for_each(|value| {
        value.trace_debug(stdio);
      });
    }

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

impl<'a, I: Io> HookContext for VmExecutor<'a, I> {
  fn gc(&self) -> &Gc {
    self.gc
  }

  fn call(&mut self, callable: Value, args: &[Value]) -> CallResult {
    let result = self.run_fun(callable, args);
    self.to_call_result(result)
  }

  fn call_method(&mut self, this: Value, method: Value, args: &[Value]) -> CallResult {
    let result = self.run_method(this, method, args);
    self.to_call_result(result)
  }

  fn call_method_by_name(
    &mut self,
    this: Value,
    method_name: Managed<String>,
    args: &[Value],
  ) -> CallResult {
    let result = self.run_method_by_name(this, method_name, args);
    self.to_call_result(result)
  }
}
