use crate::call_frame::CallFrame;
use crate::compiler::{Compiler, Parser};
use crate::constants::{define_special_string, SpecialStrings, FRAME_MAX};
use crate::memory::{Gc, NO_GC};
use spacelox_core::chunk::{ByteCode, UpvalueIndex};
use spacelox_core::managed::{Manage, Managed, Trace};
use spacelox_core::native::{NativeFun, NativeResult};
use spacelox_core::{
  utils::do_if_some,
  value::{BoundMethod, Class, Closure, Instance, Upvalue, Value},
};
use spacelox_interner::IStr;
use std::collections::HashMap;
use std::io::{stdin, stdout, Write};
use std::mem::replace;
use std::rc::Rc;

#[cfg(feature = "debug")]
use crate::debug::disassemble_instruction;

#[derive(Debug, Clone, PartialEq)]
pub enum InterpretResult {
  Ok,
  CompileError,
  RuntimeError,
  InternalError,
}

/// The virtual machine for the spacelox programming language
pub struct Vm {
  /// A stack holding all local variable currently in use
  stack: Vec<Value>,

  /// A stack holding call frames currently in use
  frames: Vec<CallFrame>,

  /// The vm's garbage collector
  gc: Gc,

  /// special strings to the vm that aren't keywords
  special_strings: SpecialStrings,

  /// A persisted set of globals most for a repl context
  globals: HashMap<Managed<IStr>, Value>,
}

impl Vm {
  pub fn new(stack: Vec<Value>, frames: Vec<CallFrame>, natives: &[Rc<dyn NativeFun>]) -> Vm {
    let mut gc = Gc::new();
    let special_strings = define_special_string();
    let globals = define_globals(&mut gc, &natives);

    Vm {
      stack,
      frames,
      gc,
      special_strings,
      globals,
    }
  }

  pub fn repl(&mut self) {
    loop {
      let mut buffer = String::new();

      print!("> ");
      stdout().flush().expect("Could not write to stdout");

      match stdin().read_line(&mut buffer) {
        Ok(_) => {
          self.interpret(&buffer);
        }
        Err(error) => panic!(error),
      }
    }
  }

  pub fn run(&mut self, source: &str) -> InterpretResult {
    self.interpret(source)
  }

  fn interpret(&mut self, source: &str) -> InterpretResult {
    let mut parser = Parser::new(source);

    let compiler = Compiler::new(&mut parser, None, &mut self.special_strings, &mut self.gc);
    let result = compiler.compile();

    if !result.success {
      return InterpretResult::CompileError;
    }

    let script_closure = self.gc.manage(Closure::new(result.fun), &NO_GC);
    let script = Value::Closure(script_closure);
    let executor = VmExecutor::new(self, script);
    executor.run()
  }
}

fn define_globals<'a>(gc: &mut Gc, natives: &[Rc<dyn NativeFun>]) -> HashMap<Managed<IStr>, Value> {
  let mut globals = HashMap::new();

  natives.iter().for_each(|native| {
    let name = gc.manage_str(&native.meta().name, &NO_GC);
    let native_value = Value::Native(gc.manage(Rc::clone(native), &NO_GC));

    globals.insert(name, native_value);
  });

  globals
}

pub struct VmExecutor<'a> {
  /// A stack of call frames for the current execution
  pub frames: &'a mut Vec<CallFrame>,

  /// The current frame depth of the program
  pub frame_count: usize,

  /// A stack holding all local variable currently in use
  pub stack: &'a mut Vec<Value>,

  /// A reference to a object currently in the vm
  gc: &'a mut Gc,

  /// the main script level function
  script: Value,

  /// class init string
  special_strings: &'a SpecialStrings,

  /// index to the top of the value stack
  pub stack_top: usize,

  /// global variable present in the vm
  pub globals: &'a mut HashMap<Managed<IStr>, Value>,

  /// A collection of currently available upvalues
  pub open_upvalues: Vec<Managed<Upvalue>>,
}

impl<'a> VmExecutor<'a> {
  pub fn new(vm: &'a mut Vm, script: Value) -> VmExecutor<'a> {
    let mut executor = VmExecutor {
      frames: &mut vm.frames,
      frame_count: 0,
      stack: &mut vm.stack,
      script,
      gc: &mut vm.gc,
      special_strings: &vm.special_strings,
      stack_top: 1,
      globals: &mut vm.globals,
      open_upvalues: Vec::with_capacity(100),
    };

    executor.call_value(executor.script, 0);
    executor
  }

  fn run(mut self) -> InterpretResult {
    loop {
      let op_code: ByteCode = self.frame_instruction();

      #[cfg(feature = "debug")]
      self.print_debug();

      self.increment_frame_ip(1);
      match op_code {
        ByteCode::Negate => {
          if let Some(result) = self.op_negate() {
            return result;
          }
        }
        ByteCode::Add => {
          if let Some(result) = self.op_add() {
            return result;
          }
        }
        ByteCode::Subtract => {
          if let Some(result) = self.op_sub() {
            return result;
          }
        }
        ByteCode::Multiply => {
          if let Some(result) = self.op_mul() {
            return result;
          }
        }
        ByteCode::Divide => {
          if let Some(result) = self.op_div() {
            return result;
          }
        }
        ByteCode::Not => self.op_not(),
        ByteCode::Equal => self.op_equal(),
        ByteCode::Greater => {
          if let Some(result) = self.op_greater() {
            return result;
          }
        }
        ByteCode::Less => {
          if let Some(result) = self.op_less() {
            return result;
          }
        }
        ByteCode::JumpIfFalse(jump) => self.op_jump_if_not_false(jump),
        ByteCode::Jump(jump) => {
          self.op_jump(jump);
        }
        ByteCode::Loop(jump) => {
          self.op_loop(jump);
        }
        ByteCode::Noop => panic!("Noop was not replaced within compiler.rs"),
        ByteCode::DefineGlobal(slot) => {
          self.op_define_global(slot);
        }
        ByteCode::GetGlobal(slot) => {
          if let Some(result) = self.op_get_global(slot) {
            return result;
          }
        }
        ByteCode::SetGlobal(slot) => {
          if let Some(result) = self.op_set_global(slot) {
            return result;
          }
        }
        ByteCode::GetLocal(slot) => self.op_get_local(slot),
        ByteCode::SetLocal(slot) => self.op_set_local(slot),
        ByteCode::GetUpvalue(slot) => self.op_get_upvalue(slot),
        ByteCode::SetUpvalue(slot) => self.op_set_upvalue(slot),
        ByteCode::GetProperty(slot) => {
          if let Some(result) = self.op_get_property(slot) {
            return result;
          }
        }
        ByteCode::SetProperty(slot) => {
          if let Some(result) = self.op_set_property(slot) {
            return result;
          }
        }
        ByteCode::UpvalueIndex(_) => {
          self.internal_error("UpvalueIndex should only be processed in closure");
        }
        ByteCode::Pop => {
          self.pop();
        }
        ByteCode::Nil => self.push(Value::Nil),
        ByteCode::True => self.push(Value::Bool(true)),
        ByteCode::False => self.push(Value::Bool(false)),
        ByteCode::Constant(store_index) => {
          self.op_constant(store_index);
        }
        ByteCode::Print => println!("{}", self.pop()),
        ByteCode::Call(arg_count) => {
          if let Some(result) = self.call_value(self.peek(arg_count as usize), arg_count) {
            return result;
          }
        }
        ByteCode::Invoke((name, arg_count)) => {
          if let Some(result) = self.op_invoke(name, arg_count) {
            return result;
          }
        }
        ByteCode::SuperInvoke((name, arg_count)) => {
          if let Some(result) = self.op_super_invoke(name, arg_count) {
            return result;
          }
        }
        ByteCode::Closure(slot) => self.op_closure(slot),
        ByteCode::Method(slot) => self.op_method(slot),
        ByteCode::Class(slot) => self.op_class(slot),
        ByteCode::Inherit => {
          if let Some(result) = self.op_inherit() {
            return result;
          }
        }
        ByteCode::GetSuper(slot) => {
          if let Some(result) = self.op_get_super(slot) {
            return result;
          }
        }
        ByteCode::CloseUpvalue => {
          self.close_upvalues(self.stack_top - 1);
          self.pop();
        }
        ByteCode::Return => {
          if let Some(result) = self.op_return() {
            return result;
          }
        }
      }
    }
  }

  /// Get an immutable reference to the current callframe
  fn current_frame(&self) -> &CallFrame {
    unsafe { self.frames.get_unchecked(self.frame_count - 1) }
  }

  /// Get a mutable reference to the current callframe
  fn current_mut_frame(&mut self) -> &mut CallFrame {
    unsafe { self.frames.get_unchecked_mut(self.frame_count - 1) }
  }

  /// Get an immutable reference to value on the stack
  fn get_val(&self, index: usize) -> Value {
    unsafe { *self.stack.get_unchecked(index) }
  }

  /// Set a value on the stack
  fn set_val(&mut self, index: usize, val: Value) {
    unsafe {
      *self.stack.get_unchecked_mut(index) = val;
    }
  }

  /// Get the current instruction from the present call frame
  fn frame_instruction(&self) -> ByteCode {
    let frame = self.current_frame();
    frame.closure.fun.chunk.instructions[frame.ip]
  }

  /// increment the the current call frame instruction pointer
  fn increment_frame_ip(&mut self, offset: usize) {
    let frame = self.current_mut_frame();
    frame.ip += offset;
  }

  /// decrement the current call frames instruction pointer
  fn decrement_frame_ip(&mut self, offset: usize) {
    let frame = self.current_mut_frame();
    frame.ip -= offset;
  }

  /// push a value onto the stack
  fn push(&mut self, value: Value) {
    self.set_val(self.stack_top, value);
    self.stack_top += 1;
  }

  /// reference a value n slots from the stack head
  fn peek(&self, distance: usize) -> Value {
    self.get_val(self.stack_top - (distance + 1))
  }

  fn pop(&mut self) -> Value {
    self.stack_top -= 1;
    let ptr = unsafe { self.stack.get_unchecked_mut(self.stack_top) };
    replace(ptr, Value::Nil)
  }

  fn read_constant<'b>(frame: &'b CallFrame, index: u8) -> Value {
    frame.closure.fun.chunk.constants[index as usize]
  }

  fn read_string(&mut self, index: u8) -> Managed<IStr> {
    let frame = self.current_frame();
    VmExecutor::read_constant(frame, index).to_string()
  }

  fn reset_stack(&mut self) {
    self.stack_top = 0;
    self.frame_count = 0;
    self.open_upvalues.clear()
  }

  fn call_value(&mut self, callee: Value, arg_count: u8) -> Option<InterpretResult> {
    match callee {
      Value::Closure(closure) => self.call(closure, arg_count),
      Value::Method(method) => self.call_method(method, arg_count),
      Value::Native(native) => self.call_native(native, arg_count),
      Value::Class(class) => self.call_class(class, arg_count),
      Value::Fun(fun) => panic!(
        "function {} was not wrapped in a closure",
        fun.name.clone().unwrap_or(IStr::new("script"))
      ),
      _ => self.runtime_error("Can only call functions and classes."),
    }
  }

  fn call_class(&mut self, class: Managed<Class>, arg_count: u8) -> Option<InterpretResult> {
    let value = Value::Instance(self.gc.manage(Instance::new(class), self));
    self.set_val(self.stack_top - (arg_count as usize) - 1, value);

    match class.init {
      Some(init) => self.call(init, arg_count),
      None => {
        if arg_count != 0 {
          self.runtime_error(&format!("Expected 0 arguments but got {}", arg_count))
        } else {
          None
        }
      }
    }
  }

  fn call_native(
    &mut self,
    native: Managed<Rc<dyn NativeFun>>,
    arg_count: u8,
  ) -> Option<InterpretResult> {
    let meta = native.meta();
    if arg_count != meta.arity {
      self.runtime_error(&format!(
        "Function {} expected {} argument but got {}",
        meta.name, meta.arity, arg_count,
      ));
      return Some(InterpretResult::RuntimeError);
    }

    let args = unsafe {
      self
        .stack
        .get_unchecked((self.stack_top - arg_count as usize)..self.stack_top)
    };
    let result = native.call(args);
    match result {
      NativeResult::Success(value) => {
        self.stack_top -= arg_count as usize + 1;
        self.push(value);
        None
      }
      NativeResult::RuntimeError(message) => {
        self.runtime_error(&message);
        Some(InterpretResult::RuntimeError)
      }
    }
  }

  fn call(&mut self, closure: Managed<Closure>, arg_count: u8) -> Option<InterpretResult> {
    if (arg_count as u16) != closure.fun.arity {
      self.runtime_error(&format!(
        "Function {} expected {} arguments but got {}",
        closure.fun.name.clone().unwrap_or(IStr::new("script")),
        closure.fun.arity,
        arg_count
      ));
      return Some(InterpretResult::RuntimeError);
    }

    if self.frame_count == FRAME_MAX {
      self.runtime_error("Stack overflow.");
      return Some(InterpretResult::RuntimeError);
    }

    let current_closure = self.gc.clone_managed(closure, self);

    let frame = &mut self.frames[self.frame_count];
    frame.closure = current_closure;
    frame.ip = 0;
    frame.slots = self.stack_top - (arg_count as usize + 1);

    self.frame_count += 1;
    None
  }

  fn call_method(&mut self, bound: Managed<BoundMethod>, arg_count: u8) -> Option<InterpretResult> {
    self.set_val(self.stack_top - (arg_count as usize) - 1, bound.receiver);
    self.call(bound.method, arg_count)
  }

  fn bind_method(&mut self, class: Managed<Class>, name: Managed<IStr>) -> Option<InterpretResult> {
    match class.methods.get(&name) {
      Some(method) => {
        let bound = self
          .gc
          .manage(BoundMethod::new(self.peek(0), *method), self);
        self.pop();
        self.push(Value::Method(bound));
        None
      }
      None => {
        self.runtime_error(&format!("Undefined property {}", name.as_str()));
        Some(InterpretResult::RuntimeError)
      }
    }
  }

  fn op_invoke(&mut self, constant: u8, arg_count: u8) -> Option<InterpretResult> {
    let method_name = self.read_string(constant);
    let receiver = self.peek(arg_count as usize);

    if let Value::Instance(instance) = receiver {
      match instance.fields.get(&method_name) {
        Some(field) => {
          self.set_val(self.stack_top - (arg_count as usize) - 1, *field);
          return self.call_value(*field, arg_count);
        }
        None => self.invoke_from_class(instance.class, method_name, arg_count),
      }
    } else {
      self.runtime_error("Only instances have methods.")
    }
  }

  fn op_super_invoke(&mut self, constant: u8, arg_count: u8) -> Option<InterpretResult> {
    let method_name = self.read_string(constant);
    let super_class = self.pop().to_class();

    self.invoke_from_class(super_class, method_name, arg_count)
  }

  fn invoke_from_class(
    &mut self,
    class: Managed<Class>,
    method_name: Managed<IStr>,
    arg_count: u8,
  ) -> Option<InterpretResult> {
    match class.methods.get(&method_name) {
      Some(method) => self.call(*method, arg_count),
      None => self.runtime_error(&format!("Undefined property {}.", method_name.as_str())),
    }
  }

  fn op_class(&mut self, slot: u8) {
    let name = self.read_string(slot);
    let class = Value::Class(self.gc.manage(Class::new(name), self));
    self.push(class)
  }

  fn op_get_super(&mut self, slot: u8) -> Option<InterpretResult> {
    let name = self.read_string(slot);
    let super_class = self.pop().to_class();

    self.bind_method(super_class, name)
  }

  fn op_inherit(&mut self) -> Option<InterpretResult> {
    let mut class = self.peek(0).to_class();
    
    match self.peek(1) {
      Value::Class(super_class) => {
        super_class.methods.iter().for_each(|(key, value)| {
          class.methods.entry(*key).or_insert_with(|| *value);
        });

        class.init = class.init.or(super_class.init);

        self.pop();
        None
      }
      _ => self.runtime_error("Superclass must be a class.")
    }
  }

  fn op_loop(&mut self, jump: u16) {
    self.decrement_frame_ip(jump as usize);
  }

  fn op_jump_if_not_false(&mut self, jump: u16) {
    if is_falsey(self.peek(0)) {
      self.increment_frame_ip(jump as usize);
    }
  }

  fn op_jump(&mut self, jump: u16) {
    self.increment_frame_ip(jump as usize);
  }

  fn op_define_global(&mut self, slot: u8) {
    let name = self.read_string(slot);
    let global = self.pop();
    self.globals.insert(name, global);
  }

  fn op_set_global(&mut self, store_index: u8) -> Option<InterpretResult> {
    let string = self.read_string(store_index);

    if self.globals.insert(string, self.peek(0)).is_none() {
      self.globals.remove_entry(&string);
      return self.runtime_error(&format!("Undefined variable {}", string.as_str()));
    }

    None
  }

  fn op_set_local(&mut self, slot: u8) {
    let copy = self.peek(0);
    let slots = self.current_frame().slots;
    self.stack[slots + slot as usize] = copy;
  }

  fn op_set_property(&mut self, slot: u8) -> Option<InterpretResult> {
    let mut value = self.peek(1);
    let name = self.read_string(slot);

    if let Value::Instance(ref mut instance) = value {
      instance.fields.insert(name, self.peek(0));

      let popped = self.pop();
      self.pop();
      self.push(popped);

      return None;
    }

    self.runtime_error("Only instances have fields.")
  }

  fn op_set_upvalue(&mut self, slot: u8) {
    let value = self.peek(0);
    let upvalue = &mut self.current_mut_frame().closure.upvalues[slot as usize];

    let open_index = match &mut *upvalue.to_upvalue() {
      Upvalue::Open(index) => Some(*index),
      Upvalue::Closed(store) => {
        replace(&mut **store, value);
        None
      }
    };

    if let Some(index) = open_index {
      self.stack[index] = value;
    }
  }

  fn op_get_global(&mut self, store_index: u8) -> Option<InterpretResult> {
    let string = self.read_string(store_index);

    match self.globals.get(&string) {
      Some(gbl) => {
        let copy = *gbl;
        self.push(copy);
        None
      }
      None => self.runtime_error(&format!("Undefined variable {}", string.as_str())),
    }
  }

  fn op_get_local(&mut self, slot: u8) {
    let slots = self.current_frame().slots;
    let copy = self.get_val(slots + slot as usize);
    self.push(copy);
  }

  fn op_get_upvalue(&mut self, slot: u8) {
    let upvalue_value = &self.current_frame().closure.upvalues[slot as usize];

    let value = match &*upvalue_value.to_upvalue() {
      Upvalue::Open(index) => self.stack[*index],
      Upvalue::Closed(store) => **store,
    };

    self.push(value);
  }

  fn op_get_property(&mut self, slot: u8) -> Option<InterpretResult> {
    let value = self.peek(0);

    if let Value::Instance(instance) = value {
      let name = self.read_string(slot);
      if let Some(value) = instance.fields.get(&name) {
        self.pop();
        self.push(*value);
        return None;
      }

      return self.bind_method(instance.class, name);
    }

    self.runtime_error("Only instances have properties.")
  }

  fn op_return(&mut self) -> Option<InterpretResult> {
    let result = self.pop();
    self.close_upvalues(self.current_frame().slots);
    self.frame_count -= 1;

    if self.frame_count == 0 {
      self.pop();
      return Some(InterpretResult::Ok);
    }

    self.stack_top = self.frames[self.frame_count].slots;
    self.push(result);

    None
  }

  fn op_negate(&mut self) -> Option<InterpretResult> {
    match self.pop() {
      Value::Number(num) => {
        self.push(Value::Number(-num));
        None
      }
      _ => self.runtime_error("Operand must be a number."),
    }
  }

  fn op_not(&mut self) {
    let value = self.pop();
    self.push(Value::Bool(is_falsey(value)))
  }

  fn op_add(&mut self) -> Option<InterpretResult> {
    match (self.pop(), self.pop()) {
      (Value::String(right), Value::String(left)) => {
        let result = format!("{}{}", left.as_str(), right.as_str());
        let string = self.gc.manage_str(&result, self);
        self.push(Value::String(string));
        None
      }
      (Value::Number(right), Value::Number(left)) => {
        self.push(Value::Number(left + right));
        None
      }
      _ => self.runtime_error("Operands must be two numbers or two strings."),
    }
  }

  fn op_sub(&mut self) -> Option<InterpretResult> {
    match (self.pop(), self.pop()) {
      (Value::Number(right), Value::Number(left)) => {
        self.push(Value::Number(left - right));
        None
      }
      _ => self.runtime_error("Operands must be numbers."),
    }
  }

  fn op_mul(&mut self) -> Option<InterpretResult> {
    match (self.pop(), self.pop()) {
      (Value::Number(right), Value::Number(left)) => {
        self.push(Value::Number(left * right));
        None
      }
      _ => self.runtime_error("Operands must be numbers."),
    }
  }

  fn op_div(&mut self) -> Option<InterpretResult> {
    match (self.pop(), self.pop()) {
      (Value::Number(right), Value::Number(left)) => {
        self.push(Value::Number(left / right));
        None
      }
      _ => self.runtime_error("Operands must be numbers."),
    }
  }

  fn op_less(&mut self) -> Option<InterpretResult> {
    match (self.pop(), self.pop()) {
      (Value::Number(right), Value::Number(left)) => {
        self.push(Value::Bool(left < right));
        None
      }
      _ => self.runtime_error("Operands must be numbers."),
    }
  }

  fn op_greater(&mut self) -> Option<InterpretResult> {
    match (self.pop(), self.pop()) {
      (Value::Number(right), Value::Number(left)) => {
        self.push(Value::Bool(left > right));
        None
      }
      _ => self.runtime_error("Operands must be numbers."),
    }
  }

  fn op_equal(&mut self) {
    let right = self.pop();
    let left = self.pop();

    self.push(Value::Bool(left == right));
  }

  fn op_method(&mut self, slot: u8) {
    let name = self.read_string(slot);

    match (self.peek(1), self.peek(0)) {
      (Value::Class(ref mut class), Value::Closure(method)) => {
        if *name == self.special_strings.init {
          class.init = Some(method);
        }
        class.methods.insert(name, method);
      }
      _ => panic!("Internal spacelox error. stack invalid for op_method"),
    }

    self.pop();
  }

  fn op_closure(&mut self, slot: u8) {
    let frame = self.current_frame();
    let fun = VmExecutor::read_constant(frame, slot).to_fun();
    let mut closure = Closure::new(fun);

    for _ in 0..fun.upvalue_count {
      let op_code = self.frame_instruction();
      self.increment_frame_ip(1);

      if let ByteCode::UpvalueIndex(upvalue_index) = op_code {
        match upvalue_index {
          UpvalueIndex::Local(index) => {
            let total_index = self.current_frame().slots + index as usize;
            closure.upvalues.push(self.capture_upvalue(total_index));
          }
          UpvalueIndex::Upvalue(upvalue) => {
            let upvalue = &self.current_frame().closure.upvalues[upvalue as usize];
            closure.upvalues.push(*upvalue);
          }
        }
      } else {
        self.internal_error("Expected upvalues following closures")
      }
    }

    let closure = Value::Closure(self.gc.manage(closure, self));

    self.push(closure);
  }

  fn capture_upvalue(&mut self, local_index: usize) -> Value {
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

  fn close_upvalues(&mut self, last_index: usize) {
    for upvalue in self.open_upvalues.iter_mut().rev() {
      let index = match **upvalue {
        Upvalue::Open(index) => index,
        Upvalue::Closed(_) => panic!("Unexpected closed upvalue"),
      };

      if index < last_index {
        break;
      }

      upvalue.hoist(self.stack);
    }

    self.open_upvalues.retain(|upvalue| upvalue.is_open())
  }

  fn op_constant(&mut self, index: u8) {
    let frame = self.current_frame();
    let constant = VmExecutor::read_constant(frame, index);
    self.push(constant);
  }

  #[cfg(feature = "debug")]
  fn print_debug(&self) {
    print!("Stack:    ");
    // print!("          ");
    for i in 1..self.stack_top {
      print!("[ {} ]", self.get_val(i));
    }
    println!();

    #[cfg(feature = "debug_upvalue")]
    {
      print!("Upvalues: ");
      let frame = self.current_frame();

      frame
        .closure
        .upvalues
        .iter()
        .for_each(|upvalue| match &*upvalue.to_upvalue() {
          Upvalue::Open(index) => {
            print!("[ stack {} ]", self.get_val(*index + frame.slots));
          }
          Upvalue::Closed(closed) => {
            print!("[ heap {} ]", closed);
          }
        });
      println!();
    }

    let frame = self.current_frame();
    disassemble_instruction(&frame.closure.fun.chunk, frame.ip);
  }

  /// As an internal error been encounter inside the vm. print
  /// the error then panic
  fn internal_error(&mut self, message: &str) {
    self.error(&format!("!=== [Internal Error]:{} ===!", message));
    panic!("Internal error")
  }

  /// Report a known spacelox runtime error to the user
  fn runtime_error(&mut self, message: &str) -> Option<InterpretResult> {
    self.error(message);
    Some(InterpretResult::RuntimeError)
  }

  /// Print an error message and the current call stack to the user
  fn error(&mut self, message: &str) {
    eprintln!("{}", message);
    eprintln!("");

    for frame in self.frames[0..self.frame_count].iter().rev() {
      let closure = &frame.closure;
      let location = match &closure.fun.name {
        Some(name) => format!("{}()", name),
        None => "script".to_string(),
      };

      eprintln!(
        "[line {}] in {}",
        closure.fun.chunk.get_line(frame.ip),
        location
      );
    }

    self.reset_stack();
  }
}

impl<'a> Trace for VmExecutor<'a> {
  fn trace(&self, mark: &mut dyn FnMut(Managed<dyn Manage>)) -> bool {
    do_if_some(self.script.get_dyn_managed(), |obj| mark(obj));

    self.stack[0..self.stack_top].iter().for_each(|value| {
      do_if_some(value.get_dyn_managed(), |obj| mark(obj));
    });

    self.frames[0..self.frame_count]
      .iter()
      .for_each(|frame| mark(frame.closure.clone_dyn()));

    self
      .open_upvalues
      .iter()
      .for_each(|upvalue| mark(upvalue.clone_dyn()));

    self.globals.iter().for_each(|(key, val)| {
      mark(key.clone_dyn());
      do_if_some(val.get_dyn_managed(), |obj| mark(obj));
    });

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
