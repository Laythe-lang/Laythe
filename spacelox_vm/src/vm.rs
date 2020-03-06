use crate::compiler::{Compiler, Parser};
use crate::memory::Gc;
use spacelox_core::chunk::{ByteCode, UpvalueIndex};
use spacelox_core::native::{NativeFun, NativeResult};
use spacelox_core::value::{Closure, FunKind, Managed, Upvalue, Value};
use spacelox_interner::IStr;
use std::collections::HashMap;
use std::io::{stdin, stdout, Write};
use std::mem::replace;
use std::ops::Drop;
use std::rc::Rc;

#[cfg(feature = "debug")]
use crate::debug::disassemble_instruction;

#[cfg(feature = "debug_upvalue")]
use crate::object::UpvalueLocation;

pub const FRAME_MAX: usize = std::u8::MAX as usize;
pub const DEFAULT_STACK_MAX: usize = FRAME_MAX * 16;

#[derive(Debug, Clone, PartialEq)]
pub enum InterpretResult {
  Ok,
  CompileError,
  RuntimeError,
}

/// A call frame in the space lox interpreter
#[derive(Clone, PartialEq)]
pub struct CallFrame {
  pub closure: Managed<Closure>,
  ip: usize,
  slots: usize,
}

impl<'a> CallFrame {
  pub fn new(closure: Managed<Closure>) -> Self {
    CallFrame {
      closure,
      ip: 0,
      slots: 0,
    }
  }
}

/// The virtual machine for the spacelox programming language
pub struct Vm {
  /// A stack holding all local variable currently in use
  pub stack: Vec<Value>,

  /// A stack holding call frames currently in use
  pub frames: Vec<CallFrame>,

  /// All the native functions
  pub natives: Vec<Rc<dyn NativeFun>>,
}

impl Vm {
  pub fn new(stack: Vec<Value>, frames: Vec<CallFrame>, natives: Vec<Rc<dyn NativeFun>>) -> Vm {
    Vm {
      stack,
      frames,
      natives,
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
    let mut gc = Gc::new();
    let mut parser = Parser::new(source);

    let compiler = Compiler::new(&mut parser, &mut gc, FunKind::Script);
    let result = compiler.compile();

    if !result.success {
      return InterpretResult::CompileError;
    }

    let script_closure = gc.allocate(Closure::new(&result.fun));
    let script = Value::Closure(Managed::from(script_closure));
    let globals = define_globals(&self.natives, &mut gc);
    let executor = VmExecutor::new(self, script, globals, gc);
    executor.run()
  }
}

fn define_globals<'a>(natives: &[Rc<dyn NativeFun>], gc: &mut Gc) -> HashMap<Managed<IStr>, Value> {
  let mut globals = HashMap::new();
  natives.iter().for_each(|native| {
    let name = Managed::from(gc.allocate_string(&native.meta().name));

    let native_value = Value::Native(Managed::from(gc.allocate(Rc::clone(native))));

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
  gc: Gc,

  /// index to the top of the value stack
  pub stack_top: usize,

  /// global variable present in the vm
  pub globals: HashMap<Managed<IStr>, Value>,

  /// A collection of currently available upvalues
  pub open_upvalues: Vec<Managed<Upvalue>>,
}

impl<'a> Drop for VmExecutor<'a> {
  fn drop(&mut self) {}
}

impl<'a> VmExecutor<'a> {
  pub fn new(
    vm: &'a mut Vm,
    script: Value,
    globals: HashMap<Managed<IStr>, Value>,
    gc: Gc,
  ) -> VmExecutor<'a> {
    let mut executor = VmExecutor {
      frames: &mut vm.frames,
      frame_count: 0,
      stack: &mut vm.stack,
      gc,
      stack_top: 1,
      globals,
      open_upvalues: Vec::with_capacity(100),
    };

    executor.call_value(script, 0);
    executor
  }

  fn run(mut self) -> InterpretResult {
    loop {
      let op_code = self.frame_instruction().clone();

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
        ByteCode::DefineGlobal(constant) => {
          self.op_define_global(constant);
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
        ByteCode::GetLocal(slot) => {
          self.op_get_local(slot);
        }
        ByteCode::SetLocal(slot) => {
          self.op_set_local(slot);
        }
        ByteCode::GetUpvalue(slot) => self.op_get_upvalue(slot),
        ByteCode::SetUpvalue(slot) => self.op_set_upvalue(slot),
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
          if let Some(result) = self.call_value(self.peek(arg_count as usize).clone(), arg_count) {
            return result;
          }
        }
        ByteCode::Closure(constant) => self.op_closure(constant),
        ByteCode::CloseUpvalue => {
          self.close_upvalues(self.stack_top - 1);
          self.pop();
        }
        ByteCode::Return => {
          let result = self.pop();
          self.close_upvalues(self.current_frame().slots);
          self.frame_count -= 1;

          if self.frame_count == 0 {
            self.pop();
            return InterpretResult::Ok;
          }

          self.stack_top = self.frames[self.frame_count].slots;
          self.push(result);
        }
      }
    }
  }

  fn current_frame(&self) -> &CallFrame {
    unsafe { self.frames.get_unchecked(self.frame_count - 1) }
  }

  fn current_mut_frame(&mut self) -> &mut CallFrame {
    unsafe { self.frames.get_unchecked_mut(self.frame_count - 1) }
  }

  fn call_value(&mut self, callee: Value, arg_count: u8) -> Option<InterpretResult> {
    match &callee {
      Value::Closure(closure) => self.call(closure, arg_count),
      Value::Native(native) => self.call_native(native, arg_count),
      Value::Fun(fun) => panic!(
        "function {} was not wrapped in a closure",
        fun.name.clone().unwrap_or("script".to_string())
      ),
      _ => {
        self.runtime_error("Can only call functions and classes.");
        Some(InterpretResult::RuntimeError)
      }
    }
  }

  fn call_native(
    &mut self,
    native: &Managed<Rc<dyn NativeFun>>,
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

  fn call(&mut self, closure: &Managed<Closure>, arg_count: u8) -> Option<InterpretResult> {
    if (arg_count as u16) != closure.get_fun().arity {
      self.runtime_error(&format!(
        "Function {} expected {} arguments but got {}",
        closure
          .get_fun()
          .name
          .clone()
          .unwrap_or("script".to_string()),
        closure.get_fun().arity,
        arg_count
      ));
      return Some(InterpretResult::RuntimeError);
    }

    if self.frame_count == FRAME_MAX {
      self.runtime_error("Stack overflow.");
      return Some(InterpretResult::RuntimeError);
    }

    self.frame_count += 1;
    let frame = &mut self.frames[self.frame_count - 1];
    frame.closure = closure.clone();
    frame.ip = 0;
    frame.slots = self.stack_top - (arg_count as usize + 1);
    None
  }

  fn increment_frame_ip(&mut self, offset: usize) {
    let frame = self.current_mut_frame();
    frame.ip += offset;
  }

  fn decrement_frame_ip(&mut self, offset: usize) {
    let frame = self.current_mut_frame();
    frame.ip -= offset;
  }

  fn internal_error(&mut self, message: &str) {
    self.runtime_error(&format!("!=== [Internal Error]:{} ===!", message))
  }

  fn runtime_error(&mut self, message: &str) {
    eprintln!("{}", message);
    eprintln!("");

    for frame in self.frames[0..self.frame_count].iter().rev() {
      let closure = &frame.closure;
      let location = match &closure.get_fun().name {
        Some(name) => format!("{}()", name),
        None => "script".to_string(),
      };

      eprintln!(
        "[line {}] in {}",
        closure.get_fun().chunk.get_line(frame.ip),
        location
      );
    }

    self.reset_stack();
  }

  fn read_string(&mut self, index: u8) -> Managed<IStr> {
    let frame = self.current_frame();

    match VmExecutor::read_constant(frame, index).clone() {
      Value::String(string) => string,
      _ => panic!("Expected string."),
    }
  }

  fn read_constant<'b>(frame: &'b CallFrame, index: u8) -> &'b Value {
    &frame.closure.get_fun().chunk.constants[index as usize]
  }

  fn push(&mut self, value: Value) {
    self.stack[self.stack_top] = value;
    self.stack_top += 1;
  }

  fn peek(&self, distance: usize) -> &Value {
    self.get_val(self.stack_top - (distance + 1))
  }

  fn pop(&mut self) -> Value {
    self.stack_top -= 1;
    let slot = self.get_val_mut(self.stack_top);
    replace(slot, Value::Nil)
  }

  fn reset_stack(&mut self) {
    self.stack_top = 0;
    self.frame_count = 0;
    self.open_upvalues.clear()
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

  fn op_define_global(&mut self, store_index: u8) {
    let string = self.read_string(store_index);
    let name = Managed::from(self.gc.copy_string(string.as_str()));
    let global = self.pop();
    self.globals.insert(name, global);
  }

  fn op_get_global(&mut self, store_index: u8) -> Option<InterpretResult> {
    let string = self.read_string(store_index);

    let global = self.globals.get(&string).map(|global| global.clone());
    match global {
      Some(gbl) => {
        self.push(gbl);
        None
      }
      None => {
        self.runtime_error(&format!("Undefined variable {}", string));
        Some(InterpretResult::RuntimeError)
      }
    }
  }

  fn op_set_global(&mut self, store_index: u8) -> Option<InterpretResult> {
    let string = self.read_string(store_index);

    if self
      .globals
      .insert(string.clone(), self.peek(0).clone())
      .is_none()
    {
      self.globals.remove_entry(&string);
      self.runtime_error(&format!("Undefined variable {}", string));
      return Some(InterpretResult::RuntimeError);
    }

    None
  }

  fn op_set_local(&mut self, slot: u8) {
    let copy = self.peek(0).clone();
    let slots = self.current_frame().slots;
    self.stack[slots + slot as usize] = copy;
  }

  fn op_set_upvalue(&mut self, slot: u8) {
    let value = self.peek(0);
    let upvalue = &self.current_frame().closure.upvalues[slot as usize];

    let open_index = match upvalue.ref_upvalue() {
      Upvalue::Open(index) => Some(*index),
      Upvalue::Closed(store) => {
        &store.replace(value.clone());
        None
      }
    };

    if let Some(index) = open_index {
      self.stack[index] = value.clone();
    }
  }

  fn op_get_local(&mut self, slot: u8) {
    let slots = self.current_frame().slots;
    let copy = self.get_val(slots + slot as usize).clone();
    self.push(copy);
  }

  fn op_get_upvalue(&mut self, slot: u8) {
    let upvalue = &self.current_frame().closure.upvalues[slot as usize];

    let value = match upvalue.ref_upvalue() {
      Upvalue::Open(index) => self.stack[*index].clone(),
      Upvalue::Closed(store) => store.borrow().clone(),
    };

    self.push(value);
  }

  fn op_negate(&mut self) -> Option<InterpretResult> {
    match self.pop() {
      Value::Number(num) => {
        self.push(Value::Number(-num));
        None
      }
      _ => {
        self.runtime_error("Operand must be a number.");
        Some(InterpretResult::RuntimeError)
      }
    }
  }

  fn op_not(&mut self) {
    let value = self.pop();
    self.push(Value::Bool(is_falsey(&value)))
  }

  fn op_add(&mut self) -> Option<InterpretResult> {
    match (self.pop(), self.pop()) {
      (Value::String(right), Value::String(left)) => {
        let result = format!("{}{}", left, right);
        let string = Managed::from(self.gc.allocate_string(&result));
        self.push(Value::String(string));
        None
      }
      (Value::Number(right), Value::Number(left)) => {
        self.push(Value::Number(left + right));
        None
      }
      _ => {
        self.runtime_error("Operands must be two numbers or two strings.");
        Some(InterpretResult::RuntimeError)
      }
    }
  }

  fn op_sub(&mut self) -> Option<InterpretResult> {
    match (self.pop(), self.pop()) {
      (Value::Number(right), Value::Number(left)) => {
        self.push(Value::Number(left - right));
        None
      }
      _ => {
        self.runtime_error("Operands must be numbers.");
        Some(InterpretResult::RuntimeError)
      }
    }
  }

  fn op_mul(&mut self) -> Option<InterpretResult> {
    match (self.pop(), self.pop()) {
      (Value::Number(right), Value::Number(left)) => {
        self.push(Value::Number(left * right));
        None
      }
      _ => {
        self.runtime_error("Operands must be numbers.");
        Some(InterpretResult::RuntimeError)
      }
    }
  }

  fn op_div(&mut self) -> Option<InterpretResult> {
    match (self.pop(), self.pop()) {
      (Value::Number(right), Value::Number(left)) => {
        self.push(Value::Number(left / right));
        None
      }
      _ => {
        self.runtime_error("Operands must be numbers.");
        Some(InterpretResult::RuntimeError)
      }
    }
  }

  fn op_less(&mut self) -> Option<InterpretResult> {
    match (self.pop(), self.pop()) {
      (Value::Number(right), Value::Number(left)) => {
        self.push(Value::Bool(left < right));
        None
      }
      _ => {
        self.runtime_error("Operands must be numbers.");
        Some(InterpretResult::RuntimeError)
      }
    }
  }

  fn op_greater(&mut self) -> Option<InterpretResult> {
    match (self.pop(), self.pop()) {
      (Value::Number(right), Value::Number(left)) => {
        self.push(Value::Bool(left > right));
        None
      }
      _ => {
        self.runtime_error("Operands must be numbers.");
        Some(InterpretResult::RuntimeError)
      }
    }
  }

  fn op_equal(&mut self) {
    let right = self.pop();
    let left = self.pop();

    self.push(Value::Bool(left == right));
  }

  fn op_closure(&mut self, index: u8) {
    let frame = self.current_frame();
    let fun = VmExecutor::read_constant(frame, index).ref_fun();
    let mut closure = Closure::new(fun);

    for _ in 0..fun.upvalue_count {
      let op_code = self.frame_instruction().clone();
      self.increment_frame_ip(1);

      if let ByteCode::UpvalueIndex(upvalue_index) = op_code {
        match upvalue_index {
          UpvalueIndex::Local(local) => {
            let slots = self.current_frame().slots;
            let index = slots + local as usize;
            closure.upvalues.push(self.capture_upvalue(index));
          }
          UpvalueIndex::Upvalue(upvalue) => {
            let upvalue = &self.current_frame().closure.upvalues[upvalue as usize];
            closure.upvalues.push(upvalue.clone());
          }
        }
      } else {
        self.internal_error("Expected upvalues following closures")
      }
    }

    let closure = Value::Closure(Managed::from(self.gc.allocate(closure)));

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
          return Value::Upvalue(upvalue.clone());
        }
      }
    }

    let created_upvalue = Managed::from(self.gc.allocate(Upvalue::Open(local_index)));
    self.open_upvalues.push(created_upvalue);

    Value::Upvalue(created_upvalue)
  }

  fn close_upvalues(&mut self, last_index: usize) {
    for upvalue in self.open_upvalues.iter().rev() {
      let index = match **upvalue {
        Upvalue::Open(index) => index,
        Upvalue::Closed(_) => panic!("Unexpected closed upvalue"),
      };

      if index < last_index {
        break;
      }

      let hoisted = Value::Upvalue(Managed::from(self.gc.allocate(upvalue.hoist(self.stack))));

      self.stack[index] = hoisted;
    }

    self.open_upvalues.retain(|upvalue| upvalue.is_open())
  }

  fn op_constant(&mut self, index: u8) {
    let frame = self.current_frame();
    let constant = VmExecutor::read_constant(frame, index).clone();
    self.push(constant);
  }

  fn get_val(&self, index: usize) -> &Value {
    unsafe { self.stack.get_unchecked(index) }
  }

  fn get_val_mut(&mut self, index: usize) -> &mut Value {
    unsafe { self.stack.get_unchecked_mut(index) }
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
      for i in 0..frame.closure.get_fun().upvalue_count {
        match &frame.closure.upvalues[i].borrow().location {
          Upvalue::Stack(loc) => {
            print!("[ stack {} ]", unsafe { &**loc });
          }
          Upvalue::Heap(loc) => {
            print!("[ heap {} ]", loc);
          }
        }
      }
      println!();
    }

    let frame = self.current_frame();
    disassemble_instruction(&frame.closure.get_fun().chunk, frame.ip);
  }

  /// Get the current instruction from the present call frame
  fn frame_instruction(&self) -> &ByteCode {
    let frame = self.current_frame();
    &frame.closure.get_fun().chunk.instructions[frame.ip]
  }
}

/// Is the provided `value` falsey according to spacelox rules
fn is_falsey(value: &Value) -> bool {
  match value {
    Value::Nil => true,
    Value::Bool(b) => !b,
    _ => false,
  }
}
