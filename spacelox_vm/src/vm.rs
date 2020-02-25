use crate::compiler::{Compiler, Parser};
use crate::memory::Allocator;
use spacelox_core::chunk::{ByteCode, UpvalueIndex};
use spacelox_core::native::{NativeFun, NativeResult};
use spacelox_core::object::{Closure, Fun, FunKind, Obj, ObjValue, Upvalue};
use spacelox_core::value::Value;
use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::{stdin, stdout, Write};
use std::mem::replace;
use std::ops::Drop;
use std::ptr::NonNull;
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
pub struct CallFrame<'a> {
  pub closure: Obj<'a>,
  ip: usize,
  slots: usize,
}

impl<'a> CallFrame<'a> {
  pub fn new(fun: &Fun<'a>) -> Self {
    CallFrame {
      closure: Obj::new(ObjValue::Closure(Closure::new(fun))),
      ip: 0,
      slots: 0,
    }
  }
}

/// The virtual machine for the spacelox programming language
pub struct Vm<'a> {
  /// A stack holding all local variable currently in use
  pub stack: Vec<Value<'a>>,

  /// A stack holding call frames currently in use
  pub frames: Vec<CallFrame<'a>>,

  /// All the native functions
  pub natives: Vec<Rc<dyn NativeFun<'a>>>,
}

impl<'a> Vm<'a> {
  pub fn new(
    stack: Vec<Value<'a>>,
    frames: Vec<CallFrame<'a>>,
    natives: Vec<Rc<dyn NativeFun<'a>>>,
  ) -> Vm<'a> {
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
    let mut allocator = Allocator::new();
    let mut parser = Parser::new(source);

    let compiler = Compiler::new(&mut parser, &mut allocator, FunKind::Script);
    let result = compiler.compile();

    if !result.success {
      return InterpretResult::CompileError;
    }

    let script = Value::Obj(allocator.allocate(ObjValue::Closure(Closure::new(&result.fun))));
    let globals = define_globals(&self.natives, &mut allocator);
    let executor = VmExecutor::new(self, script, globals, allocator);
    executor.run()
  }
}

fn define_globals<'a>(
  natives: &[Rc<dyn NativeFun<'a>>],
  allocator: &mut Allocator<'a>,
) -> HashMap<Obj<'a>, Value<'a>> {
  let mut globals = HashMap::new();
  natives.iter().for_each(|native| {
    let name = allocator.allocate_string(native.meta().name.clone());
    globals.insert(
      name,
      Value::Obj(allocator.allocate(ObjValue::NativeFn(Rc::clone(native)))),
    );
  });
  globals
}

pub struct VmExecutor<'a, 'b: 'a> {
  /// A stack of call frames for the current execution
  pub frames: &'a mut Vec<CallFrame<'b>>,

  /// The current frame depth of the program
  pub frame_count: usize,

  /// A stack holding all local variable currently in use
  pub stack: &'a mut Vec<Value<'b>>,

  /// A reference to a object currently in the vm
  allocator: Allocator<'b>,

  /// index to the top of the value stack
  pub stack_top: usize,

  /// global variable present in the vm
  pub globals: HashMap<Obj<'b>, Value<'b>>,

  /// A collection of currently available upvalues
  open_upvalues: Cell<Option<Rc<RefCell<Upvalue<'b>>>>>,
}

impl<'a, 'b: 'a> Drop for VmExecutor<'a, 'b> {
  fn drop(&mut self) {}
}

impl<'a, 'b: 'a> VmExecutor<'a, 'b> {
  pub fn new(
    vm: &'a mut Vm<'b>,
    script: Value<'b>,
    globals: HashMap<Obj<'b>, Value<'b>>,
    allocator: Allocator<'b>,
  ) -> VmExecutor<'a, 'b> {
    let mut executor = VmExecutor {
      frames: &mut vm.frames,
      frame_count: 0,
      stack: &mut vm.stack,
      allocator,
      stack_top: 1,
      globals,
      open_upvalues: Cell::new(None),
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
          let value = self.get_val(self.stack_top - 1) as *const Value<'b>;
          self.close_upvalues(value);
          self.pop();
        }
        ByteCode::Return => {
          let result = self.pop();
          let slots = self.get_val(self.current_frame().slots) as *const Value<'b>;
          self.close_upvalues(slots);
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

  fn current_frame(&self) -> &CallFrame<'b> {
    unsafe { self.frames.get_unchecked(self.frame_count - 1) }
  }

  fn current_mut_frame(&mut self) -> &mut CallFrame<'b> {
    unsafe { self.frames.get_unchecked_mut(self.frame_count - 1) }
  }

  fn call_value(&mut self, callee: Value<'b>, arg_count: u8) -> Option<InterpretResult> {
    match callee {
      Value::Obj(obj) => match obj.value {
        ObjValue::Closure(closure) => self.call(closure, arg_count),
        ObjValue::NativeFn(native) => self.call_native(&*native, arg_count),
        ObjValue::Fun(fun) => panic!(
          "function {} was not wrapped in a closure",
          fun.name.clone().unwrap_or("script".to_string())
        ),
        _ => {
          self.runtime_error("Can only call functions and classes.");
          Some(InterpretResult::RuntimeError)
        }
      },
      _ => {
        self.runtime_error("Can only call functions and classes.");
        Some(InterpretResult::RuntimeError)
      }
    }
  }

  fn call_native(&mut self, native: &dyn NativeFun<'b>, arg_count: u8) -> Option<InterpretResult> {
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

  fn call(&mut self, closure: Closure<'b>, arg_count: u8) -> Option<InterpretResult> {
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
    frame.closure = Obj::new(ObjValue::Closure(closure));
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
      let closure = &frame.closure.ref_closure();
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

  fn read_string(&mut self, index: u8) -> NonNull<str> {
    let frame = self.current_frame();

    match VmExecutor::read_constant(frame, index).clone() {
      Value::Obj(obj) => match obj.value {
        ObjValue::String(string) => string,
        _ => panic!("Expected string."),
      },
      _ => panic!("Expected object."),
    }
  }

  fn read_constant<'c>(frame: &'c CallFrame<'b>, index: u8) -> &'c Value<'b> {
    &frame.closure.ref_closure().get_fun().chunk.constants[index as usize]
  }

  fn push(&mut self, value: Value<'b>) {
    self.stack[self.stack_top] = value;
    self.stack_top += 1;
  }

  fn peek(&self, distance: usize) -> &Value<'b> {
    self.get_val(self.stack_top - (distance + 1))
  }

  fn pop(&mut self) -> Value<'b> {
    self.stack_top -= 1;
    let slot = self.get_val_mut(self.stack_top);
    replace(slot, Value::Nil)
  }

  fn reset_stack(&mut self) {
    self.stack_top = 0;
    self.frame_count = 0;
    self.open_upvalues.set(None);
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
    let name = self.allocator.copy_string(string);
    let global = self.pop();
    self.globals.insert(name, global);
  }

  fn op_get_global(&mut self, store_index: u8) -> Option<InterpretResult> {
    let string = self.read_string(store_index);
    let name = self.allocator.copy_string(string);

    let global = self.globals.get(&name).map(|global| global.clone());
    match global {
      Some(gbl) => {
        self.push(gbl);
        None
      }
      None => {
        self.runtime_error(&format!("Undefined variable {}", name));
        Some(InterpretResult::RuntimeError)
      }
    }
  }

  fn op_set_global(&mut self, store_index: u8) -> Option<InterpretResult> {
    let string = self.read_string(store_index);
    let name = self.allocator.copy_string(string);

    if self
      .globals
      .insert(name.clone(), self.peek(0).clone())
      .is_none()
    {
      self.globals.remove_entry(&name);
      self.runtime_error(&format!("Undefined variable {}", name));
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
    let value = self.peek(0) as *const Value<'b>;
    self.current_mut_frame().closure.ref_closure().upvalues[slot as usize]
      .borrow_mut()
      .set(value);
  }

  fn op_get_local(&mut self, slot: u8) {
    let slots = self.current_frame().slots;
    let copy = self.get_val(slots + slot as usize).clone();
    self.push(copy);
  }

  fn op_get_upvalue(&mut self, slot: u8) {
    let upvalue = &self.current_frame().closure.ref_closure().upvalues[slot as usize];
    let value = unsafe { &*upvalue.borrow().as_ptr() }.clone();
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
      (Value::Obj(obj1), Value::Obj(obj2)) => match (obj1.value, obj2.value) {
        (ObjValue::String(right), ObjValue::String(left)) => {
          let result = unsafe { format!("{}{}", left.as_ref(), right.as_ref()) };
          let value = self.allocator.allocate_string(result);
          self.push(Value::Obj(value));
          None
        }
        _ => {
          self.runtime_error("Operands must be two numbers or two strings.");
          Some(InterpretResult::RuntimeError)
        }
      },
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
    let fun = VmExecutor::read_constant(frame, index).ref_obj().ref_fun();
    let mut closure = Closure::new(fun);

    for _ in 0..fun.upvalue_count {
      let op_code = self.frame_instruction().clone();
      self.increment_frame_ip(1);

      if let ByteCode::UpvalueIndex(upvalue_index) = op_code {
        match upvalue_index {
          UpvalueIndex::Local(local) => {
            let slots = self.current_frame().slots;
            let value = self.get_val(slots + local as usize) as *const Value<'b>;
            closure.upvalues.push(self.capture_upvalue(value));
          }
          UpvalueIndex::Upvalue(upvalue) => {
            let upvalue = &self.current_frame().closure.ref_closure().upvalues[upvalue as usize];
            closure.upvalues.push(Rc::clone(upvalue));
          }
        }
      } else {
        self.internal_error("Expected upvalues following closures")
      }
    }

    let closure = Value::Obj(self.allocator.allocate(ObjValue::Closure(closure)));

    self.push(closure);
  }

  fn capture_upvalue(&mut self, local: *const Value<'b>) -> Rc<RefCell<Upvalue<'b>>> {
    let mut prev_upvalue: *const Option<Rc<RefCell<Upvalue<'b>>>> = &*Box::new(None);
    let mut upvalue: *const Option<Rc<RefCell<Upvalue<'b>>>> = self.open_upvalues.as_ptr();

    while let Some(upval) = unsafe { &*upvalue } {
      if upval.as_ref().borrow().as_ptr() <= local {
        break;
      }

      prev_upvalue = upvalue;
      upvalue = &upval.borrow().next as *const Option<Rc<RefCell<Upvalue<'b>>>>;
    }

    if let Some(upval) = unsafe { &*upvalue } {
      if upval.as_ref().borrow().as_ptr() == local {
        return Rc::clone(&upval);
      }
    }

    let created_upvalue = Rc::new(RefCell::new(Upvalue::new(local, unsafe {
      (*upvalue).as_ref().map(|val| Rc::clone(val))
    })));

    match unsafe { &*prev_upvalue } {
      Some(upvalue) => {
        upvalue
          .borrow_mut()
          .next
          .replace(Rc::clone(&created_upvalue));
      }
      None => {
        self
          .open_upvalues
          .replace(Some(Rc::clone(&created_upvalue)));
      }
    }

    created_upvalue
  }

  fn close_upvalues(&mut self, last: *const Value<'b>) {
    while let Some(upvalue) = unsafe { &*self.open_upvalues.as_ptr() } {
      if upvalue.as_ref().borrow().as_ptr() < last {
        break;
      }

      let mut mut_upvalue = upvalue.borrow_mut();
      mut_upvalue.hoist();
      self
        .open_upvalues
        .replace(mut_upvalue.next.as_ref().map(|rc| Rc::clone(rc)));
    }
  }

  fn op_constant(&mut self, index: u8) {
    let frame = self.current_frame();
    let constant = VmExecutor::read_constant(frame, index).clone();
    self.push(constant);
  }

  fn get_val(&self, index: usize) -> &Value<'b> {
    unsafe { self.stack.get_unchecked(index) }
  }

  fn get_val_mut(&mut self, index: usize) -> &mut Value<'b> {
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
          UpvalueLocation::Stack(loc) => {
            print!("[ stack {} ]", unsafe { &**loc });
          }
          UpvalueLocation::Heap(loc) => {
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
    &frame.closure.ref_closure().get_fun().chunk.instructions[frame.ip]
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
