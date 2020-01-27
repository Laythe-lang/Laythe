use crate::chunk::ByteCode;
use crate::chunk::Chunk;
use crate::compiler::{Compiler, CompilerAnalytics, Parser};
use crate::memory::free_objects;
use crate::native::{NativeFun, NativeResult};
use crate::object::{Fun, FunKind, Obj, ObjValue};
use crate::table::Table;
use crate::value::Value;
use std::cell::Cell;
use std::io::{stdin, stdout, Write};
use std::mem::replace;
use std::ops::Drop;
use std::rc::Rc;

#[cfg(debug_assertions)]
use crate::debug::disassemble_instruction;

pub const FRAME_MAX: usize = std::u8::MAX as usize;
pub const DEFAULT_STACK_MAX: usize = FRAME_MAX * 16;

#[derive(Debug, Clone, PartialEq)]
pub enum InterpretResult {
  Ok,
  CompileError,
  RuntimeError,
}

/// A call frame in the space lox interpreter
#[derive(Debug, Clone, PartialEq)]
pub struct CallFrame<'a> {
  fun: Rc<Fun<'a>>,
  ip: usize,
  slots: usize,
}

impl<'a> CallFrame<'a> {
  pub fn new(fun: &Rc<Fun<'a>>) -> Self {
    CallFrame {
      fun: fun.clone(),
      ip: 0,
      slots: 0,
    }
  }
}

/// The virtual machine for the spacelox programming language
pub struct Vm<'a> {
  pub frame_idx: usize,
  pub stack: Vec<Value<'a>>,
  pub objects: Cell<Option<&'a Obj<'a>>>,
  pub strings: Table<'a>,
  pub globals: Table<'a>,
}

impl<'a> Drop for Vm<'a> {
  fn drop(&mut self) {
    if let Some(obj) = self.objects.get() {
      free_objects(obj);
    }
  }
}

impl<'a> Vm<'a> {
  pub fn new(stack: Vec<Value<'a>>, natives: Vec<NativeFun<'a>>) -> Vm<'a> {
    let mut vm = Vm {
      frame_idx: 0,
      stack,
      objects: Cell::new(Option::None),
      strings: Table::default(),
      globals: Table::default(),
    };

    for native in natives.into_iter() {
      vm.define_native(native);
    }

    vm
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
    let allocate = |value: ObjValue<'a>| self.allocate(value);
    let intern = |string: String| self.intern(string);
    let analytics = CompilerAnalytics {
      allocate: &allocate,
      intern: &intern,
    };

    let mut parser = Parser::new(source);

    let compiler = Compiler::new(&mut parser, &analytics, FunKind::Script);
    let result = compiler.compile();

    if !result.success {
      return InterpretResult::CompileError;
    }

    let null_fun = Rc::new(Fun {
      arity: 0,
      chunk: Chunk::default(),
      name: Some("null function".to_string()),
    });

    let frames = vec![CallFrame::new(&null_fun); FRAME_MAX];

    let script = Value::Obj(Obj::new(ObjValue::Fun(result.fun)));
    let executor = VmExecutor::new(self, frames, script);
    executor.run()
  }

  fn define_native(&mut self, native: NativeFun<'a>) {
    self.globals.store.insert(
      native.name.clone(),
      Value::Obj(Obj::new(ObjValue::NativeFn(native))),
    );
  }

  fn allocate(&self, value: ObjValue<'a>) -> Obj<'a> {
    let obj = Obj::new(value);
    obj.next.set(self.objects.get());
    self.objects.set(obj.next.get());

    obj
  }

  fn intern(&self, value: String) -> String {
    match self.strings.store.get_key_value(&value) {
      Some((stored_key, _)) => stored_key.clone(),
      None => value,
    }
  }
}

pub struct VmExecutor<'a, 'b: 'a> {
  pub frames: Vec<CallFrame<'b>>,
  pub frame_count: usize,
  pub stack: &'a mut Vec<Value<'b>>,
  pub objects: &'a Cell<Option<&'b Obj<'b>>>,
  pub stack_top: usize,
  pub strings: &'a Table<'b>,
  pub globals: &'a mut Table<'b>,
}

impl<'a, 'b: 'a> VmExecutor<'a, 'b> {
  pub fn new(
    vm: &'a mut Vm<'b>,
    frames: Vec<CallFrame<'b>>,
    script: Value<'b>,
  ) -> VmExecutor<'a, 'b> {
    let mut executor = VmExecutor {
      frames,
      frame_count: 0,
      stack: &mut vm.stack,
      objects: &vm.objects,
      stack_top: 1,
      strings: &vm.strings,
      globals: &mut vm.globals,
    };

    executor.call_value(script, 0);
    executor
  }

  fn run(mut self) -> InterpretResult {
    loop {
      let op_code = self.frame_instruction();

      #[cfg(debug_assertions)]
      self.print_debug();

      self.increment_frame_ip(1);
      match op_code {
        ByteCode::Negate => self.op_negate(),
        ByteCode::Add => self.op_add(),
        ByteCode::Subtract => self.op_sub(),
        ByteCode::Multiply => self.op_mul(),
        ByteCode::Divide => self.op_div(),
        ByteCode::Not => self.op_not(),
        ByteCode::Equal => self.op_equal(),
        ByteCode::Greater => self.op_greater(),
        ByteCode::Less => self.op_less(),
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
        ByteCode::GetGlobal(store_index) => {
          if let Some(result) = self.op_get_global(store_index) {
            return result;
          }
        }
        ByteCode::SetGlobal(store_index) => {
          if let Some(result) = self.op_set_global(store_index) {
            return result;
          }
        }
        ByteCode::GetLocal(store_index) => {
          self.op_get_local(store_index);
        }
        ByteCode::SetLocal(store_index) => {
          self.op_set_local(store_index);
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
          if !self.call_value(self.peek(arg_count as usize).clone(), arg_count) {
            return InterpretResult::RuntimeError;
          }
        }
        ByteCode::Return => {
          let result = self.pop();
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
    &self.frames[self.frame_count - 1]
  }

  fn current_mut_frame(&mut self) -> &mut CallFrame<'b> {
    &mut self.frames[self.frame_count - 1]
  }

  fn call_value(&mut self, callee: Value<'b>, arg_count: u8) -> bool {
    match callee {
      Value::Obj(obj) => match &obj.value {
        ObjValue::Fun(fun) => self.call(fun, arg_count),
        ObjValue::NativeFn(native) => self.call_native(native, arg_count),
        _ => {
          self.runtime_error("Can only call functions and classes.");
          false
        }
      },
      _ => {
        self.runtime_error("Can only call functions and classes.");
        false
      }
    }
  }

  fn call_native(&mut self, native: &NativeFun<'b>, arg_count: u8) -> bool {
    if arg_count != native.arity {
      self.runtime_error(&format!(
        "Function {} expected {} argument but got {}",
        native.name, native.arity, arg_count,
      ));
      return false;
    }

    let result = (native.fun)(&self.stack[(self.stack_top - arg_count as usize)..self.stack_top]);
    return match result {
      NativeResult::Success(value) => {
        self.push(value);
        self.stack_top -= arg_count as usize + 1;
        true
      }
      NativeResult::RuntimeError(message) => {
        self.runtime_error(&message);
        false
      }
    };
  }

  fn call(&mut self, fun: &Rc<Fun<'b>>, arg_count: u8) -> bool {
    if (arg_count as u16) != fun.arity {
      self.runtime_error(&format!(
        "Function {} expected {} arguments but got {}",
        fun.name.clone().unwrap_or("script".to_string()),
        fun.arity,
        arg_count
      ));
      return false;
    }

    if self.frame_count == FRAME_MAX {
      self.runtime_error("Stack overflow.");
      return false;
    }

    self.frame_count += 1;
    let frame = &mut self.frames[self.frame_count - 1];
    frame.fun = fun.clone();
    frame.ip = 0;
    frame.slots = self.stack_top - (arg_count as usize + 1);
    true
  }

  fn increment_frame_ip(&mut self, offset: usize) {
    let frame = self.current_mut_frame();
    frame.ip += offset;
  }

  fn decrement_frame_ip(&mut self, offset: usize) {
    let frame = self.current_mut_frame();
    frame.ip -= offset;
  }

  fn runtime_error(&mut self, message: &str) {
    eprintln!("{}", message);
    eprintln!("");

    for frame in self.frames[0..self.frame_count].into_iter().rev() {
      let fun = &frame.fun;
      let location = match &fun.name {
        Some(name) => format!("{}()", name),
        None => "script".to_string(),
      };

      eprintln!("[line {}] in {}", fun.chunk.get_line(frame.ip), location);
    }

    self.reset_stack();
  }

  fn read_string(&mut self, index: u8) -> String {
    let frame = self.current_frame();

    match VmExecutor::read_constant(frame, index) {
      Value::Obj(obj) => match obj.value {
        ObjValue::String(string) => string,
        _ => panic!("Expected string."),
      },
      _ => panic!("Expected object."),
    }
  }

  fn read_constant(frame: &CallFrame<'b>, index: u8) -> Value<'b> {
    frame.fun.chunk.constants.values[index as usize].clone()
  }

  fn push(&mut self, value: Value<'b>) {
    self.stack[self.stack_top] = value;
    self.stack_top += 1;
  }

  fn peek(&self, distance: usize) -> &Value<'b> {
    &self.stack[self.stack_top - (distance + 1)]
  }

  fn pop(&mut self) -> Value<'b> {
    self.stack_top -= 1;
    replace(&mut self.stack[self.stack_top], Value::Nil)
  }

  fn reset_stack(&mut self) {
    self.stack_top = 0;
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
    let name = self.read_string(store_index);
    let global = self.pop();
    self.globals.store.insert(name, global);
  }

  fn op_get_global(&mut self, store_index: u8) -> Option<InterpretResult> {
    let name = self.read_string(store_index);

    match self.globals.store.get(&name) {
      Some(global) => {
        let global_clone = global.clone();
        self.push(global_clone);
        None
      }
      None => {
        self.runtime_error(&format!("Undedfined variable {}", name));
        Some(InterpretResult::RuntimeError)
      }
    }
  }

  fn op_set_global(&mut self, store_index: u8) -> Option<InterpretResult> {
    let name = self.read_string(store_index);

    if self
      .globals
      .store
      .insert(name.clone(), self.peek(0).clone())
      .is_none()
    {
      self.globals.store.remove_entry(&name);
      self.runtime_error(&format!("Undedfined variable {}", name));
      return Some(InterpretResult::RuntimeError);
    }

    None
  }

  fn op_set_local(&mut self, store_index: u8) {
    let copy = self.peek(0).clone();
    let slots = self.current_frame().slots;
    self.stack[1 + slots + store_index as usize] = copy;
  }

  fn op_get_local(&mut self, store_index: u8) {
    let slots = self.current_frame().slots;
    let copy = self.stack[1 + slots + store_index as usize].clone();
    self.push(copy);
  }

  fn op_negate(&mut self) {
    match self.pop() {
      Value::Number(num) => self.push(Value::Number(-num)),
      _ => self.runtime_error("Operand must be a number."),
    }
  }

  fn op_not(&mut self) {
    let value = self.pop();
    self.push(Value::Bool(is_falsey(&value)))
  }

  fn op_add(&mut self) {
    match self.peek(0) {
      Value::Obj(obj1) => match &obj1.value {
        ObjValue::String(_str1) => match self.peek(1) {
          Value::Obj(obj2) => match &obj2.value {
            ObjValue::String(_str2) => {
              let right = self.pop().move_obj().move_string();
              let left = self.pop().move_obj().move_string();

              let result = format!("{}{}", left, right);
              self.push(Value::Obj(Obj::new(ObjValue::String(result))));
            }
            _ => self.runtime_error("Operands must be two numbers or two strings."),
          },
          _ => self.runtime_error("Operands must be two numbers or two strings."),
        },
        _ => self.runtime_error("Operands must be two numbers or two strings."),
      },
      Value::Number(_num1) => match self.peek(1) {
        Value::Number(_num2) => {
          let right = self.pop().to_num();
          let left = self.pop().to_num();
          self.push(Value::Number(left + right));
        }
        _ => self.runtime_error("Operands must be two numbers or two strings."),
      },
      _ => self.runtime_error("Operands must be two numbers or two strings."),
    }
  }

  fn op_sub(&mut self) {
    match self.pop() {
      Value::Number(right) => match self.pop() {
        Value::Number(left) => self.push(Value::Number(left - right)),
        _ => self.runtime_error("Operands must be numbers."),
      },
      _ => self.runtime_error("Operands must be numbers."),
    }
  }

  fn op_mul(&mut self) {
    match self.pop() {
      Value::Number(right) => match self.pop() {
        Value::Number(left) => self.push(Value::Number(left * right)),
        _ => self.runtime_error("Operands must be numbers."),
      },
      _ => self.runtime_error("Operands must be numbers."),
    }
  }

  fn op_div(&mut self) {
    match self.pop() {
      Value::Number(right) => match self.pop() {
        Value::Number(left) => self.push(Value::Number(left / right)),
        _ => self.runtime_error("Operands must be numbers."),
      },
      _ => self.runtime_error("Operands must be numbers."),
    }
  }

  fn op_less(&mut self) {
    match self.pop() {
      Value::Number(right) => match self.pop() {
        Value::Number(left) => self.push(Value::Bool(left < right)),
        _ => self.runtime_error("Operands must be numbers."),
      },
      _ => self.runtime_error("Operands must be numbers."),
    }
  }

  fn op_greater(&mut self) {
    match self.pop() {
      Value::Number(right) => match self.pop() {
        Value::Number(left) => self.push(Value::Bool(left > right)),
        _ => self.runtime_error("Operands must be numbers."),
      },
      _ => self.runtime_error("Operands must be numbers."),
    }
  }

  fn op_equal(&mut self) {
    let right = self.pop();
    let left = self.pop();

    self.push(Value::Bool(left == right));
  }

  fn op_constant(&mut self, index: u8) {
    let frame = self.current_frame();
    let constant = VmExecutor::read_constant(frame, index);
    self.push(constant);
  }

  #[cfg(debug_assertions)]
  fn print_debug(&self) {
    print!("          ");
    for i in 1..self.stack_top {
      print!("[ {} ]", self.stack[i]);
    }
    println!();

    let frame = self.current_frame();
    disassemble_instruction(&frame.fun.chunk, frame.ip);
  }

  /// Get the current instruction from the present call frame
  fn frame_instruction(&self) -> ByteCode {
    let frame = self.current_frame();
    frame.fun.chunk.instructions[frame.ip].clone()
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
