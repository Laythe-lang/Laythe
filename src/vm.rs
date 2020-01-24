use crate::chunk::{ByteCode};
use crate::compiler::{Compiler, CompilerAnalytics};
use crate::memory::free_objects;
use crate::chunk::Chunk;
use crate::object::{Obj, ObjValue, Fun};
use crate::table::Table;
use crate::value::Value;
use std::cell::Cell;
use std::fs::read_to_string;
use std::io::{stdin, stdout, Write};
use std::mem::replace;
use std::ops::Drop;

#[cfg(debug_assertions)]
use crate::debug::disassemble_instruction;

pub const DEFAULT_STACK_MAX: usize = 500;
pub const FRAME_MAX: usize = 255;

#[derive(Debug)]
pub enum InterpretResult {
  Ok,
  CompileError,
  RuntimeError,
}

/// A call frame in the space lox interpreter
#[derive(Debug, Clone, PartialEq)]
pub struct CallFrame<'a, 'b: 'a> {
  fun: &'a Fun<'b>,
  ip: usize,
  slots: usize,
}

impl<'a, 'b: 'a> CallFrame<'a, 'b> {
  pub fn new(fun: &'a Fun<'b>) -> Self {
    CallFrame {
      fun,
      ip: 0,
      slots: 0
    }
  }
}

/// The virtual machine for the spacelox programming language
pub struct Vm<'a> {
  pub frame_idx: usize,
  pub stack: Vec<Value<'a>>,
  pub objects: Cell<Option<&'a Obj<'a>>>,
  pub stack_top: usize,
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
  pub fn new(stack: Vec<Value<'a>>) -> Vm<'a> {
    Vm {
      frame_idx: 0,
      stack,
      objects: Cell::new(Option::None),
      stack_top: 0,
      strings: Table::default(),
      globals: Table::default(),
    }
  }

  pub fn repl(&mut self) {
    loop {
      let mut buffer = String::new();

      print!("> ");
      stdout().flush().expect("Could not write to stdout");

      match stdin().read_line(&mut buffer) {
        Ok(_) => {
          self.interpret(buffer.to_string());
        }
        Err(error) => panic!(error),
      }
    }
  }

  pub fn run_file(&mut self, path: &str) {
    let source = read_file(path);
    let result = self.interpret(source);

    match result {
      InterpretResult::CompileError => panic!("Compiler Error"),
      InterpretResult::RuntimeError => panic!("Runtime Error"),
      InterpretResult::Ok => (),
    }
  }

  fn interpret(&mut self, source: String) -> InterpretResult {
    let allocate = |value: ObjValue<'a>| self.allocate(value);
    let intern = |string: String| self.intern(string);

    let compiler = Compiler::new(
      source,
      CompilerAnalytics {
        allocate: &allocate,
        intern: &intern,
      },
    );
    let result = compiler.compile();

    if !result.success {
      return InterpretResult::CompileError;
    }

    let null_fun = Fun {
      arity: 0,
      chunk: Chunk::default(),
      name: Some("null function".to_string())
    };

    let frames = vec![
      CallFrame::new(&null_fun);
      FRAME_MAX
    ];
    let executor = VmExecutor::new(self, frames, &result.fun);
    executor.run()
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
  pub script_frame: CallFrame<'a, 'b>,
  pub frames: Vec<CallFrame<'a, 'b>>,
  pub frame_idx: usize,
  pub stack: &'a mut Vec<Value<'b>>,
  pub objects: &'a Cell<Option<&'b Obj<'b>>>,
  pub stack_top: usize,
  pub strings: &'a Table<'b>,
  pub globals: &'a mut Table<'b>,
}

impl<'a, 'b: 'a> VmExecutor<'a, 'b> {
  pub fn new(vm: &'a mut Vm<'b>, frames: Vec<CallFrame<'a, 'b>>, script: &'a Fun<'b>) -> VmExecutor<'a, 'b> {
    VmExecutor {
      script_frame: CallFrame::new(
        script,
      ),
      frames,
      frame_idx: 0,
      stack: &mut vm.stack,
      objects: &vm.objects,
      stack_top: 0,
      strings: &vm.strings,
      globals: &mut vm.globals,
    }
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
        ByteCode::JumpIfFalse(jump) => {
          self.op_jump_if_not_false(jump)
        }
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
        ByteCode::Return => {
          return InterpretResult::Ok;
        }
      }
    }
  }

  fn current_frame(&self) -> &CallFrame<'a, 'b> {
    if self.frame_idx == 0 {
      return &self.script_frame
    }

    &self.frames[self.frame_idx - 1]
  }

  fn current_mut_frame(&mut self) -> &mut CallFrame<'a, 'b> {
    if self.frame_idx == 0 {
      return &mut self.script_frame
    }

    &mut self.frames[self.frame_idx - 1]
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
    let frame = self.current_frame();

    eprintln!("{}", message);
    eprintln!("[line{}] in script", frame.fun.chunk.get_line(frame.ip));
    self.reset_stack();
  }

  fn read_string(&mut self, index: u8) -> String {
    let frame = self.current_frame();

    match self.read_constant(frame, index) {
      Value::Obj(obj) => match obj.value {
        ObjValue::String(string) => {
          string
        }
        _ => panic!("Expected string.")
      }
      _ => panic!("Expected object.")
    }

  }

  fn read_constant(&self, frame: &CallFrame<'a, 'b>, index: u8) -> Value<'b> {
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
      },
      None => {
        self.runtime_error(&format!("Undedfined variable {}", name));
        Some(InterpretResult::RuntimeError)
      }
    }
  }

  fn op_set_global(&mut self, store_index: u8) -> Option<InterpretResult> {
    let name = self.read_string(store_index);

    if self.globals.store.insert(name.clone(), self.peek(0).clone()).is_none() {
      self.globals.store.remove_entry(&name);
      self.runtime_error(&format!("Undedfined variable {}", name));
      return Some(InterpretResult::RuntimeError);
    }

    None
  }

  fn op_set_local(&mut self, store_index: u8) {
    let copy = self.peek(0).clone();
    let slots = self.current_frame().slots;
    self.stack[slots + store_index as usize] = copy;
  }

  fn op_get_local(&mut self, store_index: u8) {
    let frame = self.current_frame();
    let copy = self.stack[frame.slots + store_index as usize].clone();
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
    let constant = self.read_constant(frame, index);
    self.push(constant);
  }

  #[cfg(debug_assertions)]
  fn print_debug(&self) {
    let frame = self.current_frame();

    print!("          ");
    for i in 0..self.stack_top {
      print!("[ {} ]", self.stack[i]);
    }
    println!();
    disassemble_instruction(&frame.fun.chunk, self.frame_idx);
  }

  /// Get the current instruction from the present call frame
  fn frame_instruction(&self) -> ByteCode {
    let frame = self.current_frame();
    frame.fun.chunk.instructions[frame.ip].clone()
  }
}


/// A utility to read a file at a given path
fn read_file(path: &str) -> String {
  read_to_string(path).expect("Could not read file")
}

/// Is the provided `value` falsey according to spacelox rules
fn is_falsey(value: &Value) -> bool {
  match value {
    Value::Nil => true,
    Value::Bool(b) => !b,
    _ => false,
  }
}
