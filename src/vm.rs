use crate::chunk::{ByteCode, Chunk};
use crate::compiler::{Compiler, CompilerAnalytics};
use crate::memory::free_objects;
use crate::object::{Obj, ObjValue};
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

#[derive(Debug)]
pub enum InterpretResult {
  Ok,
  CompileError,
  RuntimeError,
}

/// The virtual machine for the spacelox programming language
pub struct Vm<'a> {
  pub chunk: Box<Chunk<'a>>,
  pub ip: usize,
  pub stack: Vec<Value<'a>>,
  pub objects: Cell<Option<&'a Obj<'a>>>,
  pub stack_top: usize,
  pub strings: Table<'a>,
  pub globals: Table<'a>
}

impl<'a> Drop for Vm<'a> {
  fn drop(&mut self) {
    if let Some(obj) = self.objects.get() {
      free_objects(obj);
    }
  }
}

impl<'a> Vm<'a> {
  pub fn new(stack: Vec<Value>) -> Vm {
    Vm {
      chunk: Box::new(Chunk::default()),
      ip: 0,
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
    let allocate = |value: ObjValue| self.allocate(value);
    let intern = |string: String| self.intern(string);

    let compiler = Compiler::new(
      source,
      Chunk::default(),
      CompilerAnalytics {
        allocate: &allocate,
        intern: &intern,
      },
    );
    let result = compiler.compile();

    if !result.success {
      return InterpretResult::CompileError;
    }

    self.chunk = Box::new(result.chunk);
    self.ip = 0;

    self.run()
  }

  fn run(&mut self) -> InterpretResult {
    loop {
      let op_code = &self.chunk.instructions[self.ip];

      #[cfg(debug_assertions)]
      self.print_debug();

      self.ip += 1;
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
        ByteCode::DefineGlobal(constant) => {
          let index_copy = *constant;
          self.op_define_global(index_copy);
        }
        ByteCode::SetGlobal(store_index) => {
          let index_copy = *store_index;
          let name = self.read_string(index_copy);

          if self.globals.store.insert(name.clone(), self.peek(0).clone()).is_none() {
            self.globals.store.remove_entry(&name);
            self.runtime_error(&format!("Undedfined variable {}", name));
            return InterpretResult::RuntimeError;
          }
        }
        ByteCode::GetGlobal(store_index) => {
          let index_copy = *store_index;
          let name = self.read_string(index_copy);

          match self.globals.store.get(&name) {
            Some(global) => {
              let global_clone = global.clone();
              self.push(global_clone);
            },
            None => {
              self.runtime_error(&format!("Undedfined variable {}", name));
              return InterpretResult::RuntimeError;
            }
          }
        }
        ByteCode::GetLocal(store_index) => {
          let copy = self.stack[*store_index as usize].clone();
          self.push(copy);
        }
        ByteCode::SetLocal(store_index) => {
          let copy = self.peek(0).clone();
          self.stack[*store_index as usize] = copy;
        }
        ByteCode::Pop => {
          self.pop();
        }
        ByteCode::Nil => self.push(Value::Nil),
        ByteCode::True => self.push(Value::Bool(true)),
        ByteCode::False => self.push(Value::Bool(false)),
        ByteCode::Constant(store_index) => {
          let index_copy = *store_index;
          self.op_constant(index_copy);
        }
        ByteCode::Print => {
          println!("{}", self.pop());
        }
        ByteCode::Return => {
          return InterpretResult::Ok;
        }
      }
    }
  }

  fn runtime_error(&mut self, message: &str) {
    eprintln!("{}", message);
    eprintln!("[line{}] in script", self.chunk.get_line(self.ip));
    self.reset_stack();
  }

  fn read_string(&mut self, index: u8) -> String {
    match self.read_constant(index) {
      Value::Obj(obj) => match obj.value {
        ObjValue::String(string) => {
          string
        }
      }
      _ => panic!("Expected object.")
    }

  }

  fn read_constant(&self, index: u8) -> Value<'a> {
    self.chunk.constants.values[index as usize].clone()
  }

  // fn read_byte(&mut self) -> ByteCode {
  //   let byte_code = self.chunk.instructions[self.ip].clone();
  //   self.ip += 1;

  //   byte_code
  // }

  fn allocate(&self, value: ObjValue) -> Obj<'a> {
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

  fn push(&mut self, value: Value<'a>) {
    self.stack[self.stack_top] = value;
    self.stack_top += 1;
  }

  fn peek(&self, distance: usize) -> &Value<'a> {
    &self.stack[self.stack_top - (distance + 1)]
  }

  fn pop(&mut self) -> Value<'a> {
    self.stack_top -= 1;
    replace(&mut self.stack[self.stack_top], Value::Nil)
  }

  fn reset_stack(&mut self) {
    self.stack_top = 0;
  }

  fn op_define_global(&mut self, store_index: u8) {
    let name = self.read_string(store_index);
    let global = self.pop();
    self.globals.store.insert(name, global);
  }

  fn op_negate(&mut self) {
    match self.pop() {
      Value::Number(num) => self.push(Value::Number(-num)),
      _ => self.runtime_error("Operand must be a number."),
    }
  }

  fn op_not(&mut self) {
    let value = self.pop();
    self.push(Value::Bool(is_falsey(value)))
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
          },
          _ => self.runtime_error("Operands must be two numbers or two strings."),
        },
        // _ => self.runtime_error(message: &str)
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
    let constant = self.read_constant(index);
    self.push(constant);
  }

  #[cfg(debug_assertions)]
  fn print_debug(&self) {
    print!("          ");
    for i in 0..self.stack_top {
      print!("[ {} ]", self.stack[i]);
    }
    println!();
    disassemble_instruction(&self.chunk, self.ip);
  }
}

fn read_file(path: &str) -> String {
  read_to_string(path).expect("Could not read file")
}

/// Is the provided `value` falsey according to spacelox rules
///
/// # Examples
/// ```
///
/// ```
fn is_falsey(value: Value) -> bool {
  match value {
    Value::Nil => true,
    Value::Bool(b) => !b,
    _ => false,
  }
}
