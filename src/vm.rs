use crate::chunk::{ByteCode, Chunk};
use crate::compiler::Compiler;
use crate::object::{Obj, ObjValue};
use crate::value::Value;
use crate::memory::free_objects;
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
  pub objects: Option<&'a Obj<'a>>,
  pub stack_top: usize,
}

impl<'a> Drop for Vm<'a> {
  fn drop(&mut self) {
    match self.objects {
      Some(obj) => free_objects(obj),
      None => { }
    }
  }
} 

impl<'a> Vm<'a> {
  pub fn new(stack: Vec<Value>) -> Vm {
    Vm {
      chunk: Box::new(Chunk::default()),
      ip: 0,
      stack,
      objects: Option::None,
      stack_top: 0,
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
    // let allocate = |obj: Obj| self.allocate(obj);
    let compiler = Compiler::new(source, Chunk::default());
    let mut result = compiler.compile();

    match result.obj_tail {
      Some(_) => {
        result.obj_tail = self.objects;
        self.objects = result.obj_head;
      }
      None => {}
    }

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
        ByteCode::Constant(index) => {
          let index_copy = *index;
          self.op_constant(index_copy);
        }
        ByteCode::Nil => self.push(Value::Nil),
        ByteCode::True => self.push(Value::Bool(true)),
        ByteCode::False => self.push(Value::Bool(false)),
        ByteCode::Equal => self.op_equal(),
        ByteCode::Greater => self.op_greater(),
        ByteCode::Less => self.op_less(),
        ByteCode::Return => {
          if self.stack_top == 1 {
            println!("{}", self.pop());
          }

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

  fn read_constant(&self, index: u8) -> Value<'a> {
    self.chunk.constants.values[index as usize].clone()
  }

  // fn allocate(&mut self, obj: Obj) -> Obj {
  //   obj.next = Some(self.objects);
  //   let strong_ref = Rc::new(obj);
  //   self.objects = strong_ref;

  //   obj
  // }

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

pub fn pre_allocated_stack<'a>(size: usize) -> Vec<Value<'a>> {
  let mut stack = Vec::with_capacity(size);

  // fill and initialize the stack with values
  for _ in 0..size {
    stack.push(Value::Nil);
  }

  stack
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
