use std::io::{stdin, stdout, Write};
use std::fs::{read_to_string};
use std::mem::{discriminant};
use std::ptr;
use crate::chunk::{Chunk, OpCode};
use crate::compiler::{Compiler};
use crate::value::{Value};

#[cfg(debug_assertions)]
use crate::debug::disassemble_instruction;

const STACK_MAX: usize = 500;

#[derive(Debug)]
pub enum InterpretResult {
  Ok,
  CompileError,
  RuntimeError,
}

pub struct Vm {
  pub chunk: Box<Chunk>,
  pub ip: usize,
  pub stack: Vec<Value>,
  pub stack_top: usize,
}

impl Vm {
  pub fn new() -> Vm {
    let mut stack = Vec::with_capacity(STACK_MAX);
    for _ in 0..STACK_MAX {
      stack.push(Value::Nil);
    }


    return Vm {
      chunk: Box::new(Chunk::new()),
      ip: 0,
      stack: stack,
      stack_top: 0,
    }
  }

  pub fn repl(&mut self) {
    let mut buffer = String::new();

    loop {
      print!("> ");
      stdout().flush().expect("Could not write to stdout");

      match stdin().read_line(&mut buffer) {
        Ok(_) => {
          self.interpret(buffer.to_string());
        }
        Err(error) => panic!(error)
      }
    }
  }

  pub fn run_file(&mut self, path: &str) {
    let source = read_file(path);
    let result = self.interpret(source);

    match result {
      InterpretResult::CompileError => panic!("Compiler Error"),
      InterpretResult::RuntimeError => panic!("Runtime Error"),
      InterpretResult::Ok => ()
    }
  }

  fn interpret(&mut self, source: String) -> InterpretResult  {
    let compiler = Compiler::new(source, Chunk::new());
    let (success, chunk) = compiler.compile();

    if !success {
      return InterpretResult::CompileError
    }

    self.chunk = Box::new(chunk);
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
        OpCode::Negate => self.op_negate(),
        OpCode::Add => self.op_add(),
        OpCode::Subtract => self.op_sub(),
        OpCode::Multiply => self.op_mul(),
        OpCode::Divide => self.op_div(),
        OpCode::Not => self.op_not(),
        OpCode::Constant(index) => {
          let index_copy = index.clone();
          self.op_constant(index_copy);
        }
        OpCode::Nil => self.push(Value::Nil),
        OpCode::True => self.push(Value::Bool(true)),
        OpCode::False => self.push(Value::Bool(false)),
        OpCode::Equal => self.op_equal(),
        OpCode::Greater => self.op_greater(),
        OpCode::Less => self.op_less(),
        OpCode::Return => {
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

  fn read_constant(&self, index: &u8) -> Value {
    self.chunk.constants.values[*index as usize].clone()
  }

  fn push (&mut self, value: Value) {
    self.stack[self.stack_top] = value;
    self.stack_top += 1;
  }

  fn pop(&mut self) -> Value {
    unsafe {
      self.stack_top -= 1;
      ptr::read(&self.stack[self.stack_top])
    }
  }

  fn reset_stack(&mut self) {
    self.stack_top = 0;
  }

  fn op_negate(&mut self) {
    match self.pop() {
      Value::Number(num) => self.push(Value::Number(-num)),
      _ => self.runtime_error("Operand must be a number.")
    }
  }

  fn op_not(&mut self) {
    unsafe {
      self.stack_top -= 1;
      let value = ptr::read(&self.stack[self.stack_top]);
      self.push(Value::Bool(is_falsey(value)))
    }
  }

  fn op_add(&mut self) {
    match self.pop() {
      Value::Number(left) => match self.pop() {
        Value::Number(right) => self.push(Value::Number(left + right)),
        _ => self.runtime_error("Operands must be numbers."),
      },
      _ => self.runtime_error("Operands must be numbers."),
    }
  }

  fn op_sub(&mut self) {
    match self.pop() {
      Value::Number(left) => match self.pop() {
        Value::Number(right) => self.push(Value::Number(left - right)),
        _ => self.runtime_error("Operands must be numbers."),
      },
      _ => self.runtime_error("Operands must be numbers."),
    }
  }

  fn op_mul(&mut self) {
    match self.pop() {
      Value::Number(left) => match self.pop() {
        Value::Number(right) => self.push(Value::Number(left * right)),
        _ => self.runtime_error("Operands must be numbers."),
      },
      _ => self.runtime_error("Operands must be numbers."),
    }
  }

  fn op_div(&mut self) {
    match self.pop() {
      Value::Number(left) => match self.pop() {
        Value::Number(right) => self.push(Value::Number(left / right)),
        _ => self.runtime_error("Operands must be numbers."),
      },
      _ => self.runtime_error("Operands must be numbers."),
    }
  }

  fn op_less(&mut self) {
    match self.pop() {
      Value::Number(left) => match self.pop() {
        Value::Number(right) => self.push(Value::Bool(left < right)),
        _ => self.runtime_error("Operands must be numbers."),
      },
      _ => self.runtime_error("Operands must be numbers."),
    }
  }

  fn op_greater(&mut self) {
    match self.pop() {
      Value::Number(left) => match self.pop() {
        Value::Number(right) => self.push(Value::Bool(left > right)),
        _ => self.runtime_error("Operands must be numbers."),
      },
      _ => self.runtime_error("Operands must be numbers."),
    }
  }

  fn op_equal(&mut self) {
    unsafe {
      let left = ptr::read(&self.stack[self.stack_top - 1]);
      let right = ptr::read(&self.stack[self.stack_top - 2]);
      self.stack_top -= 2;

      self.push(Value::Bool(values_equal(left, right)));
    }
  }


  fn op_constant(&mut self, index: u8) {
    let constant = self.read_constant(&index);
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

///
fn values_equal(left: Value, right: Value) -> bool {
  if discriminant(&left) != discriminant(&right) {
    return false
  }

  match left {
    Value::Number(num1) => match right {
      Value::Number(num2) => num1 == num2,
      _ => panic!("discriminant failed")
    },
    Value::Bool(b1) => match right {
      Value::Bool(b2) => b1 == b2,
      _ => panic!("discriminant failed")
    },
    Value::Nil => true,
    Value::Obj(_) => panic!("panic!")
  }
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