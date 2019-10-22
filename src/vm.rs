use std::io::{stdin, stdout, Write};
use std::fs::read_to_string;
use crate::chunk::{Chunk, OpCode};
use crate::compiler::compile;

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
  pub stack: [f64; STACK_MAX],
  pub stack_top: usize,
}

impl Vm {
  pub fn new() -> Vm {
    return Vm {
      chunk: Box::new(Chunk::new()),
      ip: 0,
      stack: [0.0; STACK_MAX],
      stack_top: 0,
    }
  }

  pub fn interpret(&mut self, source: &str) -> InterpretResult  {
    compile(source);
    return InterpretResult::Ok;
  }

  pub fn run(&mut self) -> InterpretResult {
    loop {
      let op_code = &self.chunk.instructions[self.ip];

      #[cfg(debug_assertions)]
      self.print_debug();

      self.ip += 1;
      match op_code {
        OpCode::Negate =>  self.op_negate(),
        OpCode::Add => self.op_add(),
        OpCode::Subtract => self.op_sub(),
        OpCode::Multiply => self.op_mul(),
        OpCode::Divide => self.op_div(),
        OpCode::Constant(index) => {
          let index_clone = index.clone();
          self.op_constant(index_clone);
        }
        OpCode::Return => {
          println!("{}", self.pop());
          return InterpretResult::Ok;
        }
      }
    }
  }

  pub fn repl(&mut self) {
    let mut input = String::new();
    loop {
      input.clear();
      print!("> ");
      stdout().flush().expect("Could not write to stdout");

      match stdin().read_line(&mut input) {
        Ok(_) => {
          self.interpret(input.as_str());
        }
        Err(error) => panic!(error)
      }
    }
  }

  pub fn run_file(&mut self, path: &str) {
    let source = read_file(path);
    let result = self.interpret(source.as_str());

    match result {
      InterpretResult::CompileError => panic!("Compiler Error"),
      InterpretResult::RuntimeError => panic!("Runtime Error"),
      InterpretResult::Ok => ()
    }
  }

  fn read_constant(&self, index: &usize) -> f64 {
    self.chunk.constants.values[*index]
  }

  fn push (&mut self, value: f64) {
    self.stack[self.stack_top] = value;
    self.stack_top += 1;
  }

  fn pop(&mut self) -> f64 {
    self.stack_top -= 1;
    self.stack[self.stack_top]
  }

  // fn reset_stack(&mut self) {
  //   self.stack_top = 0;
  // }

  fn op_negate(&mut self) {
    let value = -self.pop();
    self.push(value);
  }

  fn op_add(&mut self) {
    let right = self.pop();
    let left = self.pop();
    self.push(left + right);
  }

  fn op_sub(&mut self) {
    let right = self.pop();
    let left = self.pop();
    self.push(left - right);
  }

  fn op_mul(&mut self) {
    let right = self.pop();
    let left = self.pop();
    self.push(left * right);
  }

  fn op_div(&mut self) {
    let right = self.pop();
    let left = self.pop();
    self.push(left / right);
  }

  fn op_constant(&mut self, index: usize) {
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

