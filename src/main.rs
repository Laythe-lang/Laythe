use space_lox::native::create_natives;
use space_lox::value::Value;
use space_lox::vm::{InterpretResult, Vm, DEFAULT_STACK_MAX, FRAME_MAX, CallFrame};
use space_lox::object::Fun;
use std::env;
use std::fs::read_to_string;
use std::process;

fn main() {
  let null_fun = Box::new(Fun::default());

  let frames = vec![CallFrame::new(&null_fun); FRAME_MAX];
  let stack = vec![Value::Nil; DEFAULT_STACK_MAX];
  let natives = create_natives();


  let mut vm = Vm::new(stack, frames, natives);
  let args: Vec<String> = env::args().collect();

  match args.as_slice() {
    [_] => {
      vm.repl();
      process::exit(0);
    }
    [_, file_path] => match read_to_string(file_path) {
      Ok(source) => match vm.run(&source) {
        InterpretResult::Ok => process::exit(0),
        InterpretResult::CompileError => process::exit(2),
        InterpretResult::RuntimeError => process::exit(3),
      },
      Err(e) => {
        eprintln!("{}", e);
        process::exit(4)
      }
    },
    _ => {
      println!("Usage: spacelox [path]");
      process::exit(1);
    }
  }
}
