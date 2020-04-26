#![deny(clippy::all)]
use spacelox_vm::vm::{default_native_vm, ExecuteResult};
use std::env;
use std::fs::read_to_string;
use std::process;

fn main() {
  let mut vm = default_native_vm();
  let args: Vec<String> = env::args().collect();

  match args.as_slice() {
    [_] => {
      vm.repl();
      process::exit(0);
    }
    [_, file_path] => match read_to_string(file_path) {
      Ok(source) => match vm.run(&source) {
        ExecuteResult::Ok => process::exit(0),
        ExecuteResult::FunResult(_) => panic!("Fun result should only be returned internally"),
        ExecuteResult::CompileError => process::exit(2),
        ExecuteResult::RuntimeError => process::exit(3),
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
