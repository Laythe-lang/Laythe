use lox_runtime::value::Value;
use lox_runtime::vm::{Vm, create_natives, DEFAULT_STACK_MAX};
use std::env;

fn main() {
  let natives = create_natives();
  let mut vm = Vm::new(vec![Value::Nil; DEFAULT_STACK_MAX], natives);
  let args: Vec<String> = env::args().collect();

  match args.as_slice() {
    [_] => vm.repl(),
    [_, file_path] => vm.run_file(file_path),
    _ => panic!("Usage: spacelox [path]"),
  }
}
