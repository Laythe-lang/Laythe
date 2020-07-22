#![deny(clippy::all)]
use laythe_vm::vm::{default_native_vm, ExecuteResult};
use std::env;
use std::fs::read_to_string;
use std::{path::PathBuf, process};

fn main() {
  let mut vm = default_native_vm();
  let args: Vec<String> = env::args().collect();

  match args.as_slice() {
    [_] => {
      vm.repl();
      process::exit(0);
    }
    _ => {
      let file_path = &args.as_slice()[1];
      let path = PathBuf::from(file_path);

      match read_to_string(&path) {
        Ok(source) => match vm.run(path, &source) {
          ExecuteResult::Ok => process::exit(0),
          ExecuteResult::FunResult(_) => panic!("Fun result should only be returned internally"),
          ExecuteResult::CompileError => process::exit(2),
          ExecuteResult::RuntimeError => process::exit(3),
          ExecuteResult::InternalError => process::exit(4),
        },
        Err(e) => {
          eprintln!("{}", e);
          process::exit(4)
        }
      }
    }
  }
}
