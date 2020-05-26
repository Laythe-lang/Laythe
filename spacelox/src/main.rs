#![deny(clippy::all)]
use spacelox_vm::vm::{default_native_vm, ExecuteResult};
use std::env;
use std::fs::read_to_string;
use std::{ffi::OsStr, path::Path, process};

fn main() {
  let mut vm = default_native_vm();
  let args: Vec<String> = env::args().collect();

  match args.as_slice() {
    [_] => {
      vm.repl();
      process::exit(0);
    }
    [_, file_path] => {
      let path = Path::new(file_path);
      let name = path.file_name().unwrap_or(OsStr::new("unknown"));
      let file_name = name.to_str();

      match (file_name, read_to_string(path)) {
        (Some(file_name), Ok(source)) => match vm.run(file_name, &source) {
          ExecuteResult::Ok => process::exit(0),
          ExecuteResult::FunResult(_) => panic!("Fun result should only be returned internally"),
          ExecuteResult::CompileError => process::exit(2),
          ExecuteResult::RuntimeError => process::exit(3),
        },
        (_, Err(e)) => {
          eprintln!("{}", e);
          process::exit(4)
        }
        (None, _) => {
          eprintln!("{}", "Could not convert filename");
          process::exit(4)
        }
      }
    }
    _ => {
      println!("Usage: spacelox [path]");
      process::exit(1);
    }
  }
}
