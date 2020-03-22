use spacelox_core::native::create_natives;
use spacelox_core::value::{Closure, Fun, Value};
use spacelox_vm::call_frame::CallFrame;
use spacelox_vm::constants::DEFAULT_STACK_MAX;
use spacelox_vm::constants::FRAME_MAX;
use spacelox_vm::memory::{Gc, NO_GC};
use spacelox_vm::vm::{InterpretResult, Vm};
use std::env;
use std::fs::read_to_string;
use std::process;

fn main() {
  let fun = Fun::default();
  let gc = Gc::new();

  let managed_fun = gc.manage(fun, &NO_GC);
  let closure = gc.manage(Closure::new(managed_fun), &NO_GC);

  let frames = vec![CallFrame::new(closure); FRAME_MAX];
  let stack = vec![Value::Nil; DEFAULT_STACK_MAX];
  let natives = create_natives();

  let mut vm = Vm::new(stack, frames, &natives);
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
        InterpretResult::InternalError => process::exit(4),
      },
      Err(e) => {
        eprintln!("{}", e);
        process::exit(5)
      }
    },
    _ => {
      println!("Usage: spacelox [path]");
      process::exit(1);
    }
  }
}
