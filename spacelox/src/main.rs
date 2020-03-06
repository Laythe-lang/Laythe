use spacelox_core::native::create_natives;
use spacelox_core::value::{Closure, Fun, Managed, Value};
use spacelox_vm::memory::Gc;
use spacelox_vm::vm::{CallFrame, InterpretResult, Vm, DEFAULT_STACK_MAX, FRAME_MAX};
use std::env;
use std::fs::read_to_string;
use std::process;

fn main() {
  let fun = Box::new(Fun::default());
  let mut gc = Gc::new();
  let closure = Managed::from(gc.allocate(Closure::new(&fun)));

  let frames = vec![CallFrame::new(closure); FRAME_MAX];
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
