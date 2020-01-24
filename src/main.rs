use lox_runtime::vm::{Vm, DEFAULT_STACK_MAX};
use lox_runtime::value::{Value};
use std::env;

fn main() {
    let mut vm = Vm::new(
        vec![Value::Nil; DEFAULT_STACK_MAX],
    );
    let args: Vec<String> = env::args().collect();

    match args.as_slice() {
        [_] => vm.repl(),
        [_, file_path] => vm.run_file(file_path),
        _ => panic!("Usage: spacelox [path]"),
    }
}
