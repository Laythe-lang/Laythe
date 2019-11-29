use lox_runtime::vm::{Vm, pre_allocated_stack, DEFAULT_STACK_MAX};
use std::env;

fn main() {
    let mut vm = Vm::new(pre_allocated_stack(DEFAULT_STACK_MAX));
    let args: Vec<String> = env::args().collect();

    match args.as_slice() {
        [_] => vm.repl(),
        [_, file_path] => vm.run_file(file_path),
        _ => panic!("Usage: johnlox [path]"),
    }
}
