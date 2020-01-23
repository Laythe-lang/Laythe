use lox_runtime::vm::{Vm, CallFrame, DEFAULT_STACK_MAX, FRAME_MAX};
use lox_runtime::value::{Value};
use lox_runtime::chunk::{Chunk};
use lox_runtime::object::{Fun};
use std::env;

fn main() {
    let null_fun = Fun {
        arity: 0,
        chunk: Chunk::default(),
        name: Some("null function".to_string())
    };

    let mut vm = Vm::new(
        vec![Value::Nil; DEFAULT_STACK_MAX],
        vec![CallFrame::new(&null_fun); FRAME_MAX]
    );
    let args: Vec<String> = env::args().collect();

    match args.as_slice() {
        [_] => vm.repl(),
        [_, file_path] => vm.run_file(file_path),
        _ => panic!("Usage: spacelox [path]"),
    }
}
