use spacelox_vm::compiler::{Compiler, Parser};
use spacelox_core::hooks::{Hooks, NoContext};
use spacelox_core::memory::Gc;
use spacelox_core::io::{NativeIo, Io};
use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::process;
use std::time::Instant;

fn load_source(path: &str) -> String {
  let mut file = File::open(path).unwrap();
  let mut source = String::new();
  file.read_to_string(&mut source).unwrap();
  source
}

fn main() {
  let args: Vec<String> = env::args().collect();
  match args.as_slice() {
    [_, file_path] => {
      let source = load_source(file_path);
      let now = Instant::now();

      for _ in 0..1000000 {
        let gc = Gc::default();
        let mut context = NoContext::new(&gc);
        let hooks = Hooks::new(&mut context);
        let io = NativeIo::new();
        let mut parser = Parser::new(io.stdio(), &source);
        let compiler = Compiler::new(io, &mut parser, &hooks);
        compiler.compile();
      }
      println!("{}", ((now.elapsed().as_micros() as f64) / 1000000.0));
    },
    _ => {
      println!("Usage: spacelox_compiler_bench [path]");
      process::exit(1);
    }
  }
}
