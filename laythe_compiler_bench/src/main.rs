use laythe_core::hooks::{GcHooks, NoContext};
use laythe_core::module::Module;
use laythe_env::{
  io::{Io, NativeIo},
  memory::Gc,
};
use laythe_vm::compiler::{Compiler, Parser};
use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::process;
use std::{path::PathBuf, time::Instant};

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
        let hooks = GcHooks::new(&mut context);
        let io = NativeIo::default();
        let mut parser = Parser::new(io.stdio(), &source);
        let module =
          Module::from_path(&hooks, hooks.manage(PathBuf::from("/Benchmark.ly"))).unwrap();
        let module = hooks.manage(module);
        let compiler = Compiler::new(module, io, &mut parser, &hooks);
        compiler.compile().unwrap();
      }
      println!("{}", ((now.elapsed().as_micros() as f64) / 1000000.0));
    }
    _ => {
      println!("Usage: laythe_compiler_bench [path]");
      process::exit(1);
    }
  }
}
