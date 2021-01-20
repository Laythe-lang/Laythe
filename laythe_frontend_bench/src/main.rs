use laythe_core::hooks::{GcHooks, NoContext};
use laythe_core::module::Module;
use laythe_env::memory::{Allocator, NO_GC};
use laythe_vm::compiler;
use laythe_vm::parser::Parser;
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

fn compiler_bench(src: &str) {
  for _ in 0..1000000 {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);
    let gc = Allocator::default();

    let (ast, line_offsets) = Parser::new(&src, 0).parse();
    let ast = ast.unwrap();
    let module = Module::from_path(&hooks, PathBuf::from("/Benchmark.ly")).unwrap();
    let module = hooks.manage(module);
    let compiler = compiler::Compiler::new(module, &ast, &line_offsets, 0, &NO_GC, gc);
    compiler.compile().0.unwrap();
  }
}

fn parser_bench(src: &str) {
  for _ in 0..1000000 {
    let parser = Parser::new(src, 0);
    parser.parse().0.unwrap();
  }
}

fn main() {
  let args: Vec<String> = env::args().collect();
  match args.as_slice() {
    [_, file_path] => {
      let src = load_source(file_path);
      let now = Instant::now();

      parser_bench(&src);

      println!("{}", ((now.elapsed().as_micros() as f64) / 1000000.0));
    }
    [_, bench_type, file_path] => {
      let src = load_source(file_path);
      let now = Instant::now();

      if bench_type == "compile" {
        compiler_bench(&src);
      } else {
        parser_bench(&src);
      }

      println!("{}", ((now.elapsed().as_micros() as f64) / 1000000.0));
    }
    _ => {
      println!("Usage: laythe_compiler_bench [path]");
      process::exit(1);
    }
  }
}
