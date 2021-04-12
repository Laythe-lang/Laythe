use laythe_core::{
  hooks::{GcHooks, NoContext},
  managed::GcObj,
  memory::{Allocator, NO_GC},
  module::Module,
  object::Class,
};
use laythe_vm::{
  compiler::{Compiler, Parser},
  source::Source,
};
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

pub fn test_class(hooks: &GcHooks, name: &str) -> GcObj<Class> {
  let mut object_class = hooks.manage_obj(Class::bare(hooks.manage_str("Object")));
  let mut class_class = hooks.manage_obj(Class::bare(hooks.manage_str("Object")));
  class_class.inherit(hooks, object_class);

  let class_copy = class_class;
  class_class.set_meta(class_copy);

  let object_meta_class = Class::with_inheritance(
    hooks,
    hooks.manage_str(format!("{} metaClass", object_class.name())),
    class_class,
  );

  object_class.set_meta(object_meta_class);
  Class::with_inheritance(hooks, hooks.manage_str(name), object_class)
}

fn compiler_bench(src: &str) {
  for _ in 0..1000000 {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);
    let mut gc = Allocator::default();
    let source = Source::new(gc.manage_str(src, &NO_GC));
    let class = test_class(&hooks, "class");

    let (ast, line_offsets) = Parser::new(&source, 0).parse();
    let ast = ast.unwrap();
    let module = Module::from_path(&hooks, PathBuf::from("/Benchmark.ly"), class, 0).unwrap();
    let module = hooks.manage(module);

    let compiler = Compiler::new(module, &ast, &line_offsets, 0, &NO_GC, gc);
    compiler.compile().0.unwrap();
  }
}

fn parser_bench(src: &str) {
  let mut gc = Allocator::default();
  let src = gc.manage_str(src, &NO_GC);

  for _ in 0..1000000 {
    let source = Source::new(src);
    let parser = Parser::new(&source, 0);
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
    },
    [_, bench_type, file_path] => {
      let src = load_source(file_path);
      let now = Instant::now();

      if bench_type == "compile" {
        compiler_bench(&src);
      } else {
        parser_bench(&src);
      }

      println!("{}", ((now.elapsed().as_micros() as f64) / 1000000.0));
    },
    _ => {
      println!("Usage: laythe_compiler_bench [path]");
      process::exit(1);
    },
  }
}
