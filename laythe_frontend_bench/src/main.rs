use bumpalo::Bump;
use laythe_core::{
  hooks::{GcHooks, NoContext}, module::Module, utils::IdEmitter, Allocator, NO_GC
};
use laythe_lib::create_std_lib;
use laythe_vm::{
  compiler::{Compiler, Parser, Resolver},
  source::{Source, VM_FILE_TEST_ID},
};
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

fn compiler_bench(src: &str) {
  for _ in 0..1000000 {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);
    let source = Source::new(hooks.manage_str(src));

    let mut emitter = IdEmitter::default();
    let std_lib = create_std_lib(&hooks, &mut emitter).expect("Standard library creation failed");
    let global_module = std_lib.root_module();

    let module_name = hooks.manage_str("Module");
    let module_class = global_module
      .get_symbol_by_name(module_name)
      .unwrap()
      .to_obj()
      .to_class();

    let (ast, line_offsets) = Parser::new(&source, VM_FILE_TEST_ID).parse();
    let mut ast = ast.unwrap();
    let module = hooks.manage(Module::new(&hooks, module_class, "module/path", 0));

    let gc = context.done();
    assert!(
      Resolver::new(global_module, module, &gc, &source, VM_FILE_TEST_ID, false)
        .resolve(&mut ast)
        .is_ok()
    );

    let alloc = Bump::new();
    let compiler = Compiler::new(
      module,
      &alloc,
      &line_offsets,
      VM_FILE_TEST_ID,
      false,
      &NO_GC,
      gc,
    );
    compiler.compile(&ast).0.unwrap();
  }
}

fn parser_bench(src: &str) {
  let mut gc = Allocator::default();
  let src = gc.manage_str(src, &NO_GC);

  for _ in 0..1000000 {
    let source = Source::new(src);
    let parser = Parser::new(&source, VM_FILE_TEST_ID);
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
