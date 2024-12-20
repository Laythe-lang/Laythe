use bumpalo::Bump;
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use laythe_core::module::Module;
use laythe_core::object::LyStr;
use laythe_core::{Allocator, NO_GC};
use laythe_core::{
  hooks::{GcHooks, NoContext},
  utils::IdEmitter,
};
use laythe_lib::create_std_lib;
use laythe_vm::compiler::{Parser, Resolver};
use laythe_vm::source::VM_FILE_TEST_ID;
use laythe_vm::{compiler::Compiler, source::Source};
use std::fs::File;
use std::io::prelude::*;
use std::path::{Path, PathBuf};

const FILE_PATH: &str = file!();

fn fixture_path<P: AsRef<Path>>(bench_path: P) -> Option<PathBuf> {
  let test_path = Path::new(FILE_PATH);

  test_path
    .parent()
    .and_then(|path| path.parent())
    .and_then(|path| path.parent())
    .map(|path| path.join(bench_path))
}

fn load_source<P: AsRef<Path>>(gc: &mut Allocator, dir: P) -> LyStr {
  let assert = fixture_path(dir).expect("No parent directory");
  let mut file = File::open(assert).unwrap();
  let mut source = String::new();
  file.read_to_string(&mut source).unwrap();
  gc.manage_str(source, &NO_GC)
}

fn compile_source(source: LyStr) {
  let context = NoContext::default();
  let hooks = GcHooks::new(&context);

  let mut emitter = IdEmitter::default();
  let std_lib = create_std_lib(&hooks, &mut emitter).expect("Standard library creation failed");
  let global_module = std_lib.root_module();

  let module_name = hooks.manage_str("Module");
  let module_class = global_module
    .get_symbol_by_name(module_name)
    .unwrap()
    .to_obj()
    .to_class();
  let module = hooks.manage(Module::new(&hooks, module_class, "module/path", 0));
  let source = Source::new(source);
  let (ast, line_offsets) = Parser::new(&source, VM_FILE_TEST_ID).parse();
  let mut ast = ast.unwrap();

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

fn criterion_benchmark(c: &mut Criterion) {
  let mut gc = Allocator::default();

  let binary_trees = load_source(
    &mut gc,
    PathBuf::from("fixture")
      .join("criterion")
      .join("binary_trees.lay"),
  );
  let channel = load_source(
    &mut gc,
    PathBuf::from("fixture")
      .join("criterion")
      .join("channel.lay"),
  );
  let equality = load_source(
    &mut gc,
    PathBuf::from("fixture")
      .join("criterion")
      .join("equality.lay"),
  );
  let fib = load_source(
    &mut gc,
    PathBuf::from("fixture").join("criterion").join("fib.lay"),
  );
  let fibers = load_source(
    &mut gc,
    PathBuf::from("fixture")
      .join("criterion")
      .join("fibers.lay"),
  );
  let instantiation = load_source(
    &mut gc,
    PathBuf::from("fixture")
      .join("criterion")
      .join("instantiation.lay"),
  );
  let invocation = load_source(
    &mut gc,
    PathBuf::from("fixture")
      .join("criterion")
      .join("invocation.lay"),
  );
  let method_call = load_source(
    &mut gc,
    PathBuf::from("fixture")
      .join("criterion")
      .join("method_call.lay"),
  );
  let properties = load_source(
    &mut gc,
    PathBuf::from("fixture")
      .join("criterion")
      .join("properties.lay"),
  );
  let trees = load_source(
    &mut gc,
    PathBuf::from("fixture").join("criterion").join("trees.lay"),
  );
  let zoo = load_source(
    &mut gc,
    PathBuf::from("fixture").join("criterion").join("zoo.lay"),
  );
  let lox = load_source(
    &mut gc,
    PathBuf::from("fixture")
      .join("lox_interpreter")
      .join("lox.lay"),
  );

  c.bench_with_input(
    BenchmarkId::new("compile", "binary_trees"),
    &binary_trees,
    |b, s| {
      b.iter(|| compile_source(*s));
    },
  );
  c.bench_with_input(BenchmarkId::new("compile", "channel"), &channel, |b, s| {
    b.iter(|| compile_source(*s));
  });
  c.bench_with_input(
    BenchmarkId::new("compile", "equality"),
    &equality,
    |b, s| {
      b.iter(|| compile_source(*s));
    },
  );
  c.bench_with_input(BenchmarkId::new("compile", "fib"), &fib, |b, s| {
    b.iter(|| compile_source(*s));
  });
  c.bench_with_input(BenchmarkId::new("compile", "fibers"), &fibers, |b, s| {
    b.iter(|| compile_source(*s));
  });
  c.bench_with_input(
    BenchmarkId::new("compile", "invocation"),
    &invocation,
    |b, s| {
      b.iter(|| compile_source(*s));
    },
  );
  c.bench_with_input(
    BenchmarkId::new("compile", "instantiation"),
    &instantiation,
    |b, s| {
      b.iter(|| compile_source(*s));
    },
  );
  c.bench_with_input(
    BenchmarkId::new("compile", "method_call"),
    &method_call,
    |b, s| {
      b.iter(|| compile_source(*s));
    },
  );
  c.bench_with_input(
    BenchmarkId::new("compile", "properties"),
    &properties,
    |b, s| {
      b.iter(|| compile_source(*s));
    },
  );
  c.bench_with_input(BenchmarkId::new("compile", "trees"), &trees, |b, s| {
    b.iter(|| compile_source(*s));
  });
  c.bench_with_input(BenchmarkId::new("compile", "zoo"), &zoo, |b, s| {
    b.iter(|| compile_source(*s));
  });
  c.bench_with_input(BenchmarkId::new("compile", "lox"), &lox, |b, s| {
    b.iter(|| compile_source(*s));
  });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
