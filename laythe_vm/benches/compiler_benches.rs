use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use laythe_core::hooks::{GcHooks, NoContext};
use laythe_core::module::Module;
use laythe_env::memory::NO_GC;
use laythe_vm::compiler::Compiler;
use laythe_vm::parser::Parser;
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
    .and_then(|path| Some(path.join(bench_path)))
}

fn load_source<P: AsRef<Path>>(dir: P) -> String {
  let assert = fixture_path(dir).expect("No parent directory");
  let mut file = File::open(assert).unwrap();
  let mut source = String::new();
  file.read_to_string(&mut source).unwrap();
  source
}

fn compile_source(source: &str) {
  let mut context = NoContext::default();
  let hooks = GcHooks::new(&mut context);

  let path = hooks.manage(PathBuf::from("./Benchmark.lay"));
  hooks.push_root(path);

  let module = hooks.manage(Module::from_path(&hooks, path).unwrap());
  hooks.pop_roots(1);
  let (ast, line_offsets) = Parser::new(source, 0).parse();
  let ast = ast.unwrap();

  let gc = context.done();
  let compiler = Compiler::new(module, &ast, &line_offsets, 0, &NO_GC, gc);
  compiler.compile().0.unwrap();
}

fn criterion_benchmark(c: &mut Criterion) {
  let binary_trees = load_source(
    PathBuf::from("fixture")
      .join("criterion")
      .join("binary_trees.lay"),
  );
  let equality = load_source(
    PathBuf::from("fixture")
      .join("criterion")
      .join("equality.lay"),
  );
  let fib = load_source(PathBuf::from("fixture").join("criterion").join("fib.lay"));
  let instantiation = load_source(
    PathBuf::from("fixture")
      .join("criterion")
      .join("instantiation.lay"),
  );
  let invocation = load_source(
    PathBuf::from("fixture")
      .join("criterion")
      .join("invocation.lay"),
  );
  let method_call = load_source(
    PathBuf::from("fixture")
      .join("criterion")
      .join("method_call.lay"),
  );
  let properties = load_source(
    PathBuf::from("fixture")
      .join("criterion")
      .join("properties.lay"),
  );
  let trees = load_source(PathBuf::from("fixture").join("criterion").join("trees.lay"));
  let zoo = load_source(PathBuf::from("fixture").join("criterion").join("zoo.lay"));

  c.bench_with_input(
    BenchmarkId::new("compile binary_trees", 201),
    &binary_trees,
    |b, s| {
      b.iter(|| compile_source(s));
    },
  );
  c.bench_with_input(
    BenchmarkId::new("compile equality", 202),
    &equality,
    |b, s| {
      b.iter(|| compile_source(s));
    },
  );
  c.bench_with_input(BenchmarkId::new("compile fib", 203), &fib, |b, s| {
    b.iter(|| compile_source(s));
  });
  c.bench_with_input(
    BenchmarkId::new("compile invocation", 204),
    &invocation,
    |b, s| {
      b.iter(|| compile_source(s));
    },
  );
  c.bench_with_input(
    BenchmarkId::new("compile instantiation", 205),
    &instantiation,
    |b, s| {
      b.iter(|| compile_source(s));
    },
  );
  c.bench_with_input(
    BenchmarkId::new("compile method_call", 206),
    &method_call,
    |b, s| {
      b.iter(|| compile_source(s));
    },
  );
  c.bench_with_input(
    BenchmarkId::new("compile properties", 207),
    &properties,
    |b, s| {
      b.iter(|| compile_source(s));
    },
  );
  c.bench_with_input(BenchmarkId::new("compile trees", 208), &trees, |b, s| {
    b.iter(|| compile_source(s));
  });
  c.bench_with_input(BenchmarkId::new("compile zoo", 209), &zoo, |b, s| {
    b.iter(|| compile_source(s));
  });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
