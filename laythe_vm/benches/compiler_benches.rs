use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use laythe_core::hooks::{support::TestContext, GcHooks};
use laythe_core::module::Module;
use laythe_env::stdio::Stdio;
use laythe_vm::compiler::{Compiler, Parser};
use std::fs::File;
use std::io::prelude::*;
use std::path::{Path, PathBuf};

const FILE_PATH: &str = file!();

fn fixture_path(bench_path: &str) -> Option<PathBuf> {
  let test_path = Path::new(FILE_PATH);

  test_path
    .parent()
    .and_then(|path| path.parent())
    .and_then(|path| path.parent())
    .and_then(|path| Some(path.join("fixture").join("criterion").join(bench_path)))
}

fn load_source(dir: &str) -> String {
  let assert = fixture_path(dir).expect("No parent directory");
  let mut file = File::open(assert).unwrap();
  let mut source = String::new();
  file.read_to_string(&mut source).unwrap();
  source
}

fn compile_source(source: &str) {
  let mut context = TestContext::default();
  let hooks = GcHooks::new(&mut context);
  let module =
    hooks.manage(Module::from_path(&hooks, hooks.manage(PathBuf::from("./Benchmark.lay"))).unwrap());
  let mut parser = Parser::new(Stdio::default(), source);
  let compiler = Compiler::new(module, &mut parser, &hooks);
  compiler.compile().unwrap();
}

fn criterion_benchmark(c: &mut Criterion) {
  let binary_trees = load_source("binary_trees.lay");
  let equality = load_source("equality.lay");
  let fib = load_source("fib.lay");
  let instantiation = load_source("instantiation.lay");
  let invocation = load_source("invocation.lay");
  let method_call = load_source("method_call.lay");
  let properties = load_source("properties.lay");
  let trees = load_source("trees.lay");
  let zoo = load_source("zoo.lay");

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
