use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use spacelox_core::hooks::{Hooks, NoContext};
use spacelox_core::io::{Io, NativeIo};
use spacelox_core::{memory::Gc, module::Module};
use spacelox_vm::compiler::{Compiler, Parser};
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
  let gc = Gc::default();
  let mut context = NoContext::new(&gc);
  let hooks = Hooks::new(&mut context);
  let module = hooks.manage(Module::new(hooks.manage_str("Benchmark".to_string())));
  let io = NativeIo::new();
  let mut parser = Parser::new(io.stdio(), source);
  let compiler = Compiler::new(module, io, &mut parser, &hooks);
  compiler.compile();
}

fn criterion_benchmark(c: &mut Criterion) {
  let binary_trees = load_source("binary_trees.lox");
  let equality = load_source("equality.lox");
  let fib = load_source("fib.lox");
  let instantiation = load_source("instantiation.lox");
  let invocation = load_source("invocation.lox");
  let method_call = load_source("method_call.lox");
  let properties = load_source("properties.lox");
  let trees = load_source("trees.lox");
  let zoo = load_source("zoo.lox");

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
