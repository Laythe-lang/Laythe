use criterion::{criterion_group, criterion_main, Criterion};
use spacelox_vm::vm::default_native_vm;
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

fn criterion_benchmark(c: &mut Criterion) {
  let binary_trees = load_source("binary_trees.lox");
  let equality = load_source("equality.lox");
  let fib = load_source("fib.lox");
  let invocation = load_source("invocation.lox");
  let method_call = load_source("method_call.lox");
  let properties = load_source("properties.lox");

  let mut vm = default_native_vm();

  c.bench_function("binary_trees", |b| {
    b.iter(|| vm.run(&binary_trees));
  });
  c.bench_function("equality", |b| {
    b.iter(|| vm.run(&equality));
  });
  c.bench_function("fib", |b| {
    b.iter(|| vm.run(&fib));
  });
  c.bench_function("invocation", |b| {
    b.iter(|| vm.run(&invocation));
  });
  c.bench_function("method_call", |b| {
    b.iter(|| vm.run(&method_call));
  });
  c.bench_function("properties", |b| {
    b.iter(|| vm.run(&properties));
  });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
