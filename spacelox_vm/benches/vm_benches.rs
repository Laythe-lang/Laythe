use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
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
  let instantiation = load_source("instantiation.lox");
  let invocation = load_source("invocation.lox");
  let method_call = load_source("method_call.lox");
  let properties = load_source("properties.lox");
  let trees = load_source("trees.lox");
  let zoo = load_source("zoo.lox");
  let bench_module_name = PathBuf::from("./Benchmark.lox");

  c.bench_with_input(
    BenchmarkId::new("run binary_trees", 1),
    &binary_trees,
    |b, s| {
      let mut vm = default_native_vm();
      b.iter(|| vm.run(bench_module_name.clone(), &s));
    },
  );
  c.bench_with_input(BenchmarkId::new("run equality", 2), &equality, |b, s| {
    let mut vm = default_native_vm();
    b.iter(|| vm.run(bench_module_name.clone(), &s));
  });
  c.bench_with_input(BenchmarkId::new("run fib", 3), &fib, |b, s| {
    let mut vm = default_native_vm();
    b.iter(|| vm.run(bench_module_name.clone(), &s));
  });
  c.bench_with_input(
    BenchmarkId::new("run invocation", 4),
    &invocation,
    |b, s| {
      let mut vm = default_native_vm();
      b.iter(|| vm.run(bench_module_name.clone(), &s));
    },
  );
  c.bench_with_input(
    BenchmarkId::new("run instantiation", 5),
    &instantiation,
    |b, s| {
      let mut vm = default_native_vm();
      b.iter(|| vm.run(bench_module_name.clone(), &s));
    },
  );
  c.bench_with_input(
    BenchmarkId::new("run method_call", 6),
    &method_call,
    |b, s| {
      let mut vm = default_native_vm();
      b.iter(|| vm.run(bench_module_name.clone(), &s));
    },
  );
  c.bench_with_input(
    BenchmarkId::new("run properties", 7),
    &properties,
    |b, s| {
      let mut vm = default_native_vm();
      b.iter(|| vm.run(bench_module_name.clone(), &s));
    },
  );
  c.bench_with_input(BenchmarkId::new("run trees", 8), &trees, |b, s| {
    let mut vm = default_native_vm();
    b.iter(|| vm.run(bench_module_name.clone(), &s));
  });
  c.bench_with_input(BenchmarkId::new("run zoo", 9), &zoo, |b, s| {
    let mut vm = default_native_vm();
    b.iter(|| vm.run(bench_module_name.clone(), &s));
  });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
