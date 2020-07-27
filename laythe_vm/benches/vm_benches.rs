use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use laythe_vm::vm::default_native_vm;
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

fn load_source(path: &PathBuf) -> String {
  let mut file = File::open(path).unwrap();
  let mut source = String::new();
  file.read_to_string(&mut source).unwrap();
  source
}

fn criterion_benchmark(c: &mut Criterion) {
  let binary_trees_path = fixture_path("binary_trees.lay").expect("Unable to load benchmark file.");
  let equality_path = fixture_path("equality.lay").expect("Unable to load benchmark file.");
  let fib_path = fixture_path("fib.lay").expect("Unable to load benchmark file.");
  let instantiation_path =
    fixture_path("instantiation.lay").expect("Unable to load benchmark file.");
  let invocation_path = fixture_path("invocation.lay").expect("Unable to load benchmark file.");
  let method_call_path = fixture_path("method_call.lay").expect("Unable to load benchmark file.");
  let properties_path = fixture_path("properties.lay").expect("Unable to load benchmark file.");
  let trees_path = fixture_path("trees.lay").expect("Unable to load benchmark file.");
  let zoo_path = fixture_path("zoo.lay").expect("Unable to load benchmark file.");
  let fluent_path = fixture_path("fluent.lay").expect("Unable to load benchmark file.");

  let binary_trees = load_source(&binary_trees_path);
  let equality = load_source(&equality_path);
  let fib = load_source(&fib_path);
  let instantiation = load_source(&instantiation_path);
  let invocation = load_source(&invocation_path);
  let method_call = load_source(&method_call_path);
  let properties = load_source(&properties_path);
  let trees = load_source(&trees_path);
  let zoo = load_source(&zoo_path);
  let fluent = load_source(&fluent_path);

  c.bench_with_input(
    BenchmarkId::new("run binary_trees", 1),
    &binary_trees,
    |b, s| {
      let mut vm = default_native_vm();
      b.iter(|| vm.run(binary_trees_path.clone(), &s));
    },
  );
  c.bench_with_input(BenchmarkId::new("run equality", 2), &equality, |b, s| {
    let mut vm = default_native_vm();
    b.iter(|| vm.run(equality_path.clone(), &s));
  });
  c.bench_with_input(BenchmarkId::new("run fib", 3), &fib, |b, s| {
    let mut vm = default_native_vm();
    b.iter(|| vm.run(fib_path.clone(), &s));
  });
  c.bench_with_input(
    BenchmarkId::new("run invocation", 4),
    &invocation,
    |b, s| {
      let mut vm = default_native_vm();
      b.iter(|| vm.run(invocation_path.clone(), &s));
    },
  );
  c.bench_with_input(
    BenchmarkId::new("run instantiation", 5),
    &instantiation,
    |b, s| {
      let mut vm = default_native_vm();
      b.iter(|| vm.run(instantiation_path.clone(), &s));
    },
  );
  c.bench_with_input(
    BenchmarkId::new("run method_call", 6),
    &method_call,
    |b, s| {
      let mut vm = default_native_vm();
      b.iter(|| vm.run(method_call_path.clone(), &s));
    },
  );
  c.bench_with_input(
    BenchmarkId::new("run properties", 7),
    &properties,
    |b, s| {
      let mut vm = default_native_vm();
      b.iter(|| vm.run(properties_path.clone(), &s));
    },
  );
  c.bench_with_input(BenchmarkId::new("run trees", 8), &trees, |b, s| {
    let mut vm = default_native_vm();
    b.iter(|| vm.run(trees_path.clone(), &s));
  });
  c.bench_with_input(BenchmarkId::new("run zoo", 9), &zoo, |b, s| {
    let mut vm = default_native_vm();
    b.iter(|| vm.run(zoo_path.clone(), &s));
  });
  c.bench_with_input(BenchmarkId::new("run fluent", 10), &fluent, |b, s| {
    let mut vm = default_native_vm();
    b.iter(|| vm.run(fluent_path.clone(), &s));
  });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
