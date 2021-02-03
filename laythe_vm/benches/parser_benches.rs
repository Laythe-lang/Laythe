use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use laythe_vm::compiler::Parser;
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

fn parse_source(source: &str) {
  let parser = Parser::new(source, 0);
  parser.parse().0.unwrap();
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
  let fluent = load_source(
    PathBuf::from("fixture")
      .join("criterion")
      .join("fluent.lay"),
  );
  let lox = load_source(
    PathBuf::from("fixture")
      .join("lox_interpreter")
      .join("lox.lay"),
  );

  c.bench_with_input(
    BenchmarkId::new("parse", "binary trees"),
    &binary_trees,
    |b, s| {
      b.iter(|| parse_source(s));
    },
  );
  c.bench_with_input(BenchmarkId::new("parse", "equality"), &equality, |b, s| {
    b.iter(|| parse_source(s));
  });
  c.bench_with_input(BenchmarkId::new("parse", "fib"), &fib, |b, s| {
    b.iter(|| parse_source(s));
  });
  c.bench_with_input(
    BenchmarkId::new("parse", "invocation"),
    &invocation,
    |b, s| {
      b.iter(|| parse_source(s));
    },
  );
  c.bench_with_input(
    BenchmarkId::new("parse", "instantiation"),
    &instantiation,
    |b, s| {
      b.iter(|| parse_source(s));
    },
  );
  c.bench_with_input(
    BenchmarkId::new("parse", "method_call"),
    &method_call,
    |b, s| {
      b.iter(|| parse_source(s));
    },
  );
  c.bench_with_input(
    BenchmarkId::new("parse", "properties"),
    &properties,
    |b, s| {
      b.iter(|| parse_source(s));
    },
  );
  c.bench_with_input(BenchmarkId::new("parse", "trees"), &trees, |b, s| {
    b.iter(|| parse_source(s));
  });
  c.bench_with_input(BenchmarkId::new("parse", "zoo"), &zoo, |b, s| {
    b.iter(|| parse_source(s));
  });
  c.bench_with_input(BenchmarkId::new("parse", "fluent"), &fluent, |b, s| {
    b.iter(|| parse_source(s));
  });
  c.bench_with_input(BenchmarkId::new("parse", "lox"), &lox, |b, s| {
    b.iter(|| parse_source(s));
  });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
