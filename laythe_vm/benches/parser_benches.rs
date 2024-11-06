use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use laythe_core::{
  managed::GcStr,
  managed::{Allocator, NO_GC},
};
use laythe_vm::{compiler::Parser, source::{Source, VM_FILE_TEST_ID}};
use std::fs::File;
use std::io::prelude::*;
use std::path::{Path, PathBuf};

const FILE_PATH: &str = file!();

fn fixture_path<P: AsRef<Path>>(bench_path: P) -> Option<PathBuf> {
  let test_path = Path::new(FILE_PATH);

  test_path
    .parent()
    .and_then(|path| path.parent())
    .and_then(|path| path.parent()).map(|path| path.join(bench_path))
}

fn load_source<P: AsRef<Path>>(gc: &mut Allocator, dir: P) -> GcStr {
  let assert = fixture_path(dir).expect("No parent directory");
  let mut file = File::open(assert).unwrap();
  let mut source = String::new();
  file.read_to_string(&mut source).unwrap();
  gc.manage_str(source, &NO_GC)
}

fn parse_source(source: GcStr) {
  let source = Source::new(source);
  let parser = Parser::new(&source, VM_FILE_TEST_ID);
  parser.parse().0.unwrap();
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
  let fluent = load_source(
    &mut gc,
    PathBuf::from("fixture")
      .join("criterion")
      .join("fluent.lay"),
  );
  let lox = load_source(
    &mut gc,
    PathBuf::from("fixture")
      .join("lox_interpreter")
      .join("lox.lay"),
  );

  c.bench_with_input(
    BenchmarkId::new("parse", "binary trees"),
    &binary_trees,
    |b, s| {
      b.iter(|| parse_source(*s));
    },
  );
  c.bench_with_input(BenchmarkId::new("parse", "channel"), &channel, |b, s| {
    b.iter(|| parse_source(*s));
  });
  c.bench_with_input(BenchmarkId::new("parse", "equality"), &equality, |b, s| {
    b.iter(|| parse_source(*s));
  });
  c.bench_with_input(BenchmarkId::new("parse", "fib"), &fib, |b, s| {
    b.iter(|| parse_source(*s));
  });
  c.bench_with_input(BenchmarkId::new("parse", "fibers"), &fibers, |b, s| {
    b.iter(|| parse_source(*s));
  });
  c.bench_with_input(
    BenchmarkId::new("parse", "invocation"),
    &invocation,
    |b, s| {
      b.iter(|| parse_source(*s));
    },
  );
  c.bench_with_input(
    BenchmarkId::new("parse", "instantiation"),
    &instantiation,
    |b, s| {
      b.iter(|| parse_source(*s));
    },
  );
  c.bench_with_input(
    BenchmarkId::new("parse", "method_call"),
    &method_call,
    |b, s| {
      b.iter(|| parse_source(*s));
    },
  );
  c.bench_with_input(
    BenchmarkId::new("parse", "properties"),
    &properties,
    |b, s| {
      b.iter(|| parse_source(*s));
    },
  );
  c.bench_with_input(BenchmarkId::new("parse", "trees"), &trees, |b, s| {
    b.iter(|| parse_source(*s));
  });
  c.bench_with_input(BenchmarkId::new("parse", "zoo"), &zoo, |b, s| {
    b.iter(|| parse_source(*s));
  });
  c.bench_with_input(BenchmarkId::new("parse", "fluent"), &fluent, |b, s| {
    b.iter(|| parse_source(*s));
  });
  c.bench_with_input(BenchmarkId::new("parse", "lox"), &lox, |b, s| {
    b.iter(|| parse_source(*s));
  });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
