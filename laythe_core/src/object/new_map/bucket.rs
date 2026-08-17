use crate::{
  managed::{DebugHeap, DebugWrap, Trace},
  object::new_map::header::FnvBuild,
  value::Value,
};
use std::{
  fmt::{self},
  hash::BuildHasher,
};

#[derive(Clone, Copy, Debug)]
pub struct Bucket {
  key: Value,
  value: Value,
  hash: u64,
}

impl Bucket {
  pub fn key(&self) -> Value {
    self.key
  }

  pub fn value(&self) -> Value {
    self.value
  }

  pub fn build(key: Value, value: Value, hasher: FnvBuild) -> Self {
    Self {
      key,
      value,
      hash: hasher.hash_one(key),
    }
  }
}

impl Trace for Bucket {
  fn trace(&self) {
    self.key.trace();
    self.value.trace();
  }

  fn trace_debug(&self, log: &mut dyn std::io::Write) {
    self.key.trace_debug(log);
    self.value.trace_debug(log);
  }
}

impl DebugHeap for Bucket {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    f.debug_struct(&"Bucket")
      .field("key", &DebugWrap(&self.key, depth))
      .field("value", &DebugWrap(&self.value, depth))
      .field("hash", &self.hash)
      .finish()
  }
}
