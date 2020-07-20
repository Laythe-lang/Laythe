use crate::{
  managed::{Manage, Managed, Trace},
  stdio::Stdio,
};
use smol_str::SmolStr;
use std::{mem, path::PathBuf};

impl Trace for PathBuf {
  fn trace(&self) -> bool {
    true
  }
  fn trace_debug(&self, _: &mut Stdio) -> bool {
    true
  }
}

impl Manage for PathBuf {
  fn alloc_type(&self) -> &str {
    "path"
  }
  fn debug(&self) -> String {
    format!("{:?}", self)
  }
  fn debug_free(&self) -> String {
    format!("{:?}", self)
  }
  fn size(&self) -> usize {
    mem::size_of::<Self>() // TODO add capacity once stabilized? + self.capacity()
  }
}

impl<T: AsRef<str>> PartialEq<T> for Managed<SmolStr> {
  fn eq(&self, other: &T) -> bool {
    &**self == other.as_ref()
  }
}

impl Trace for SmolStr {
  fn trace(&self) -> bool {
    true
  }

  fn trace_debug(&self, _: &mut Stdio) -> bool {
    true
  }
}

impl Manage for SmolStr {
  fn alloc_type(&self) -> &str {
    "string"
  }

  fn debug(&self) -> String {
    format!("{:?}", self)
  }

  fn debug_free(&self) -> String {
    format!("{:?}", self)
  }

  fn size(&self) -> usize {
    if self.is_heap_allocated() {
      mem::size_of::<Self>() + self.len()
    } else {
      mem::size_of::<Self>()
    }
  }
}
