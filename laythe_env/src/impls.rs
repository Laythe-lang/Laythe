use crate::{
  managed::{Manage, Managed, Trace},
  stdio::Stdio,
};
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

impl<T: AsRef<str>> PartialEq<T> for Managed<String> {
  fn eq(&self, other: &T) -> bool {
    &**self == other.as_ref()
  }
}

impl Trace for String {
  fn trace(&self) -> bool {
    true
  }

  fn trace_debug(&self, _: &mut Stdio) -> bool {
    true
  }
}

impl Manage for String {
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
    mem::size_of::<Self>() + self.capacity()
  }
}
