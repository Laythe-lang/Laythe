use crate::{
  managed::{Manage, Trace},
  stdio::StdIo,
};
use std::{mem, path::PathBuf};

impl Trace for PathBuf {
  fn trace(&self) -> bool {
    true
  }
  fn trace_debug(&self, _: &dyn StdIo) -> bool {
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

impl Trace for String {
  fn trace(&self) -> bool {
    true
  }

  fn trace_debug(&self, _: &dyn StdIo) -> bool {
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
