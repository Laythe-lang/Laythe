mod files;
pub use files::{LineError, LineOffsets, VmFileId, VmFiles};

use bumpalo::{boxed::Box, Bump};
use laythe_core::managed::{GcStr, Trace};
use std::ops::Deref;

pub struct Source {
  content: GcStr,
  alloc: Bump,
}

impl Source {
  pub fn new(content: GcStr) -> Self {
    Self {
      content,
      alloc: Bump::with_capacity(content.len()),
    }
  }

  pub fn node<T>(&self, node: T) -> Box<T> {
    Box::new_in(node, &self.alloc)
  }
}

impl Deref for Source {
  type Target = str;

  fn deref(&self) -> &Self::Target {
    &self.content
  }
}

impl AsRef<str> for Source {
  fn as_ref(&self) -> &str {
    &*self
  }
}

impl Trace for Source {
  fn trace(&self) {
    self.content.trace()
  }

  fn trace_debug(&self, log: &mut dyn std::io::Write) {
    self.content.trace_debug(log)
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use laythe_core::memory::{Allocator, NO_GC};

  #[test]
  fn new() {
    let mut gc = Allocator::default();
    let content = "print('example');";

    let content = gc.manage_str(content, &NO_GC);
    let src = Source::new(content);

    assert_eq!(&*src, &*content)
  }
}
