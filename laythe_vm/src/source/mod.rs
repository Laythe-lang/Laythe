mod files;
pub use files::{LineError, LineOffsets, VmFileId, VmFiles, VM_FILE_TEST_ID};

use bumpalo::{boxed::Box, collections::Vec, Bump};
use laythe_core::{managed::Trace, object::LyStr};
use std::ops::Deref;

pub struct Source {
  content: LyStr,
  alloc: Bump,
}

impl Source {
  pub fn new(content: LyStr) -> Self {
    Self {
      content,
      alloc: Bump::with_capacity(content.len() * 2),
    }
  }

  pub fn node<T>(&self, node: T) -> Box<T> {
    Box::new_in(node, &self.alloc)
  }

  pub fn vec<T>(&self) -> Vec<T> {
    Vec::new_in(&self.alloc)
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
    self
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
  use laythe_core::{Allocator, NO_GC};

use super::*;

  #[test]
  fn new() {
    let mut gc = Allocator::default();
    let content = "print('example');";

    let content = gc.manage_str(content, &NO_GC);
    let src = Source::new(content);

    assert_eq!(&*src, &*content)
  }
}
