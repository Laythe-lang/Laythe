use std::{fmt, mem};

use super::{
  header::Header,
  manage::{DebugHeap, Manage},
  Mark, Marked, Unmark,
};

#[derive(Debug)]
/// An allocation onto the Laythe Heap. This struct
/// attaches a header to all objects the GC uses to
/// manage the object
pub struct Allocation<T: 'static> {
  // The object header
  header: Header,

  // The underlying date being managed
  pub(crate) data: T,
}

impl<T: 'static> Allocation<T> {
  /// Create a new allocation from a struct that is Manage
  pub fn new(data: T) -> Self {
    Self {
      data,
      header: Header::new(false),
    }
  }
}

impl<T> Mark for Allocation<T> {
  /// Mark this allocation as visited, returning
  /// the existing marked status
  #[inline]
  fn mark(&self) -> bool {
    self.header.mark()
  }
}

impl<T> Marked for Allocation<T> {
  #[inline]
  fn marked(&self) -> bool {
    self.header.marked()
  }
}

impl<T> Unmark for Allocation<T> {
  #[inline]
  fn unmark(&self) -> bool {
    self.header.unmark()
  }
}

impl<T: 'static + DebugHeap> Manage for Allocation<T> {
  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }

  fn loc(&self) -> *const u8 {
    (self as *const Allocation<T>) as *const u8
  }
}

impl<T: 'static + DebugHeap> DebugHeap for Allocation<T> {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    self.data.fmt_heap(f, depth.saturating_sub(1))
  }
}
