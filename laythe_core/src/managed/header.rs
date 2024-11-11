use super::{Mark, Marked, Trace, Unmark};
use std::sync::atomic::{AtomicBool, Ordering};

/// The header of an allocation indicate meta data about the object
#[derive(Debug, Default)]
#[repr(C)]
pub struct Header {
  // has this allocation been marked by the garbage collector
  marked: AtomicBool,
}

impl Header {
  pub fn new() -> Self {
    Self {
      marked: AtomicBool::new(false),
    }
  }
}

impl Mark for Header {
  /// Mark this allocation as visited, returning
  /// the existing marked status
  #[inline]
  fn mark(&self) -> bool {
    self.marked.swap(true, Ordering::Release)
  }
}

impl Marked for Header {
  #[inline]
  fn marked(&self) -> bool {
    self.marked.load(Ordering::Acquire)
  }
}

impl Unmark for Header {
  #[inline]
  fn unmark(&self) -> bool {
    self.marked.swap(false, Ordering::Release)
  }
}

impl Trace for Header {}

#[cfg(test)]
mod test {
  use crate::managed::header::Header;
  use std::mem;

  #[test]
  fn size() {
    assert_eq!(mem::size_of::<Header>(), 1);
  }

  #[test]
  fn alignment() {
    assert_eq!(mem::align_of::<Header>(), 1);
  }
}
