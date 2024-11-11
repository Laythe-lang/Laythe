use std::sync::atomic::{AtomicBool, Ordering};

use crate::managed::{Mark, Marked, Trace, Unmark};

use super::ObjectKind;

/// The `Header` meta data for `ObjRef<T>` and `ObjRefect`. This struct
/// is positioned at the front of the array such that the layout looks like
/// this
/// ```markdown
/// [Header (potential padding)| T ]
/// ```
#[repr(C)]
pub struct Header {
  /// Has this allocation been marked by the garbage collector
  marked: AtomicBool,

  /// The underlying value kind of this object
  kind: ObjectKind,
}

impl Header {
  /// Create a new object header
  #[inline]
  pub fn new(kind: ObjectKind) -> Self {
    Self {
      marked: AtomicBool::new(false),
      kind,
    }
  }

  /// What is the value kind of this object
  #[inline]
  pub fn kind(&self) -> ObjectKind {
    self.kind
  }
}

impl Mark for Header {
  #[inline]
  fn mark(&self) -> bool {
    self.marked.swap(true, Ordering::Release)
  }
}

impl Unmark for Header {
  #[inline]
  fn unmark(&self) -> bool {
    self.marked.swap(false, Ordering::Release)
  }
}

impl Marked for Header {
  #[inline]
  fn marked(&self) -> bool {
    self.marked.load(Ordering::Acquire)
  }
}

impl Trace for Header {}

#[cfg(test)]
mod test {
  use std::mem;

use crate::object::header::Header;

  #[test]
  fn size() {
    assert_eq!(mem::size_of::<Header>(), 2);
  }

  #[test]
  fn alignment() {
    assert_eq!(mem::align_of::<Header>(), 1);
  }
}
