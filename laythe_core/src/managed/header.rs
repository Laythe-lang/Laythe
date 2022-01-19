use std::sync::atomic::{AtomicBool, Ordering};

use crate::object::ObjectKind;

use super::{Mark, Marked, Unmark};

/// The header of an allocation indicate meta data about the object
#[derive(Debug, Default)]
pub struct Header {
  // has this allocation been marked by the garbage collector
  marked: AtomicBool,
}

impl Header {
  pub fn new(marked: bool) -> Self {
    Self {
      marked: AtomicBool::new(marked),
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

/// The `Header` meta data for `GcObj<T>` and `GcObject`. This struct
/// is positioned at the front of the array such that the layout looks like
/// this
/// ```markdown
/// [Header (potential padding)| T ]
/// ```
pub struct ObjHeader {
  /// Has this allocation been marked by the garbage collector
  marked: AtomicBool,

  /// The underlying value kind of this object
  kind: ObjectKind,
}

impl ObjHeader {
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

impl Mark for ObjHeader {
  #[inline]
  fn mark(&self) -> bool {
    self.marked.swap(true, Ordering::Release)
  }
}

impl Unmark for ObjHeader {
  #[inline]
  fn unmark(&self) -> bool {
    self.marked.swap(false, Ordering::Release)
  }
}

impl Marked for ObjHeader {
  #[inline]
  fn marked(&self) -> bool {
    self.marked.load(Ordering::Acquire)
  }
}
