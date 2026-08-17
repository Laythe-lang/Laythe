use crate::{
  managed::{Mark, Marked, Trace, Unmark},
  object::ObjectKind,
};
use fnv::FnvHasher;
use std::{
  hash::BuildHasherDefault,
  sync::atomic::{AtomicBool, Ordering},
};

pub type FnvBuild = BuildHasherDefault<FnvHasher>;

/// The `Header` meta data for `Map` and it's degraded `ObjRefect`. This struct
/// is positioned at the front of the array such that the layout looks like
/// this
/// ```markdown
/// [Header (potential padding)| T ]
/// ```
#[repr(C)]
pub struct MapHeader {
  /// Has this allocation been marked by the garbage collector
  marked: AtomicBool,

  /// The underlying value kind of this object
  kind: ObjectKind,

  /// The hasher for this map
  hasher: FnvBuild,
}

impl MapHeader {
  /// Create a new object header
  #[inline]
  pub fn new(hasher: FnvBuild) -> Self {
    Self {
      marked: AtomicBool::new(false),
      kind: ObjectKind::Map,
      hasher,
    }
  }

  #[inline]
  pub fn hasher(&self) -> &FnvBuild {
    &self.hasher
  }
}

impl Mark for MapHeader {
  #[inline]
  fn mark(&self) -> bool {
    self.marked.swap(true, Ordering::Release)
  }
}

impl Unmark for MapHeader {
  #[inline]
  fn unmark(&self) -> bool {
    self.marked.swap(false, Ordering::Release)
  }
}

impl Marked for MapHeader {
  #[inline]
  fn marked(&self) -> bool {
    self.marked.load(Ordering::Acquire)
  }
}

impl Trace for MapHeader {}
