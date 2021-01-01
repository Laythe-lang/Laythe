use std::{
  fmt, mem,
  sync::atomic::{AtomicBool, Ordering},
};

use super::manage::{DebugHeap, Manage, Trace};

/// The header of an allocation indicate meta data about the object
#[derive(Debug, Default)]
pub struct Header {
  // has this allocation been marked by the garbage collector
  marked: AtomicBool,
}

#[derive(Debug)]
/// An allocation onto the Laythe Heap. This struct
/// attaches a header to all objects the GC uses to
/// manage the object
pub struct Allocation<T: 'static + Trace + ?Sized> {
  // The object header
  header: Header,

  // The underlying date being managed
  pub data: T,
}

impl<T: 'static + Manage> Allocation<T> {
  /// Create a new allocation from a struct that is Manage
  ///
  /// # Examples
  /// ```
  /// use laythe_env::Managed::Allocation;
  /// use smol_str::SmolStr;
  ///
  /// let s = SmolStr::from("example");
  /// let alloc = Allocation::new(s);
  /// ```
  pub fn new(data: T) -> Self {
    Self {
      data,
      header: Header {
        marked: AtomicBool::new(false),
      },
    }
  }

  /// What is the size of this allocation in bytes
  pub fn size(&self) -> usize {
    self.data.size() + mem::size_of::<Header>()
  }

  /// Get a trait object is can be logged for the heap
  pub fn as_debug(&self) -> &dyn DebugHeap {
    self.data.as_debug()
  }
}

impl Allocation<dyn Manage> {
  /// What is the size of this allocation in bytes
  pub fn size(&self) -> usize {
    self.data.size() + mem::size_of::<Header>()
  }

  /// Get a trait object is can be logged for the heap
  pub fn as_debug(&self) -> &dyn DebugHeap {
    self.data.as_debug()
  }
}

impl<T: 'static + Manage + ?Sized> Allocation<T> {
  /// Mark this allocation as visited, returning
  /// the existing marked status
  pub fn mark(&self) -> bool {
    self
      .header
      .marked
      .compare_and_swap(false, true, Ordering::Release)
  }

  /// Unmark this allocation as visited, returning
  /// the existing marked status
  pub fn unmark(&self) -> bool {
    self
      .header
      .marked
      .compare_and_swap(true, false, Ordering::Release)
  }

  /// Is this allocation marked
  pub fn marked(&self) -> bool {
    self.header.marked.load(Ordering::Acquire)
  }
}

impl<T: 'static + Manage + ?Sized> DebugHeap for Allocation<T> {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    self.data.fmt_heap(f, depth)
  }
}
