use std::{fmt, io::Write};

/// A wrapper struct to print debug information to
/// a provided depth. This is to handled cycles in
/// managed Gc pointers
pub struct DebugWrap<'a, T>(pub &'a T, pub usize);

impl<'a, T: DebugHeap> fmt::Debug for DebugWrap<'a, T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.0.fmt_heap(f, self.1)
  }
}

/// A wrapper struct to print debug information to
/// a provided depth. This is to handled cycles in
/// managed Gc pointers
pub struct DebugWrapDyn<'a>(pub &'a dyn DebugHeap, pub usize);

impl<'a> fmt::Debug for DebugWrapDyn<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.0.fmt_heap(f, self.1)
  }
}

/// A utility to print debug information to a fixed depth in the Laythe heap
pub trait DebugHeap {
  /// A debugging string for this managed object. Typically just wrapping
  /// wrapping fmt::Debug so we can have dyn Manage
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result;
}

/// A utility to print debug information to a fixed depth in the Laythe heap
pub trait DebugHeapRef: DebugHeap + fmt::Pointer {}

/// A struct that can indicate it's status are marked or unmarked
pub trait Marked {
  /// Is this structure marked
  fn marked(&self) -> bool;
}

/// A struct that can be marked as having being visited by
/// a garbage collector
pub trait Mark: Marked {
  /// Mark this struct returning if it was previously marked
  fn mark(&self) -> bool;
}

pub trait Unmark: Marked {
  /// Unmark this struct returning if it was previously marked
  fn unmark(&self) -> bool;
}

/// An entity that is traceable by the garbage collector
pub trait Trace {
  /// Mark all objects that are reachable from this object
  fn trace(&self) {}

  /// Mark all objects that are reachable printing debugging information
  /// for each object
  fn trace_debug(&self, _log: &mut dyn Write) {}
}

/// An entity that can provide tracing roots to the garbage collector
pub trait TraceRoot {
  /// Mark all objects that are reachable from this object
  fn trace(&self);

  /// Mark all objects that are reachable printing debugging information
  /// for each object
  fn trace_debug(&self, log: &mut dyn Write);

  /// Are we in a context were we can collect garbage.
  fn can_collect(&self) -> bool;
}

/// An entity that can be managed and collected by the garbage collector.
/// This trait provided debugging capabilities and statistics for the gc.
pub trait Manage: DebugHeap + Unmark {
  /// What is the size of this allocation
  fn size(&self) -> usize;

  /// Helper function to get a trait object for Debug Heap
  fn as_debug(&self) -> &dyn DebugHeap;

  /// Get the actual allocation location where
  /// this resource is managed
  fn loc(&self) -> *const u8;
}

/// Define how a struct should be allocated by the garbage collector
pub trait Allocate<R: Trace> {
  // The handle to manage this allocated object
  fn alloc(self) -> AllocResult<R>;
}

pub struct AllocResult<R> {
  pub handle: Box<dyn Manage>,
  pub reference: R,
}

impl<R> AllocResult<R> {
  pub fn new(handle: Box<dyn Manage>, reference: R) -> Self {
    Self { handle, reference }
  }
}
