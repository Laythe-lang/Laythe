use crate::{
  hooks::Hooks,
  managed::{DebugHeap, DebugWrap, DebugWrapDyn, Object, Trace},
  value::{Value, VALUE_NIL},
  Call,
};
use std::fmt::Debug;
use std::{fmt, io::Write};

use super::ObjectKind;

/// A container for iterators in laythe. The IterContainer
/// is really a optimization for the Instance struct to avoid
/// the need for hashing when the special IterCurrent and IterNext
/// bytecodes are used
#[derive(Debug)]
pub struct Enumerator {
  /// The underlying iterator
  iterator: Box<dyn Enumerate>,

  /// The current value of the iterator
  current: Value,
}

impl Enumerator {
  /// Create a new iterator container
  pub fn new(iterator: Box<dyn Enumerate>) -> Self {
    Self {
      iterator,
      current: VALUE_NIL,
    }
  }

  /// Get the name of this iterator
  pub fn name(&self) -> &str {
    self.iterator.name()
  }

  /// Increment the iterator
  pub fn next(&mut self, hooks: &mut Hooks) -> Call {
    let result = self.iterator.next(hooks);
    self.current = self.iterator.current();
    result
  }

  #[inline]
  /// Get the current value of the iterator
  pub fn current(&self) -> Value {
    self.current
  }

  #[inline]
  pub fn size_hint(&self) -> Option<usize> {
    self.iterator.size_hint()
  }
}

impl fmt::Display for Enumerator {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<iter {} {:p}>", self.name(), self)
  }
}

impl Trace for Enumerator {
  fn trace(&self) {
    self.current.trace();
    self.iterator.trace();
  }

  fn trace_debug(&self, stdout: &mut dyn Write) {
    self.current.trace_debug(stdout);
    self.iterator.trace_debug(stdout);
  }
}

impl DebugHeap for Enumerator {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    f.debug_struct("Enumerator")
      .field("current", &DebugWrap(&self.current, depth))
      .field("iterator", &DebugWrapDyn(self.iterator.as_debug(), depth))
      .finish()
  }
}

impl Object for Enumerator {
  fn kind(&self) -> ObjectKind {
    ObjectKind::Enumerator
  }
}

unsafe impl Send for Enumerator {}

pub trait Enumerate: Trace + fmt::Debug + Send {
  /// The name of the iterator mostly for debugging purposes
  fn name(&self) -> &str;

  /// Get the current value from the iterator
  fn current(&self) -> Value;

  /// Get the next value indicating if the iterator has reached the end
  fn next(&mut self, hooks: &mut Hooks) -> Call;

  /// If known how many elements will this iterator produce
  fn size_hint(&self) -> Option<usize>;

  // Get this enumerator as a DebugHeap
  fn as_debug(&self) -> &dyn DebugHeap;
}
