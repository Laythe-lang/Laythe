use crate::{
  hooks::Hooks,
  managed::{DebugHeap, Manage, Object, Trace},
  value::{Value, VALUE_NIL},
  Call,
};
use std::mem;
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
    write!(f, "<{} iter {:p}>", self.name(), &self)
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
  fn fmt_heap(&self, f: &mut fmt::Formatter, _: usize) -> fmt::Result {
    f.write_fmt(format_args!("{:?}", self))
  }
}

impl Manage for Enumerator {
  fn size(&self) -> usize {
    mem::size_of::<Enumerator>() + self.iterator.size()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

impl Object for Enumerator {
  fn kind(&self) -> ObjectKind {
    ObjectKind::Enumerator
  }
}

pub trait Enumerate: Trace + fmt::Debug {
  /// The name of the iterator mostly for debugging purposes
  fn name(&self) -> &str;

  /// Get the current value from the iterator
  fn current(&self) -> Value;

  /// Get the next value indicating if the iterator has reached the end
  fn next(&mut self, hooks: &mut Hooks) -> Call;

  /// If known how many elements will this iterator produce
  fn size_hint(&self) -> Option<usize>;

  /// What is the size of this iterator
  fn size(&self) -> usize;
}
