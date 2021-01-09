use crate::{
  hooks::Hooks,
  value::{Value, VALUE_NIL},
  Call,
};
use laythe_env::managed::{DebugHeap, Manage, Trace};
use std::mem;
use std::{fmt, io::Write};

/// A container for iterators in laythe. The IterContainer
/// is really a optimization for the Instance struct to avoid
/// the need for hashing when the special IterCurrent and IterNext
/// bytecodes are used
#[derive(Debug)]
pub struct LyIterator {
  /// The underlying iterator
  iterator: Box<dyn LyIter>,

  /// The current value of the iterator
  current: Value,
}

impl LyIterator {
  /// Create a new iterator container
  pub fn new(iterator: Box<dyn LyIter>) -> Self {
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

  /// Get the current value of the iterator
  pub fn current(&self) -> Value {
    self.current
  }

  pub fn size_hint(&self) -> Option<usize> {
    self.iterator.size_hint()
  }
}

impl Trace for LyIterator {
  fn trace(&self) {
    self.current.trace();
    self.iterator.trace();
  }

  fn trace_debug(&self, stdout: &mut dyn Write) {
    self.current.trace_debug(stdout);
    self.iterator.trace_debug(stdout);
  }
}

impl DebugHeap for LyIterator {
  fn fmt_heap(&self, f: &mut fmt::Formatter, _: usize) -> fmt::Result {
    f.write_fmt(format_args!("{:?}", self))
  }
}

impl Manage for LyIterator {
  fn size(&self) -> usize {
    mem::size_of::<LyIterator>() + self.iterator.size()
  }

  fn as_debug(&self) -> &dyn laythe_env::managed::DebugHeap {
    self
  }
}

pub trait LyIter: Trace + fmt::Debug {
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
