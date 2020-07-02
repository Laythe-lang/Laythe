use crate::{
  hooks::Hooks,
  value::{Value, VALUE_NIL},
  CallResult,
};
use laythe_env::{
  managed::{Manage, Managed, Trace},
  stdio::StdIo,
};
use std::fmt;
use std::mem;

/// A container for iterators in laythe. The IterContainer
/// is really a optimization for the Instance struct to avoid
/// the need for hashing when the special IterCurrent and IterNext
/// bytecodes are used
#[derive(Debug)]
pub struct SlIterator {
  /// The underlying iterator
  iterator: Box<dyn SlIter>,

  /// The current value of the iterator
  current: Value,
}

impl SlIterator {
  /// Create a new iterator container
  pub fn new(iterator: Box<dyn SlIter>) -> Self {
    Self {
      iterator,
      current: VALUE_NIL,
    }
  }

  /// Allow access the "current" field in the iterator
  pub fn get_field(&self, name: &Managed<String>) -> Option<&Value> {
    if &***name == "current" {
      return Some(&self.current);
    }

    None
  }

  /// Get the name of this iterator
  pub fn name(&self) -> &str {
    self.iterator.name()
  }

  /// Increment the iterator
  pub fn next(&mut self, hooks: &mut Hooks) -> CallResult {
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

impl Trace for SlIterator {
  fn trace(&self) -> bool {
    self.current.trace();
    self.iterator.trace();

    true
  }

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    self.current.trace_debug(stdio);
    self.iterator.trace_debug(stdio);

    true
  }
}

impl Manage for SlIterator {
  fn alloc_type(&self) -> &str {
    "iterator"
  }

  fn debug(&self) -> String {
    format!("{:?}", self)
  }

  fn debug_free(&self) -> String {
    "Iterator: {{ class: {{...}}, fields: {{...}} }}, iterator: {{ ... }}".to_string()
  }

  fn size(&self) -> usize {
    mem::size_of::<SlIterator>() + self.iterator.size()
  }
}

pub trait SlIter: Trace + fmt::Debug {
  /// The name of the iterator mostly for debugging purposes
  fn name(&self) -> &str;

  /// Get the current value from the iterator
  fn current(&self) -> Value;

  /// Get the next value indicating if the iterator has reached the end
  fn next(&mut self, hooks: &mut Hooks) -> CallResult;

  /// If known how many elements will this iterator produce
  fn size_hint(&self) -> Option<usize>;

  /// What is the size of this iterator
  fn size(&self) -> usize;
}
