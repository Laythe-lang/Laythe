use crate::{
  hooks::Hooks,
  io::StdIo,
  managed::{Manage, Managed, Trace},
  value::{Class, Value},
};
use std::fmt;
use std::mem;

/// A container for iterators in spacelox. The IterContainer
/// is really a optimization for the Instance struct to avoid
/// the need for hashing when the special IterCurrent and IterNext
/// bytecodes are used
#[derive(Debug)]
pub struct SlIterator {
  /// The underlying iterator
  iterator: Box<dyn SlIter>,

  /// The current value of the iterator
  current: Value,

  /// The class of this iterator
  pub class: Managed<Class>,
}

impl SlIterator {
  /// Create a new iterator container
  pub fn new(iterator: Box<dyn SlIter>, class: Managed<Class>) -> Self {
    Self {
      iterator,
      current: Value::Nil,
      class,
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
  pub fn name(&self, hooks: &Hooks) -> Value {
    Value::String(hooks.manage_str(String::from(self.iterator.name())))
  }

  /// Increment the iterator
  pub fn next(&mut self, hooks: &Hooks) -> Value {
    let result = self.iterator.next(hooks);
    self.current = self.iterator.current();
    result
  }

  /// Get the current value of the iterator
  pub fn current(&self) -> Value {
    self.current
  }
}

impl Trace for SlIterator {
  fn trace(&self) -> bool {
    self.class.trace();
    self.current.trace();
    self.iterator.trace();

    true
  }

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    self.class.trace_debug(stdio);
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
    String::from("Iterator: {{ class: {{...}}, fields: {{...}} }}, iterator: {{ ... }}")
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
  fn next(&mut self, hooks: &Hooks) -> Value;

  /// What is the size of this iterator
  fn size(&self) -> usize;
}
