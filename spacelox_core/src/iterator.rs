use crate::{
  dynamic_map::DynamicMap,
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
pub struct SlIterator {
  /// The underlying iterator
  iterator: Box<dyn SlIter>,

  /// The fields in the case that a user sets an additional fields
  /// on the iterator
  fields: DynamicMap<Managed<String>, Value>,

  /// The class of this iterator
  pub class: Managed<Class>,
}

impl SlIterator {
  pub fn new(iterator: Box<dyn SlIter>, class: Managed<Class>) -> Self {
    Self {
      iterator,
      fields: DynamicMap::default(),
      class,
    }
  }

  pub fn set_field(&mut self, hooks: &Hooks, name: Managed<String>, value: Value) {
    hooks.resize(self, |instance: &mut SlIterator| {
      instance.fields.insert(name, value);
    });
  }

  pub fn get_field(&self, name: &Managed<String>) -> Option<&Value> {
    self.fields.get(name)
  }

  pub fn next(&mut self) -> Value {
    self.iterator.next()
  }

  pub fn current(&self) -> Value {
    self.iterator.current()
  }
}

impl fmt::Debug for SlIterator {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    f.debug_struct("Instance")
      .field("class", &self.class)
      .field("fields", &self.fields)
      .field("iterator", &self.iterator)
      .finish()
  }
}

impl Trace for SlIterator {
  fn trace(&self) -> bool {
    self.class.trace();

    self.fields.for_each(|(key, val)| {
      key.trace();
      val.trace();
    });

    self.iterator.trace();

    true
  }

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    self.class.trace_debug(stdio);

    self.fields.for_each(|(key, val)| {
      key.trace_debug(stdio);
      val.trace_debug(stdio);
    });

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
    mem::size_of::<SlIterator>()
      + (mem::size_of::<Managed<String>>() + mem::size_of::<Value>()) * self.fields.capacity()
      + self.iterator.size()
  }
}

pub trait SlIter: Trace + fmt::Debug {
  /// Get the current value from the iterator
  fn current(&self) -> Value;

  /// Get the next value indicating if the iterator has reached the end
  fn next(&mut self) -> Value;

  /// What is the size of this iterator
  fn size(&self) -> usize;
}
