mod raw_unique_vector;

use std::{
  fmt::{self, Debug, Display, Pointer},
  ops::{Deref, DerefMut},
  ptr::{self},
};

use raw_unique_vector::RawUniqueVector;

use crate::{
  collections::{IndexedResult, VecBuilder},
  managed::{DebugHeap, Mark, Trace, Unmark},
  GcHooks,
};

pub struct UniqueVector<T, H>(RawUniqueVector<T, H>);

impl<T, H> UniqueVector<T, H> {
  pub fn new(raw_vector: RawUniqueVector<T, H>) -> Self {
    Self(raw_vector)
  }

  pub fn len(&self) -> usize {
    self.0.len()
  }

  #[allow(dead_code)]
  pub fn cap(&self) -> usize {
    self.0.cap()
  }

  /// Pop the element off the vector
  #[allow(dead_code)]
  pub fn pop(&mut self) -> Option<T> {
    let len = self.0.len();

    if len == 0 {
      None
    } else {
      unsafe {
        self.0.write_len(len - 1);
        Some(self.0.read_value(len - 1))
      }
    }
  }

  /// Remove an element from this list at the provided offset
  #[allow(dead_code)]
  pub fn remove(&mut self, index: usize) -> IndexedResult<T> {
    let len = self.0.len();
    if index >= len {
      return IndexedResult::OutOfBounds;
    }

    unsafe {
      let value = self.0.read_value(index);
      ptr::copy(
        self.0.item_ptr(index + 1),
        self.0.item_mut(index),
        len - index - 1,
      );

      self.0.write_len(len - 1);
      IndexedResult::Ok(value)
    }
  }
}

impl<T, H> UniqueVector<T, H>
where
  T: 'static + Trace + DebugHeap + Copy,
  H: 'static + Send + Mark + Trace + Unmark + Default,
{
  /// Push a new element onto this list. If a resize is needed
  /// a new list will be allocated and elements will be transferred
  pub fn push(&mut self, value: T, hooks: &GcHooks) {
    let len = self.0.len();

    // determine if we need to grow the list then
    // persist the value
    self.ensure_capacity(len + 1, hooks);

    unsafe {
      self.0.write_value(value, len);
      self.0.write_len(len + 1);
    }
  }

  /// Insert an element into this list at the provided offset
  #[allow(dead_code)]
  pub fn insert(&mut self, index: usize, value: T, hooks: &GcHooks) -> IndexedResult {
    let len = self.0.len();
    if index > len {
      return IndexedResult::OutOfBounds;
    }

    // determine if we need to grow the list then
    // persist the value
    self.ensure_capacity(len + 1, hooks);

    unsafe {
      ptr::copy(
        self.0.item_ptr(index),
        self.0.item_mut(index + 1),
        len - index,
      );
      self.0.write_value(value, index);
      self.0.write_len(len + 1);
    }

    IndexedResult::Ok(())
  }

  /// Ensure this list has enough capacity for the operation
  /// If it does it returns itself. Otherwise it returns a new list
  /// which it will have just allocated
  fn ensure_capacity(&mut self, needed: usize, hooks: &GcHooks) {
    let cap = self.0.cap();

    if needed > cap {
      self.0 = hooks.manage::<RawUniqueVector<T, H>, _>(VecBuilder::new(self, cap * 2));
    }
  }
}

impl<T, H> Deref for UniqueVector<T, H> {
  type Target = [T];

  #[inline]
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl<T, H> DerefMut for UniqueVector<T, H> {
  #[inline]
  fn deref_mut(&mut self) -> &mut Self::Target {
    self.0.deref_mut()
  }
}

impl<T, H> Trace for UniqueVector<T, H>
where
  T: Trace + DebugHeap,
  H: Trace + Mark + Send,
{
  #[inline]
  fn trace(&self) {
    self.0.trace();
  }

  fn trace_debug(&self, log: &mut dyn std::io::Write) {
    self.0.trace_debug(log);
  }
}

impl<T, H> DebugHeap for UniqueVector<T, H>
where
  T: DebugHeap,
{
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    self.0.fmt_heap(f, depth)
  }
}

impl<T, H> fmt::Pointer for UniqueVector<T, H> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    Pointer::fmt(&self, f)
  }
}

// TODO we need this for now
// As of today the allocator needs to hold a
// boxed version of everything that is allocated
// as a temporary root
impl<T, H> Copy for UniqueVector<T, H> {}
impl<T, H> Clone for UniqueVector<T, H> {
  fn clone(&self) -> Self {
    *self
  }
}

impl<T, H> PartialEq<UniqueVector<T, H>> for UniqueVector<T, H> {
  #[inline]
  fn eq(&self, other: &UniqueVector<T, H>) -> bool {
    self.0.eq(&other.0)
  }
}
impl<T, H> Eq for UniqueVector<T, H> {}

impl<T, H> Debug for UniqueVector<T, H> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    Debug::fmt(&self, f)
  }
}

impl<T, H> Display for UniqueVector<T, H>
where
  T: Display,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "[")?;

    if let Some((last, rest)) = self.split_last() {
      for item in rest.iter() {
        write!(f, "{item}, ")?;
      }

      write!(f, "{last}")?;
    }

    write!(f, "]")
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::{managed::Header, NoContext};

  #[test]
  fn new() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    UniqueVector::<u16, Header>::new(hooks.manage(VecBuilder::new(&[1, 2, 3], 4)));
  }

  #[test]
  fn len() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let vector: UniqueVector<u16, Header> =
      UniqueVector::new(hooks.manage(VecBuilder::new(&[1, 2, 3], 4)));
    assert_eq!(vector.len(), 3)
  }

  #[test]
  fn cap() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let vector: UniqueVector<u16, Header> =
      UniqueVector::new(hooks.manage(VecBuilder::new(&[], 4)));
    assert_eq!(vector.cap(), 4)
  }

  #[test]
  fn deref() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let vector: UniqueVector<u16, Header> =
      UniqueVector::new(hooks.manage(VecBuilder::new(&[1, 2, 3], 4)));
    assert_eq!(vector[0], 1);
    assert_eq!(vector[1], 2);
    assert_eq!(vector[2], 3);
  }

  #[test]
  fn deref_mut() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut vector: UniqueVector<u16, Header> =
      UniqueVector::new(hooks.manage(VecBuilder::new(&[1, 2, 3], 4)));
    vector[1] = 5;

    assert_eq!(vector[0], 1);
    assert_eq!(vector[1], 5);
    assert_eq!(vector[2], 3);
  }

  #[test]
  fn pop() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut vector: UniqueVector<u16, Header> =
      UniqueVector::new(hooks.manage(VecBuilder::new(&[1, 2, 3], 4)));
    assert_eq!(vector.pop(), Some(3));
    assert_eq!(vector.pop(), Some(2));
    assert_eq!(vector.pop(), Some(1));
    assert_eq!(vector.pop(), None);

    assert_eq!(vector.len(), 0);
    assert_eq!(vector.cap(), 4);
  }

  #[test]
  fn remove() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut vector: UniqueVector<u16, Header> =
      UniqueVector::new(hooks.manage(VecBuilder::new(&[1, 2, 3], 4)));
    assert_eq!(vector.remove(2), IndexedResult::Ok(3));
    assert_eq!(vector.remove(2), IndexedResult::OutOfBounds);
    assert_eq!(vector.remove(0), IndexedResult::Ok(1));

    assert_eq!(vector.len(), 1);
    assert_eq!(vector.cap(), 4);
  }

  #[test]
  fn push() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut vector: UniqueVector<u16, Header> =
      UniqueVector::new(hooks.manage(VecBuilder::new(&[1, 2, 3], 4)));

    vector.push(4, &hooks);
    vector.push(5, &hooks);

    assert_eq!(vector[0], 1);
    assert_eq!(vector[1], 2);
    assert_eq!(vector[2], 3);
    assert_eq!(vector[3], 4);
    assert_eq!(vector[4], 5);

    assert_eq!(vector.len(), 5);
    assert_eq!(vector.cap(), 8);
  }

  #[test]
  fn insert() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut vector: UniqueVector<u16, Header> =
      UniqueVector::new(hooks.manage(VecBuilder::new(&[1, 2, 3], 4)));

    vector.push(4, &hooks);
    vector.push(5, &hooks);

    assert_eq!(vector[0], 1);
    assert_eq!(vector[1], 2);
    assert_eq!(vector[2], 3);
    assert_eq!(vector[3], 4);
    assert_eq!(vector[4], 5);

    assert_eq!(vector.len(), 5);
    assert_eq!(vector.cap(), 8);
  }
}