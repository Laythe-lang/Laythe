mod raw_unique_vector;

use std::{
  cell::RefMut,
  fmt::{self, Debug, Display, Pointer},
  ops::{Deref, DerefMut},
  ptr::{self},
};

use raw_unique_vector::RawUniqueVector;

use crate::{
  collections::{IndexedResult, VecBuilder},
  managed::{DebugHeap, Mark, Trace, TraceRoot, Unmark},
  Allocator, GcHooks,
};

pub struct UniqueVector<T, H>(Option<RawUniqueVector<T, H>>);

impl<T, H> UniqueVector<T, H> {
  #[inline]
  pub fn new(raw_vector: RawUniqueVector<T, H>) -> Self {
    Self(Some(raw_vector))
  }

  #[inline]
  pub fn len(&self) -> usize {
    match self.0 {
      Some(raw_vector) => raw_vector.len(),
      None => 0,
    }
  }

  #[inline]
  pub fn is_empty(&self) -> bool {
    match self.0 {
      Some(raw_vector) => raw_vector.is_empty(),
      None => true,
    }
  }

  #[allow(dead_code)]
  pub fn cap(&self) -> usize {
    match self.0 {
      Some(raw_vector) => raw_vector.cap(),
      None => 0,
    }
  }

  /// Pop the element off the vector
  #[inline]
  pub fn pop(&mut self) -> Option<T> {
    match &mut self.0 {
      Some(raw_vector) => {
        let len = raw_vector.len();

        if len == 0 {
          None
        } else {
          unsafe {
            raw_vector.write_len(len - 1);
            Some(raw_vector.read_value(len - 1))
          }
        }
      },
      None => None,
    }
  }

  /// Clear all elements of this vector
  #[allow(dead_code)]
  pub fn clear(&mut self) {
    if let Some(raw_vector) = &mut self.0 {
      let elements: *mut [T] = raw_vector.deref_mut();

      unsafe {
        raw_vector.write_len(0);
        ptr::drop_in_place(elements);
      }
    }
  }

  /// Clear all elements of this vector
  #[allow(dead_code)]
  pub fn truncate(&mut self, new_len: usize) {
    if let Some(raw_vector) = &mut self.0 {
      let len = raw_vector.len();
      if new_len >= len {
        return;
      }
      let remaining_len = len - new_len;

      unsafe {
        let s = ptr::slice_from_raw_parts_mut(raw_vector.as_mut_ptr().add(len), remaining_len);
        raw_vector.write_len(new_len);
        ptr::drop_in_place(s);
      }
    }
  }

  /// Remove an element from this list at the provided offset
  #[allow(dead_code)]
  pub fn remove(&mut self, index: usize) -> IndexedResult<T> {
    match &mut self.0 {
      Some(raw_vector) => {
        let len = raw_vector.len();
        if index >= len {
          return IndexedResult::OutOfBounds;
        }

        unsafe {
          let value = raw_vector.read_value(index);
          ptr::copy(
            raw_vector.item_ptr(index + 1),
            raw_vector.item_mut(index),
            len - index - 1,
          );

          raw_vector.write_len(len - 1);
          IndexedResult::Ok(value)
        }
      },
      None => IndexedResult::OutOfBounds,
    }
  }
}

impl<T, H> UniqueVector<T, H>
where
  T: 'static + Trace + DebugHeap + Copy,
  H: 'static + Send + Mark + Unmark + Trace + Default,
{
  /// Clear all elements of this vector using GcHooks
  pub fn reserve_with_hooks(&mut self, hooks: &GcHooks, additional: usize) {
    let (len, cap) = match self.0 {
      Some(raw_vector) => (raw_vector.len(), raw_vector.cap()),
      None => (0, 0),
    };

    #[cfg(not(feature = "gc_stress"))]
    if len + additional > cap {
      self.0 = Some(hooks.manage::<RawUniqueVector<T, H>, _>(VecBuilder::new(
        self,
        reserve_cap_growth(cap, len + additional),
      )));
    }

    // when stress testing if we don't allocate collect anyways
    #[cfg(feature = "gc_stress")]
    if len + additional > cap {
      self.0 = Some(hooks.manage::<RawUniqueVector<T, H>, _>(VecBuilder::new(
        self,
        reserve_cap_growth(cap, len + additional),
      )));
    } else {
      hooks.collect_garbage();
    }
  }

  /// Clear all elements of this vector
  #[inline]
  pub fn reserve<C: TraceRoot>(
    &mut self,
    mut allocator: RefMut<'_, Allocator>,
    context: &C,
    additional: usize,
  ) {
    let (len, cap) = match self.0 {
      Some(raw_vector) => (raw_vector.len(), raw_vector.cap()),
      None => (0, 0),
    };

    #[cfg(not(feature = "gc_stress"))]
    if len + additional > cap {
      self.0 = Some(allocator.manage(
        VecBuilder::new(self, reserve_cap_growth(cap, len + additional)),
        context,
      ));
    }

    // when stress testing if we don't allocate collect anyways
    #[cfg(feature = "gc_stress")]
    if len + additional > cap {
      self.0 = Some(allocator.manage(
        VecBuilder::new(self, reserve_cap_growth(cap, len + additional)),
        context,
      ));
    } else {
      allocator.collect_garbage(context);
    }
  }

  /// Push a new element onto this list. If a resize is needed
  /// a new list will be allocated and elements will be transferred
  pub fn push_with_hooks(&mut self, hooks: &GcHooks, value: T) {
    let len = self.len();

    // determine if we need to grow the list then
    // persist the value
    self.reserve_with_hooks(hooks, 1);

    unsafe {
      // we know there is now an allocation so we can blindly unwrap
      let mut raw_vector = self.0.unwrap();
      raw_vector.write_value(value, len);
      raw_vector.write_len(len + 1);
    }
  }

  /// Clear all elements of this vector
  #[inline]
  pub fn push<C: TraceRoot>(&mut self, allocator: RefMut<'_, Allocator>, context: &C, value: T) {
    let len = self.len();

    // determine if we need to grow the list then
    // persist the value
    self.reserve(allocator, context, 1);

    unsafe {
      // we know there is now an allocation so we can blindly unwrap
      let mut raw_vector = self.0.unwrap();
      raw_vector.write_value(value, len);
      raw_vector.write_len(len + 1);
    }
  }

  /// Clear all elements of this vector
  #[inline]
  pub fn extend<C: TraceRoot>(
    &mut self,
    allocator: RefMut<'_, Allocator>,
    context: &C,
    values: &[T],
  ) {
    let len = self.len();

    // determine if we need to grow the list then
    // persist the value
    self.reserve(allocator, context, values.len());

    unsafe {
      // we know there is now an allocation so we can blindly unwrap
      let mut raw_vector = self.0.unwrap();
      for (idx, value) in values.iter().enumerate() {
        raw_vector.write_value(*value, len + idx);
      }
      raw_vector.write_len(len + values.len());
    }
  }

  /// Insert an element into this list at the provided offset
  #[allow(dead_code)]
  pub fn insert(&mut self, index: usize, value: T, hooks: &GcHooks) -> IndexedResult {
    let len = self.len();
    if index > len {
      return IndexedResult::OutOfBounds;
    }

    // determine if we need to grow the list then
    // persist the value
    self.reserve_with_hooks(hooks, 1);

    unsafe {
      let mut raw_vector = self.0.unwrap();
      ptr::copy(
        raw_vector.item_ptr(index),
        raw_vector.item_mut(index + 1),
        len - index,
      );
      raw_vector.write_value(value, index);
      raw_vector.write_len(len + 1);
    }

    IndexedResult::Ok(())
  }
}

fn reserve_cap_growth(cap: usize, new_cap: usize) -> usize {
  usize::max(usize::max(new_cap, 4), cap * 2)
}

impl<T, H> Default for UniqueVector<T, H> {
  #[inline]
  fn default() -> Self {
    Self(None)
  }
}

impl<T, H> Deref for UniqueVector<T, H> {
  type Target = [T];

  #[inline]
  fn deref(&self) -> &Self::Target {
    match &self.0 {
      Some(raw_vector) => raw_vector,
      None => &[],
    }
  }
}

impl<T, H> DerefMut for UniqueVector<T, H> {
  #[inline]
  fn deref_mut(&mut self) -> &mut Self::Target {
    match &mut self.0 {
      Some(raw_vector) => &mut *raw_vector,
      None => &mut [],
    }
  }
}

impl<T, H> Trace for UniqueVector<T, H>
where
  T: Trace + DebugHeap,
  H: Trace + Mark + Send,
{
  #[inline]
  fn trace(&self) {
    if let Some(raw_vector) = self.0 {
      raw_vector.trace();
    }
  }

  fn trace_debug(&self, log: &mut dyn std::io::Write) {
    if let Some(raw_vector) = self.0 {
      raw_vector.trace_debug(log);
    }
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
  fn reserve() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut vector: UniqueVector<u16, Header> =
      UniqueVector::new(hooks.manage(VecBuilder::new(&[2, 2], 4)));

    vector.reserve_with_hooks(&hooks, 2);
    assert_eq!(vector.cap(), 4);

    vector.reserve_with_hooks(&hooks, 4);
    assert_eq!(vector.cap(), 8)
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
  fn clear() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut vector: UniqueVector<u16, Header> =
      UniqueVector::new(hooks.manage(VecBuilder::new(&[1, 2, 3], 4)));
    assert_eq!(vector.len(), 3);
    assert_eq!(vector.cap(), 4);

    vector.clear();
    assert_eq!(vector.len(), 0);
    assert_eq!(vector.cap(), 4);
  }

  #[test]
  fn truncate() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut vector: UniqueVector<u16, Header> =
      UniqueVector::new(hooks.manage(VecBuilder::new(&[1, 2, 3], 4)));
    assert_eq!(vector.len(), 3);
    assert_eq!(vector.cap(), 4);

    vector.truncate(6);
    assert_eq!(vector.len(), 3);
    assert_eq!(vector.cap(), 4);

    vector.truncate(1);
    assert_eq!(vector.len(), 1);
    assert_eq!(vector.cap(), 4);
    assert_eq!(vector[0], 1);
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

    vector.push_with_hooks(&hooks, 4);
    vector.push_with_hooks(&hooks, 5);

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

    vector.push_with_hooks(&hooks, 4);
    vector.push_with_hooks(&hooks, 5);

    assert_eq!(vector[0], 1);
    assert_eq!(vector[1], 2);
    assert_eq!(vector[2], 3);
    assert_eq!(vector[3], 4);
    assert_eq!(vector[4], 5);

    assert_eq!(vector.len(), 5);
    assert_eq!(vector.cap(), 8);
  }
}
