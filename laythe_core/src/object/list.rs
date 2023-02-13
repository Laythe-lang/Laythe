use std::{
  cmp::Ordering,
  fmt::{self, Display},
  io::Write,
  iter::FromIterator,
  ops::{Deref, Index, IndexMut},
  slice::{self, SliceIndex},
};

use crate::{
  managed::{DebugHeap, DebugWrap, Object, Trace},
  value::Value,
};

use super::ObjectKind;

#[derive(Clone, Debug)]
pub struct List<T>(Vec<T>);

impl<T> List<T> {
  #[inline]
  pub fn new() -> Self {
    Self(Vec::new())
  }

  #[inline]
  pub fn with_capacity(capacity: usize) -> Self {
    Self(Vec::with_capacity(capacity))
  }

  pub fn iter(&self) -> slice::Iter<'_, T> {
    self.0.iter()
  }

  #[inline]
  pub fn capacity(&self) -> usize {
    self.0.capacity()
  }

  #[inline]
  pub fn len(&self) -> usize {
    self.0.len()
  }

  #[inline]
  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }

  #[inline]
  pub fn push(&mut self, value: T) {
    self.0.push(value)
  }

  #[inline]
  pub fn pop(&mut self) -> Option<T> {
    self.0.pop()
  }

  pub fn remove(&mut self, index: usize) -> T {
    self.0.remove(index)
  }

  pub fn insert(&mut self, index: usize, value: T) {
    self.0.insert(index, value)
  }

  pub fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
    self.0.extend(iter)
  }

  pub fn sort_by<F: FnMut(&T, &T) -> Ordering>(&mut self, compare: F) {
    self.0.sort_by(compare)
  }

  pub fn clear(&mut self) {
    self.0.clear()
  }
}

impl<T: Display> Display for List<T> {
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

impl<T: PartialEq> List<T> {
  pub fn contains(&mut self, value: &T) -> bool {
    self.0.contains(value)
  }
}

impl<T> Default for List<T> {
  fn default() -> Self {
    Self(Vec::new())
  }
}

impl<T: Clone> List<T> {
  pub fn to_list(&self) -> List<T> {
    List(self.0.to_vec())
  }

  pub fn extend_from_slice(&mut self, other: &[T]) {
    self.0.extend_from_slice(other)
  }
}

impl<T> FromIterator<T> for List<T> {
  fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
    List(Vec::from_iter(iter))
  }
}

impl<T, I: SliceIndex<[T]>> Index<I> for List<T> {
  type Output = <I as SliceIndex<[T]>>::Output;

  #[inline]
  fn index(&self, index: I) -> &<Vec<T> as Index<I>>::Output {
    &self.0[index]
  }
}

impl<T, I: SliceIndex<[T]>> IndexMut<I> for List<T> {
  #[inline]
  fn index_mut(&mut self, index: I) -> &mut <Vec<T> as Index<I>>::Output {
    &mut self.0[index]
  }
}

impl<T> Deref for List<T> {
  type Target = [T];

  #[inline]
  fn deref(&self) -> &[T] {
    self.0.deref()
  }
}

impl<T> From<Vec<T>> for List<T> {
  fn from(vec: Vec<T>) -> Self {
    List(vec)
  }
}

impl<T: Clone> From<&[T]> for List<T> {
  fn from(slice: &[T]) -> Self {
    List(Vec::from(slice))
  }
}

impl<T: 'static + Trace> Trace for List<T> {
  fn trace(&self) {
    self.iter().for_each(|value| {
      value.trace();
    });
  }

  fn trace_debug(&self, stdio: &mut dyn Write) {
    self.iter().for_each(|value| {
      value.trace_debug(stdio);
    });
  }
}

impl<T: 'static + Trace + DebugHeap> DebugHeap for List<T> {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    f.debug_list()
      .entries(self.0.iter().map(|x| DebugWrap(x, depth)))
      .finish()
  }
}

impl Object for List<Value> {
  fn kind(&self) -> ObjectKind {
    ObjectKind::List
  }
}
