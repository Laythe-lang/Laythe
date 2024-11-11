mod raw_vector;
mod vec_builder;

use crate::{
  hooks::GcHooks,
  managed::{DebugHeap, Mark, Marked, Trace, Unmark},
};
use ptr::NonNull;
pub use raw_vector::{IndexedResult, RawVecLocation, RawVector, VectorHandle};
use std::{
  fmt::{self, Debug, Display},
  ops::{Deref, DerefMut},
  ptr,
};
pub use vec_builder::VecBuilder;

/// The location of this list
#[allow(dead_code)]
pub enum VecLocation<T, H> {
  /// The list is in it's default state which returns it's capacity
  Here(usize),

  /// The list has been forwarded to a new location
  Forwarded(Vector<T, H>),
}

/// A non owning reference to a Garbage collector
/// allocated list. Note this list is the same size
/// as a single pointer.
pub struct Vector<T, H>(RawVector<T, H>);

impl<T, H> Vector<T, H> {
  /// Retrieve the len from this list
  #[inline]
  #[allow(dead_code)]
  pub fn len(&self) -> usize {
    self.0.len()
  }

  /// Retrieve the capacity of this vector
  #[inline]
  #[allow(dead_code)]
  pub fn cap(&self) -> usize {
    self.0.cap()
  }

  /// Retrieve the underlying non Null
  #[allow(dead_code)]
  pub fn ptr(&self) -> NonNull<u8> {
    self.0.ptr()
  }

  /// Create a usize from the buffer pointer. This is used
  /// when the value is boxed
  #[allow(dead_code)]
  pub fn to_usize(self) -> usize {
    self.0.to_usize()
  }

  /// Has this list moved
  #[inline]
  #[allow(dead_code)]
  pub fn has_moved(&self) -> bool {
    self.0.has_moved()
  }

  /// Construct a `Vector<T>` from `NonNull<u8>`
  ///
  /// ## Safety
  /// This should only be constructed from a box value
  #[allow(dead_code)]
  pub unsafe fn from_alloc_ptr(ptr: NonNull<u8>) -> Self {
    Vector(RawVector::from_alloc_ptr(ptr))
  }

  /// Determine the state of this list
  #[allow(dead_code)]
  pub fn state(&self) -> VecLocation<T, H> {
    match self.0.state() {
      RawVecLocation::Here(cap) => VecLocation::Here(cap),
      RawVecLocation::Forwarded(raw_vector) => VecLocation::Forwarded(Vector(raw_vector)),
    }
  }

  /// Pop and element off the list.
  #[allow(dead_code)]
  pub fn pop(&mut self) -> Option<T> {
    self.0.pop()
  }

  /// Remove an element from this list at the provided offset
  #[allow(dead_code)]
  pub fn remove(&mut self, index: usize) -> IndexedResult<T> {
    self.0.remove(index)
  }
}

impl<T, H> Vector<T, H>
where
  T: Trace + DebugHeap + Copy + 'static,
  H: Trace + Mark + Unmark + Send + 'static + Default,
{
  /// Push a new element onto this list. If a resize is needed
  /// a new list will be allocated and elements will be transferred
  ///
  #[allow(dead_code)]
  pub fn push(&mut self, value: T, hooks: &GcHooks) {
    self.0.push(value, |slice, cap| {
      hooks.manage(VecBuilder::new(slice, cap))
    });
  }

  /// Insert an element into this list at the provided offset
  #[allow(dead_code)]
  pub fn insert(&mut self, index: usize, value: T, hooks: &GcHooks) -> IndexedResult {
    self.0.insert(index, value, |slice, cap| {
      hooks.manage(VecBuilder::new(slice, cap))
    })
  }
}

impl<T, H: Mark> Mark for Vector<T, H> {
  /// Mark the list itself as visited
  #[inline]
  fn mark(&self) -> bool {
    self.0.mark()
  }
}

impl<T, H: Marked> Marked for Vector<T, H> {
  /// Is this list marked
  #[inline]
  fn marked(&self) -> bool {
    self.0.marked()
  }
}

impl<T: Trace + DebugHeap, H: Send + Mark + Trace> Trace for Vector<T, H> {
  #[inline]
  fn trace(&self) {
    self.0.trace();
  }

  fn trace_debug(&self, log: &mut dyn std::io::Write) {
    self.0.trace_debug(log);
  }
}

impl<T: DebugHeap, H> DebugHeap for Vector<T, H> {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    self.0.fmt_heap(f, depth)
  }
}

impl<T, H> fmt::Pointer for Vector<T, H> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Pointer::fmt(&self.0, f)
  }
}

impl<T, H> Copy for Vector<T, H> {}
impl<T, H> Clone for Vector<T, H> {
  fn clone(&self) -> Self {
    *self
  }
}

impl<T, H> Deref for Vector<T, H> {
  type Target = [T];

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.0.deref()
  }
}

impl<T, H> DerefMut for Vector<T, H> {
  #[inline]
  fn deref_mut(&mut self) -> &mut Self::Target {
    self.0.deref_mut()
  }
}

impl<T, H> PartialEq<Vector<T, H>> for Vector<T, H> {
  #[inline]
  fn eq(&self, other: &Vector<T, H>) -> bool {
    self.0.eq(&other.0)
  }
}
impl<T, H> Eq for Vector<T, H> {}

impl<T: Display, H> Display for Vector<T, H> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    Display::fmt(&self.0, f)
  }
}

impl<T: Debug, H> Debug for Vector<T, H> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    Debug::fmt(&self.0, f)
  }
}

unsafe impl<T: Send, H: Send> Send for Vector<T, H> {}
unsafe impl<T: Sync, H: Sync> Sync for Vector<T, H> {}

#[cfg(test)]
mod test {
  use super::*;

  mod list {
    use raw_vector::VectorHandle;

    use crate::{hooks::NoContext, managed::Header, val};

    use super::*;

    #[test]
    fn len() {
      let handle = VectorHandle::from_slice(&[1, 2, 3, 4, 5], 5, String::from("header"));
      let list = Vector(handle.value());

      assert_eq!(list.len(), 5);
    }

    #[test]
    fn is_empty() {
      let handle1 = VectorHandle::from_slice(&[1, 2, 3, 4, 5], 5, String::from("header"));
      let handle2 = VectorHandle::<i32, String>::from_slice(&[], 5, String::from("header"));
      let list1 = Vector(handle1.value());
      let list2 = Vector(handle2.value());

      assert!(!list1.is_empty());
      assert!(list2.is_empty());
    }

    #[test]
    fn cap() {
      let handle = VectorHandle::from_slice(&[1, 2, 3, 4, 5], 10, String::from("header"));
      let list = Vector(handle.value());

      assert_eq!(list.cap(), 10);
    }

    #[test]
    fn pop() {
      let handle = VectorHandle::from_slice(&[val!(1.0), val!(2.0), val!(true)], 3, Header::new());
      let mut list = Vector(handle.value());

      assert_eq!(list.pop(), Some(val!(true)));
      assert_eq!(list.len(), 2);

      assert_eq!(list.pop(), Some(val!(2.0)));
      assert_eq!(list.len(), 1);

      assert_eq!(list.pop(), Some(val!(1.0)));
      assert_eq!(list.len(), 0);

      assert_eq!(list.pop(), None);
      assert_eq!(list.len(), 0);
    }

    mod push {
      use super::*;
      #[test]
      fn with_capacity() {
        let context = NoContext::default();
        let hooks = GcHooks::new(&context);

        let handle = VectorHandle::from_slice(&[val!(1.0)], 2, Header::new());
        let mut list = Vector(handle.value());

        list.push(val!(3.0), &hooks);
        assert_eq!(list[0], val!(1.0));
        assert_eq!(list[1], val!(3.0));
        assert_eq!(list.len(), 2);
        assert_eq!(list.cap(), 2);
      }

      #[test]
      fn without_capacity() {
        let context = NoContext::default();
        let hooks = GcHooks::new(&context);

        let handle = VectorHandle::from_slice(&[val!(1.0)], 1, Header::new());
        let mut list = Vector(handle.value());

        list.push(val!(3.0), &hooks);
        list.push(val!(5.0), &hooks);
        assert_eq!(list[0], val!(1.0));
        assert_eq!(list[1], val!(3.0));
        assert_eq!(list[2], val!(5.0));
        assert_eq!(list.len(), 3);
        assert_eq!(list.cap(), 4);
      }
    }

    mod insert {
      use super::*;

      #[test]
      fn with_capacity() {
        let context = NoContext::default();
        let hooks = GcHooks::new(&context);

        let handle = VectorHandle::from_slice(&[val!(1.0)], 4, Header::new());
        let mut list = Vector(handle.value());

        assert_eq!(list.insert(0, val!(2.0), &hooks), IndexedResult::Ok(()));

        assert_eq!(list.len(), 2);
        assert_eq!(list.cap(), 4);
        assert_eq!(list[0], val!(2.0));
        assert_eq!(list[1], val!(1.0));

        assert_eq!(list.insert(2, val!(3.0), &hooks), IndexedResult::Ok(()));

        assert_eq!(list.len(), 3);
        assert_eq!(list.cap(), 4);
        assert_eq!(list[0], val!(2.0));
        assert_eq!(list[1], val!(1.0));
        assert_eq!(list[2], val!(3.0));

        assert_eq!(
          list.insert(5, val!(5.0), &hooks),
          IndexedResult::OutOfBounds
        );

        assert_eq!(list.len(), 3);
        assert_eq!(list.cap(), 4);
        assert_eq!(list[0], val!(2.0));
        assert_eq!(list[1], val!(1.0));
        assert_eq!(list[2], val!(3.0));
      }

      #[test]
      fn without_capacity() {
        let context = NoContext::default();
        let hooks = GcHooks::new(&context);

        let handle = VectorHandle::from_slice(&[val!(1.0)], 1, Header::new());
        let mut list = Vector(handle.value());

        assert_eq!(list.insert(0, val!(2.0), &hooks), IndexedResult::Ok(()));

        assert_eq!(list.len(), 2);
        assert_eq!(list.cap(), 2);
        assert_eq!(list[0], val!(2.0));
        assert_eq!(list[1], val!(1.0));

        assert_eq!(list.insert(2, val!(3.0), &hooks), IndexedResult::Ok(()));

        assert_eq!(list.len(), 3);
        assert_eq!(list.cap(), 4);
        assert_eq!(list[0], val!(2.0));
        assert_eq!(list[1], val!(1.0));
        assert_eq!(list[2], val!(3.0));

        assert_eq!(
          list.insert(5, val!(5.0), &hooks),
          IndexedResult::OutOfBounds
        );

        assert_eq!(list.len(), 3);
        assert_eq!(list.cap(), 4);
        assert_eq!(list[0], val!(2.0));
        assert_eq!(list[1], val!(1.0));
        assert_eq!(list[2], val!(3.0));
      }
    }

    #[test]
    fn remove() {
      let handle = VectorHandle::from_slice(&[val!(1.0), val!(3.0), val!(false)], 3, Header::new());
      let mut list = Vector(handle.value());

      assert_eq!(list.remove(3), IndexedResult::OutOfBounds);
      assert_eq!(list.remove(1), IndexedResult::Ok(val!(3.0)));

      assert_eq!(list.len(), 2);
      assert_eq!(list.cap(), 3);
      assert_eq!(list[0], val!(1.0));
      assert_eq!(list[1], val!(false));

      assert_eq!(list.remove(1), IndexedResult::Ok(val!(false)));

      assert_eq!(list.len(), 1);
      assert_eq!(list.cap(), 3);
      assert_eq!(list[0], val!(1.0));

      assert_eq!(list.remove(0), IndexedResult::Ok(val!(1.0)));

      assert_eq!(list.len(), 0);
      assert_eq!(list.cap(), 3);
    }

    #[test]
    fn remove_forwarded() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let handle = VectorHandle::from_slice(&[val!(1.0)], 1, Header::new());
      let mut list = Vector(handle.value());

      list.push(val!(3.0), &hooks);
      list.push(val!(false), &hooks);

      assert_eq!(list.remove(3), IndexedResult::OutOfBounds);
      assert_eq!(list.remove(1), IndexedResult::Ok(val!(3.0)));

      assert_eq!(list.len(), 2);
      assert_eq!(list.cap(), 4);
      assert_eq!(list[0], val!(1.0));
      assert_eq!(list[1], val!(false));

      assert_eq!(list.remove(1), IndexedResult::Ok(val!(false)));

      assert_eq!(list.len(), 1);
      assert_eq!(list.cap(), 4);
      assert_eq!(list[0], val!(1.0));

      assert_eq!(list.remove(0), IndexedResult::Ok(val!(1.0)));

      assert_eq!(list.len(), 0);
      assert_eq!(list.cap(), 4);
    }
  }

  mod handle {
    use super::*;

    #[test]
    fn from_slice() {
      let list_handle = VectorHandle::from_slice(&[1, 2, 3, 4, 5], 6, String::from("header"));
      let list = list_handle.value();

      assert_eq!(list.len(), 5);
      assert_eq!(list.cap(), 6);
      assert_eq!(list[0], 1);
      assert_eq!(list[1], 2);
      assert_eq!(list[2], 3);
      assert_eq!(list[3], 4);
      assert_eq!(list[4], 5);
    }

    #[test]
    #[should_panic]
    fn from_slice_bad_cap() {
      VectorHandle::from_slice(&[1, 2, 3, 4, 5], 3, String::from("header"));
    }
  }
}
