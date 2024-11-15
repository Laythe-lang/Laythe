use std::{
  fmt::{self, Debug, Display, Pointer},
  mem,
  ops::{Deref, DerefMut},
  ptr::{self, NonNull},
};

use crate::{
  collections::{
    IndexedResult, RawSharedVector, RawSharedVectorHandle, RawVecLocation, VecBuilder,
  },
  managed::{AllocObjResult, AllocateObj, DebugHeap, Trace},
  object::ObjectKind,
  reference::ObjectHandle,
  value::Value,
  GcHooks,
};

#[cfg(not(feature = "nan_boxing"))]
use crate::ObjectRef;

use super::ObjHeader;

/// The location of this list
pub enum ListLocation {
  /// The list is in it's default state which returns it's capacity
  Here(usize),

  /// The list has been forwarded to a new location
  Forwarded(List),
}

pub struct List(RawSharedVector<Value, ObjHeader>);

impl List {
  pub fn new(raw_vector: RawSharedVector<Value, ObjHeader>) -> Self {
    Self(raw_vector)
  }

  pub fn len(&self) -> usize {
    self.0.len()
  }

  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }

  pub fn cap(&self) -> usize {
    self.0.cap()
  }

  /// Create a usize from the buffer pointer. This is used
  /// when the value is boxed
  #[cfg(feature = "nan_boxing")]
  pub fn to_usize(self) -> usize {
    self.0.to_usize()
  }

  /// Degrade this List into the more generic ObjectRef.
  /// This allows the string to meet the same interface
  /// as the other managed objects
  #[cfg(not(feature = "nan_boxing"))]
  pub fn degrade(self) -> ObjectRef {
    ObjectRef::new(self.0.ptr())
  }

  /// Construct a `Tuple` from `NonNull<u8>`
  ///
  /// ## Safety
  /// This should only be constructed from a box value
  pub unsafe fn from_alloc_ptr(ptr: NonNull<u8>) -> Self {
    List(RawSharedVector::from_alloc_ptr(ptr))
  }

  /// Has this list moved
  pub fn has_moved(&self) -> bool {
    self.0.has_moved()
  }

  /// What is the current state of this list
  pub fn state(&self) -> ListLocation {
    match self.0.state() {
      RawVecLocation::Here(cap) => ListLocation::Here(cap),
      RawVecLocation::Forwarded(raw_vector) => ListLocation::Forwarded(List(raw_vector)),
    }
  }

  /// Pop the element off the list
  pub fn pop(&mut self) -> Option<Value> {
    match self.state() {
      ListLocation::Here(_) => {
        let len: usize = unsafe { self.0.read_len() };
        if len == 0 {
          None
        } else {
          unsafe {
            self.0.write_len(len - 1);
            Some(self.0.read_value(len - 1))
          }
        }
      },
      ListLocation::Forwarded(mut gc_list) => gc_list.pop(),
    }
  }

  /// Remove an element from this list at the provided offset
  pub fn remove(&mut self, index: usize) -> IndexedResult<Value> {
    match self.state() {
      ListLocation::Here(_) => {
        let len = unsafe { self.0.read_len() };
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
      },
      ListLocation::Forwarded(mut gc_list) => gc_list.remove(index),
    }
  }

  /// Push a new element onto this list. If a resize is needed
  /// a new list will be allocated and elements will be transferred
  pub fn push(&mut self, value: Value, hooks: &GcHooks) {
    match self.state() {
      ListLocation::Here(cap) => {
        let len = unsafe { self.0.read_len() };

        // determine if we need to grow the list then
        // persist the value
        let mut list = self.ensure_capacity(len + 1, cap, hooks);

        unsafe {
          list.0.write_value(value, len);
          list.0.write_len(len + 1);
        }
      },
      ListLocation::Forwarded(mut list) => list.push(value, hooks),
    }
  }

  /// Insert an element into this list at the provided offset
  pub fn insert(&mut self, index: usize, value: Value, hooks: &GcHooks) -> IndexedResult {
    match self.state() {
      ListLocation::Here(cap) => {
        let len = unsafe { self.0.read_len() };
        if index > len {
          return IndexedResult::OutOfBounds;
        }

        // determine if we need to grow the list then
        // persist the value
        let mut list = self.ensure_capacity(len + 1, cap, hooks);

        unsafe {
          ptr::copy(
            list.0.item_ptr(index),
            list.0.item_mut(index + 1),
            len - index,
          );
          list.0.write_value(value, index);
          list.0.write_len(len + 1);
        }

        IndexedResult::Ok(())
      },
      ListLocation::Forwarded(mut gc_list) => gc_list.insert(index, value, hooks),
    }
  }

  /// Ensure this list has enough capacity for the operation
  /// If it does it returns itself. Otherwise it returns a new list
  /// which it will have just allocated
  fn ensure_capacity(&mut self, needed: usize, cap: usize, hooks: &GcHooks) -> List {
    if needed > cap {
      self.grow(cap, cap * 2, hooks)
    } else {
      *self
    }
  }

  /// Allocate a new list which the specified capacity. Mark the existing list as moved
  /// by setting the MSB for capacity and replacing len with a pointer to the
  /// new list
  fn grow(&mut self, cap: usize, new_cap: usize, hooks: &GcHooks) -> List {
    let new_list = List::new(hooks.manage_obj(VecBuilder::new(self, new_cap)));

    unsafe {
      self.0.write_len(new_list);
      self.0.mark_moved(cap);
    }
    new_list
  }
}

impl Deref for List {
  type Target = [Value];

  #[inline]
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl DerefMut for List {
  #[inline]
  fn deref_mut(&mut self) -> &mut Self::Target {
    self.0.deref_mut()
  }
}

impl<T> RawSharedVectorHandle<T, ObjHeader> {
  /// Degrade this handle into
  pub fn degrade(self) -> ObjectHandle {
    let handle = ObjectHandle::new(self.value().ptr());
    mem::forget(self);
    handle
  }
}

impl<'a> AllocateObj<RawSharedVector<Value, ObjHeader>> for VecBuilder<'a, Value> {
  fn alloc(self) -> AllocObjResult<RawSharedVector<Value, ObjHeader>> {
    debug_assert!(self.slice().len() <= self.cap());
    let handle =
      RawSharedVectorHandle::from_slice(self.slice(), self.cap(), ObjHeader::new(ObjectKind::List));

    let size = handle.size();
    let reference = handle.value();

    AllocObjResult {
      handle: handle.degrade(),
      size,
      reference,
    }
  }
}

impl Trace for List {
  #[inline]
  fn trace(&self) {
    self.0.trace();
  }

  fn trace_debug(&self, log: &mut dyn std::io::Write) {
    self.0.trace_debug(log);
  }
}

impl DebugHeap for List {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    self.0.fmt_heap(f, depth)
  }
}

impl fmt::Pointer for List {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    Pointer::fmt(&self, f)
  }
}

impl Copy for List {}
impl Clone for List {
  fn clone(&self) -> Self {
    *self
  }
}

impl PartialEq<List> for List {
  #[inline]
  fn eq(&self, other: &List) -> bool {
    self.0.eq(&other.0)
  }
}
impl Eq for List {}

impl Debug for List {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    Debug::fmt(&self, f)
  }
}

impl Display for List {
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
  use crate::{val, NoContext};

  use super::*;

  #[test]
  fn len() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let list = List::new(hooks.manage_obj(VecBuilder::new(&[val!(1.0), val!(2.0), val!(true)], 4)));

    assert_eq!(list.len(), 3);
  }

  #[test]
  fn cap() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let list = List::new(hooks.manage_obj(VecBuilder::new(&[val!(1.0), val!(2.0), val!(true)], 4)));

    assert_eq!(list.cap(), 4);
  }

  #[test]
  fn pop() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut list =
      List::new(hooks.manage_obj(VecBuilder::new(&[val!(1.0), val!(2.0), val!(true)], 4)));

    assert_eq!(list.pop(), Some(val!(true)));
    assert_eq!(list.len(), 2);

    assert_eq!(list.pop(), Some(val!(2.0)));
    assert_eq!(list.len(), 1);

    assert_eq!(list.pop(), Some(val!(1.0)));
    assert_eq!(list.len(), 0);

    assert_eq!(list.pop(), None);
    assert_eq!(list.len(), 0);
  }

  mod insert {
    use super::*;

    #[test]
    fn with_capacity() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let mut list = List::new(hooks.manage_obj(VecBuilder::new(&[val!(1.0)], 4)));

      assert_eq!(list.insert(0, val!(2.0), &hooks), IndexedResult::Ok(()));
      assert_eq!(list.len(), 2);
      assert_eq!(list.cap(), 4);

      assert_eq!(list[0], val!(2.0));
      assert_eq!(list[1], val!(1.0));

      assert_eq!(list.insert(2, val!(3.0), &hooks), IndexedResult::Ok(()));
      assert_eq!(list.len(), 3);
      assert_eq!(list.cap(), 4);

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

      let mut list = List::new(hooks.manage_obj(VecBuilder::new(&[val!(1.0)], 1)));

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
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut list = List::new(hooks.manage_obj(VecBuilder::new(&[val!(1.0), val!(3.0), val!(false)], 3)));
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

    let mut list = List::new(hooks.manage_obj(VecBuilder::new(&[val!(1.0)], 1)));

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
