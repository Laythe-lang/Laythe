use std::{
  fmt::{self, Debug, Display, Pointer},
  mem,
  ops::{Deref, DerefMut},
  ptr::NonNull,
};

use crate::{
  collections::{IndexedResult, RawVecLocation, RawVector, VecBuilder, VectorHandle},
  managed::{AllocObjResult, AllocateObj, DebugHeap, Trace},
  object::ObjectKind,
  reference::ObjectHandle,
  value::Value,
  GcHooks,
};

#[cfg(not(feature = "nan_boxing"))]
use crate::ObjectRef;

use super::header::Header;

/// The location of this list
pub enum ListLocation {
  /// The list is in it's default state which returns it's capacity
  Here(usize),

  /// The list has been forwarded to a new location
  Forwarded(List),
}

pub struct List(RawVector<Value, Header>);

impl List {
  pub fn new(raw_vector: RawVector<Value, Header>) -> Self {
    Self(raw_vector)
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
    List(RawVector::from_alloc_ptr(ptr))
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
    self.0.pop()
  }

  /// Remove an element from this list at the provided offset
  pub fn remove(&mut self, index: usize) -> IndexedResult<Value> {
    self.0.remove(index)
  }

  /// Push a new element onto this list. If a resize is needed
  /// a new list will be allocated and elements will be transferred
  pub fn push(&mut self, value: Value, hooks: &GcHooks) {
    self.0.push(value, |slice, cap| {
      hooks.manage_obj(VecBuilder::new(slice, cap))
    });
  }

  /// Insert an element into this list at the provided offset
  pub fn insert(&mut self, index: usize, value: Value, hooks: &GcHooks) -> IndexedResult {
    self.0.insert(index, value, |slice, cap| {
      hooks.manage_obj(VecBuilder::new(slice, cap))
    })
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

impl<T> VectorHandle<T, Header> {
  /// Degrade this handle into
  pub fn degrade(self) -> ObjectHandle {
    let handle = ObjectHandle::new(self.value().ptr());
    mem::forget(self);
    handle
  }
}

impl<'a> AllocateObj<RawVector<Value, Header>> for VecBuilder<'a, Value> {
  fn alloc(self) -> AllocObjResult<RawVector<Value, Header>> {
    debug_assert!(self.slice().len() <= self.cap());
    let handle = VectorHandle::from_slice(self.slice(), self.cap(), Header::new(ObjectKind::List));

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
