mod header;

use std::{
  fmt::{self, Debug, Display, Pointer},
  mem,
  ops::{Deref, DerefMut},
  ptr::{self, NonNull}, sync::atomic::AtomicBool,
  hash::{BuildHasherDefault}
};
use fnv::FnvHasher;
use crate::{
  collections::{
    IndexedResult, RawSharedVector, RawSharedVectorHandle, RawVecLocation, VecBuilder,
  },
  managed::{AllocObjResult, AllocateObj, DebugHeap, Trace},
  object::{new_map::header::{FnvBuild, Header}, ObjectKind},
  reference::ObjectHandle,
  value::Value,
  GcHooks,
};

#[cfg(not(feature = "nan_boxing"))]
use crate::ObjectRef;


/// The location of this list
pub enum MapLocation {
  /// The list is in it's default state which returns it's capacity
  Here(usize),

  /// The list has been forwarded to a new location
  Forwarded(Map),
}

pub struct Map(RawSharedVector<(Value, Value), Header>);

impl Map {
  pub fn new(raw_vector: RawSharedVector<(Value, Value), Header>) -> Self {
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
  pub unsafe fn from_alloc_ptr(ptr: NonNull<u8>) -> Self { unsafe {
    Map(RawSharedVector::from_alloc_ptr(ptr))
  }}

  /// Has this list moved
  pub fn has_moved(&self) -> bool {
    self.0.has_moved()
  }

  /// What is the current state of this list
  pub fn state(&self) -> MapLocation {
    match self.0.state() {
      RawVecLocation::Here(cap) => MapLocation::Here(cap),
      RawVecLocation::Forwarded(raw_vector) => MapLocation::Forwarded(Map(raw_vector)),
    }
  }

  // TODO hash methods

  /// Ensure this list has enough capacity for the operation
  /// If it does it returns itself. Otherwise it returns a new list
  /// which it will have just allocated
  fn ensure_capacity(&mut self, needed: usize, cap: usize, hooks: &GcHooks) -> Self {
    if needed > cap {
      self.grow(cap, cap * 2, hooks)
    } else {
      *self
    }
  }

  /// Allocate a new list which the specified capacity. Mark the existing list as moved
  /// by setting the MSB for capacity and replacing len with a pointer to the
  /// new list
  fn grow(&mut self, cap: usize, new_cap: usize, hooks: &GcHooks) -> Self {
    let new_map = Map::new(hooks.manage_obj(VecBuilder::new(self, new_cap)));

    unsafe {
      self.0.write_len(new_map);
      self.0.mark_moved(cap);
    }
    new_map
  }
}

impl Deref for Map {
  type Target = [(Value, Value)];

  #[inline]
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl DerefMut for Map {
  #[inline]
  fn deref_mut(&mut self) -> &mut Self::Target {
    self.0.deref_mut()
  }
}

impl<T> RawSharedVectorHandle<T, Header> {
  /// Degrade this handle into
  pub fn degrade(self) -> ObjectHandle {
    let handle = ObjectHandle::new(self.value().ptr());
    mem::forget(self);
    handle
  }
}

impl<'a> AllocateObj<RawSharedVector<(Value, Value), Header>> for VecBuilder<'a, (Value, Value)> {
  fn alloc(self) -> AllocObjResult<RawSharedVector<(Value, Value), Header>> {
    debug_assert!(self.slice().len() <= self.cap());
    let handle =
      RawSharedVectorHandle::from_slice(self.slice(), self.cap(), Header::new(FnvBuild::default()));

    let size = handle.size();
    let reference = handle.value();

    AllocObjResult {
      handle: handle.degrade(),
      size,
      reference,
    }
  }
}

impl Trace for Map {
  #[inline]
  fn trace(&self) {
    self.0.trace();
  }

  fn trace_debug(&self, log: &mut dyn std::io::Write) {
    self.0.trace_debug(log);
  }
}

impl DebugHeap for Map {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    self.0.fmt_heap(f, depth)
  }
}

impl fmt::Pointer for Map {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    Pointer::fmt(&self, f)
  }
}

impl Copy for Map {}
impl Clone for Map {
  fn clone(&self) -> Self {
    *self
  }
}

impl PartialEq<Map> for Map {
  #[inline]
  fn eq(&self, other: &Map) -> bool {
    self.0.eq(&other.0)
  }
}
impl Eq for Map {}

impl Debug for Map {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    Debug::fmt(&self, f)
  }
}

impl Display for Map {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "[")?;

    if let Some(((last_key, last_value), rest)) = self.split_last() {
      for (key, value) in rest.iter() {
        write!(f, "{key}: {value}, ")?;
      }

      write!(f, "{last_key}: {last_value}")?;
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

    let map = Map::new(hooks.manage_obj(VecBuilder::new(&[(val!(1.0), val!(true))], 4)));

    assert_eq!(map.len(), 1);
  }

  #[test]
  fn cap() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let map = Map::new(hooks.manage_obj(VecBuilder::new(&[(val!(1.0), val!(2.0))], 4)));

    assert_eq!(map.cap(), 4);
  }
}
