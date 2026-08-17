mod bucket;
mod header;
mod map_builder;

pub use bucket::Bucket;
pub use header::MapHeader;
pub use map_builder::MapBuilder;

use crate::{
  GcHooks,
  collections::{RawSharedVector, RawSharedVectorHandle, RawVecLocation},
  managed::{AllocObjResult, AllocateObj, DebugHeap, Trace},
  object::new_map::header::FnvBuild,
  reference::ObjectHandle,
  value::Value,
};
use std::{
  fmt::{self, Debug, Display, Pointer},
  mem,
  ops::{Deref, DerefMut},
  ptr::NonNull,
};

#[cfg(not(feature = "nan_boxing"))]
use crate::ObjectRef;

/// The location of this list
pub enum MapLocation {
  /// The list is in it's default state which returns it's capacity
  Here(usize),

  /// The list has been forwarded to a new location
  Forwarded(NewMap),
}

pub struct NewMap(RawSharedVector<Bucket, MapHeader>);

impl NewMap {
  pub fn new(raw_vector: RawSharedVector<Bucket, MapHeader>) -> Self {
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
    unsafe { NewMap(RawSharedVector::from_alloc_ptr(ptr)) }
  }

  /// Has this list moved
  pub fn has_moved(&self) -> bool {
    self.0.has_moved()
  }

  /// What is the current state of this list
  pub fn state(&self) -> MapLocation {
    match self.0.state() {
      RawVecLocation::Here(cap) => MapLocation::Here(cap),
      RawVecLocation::Forwarded(raw_vector) => MapLocation::Forwarded(NewMap(raw_vector)),
    }
  }

  // TODO hash methods
  /// Public insert: returns the old value if the key existed.
  pub fn insert(&mut self, key: Value, value: Value) -> Option<Value> {
    // determine if we need to grow the list then
    // persist the value
    let mut map = self.ensure_capacity(len + 1, cap, hooks);

    let hash = self.make_hash(&key);
    self.insert_hashed(hash, key, value)
  }

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

  fn needs_resize(&self) -> bool {
    let used = self.len + self.tombstones;
    used * 10 > self.capacity() * 7
  }

  /// Allocate a new list which the specified capacity. Mark the existing list as moved
  /// by setting the MSB for capacity and replacing len with a pointer to the
  /// new list
  fn grow(&mut self, cap: usize, new_cap: usize, hooks: &GcHooks) -> Self {
    let new_map = NewMap::new(hooks.manage_obj(MapBuilder::new(new_cap)));

    unsafe {
      self.0.write_len(new_map);
      self.0.mark_moved(cap);
    }
    new_map
  }
}

impl Deref for NewMap {
  type Target = [Bucket];

  #[inline]
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl DerefMut for NewMap {
  #[inline]
  fn deref_mut(&mut self) -> &mut Self::Target {
    self.0.deref_mut()
  }
}

impl<T> RawSharedVectorHandle<T, MapHeader> {
  /// Degrade this handle into
  pub fn degrade(self) -> ObjectHandle {
    let handle = ObjectHandle::new(self.value().ptr());
    mem::forget(self);
    handle
  }
}

impl AllocateObj<RawSharedVector<Bucket, MapHeader>> for MapBuilder {
  fn alloc(self) -> AllocObjResult<RawSharedVector<Bucket, MapHeader>> {
    let handle = RawSharedVectorHandle::with_cap(self.cap(), MapHeader::new(FnvBuild::default()));

    let size = handle.size();
    let reference = handle.value();

    AllocObjResult {
      handle: handle.degrade(),
      size,
      reference,
    }
  }
}

impl Trace for NewMap {
  #[inline]
  fn trace(&self) {
    self.0.trace();
  }

  fn trace_debug(&self, log: &mut dyn std::io::Write) {
    self.0.trace_debug(log);
  }
}

impl DebugHeap for NewMap {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    self.0.fmt_heap(f, depth)
  }
}

impl fmt::Pointer for NewMap {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    Pointer::fmt(&self, f)
  }
}

impl Copy for NewMap {}
impl Clone for NewMap {
  fn clone(&self) -> Self {
    *self
  }
}

impl PartialEq<NewMap> for NewMap {
  #[inline]
  fn eq(&self, other: &NewMap) -> bool {
    self.0.eq(&other.0)
  }
}
impl Eq for NewMap {}

impl Debug for NewMap {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    Debug::fmt(&self, f)
  }
}

impl Display for NewMap {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "[")?;

    if let Some((last_bucket, rest)) = self.split_last() {
      for bucket in rest.iter() {
        write!(f, "{}: {}, ", bucket.key(), bucket.value())?;
      }

      write!(f, "{}: {}", last_bucket.key(), last_bucket.value())?;
    }

    write!(f, "]")
  }
}

#[cfg(test)]
mod test {
  use crate::{NoContext, val};

  use super::*;

  #[test]
  fn len() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);
    let hasher = FnvBuild::default();

    let map = NewMap::new(hooks.manage_obj(MapBuilder::new(
      &[Bucket::build(val!(1.0), val!(false), hasher)],
      4,
    )));

    assert_eq!(map.len(), 1);
  }

  #[test]
  fn cap() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);
    let hasher = FnvBuild::default();

    let map = NewMap::new(hooks.manage_obj(MapBuilder::new(
      &[Bucket::build(val!(1.0), val!(false), hasher)],
      4,
    )));

    assert_eq!(map.cap(), 4);
  }
}
