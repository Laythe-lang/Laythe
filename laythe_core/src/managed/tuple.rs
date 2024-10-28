use std::{
  fmt::{self, Debug, Display, Pointer},
  ops::{Deref, DerefMut},
  ptr::NonNull,
};

use super::{
  allocate::AllocObjResult,
  gc_array::{GcArray, GcArrayHandle},
  header::ObjHeader,
  AllocateObj, DebugHeap, Trace,
};
use crate::{object::ObjectKind, value::Value};

#[cfg(not(feature = "nan_boxing"))]
use super::gc_obj::GcObject;

pub struct Tuple(GcArray<Value, ObjHeader>);

impl Tuple {
  /// Create a usize from the buffer pointer. This is used
  /// when the value is boxed
  #[cfg(feature = "nan_boxing")]
  pub fn to_usize(self) -> usize {
    self.0.to_usize()
  }

  /// Degrade this Tuple into the more generic GcObject.
  /// This allows the string to meet the same interface
  /// as the other managed objects
  #[cfg(not(feature = "nan_boxing"))]
  pub fn degrade(self) -> GcObject {
    self.0.degrade()
  }

  /// Construct a `Tuple` from `NonNull<u8>`
  ///
  /// ## Safety
  /// This should only be constructed from a box value
  pub unsafe fn from_alloc_ptr(ptr: NonNull<u8>) -> Self {
    Tuple(GcArray::from_alloc_ptr(ptr))
  }
}

impl Deref for Tuple {
  type Target = [Value];

  #[inline]
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl DerefMut for Tuple {
  #[inline]
  fn deref_mut(&mut self) -> &mut Self::Target {
    self.0.deref_mut()
  }
}

impl AllocateObj<Tuple> for &[Value] {
  fn alloc(self) -> AllocObjResult<Tuple> {
    let handle = GcArrayHandle::from_slice(self, ObjHeader::new(ObjectKind::Tuple));

    let size = handle.size();
    let reference = Tuple(handle.value());

    AllocObjResult {
      handle: handle.degrade(),
      size,
      reference,
    }
  }
}

impl Trace for Tuple {
  #[inline]
  fn trace(&self) {
    self.0.trace();
  }

  fn trace_debug(&self, log: &mut dyn std::io::Write) {
    self.0.trace_debug(log);
  }
}

impl DebugHeap for Tuple {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    self.0.fmt_heap(f, depth)
  }
}

impl fmt::Pointer for Tuple {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    Pointer::fmt(&self, f)
  }
}

impl Copy for Tuple {}
impl Clone for Tuple {
  fn clone(&self) -> Self {
    *self
  }
}

impl PartialEq<Tuple> for Tuple {
  #[inline]
  fn eq(&self, other: &Tuple) -> bool {
    self.0.eq(&other.0)
  }
}
impl Eq for Tuple {}

impl Debug for Tuple {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    Debug::fmt(&self, f)
  }
}

impl Display for Tuple {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "(")?;

    if let Some((last, rest)) = self.split_last() {
      for item in rest.iter() {
        write!(f, "{item}, ")?;
      }

      write!(f, "{last}")?;
    }

    write!(f, ")")
  }
}
