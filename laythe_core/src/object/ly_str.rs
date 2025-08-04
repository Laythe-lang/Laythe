use crate::{
  collections::{Array, ArrayHandle}, managed::{AllocObjResult, AllocateObj, DebugHeap, Mark, Marked, Trace, Unmark}, object::ObjectKind, reference::ObjectHandle
};
use std::{
  cmp::Ordering,
  ffi::OsStr,
  fmt::{self, Pointer},
  hash::{Hash, Hasher},
  io::Write,
  ops::Deref,
  ptr::{self, NonNull},
  str,
};

#[cfg(not(feature = "nan_boxing"))]
use crate::ObjectRef;
use super::header::Header;

/// A non owning reference to a Garbage collector
/// allocated string. Note this string is the same size
/// as a single pointer.
pub struct LyStr(Array<u8, Header>);

impl LyStr {
  /// Create a usize from the buffer pointer. This is used
  /// when the value is boxed
  #[cfg(feature = "nan_boxing")]
  pub fn to_usize(self) -> usize {
    self.0.to_usize()
  }

  /// Degrade this LyStr into the more generic ObjRefect.
  /// This allows the string to meet the same interface
  /// as the other managed objects
  #[cfg(not(feature = "nan_boxing"))]
  pub fn degrade(self) -> ObjectRef {
    self.0.degrade()
  }

  /// Get a static reference to the underlying data str slice.
  ///
  /// ## Safety
  /// This method only make sense when created from a GcHandle
  /// itself created by the garbage collector. The reference
  /// should truly be of 'a for the lifetime of the allocator.
  /// This will need to be refactored later
  pub unsafe fn deref_static(&self) -> &'static str { unsafe {
    str::from_utf8_unchecked(self.0.deref_static())
  }}

  /// Create a LyStr from a `NonNull<u8>`.
  ///
  /// ## Safety
  /// This functions assumes that the pointer was originally
  /// from a different instance of LyStr. Other pointer
  /// will likely crash immediately
  pub unsafe fn from_alloc_ptr(ptr: NonNull<u8>) -> Self { unsafe {
    LyStr(Array::from_alloc_ptr(ptr))
  }}
}

impl Mark for LyStr {
  #[inline]
  fn mark(&self) -> bool {
    self.0.mark()
  }
}

impl Marked for LyStr {
  #[inline]
  fn marked(&self) -> bool {
    self.0.marked()
  }
}

impl Trace for LyStr {
  #[inline]
  fn trace(&self) {
    self.mark();
  }

  fn trace_debug(&self, log: &mut dyn Write) {
    if self.mark() {
      return;
    }

    log
      .write_fmt(format_args!("{:p} mark {}\n", self.0.as_alloc_ptr(), self))
      .expect("unable to write to stdout");
  }
}

impl DebugHeap for LyStr {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> std::fmt::Result {
    if depth == 0 {
      f.write_fmt(format_args!("{:p}", self.0.ptr()))
    } else {
      f.write_fmt(format_args!("'{self}'"))
    }
  }
}

impl fmt::Debug for LyStr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "'{}'", self.deref())
  }
}

impl fmt::Display for LyStr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(self)
  }
}

impl fmt::Pointer for LyStr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    Pointer::fmt(&self.0, f)
  }
}

impl PartialEq<LyStr> for LyStr {
  #[inline]
  fn eq(&self, other: &LyStr) -> bool {
    ptr::eq(self.0.as_alloc_ptr(), other.0.as_alloc_ptr())
  }
}

impl PartialEq<str> for LyStr {
  #[inline]
  fn eq(&self, other: &str) -> bool {
    &**self == other
  }
}

impl PartialEq<&str> for LyStr {
  #[inline]
  fn eq(&self, other: &&str) -> bool {
    &**self == *other
  }
}

impl Eq for LyStr {}

impl PartialOrd for LyStr {
  #[inline]
  fn partial_cmp(&self, other: &LyStr) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl Ord for LyStr {
  #[inline]
  fn cmp(&self, other: &LyStr) -> Ordering {
    self.0.as_alloc_ptr().cmp(&other.0.as_alloc_ptr())
  }
}

impl Hash for LyStr {
  #[inline]
  fn hash<H: Hasher>(&self, state: &mut H) {
    ptr::hash(self.0.as_alloc_ptr(), state)
  }
}

impl Deref for LyStr {
  type Target = str;

  #[inline]
  fn deref(&self) -> &Self::Target {
    unsafe { str::from_utf8_unchecked(&self.0) }
  }
}

impl Copy for LyStr {}
impl Clone for LyStr {
  fn clone(&self) -> Self {
    *self
  }
}

impl AsRef<str> for LyStr {
  fn as_ref(&self) -> &str {
    self
  }
}

impl AsRef<OsStr> for LyStr {
  fn as_ref(&self) -> &OsStr {
    let str: &str = self;
    OsStr::new(str)
  }
}

/// A owning reference to a Garbage collector
/// allocated string. Note this string is the same size
/// as a single pointer.
pub struct LyStrHandle(ArrayHandle<u8, Header>);

impl LyStrHandle {
  /// Create a non owning reference to this string.
  #[inline]
  pub fn value(&self) -> LyStr {
    LyStr(self.0.value())
  }

  /// Degrade this LyStrHandle into the more generic ObjectHandle.
  /// This allows the string to meet the same interface
  /// as the other managed objects
  pub fn degrade(self) -> ObjectHandle {
    self.0.degrade()
  }

  /// Determine the size of the handle and the pointed to
  /// allocation
  #[inline]
  pub fn size(&self) -> usize {
    self.0.size()
  }
}

impl<T: AsRef<str>> From<T> for LyStrHandle {
  fn from(string: T) -> Self {
    LyStrHandle(ArrayHandle::from_slice(
      string.as_ref().as_bytes(),
      Header::new(ObjectKind::String),
    ))
  }
}

impl Unmark for LyStrHandle {
  #[inline]
  fn unmark(&self) -> bool {
    self.0.unmark()
  }
}

impl Marked for LyStrHandle {
  #[inline]
  fn marked(&self) -> bool {
    self.0.marked()
  }
}

impl DebugHeap for LyStrHandle {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> std::fmt::Result {
    self.value().fmt_heap(f, depth)
  }
}

impl fmt::Pointer for LyStrHandle {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.value().0.ptr().fmt(f)
  }
}

impl<T: AsRef<str>> AllocateObj<LyStr> for T {
  fn alloc(self) -> AllocObjResult<LyStr> {
    let handle = LyStrHandle::from(self);
    let size = handle.size();

    let reference = handle.value();

    AllocObjResult {
      handle: handle.degrade(),
      size,
      reference,
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod value {
    use super::*;

    #[test]
    fn deref_static() {
      let handle = LyStrHandle::from("example");
      let value = handle.value();

      unsafe {
        assert_eq!(value.deref_static(), "example");
      }
    }
  }

  mod handle {
    use super::*;

    #[test]
    fn from() {
      let handle = LyStrHandle::from("example");
      let value = handle.value();

      assert_eq!(&*value, "example");
    }
  }
}
