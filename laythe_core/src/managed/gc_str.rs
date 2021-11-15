use crate::{
  managed::{
    gc_array::{GcArray, GcArrayHandle},
    DebugHeap, Manage, Mark, Trace,
  },
  object::ObjectKind,
};
use std::{
  cmp::Ordering,
  fmt,
  hash::{Hash, Hasher},
  io::Write,
  mem,
  ops::Deref,
  ptr::{self, NonNull},
  slice, str,
};

use super::{
  gc_obj::{GcObject, ObjHeader},
  utils::make_array_layout,
  Marked, Unmark,
};

/// A non owning reference to a Garbage collector
/// allocated string. Note this string is the same size
/// as a single pointer.
///
/// ## Example
/// ```
/// use laythe_core::managed::{GcStr, GcStrHandle};
/// use std::mem;
///
/// let str = "my string";
/// let handle = GcStrHandle::from(str);
/// let string = handle.value();
///
/// assert_eq!(mem::size_of::<GcStr>(), mem::size_of::<usize>());
/// assert_eq!(string.len(), str.len());
/// assert_eq!(string, str);
/// ```
pub struct GcStr(GcArray<u8, ObjHeader>);

impl GcStr {
  /// Create a usize from the buffer pointer. This is used
  /// when the value is boxed
  ///
  /// ## Example
  /// ```
  /// use laythe_core::managed::{GcStr, GcStrHandle};
  ///
  /// let handle = GcStrHandle::from("some string");
  /// let value = handle.value();
  /// assert!(value.to_usize() > 0);
  /// ```
  pub fn to_usize(self) -> usize {
    self.0.as_alloc_ptr() as *const () as usize
  }

  pub fn degrade(self) -> GcObject {
    GcObject::new(self.0.ptr)
  }

  /// Get a static reference to the underlying data str slice.
  ///
  /// ## Safety
  /// This method only make sense when created from a GcHandle
  /// itself created by the garbage collector. The reference
  /// should truly be of 'a for the lifetime of the allocator.
  /// This will need to be refactored later
  ///
  /// ## Example
  /// ```
  /// use laythe_core::managed::{GcStr, GcStrHandle};
  ///
  /// let handle = GcStrHandle::from("some string");
  /// let value = handle.value();
  ///
  /// let split = unsafe { value.deref_static() }.split(" ");
  /// ```
  pub unsafe fn deref_static(&self) -> &'static str {
    str::from_utf8_unchecked(slice::from_raw_parts(self.as_ptr(), self.len()))
  }

  /// Create a GcStr from a `NonNull<u8>`.
  ///
  /// ## Safety
  /// This functions assumes that the pointer was originally
  /// from a different instance of GcStr. Other pointer
  /// will likely crash immediately
  pub unsafe fn from_alloc_ptr(ptr: NonNull<u8>) -> Self {
    GcStr(GcArray::from_alloc_ptr(ptr))
  }
}

impl Mark for GcStr {
  #[inline]
  fn mark(&self) -> bool {
    self.0.mark()
  }
}

impl Marked for GcStr {
  #[inline]
  fn marked(&self) -> bool {
    self.0.marked()
  }
}

impl Trace for GcStr {
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

impl DebugHeap for GcStr {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> std::fmt::Result {
    if depth == 0 {
      f.write_str("*")
    } else {
      f.write_fmt(format_args!("{}", self))
    }
  }
}

impl Manage for GcStr {
  fn size(&self) -> usize {
    mem::size_of::<Self>() + make_array_layout::<ObjHeader, u8>(self.len()).size()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

impl fmt::Debug for GcStr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "'{}'", self.deref())
  }
}

impl fmt::Display for GcStr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(self)
  }
}

impl fmt::Pointer for GcStr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.0.as_alloc_ptr().fmt(f)
  }
}

impl PartialEq<GcStr> for GcStr {
  #[inline]
  fn eq(&self, other: &GcStr) -> bool {
    ptr::eq(self.0.as_alloc_ptr(), other.0.as_alloc_ptr())
  }
}

impl PartialEq<str> for GcStr {
  #[inline]
  fn eq(&self, other: &str) -> bool {
    &**self == other
  }
}

impl PartialEq<&str> for GcStr {
  #[inline]
  fn eq(&self, other: &&str) -> bool {
    &**self == *other
  }
}

impl Eq for GcStr {}

impl PartialOrd for GcStr {
  #[inline]
  fn partial_cmp(&self, other: &GcStr) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl Ord for GcStr {
  #[inline]
  fn cmp(&self, other: &GcStr) -> Ordering {
    self.0.as_alloc_ptr().cmp(&other.0.as_alloc_ptr())
  }
}

impl Hash for GcStr {
  #[inline]
  fn hash<H: Hasher>(&self, state: &mut H) {
    ptr::hash(self.0.as_alloc_ptr(), state)
  }
}

impl Deref for GcStr {
  type Target = str;

  fn deref(&self) -> &Self::Target {
    unsafe { str::from_utf8_unchecked(&self.0) }
  }
}

impl Copy for GcStr {}
impl Clone for GcStr {
  fn clone(&self) -> Self {
    *self
  }
}

impl AsRef<str> for GcStr {
  fn as_ref(&self) -> &str {
    self
  }
}

/// A owning reference to a Garbage collector
/// allocated string. Note this string is the same size
/// as a single pointer.
///
/// ## Example
/// ```
/// use laythe_core::managed::GcStrHandle;
/// use std::mem;
///
/// let data = &"example";
/// let handle = GcStrHandle::from(data);
///
/// assert_eq!(mem::size_of::<GcStrHandle>(), mem::size_of::<usize>());
/// ```
pub struct GcStrHandle(GcArrayHandle<u8, ObjHeader>);

impl GcStrHandle {
  /// Create a non owning reference to this string.
  ///
  /// ## Examples
  /// ```
  /// use laythe_core::managed::GcStrHandle;
  /// use std::mem;
  ///
  /// let data = &"example";
  /// let handle = GcStrHandle::from(data);
  ///
  /// let str1 = handle.value();
  /// let str2 = handle.value();
  ///
  /// assert_eq!(str1, str2);
  /// assert_eq!(str1[..], str2[..]);
  /// ```
  #[inline]
  pub fn value(&self) -> GcStr {
    GcStr(self.0.value())
  }

  /// Determine the size of the handle and the pointed to
  /// allocation
  ///
  /// ## Examples
  /// ```
  /// use laythe_core::managed::GcStrHandle;
  /// use std::mem;
  ///
  /// let data = &"example";
  /// let handle = GcStrHandle::from(data);
  ///
  /// assert_eq!(handle.size(), 31);
  /// ```
  #[inline]
  pub fn size(&self) -> usize {
    mem::size_of::<Self>() + make_array_layout::<ObjHeader, u8>(self.0.len()).size()
  }
}

impl<T: AsRef<str>> From<T> for GcStrHandle {
  fn from(string: T) -> Self {
    GcStrHandle(GcArrayHandle::from_slice(
      string.as_ref().as_bytes(),
      ObjHeader::new(ObjectKind::String),
    ))
  }
}

impl Unmark for GcStrHandle {
  #[inline]
  fn unmark(&self) -> bool {
    self.0.unmark()
  }
}

impl Marked for GcStrHandle {
  #[inline]
  fn marked(&self) -> bool {
    self.0.marked()
  }
}

impl DebugHeap for GcStrHandle {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> std::fmt::Result {
    self.value().fmt_heap(f, depth)
  }
}

impl fmt::Pointer for GcStrHandle {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.value().0.ptr.fmt(f)
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod value {
    use super::*;

    #[test]
    fn deref_static() {
      let handle = GcStrHandle::from("example");
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
      let handle = GcStrHandle::from("example");
      let value = handle.value();

      assert_eq!(&*value, "example");
    }
  }
}
