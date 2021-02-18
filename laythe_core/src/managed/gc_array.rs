extern crate alloc;

use crate::managed::{Mark, Marked, Trace, Unmark};
use ptr::NonNull;
use std::{
  fmt::Debug,
  marker::PhantomData,
  mem,
  ops::{Deref, DerefMut},
  ptr, slice,
};

use super::utils::{get_array_len_offset, get_array_offset, make_array_layout};

/// A non owning reference to a Garbage collector
/// allocated array. Note this array is the same size
/// as a single pointer.
///
/// ## Example
/// ```
/// use laythe_core::managed::{GcArray, GcArrayHandle};
/// use std::mem;
///
/// let data: &[u32] = &[1, 2, 3, 4];
/// let handle = GcArrayHandle::<u32, u64>::from(data);
/// let array = handle.value();
///
/// assert_eq!(mem::size_of::<GcArray<u32, u64>>(), mem::size_of::<&u32>());
/// assert_eq!(data[0], array[0]);
/// assert_eq!(data.len(), array.len());
/// ```
pub struct GcArray<T, H> {
  /// Pointer to the header of the array
  pub(super) ptr: NonNull<u8>,

  /// Phantom data to hold the array data type
  data: PhantomData<T>,

  /// Phantom data to hold the array header type
  header: PhantomData<H>,
}

impl<T, H> GcArray<T, H> {
  /// Retrieve the header from this array
  #[inline]
  pub fn header(&self) -> &H {
    #[allow(clippy::cast_ptr_alignment)]
    unsafe {
      &*(self.ptr.as_ptr() as *const H)
    }
  }

  /// Retrieve the header from this array
  #[inline]
  pub fn len(&self) -> usize {
    #[allow(clippy::cast_ptr_alignment)]
    let count = get_array_len_offset::<H>();
    unsafe { *(self.ptr.as_ptr().add(count) as *mut usize) }
  }

  /// Retrieve a pointer data array
  #[inline]
  fn data(&self) -> *mut T {
    let count = get_array_offset::<H, T>();
    unsafe { self.ptr.as_ptr().add(count) as *mut T }
  }

  /// Get a raw pointer to allocation
  #[inline]
  pub fn as_alloc_ptr(&self) -> *const u8 {
    self.ptr.as_ptr()
  }

  /// Construct a `GcArray<T>` from `NonNull<u8>`
  ///
  /// ## Safety
  /// This should only be constructed from a box value
  pub unsafe fn from_alloc_ptr(ptr: NonNull<u8>) -> Self {
    Self {
      ptr,
      data: PhantomData,
      header: PhantomData,
    }
  }
}

impl<T, H: Mark> Mark for GcArray<T, H> {
  /// Mark the array itself as visited
  #[inline]
  fn mark(&self) -> bool {
    self.header().mark()
  }
}

impl<T, H: Marked> Marked for GcArray<T, H> {
  /// Is this array marked
  #[inline]
  fn marked(&self) -> bool {
    self.header().marked()
  }
}

impl<T: Trace, H: Send + Mark> Trace for GcArray<T, H> {
  fn trace(&self) {
    if self.mark() {
      return;
    }

    self.iter().for_each(|i| i.trace());
  }

  fn trace_debug(&self, log: &mut dyn std::io::Write) {
    if self.mark() {
      return;
    }

    self.iter().for_each(|i| i.trace_debug(log));
  }
}

impl<T, H> Copy for GcArray<T, H> {}
impl<T, H> Clone for GcArray<T, H> {
  fn clone(&self) -> Self {
    *self
  }
}

impl<T, H> Deref for GcArray<T, H> {
  type Target = [T];

  fn deref(&self) -> &Self::Target {
    let data = self.data();
    let len = self.len();
    unsafe { slice::from_raw_parts(data, len) }
  }
}

impl<T, H> DerefMut for GcArray<T, H> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    let data = self.data();
    let len = self.len();
    unsafe { slice::from_raw_parts_mut(data, len) }
  }
}

impl<T, H> PartialEq<GcArray<T, H>> for GcArray<T, H> {
  #[inline]
  fn eq(&self, other: &GcArray<T, H>) -> bool {
    ptr::eq(self.as_alloc_ptr(), other.as_alloc_ptr())
  }
}

impl<T: Debug, H> Debug for GcArray<T, H> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_list().entry(self).finish()
  }
}

unsafe impl<T: Send, H: Send> Send for GcArray<T, H> {}
unsafe impl<T: Sync, H: Sync> Sync for GcArray<T, H> {}

/// A owning reference to a Garbage collector
/// allocated array. Note this array is the same size
/// as a single pointer.
///
/// ## Example
/// ```
/// use laythe_core::managed::GcArrayHandle;
/// use std::mem;
///
/// let data: &[u32] = &[1, 2, 3, 4];
/// let handle = GcArrayHandle::<u32, u32>::from(data);
///
/// assert_eq!(mem::size_of::<GcArrayHandle<u32, u32>>(), mem::size_of::<&u32>());
/// ```
pub struct GcArrayHandle<T, H>(GcArray<T, H>);

impl<T, H> GcArrayHandle<T, H> {
  /// Create a non owning reference to this array.
  ///
  /// ## Examples
  /// ```
  /// use laythe_core::managed::GcArrayHandle;
  /// use std::mem;
  ///
  /// let data: &[u32] = &[1, 2, 3, 4];
  /// let handle = GcArrayHandle::<u32, u32>::from(data);
  ///
  /// let array1 = handle.value();
  /// let array2 = handle.value();
  ///
  /// assert_eq!(array1, array2);
  /// assert_eq!(handle[0], array1[0]);
  /// ```
  pub fn value(&self) -> GcArray<T, H> {
    self.0
  }
}

impl<T: Copy, H> GcArrayHandle<T, H> {
  /// Create a new `GcArrayHandle` from the provided header
  /// and a copyable slice
  pub fn from_slice(slice: &[T], header: H) -> Self {
    assert!(mem::size_of::<T>() > 0, "ZSTs currently not supported");

    let len = slice.len();
    let new_layout = make_array_layout::<H, T>(len);
    let buf = unsafe { alloc::alloc::alloc(new_layout) };

    if buf.is_null() {
      alloc::alloc::handle_alloc_error(new_layout);
    }

    #[allow(clippy::cast_ptr_alignment)]
    let mut array = unsafe {
      ptr::write(buf as *mut H, header);
      ptr::write(buf.add(get_array_len_offset::<H>()) as *mut usize, len);

      GcArrayHandle(GcArray {
        ptr: NonNull::new_unchecked(buf),
        data: PhantomData,
        header: PhantomData,
      })
    };

    array.0.copy_from_slice(slice);
    array
  }
}

impl<T, H: Unmark> Unmark for GcArrayHandle<T, H> {
  /// Unmark this allocation as visited, returning
  /// the existing marked status
  #[inline]
  fn unmark(&self) -> bool {
    self.0.header().unmark()
  }
}

impl<T, H: Marked> Marked for GcArrayHandle<T, H> {
  /// Is this allocation marked
  #[inline]
  fn marked(&self) -> bool {
    self.0.marked()
  }
}

impl<T, H> Deref for GcArrayHandle<T, H> {
  type Target = [T];

  fn deref(&self) -> &Self::Target {
    self.0.deref()
  }
}

impl<T, H> DerefMut for GcArrayHandle<T, H> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    self.0.deref_mut()
  }
}

impl<T: Copy, H: Default> From<&[T]> for GcArrayHandle<T, H> {
  fn from(slice: &[T]) -> Self {
    GcArrayHandle::from_slice(slice, H::default())
  }
}

impl<T, H> Drop for GcArrayHandle<T, H> {
  fn drop(&mut self) {
    unsafe {
      #[allow(clippy::cast_ptr_alignment)]
      ptr::read(self.0.ptr.as_ptr() as *const H);
      let len = self.0.len();

      for i in 0..len {
        ptr::read(self.0.data().add(i));
      }

      alloc::alloc::dealloc(self.0.ptr.as_ptr(), make_array_layout::<H, T>(len));
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod array {
    use super::*;

    #[test]
    fn header() {
      let handle = GcArrayHandle::from_slice(&[1, 2, 3, 4, 5], String::from("header"));
      let array = handle.value();

      assert_eq!(array.header(), "header");
    }

    #[test]
    fn len() {
      let handle = GcArrayHandle::from_slice(&[1, 2, 3, 4, 5], String::from("header"));
      let array = handle.value();

      assert_eq!(array.len(), 5);
    }
  }

  mod handle {
    use super::*;

    #[test]
    fn from_slice() {
      let handle = GcArrayHandle::from_slice(&[1, 2, 3, 4, 5], String::from("header"));

      assert_eq!(handle.len(), 5);
      assert_eq!(handle[0], 1);
      assert_eq!(handle[1], 2);
      assert_eq!(handle[2], 3);
      assert_eq!(handle[3], 4);
      assert_eq!(handle[4], 5);
    }

    #[test]
    fn from_value() {
      let handle = GcArrayHandle::from_slice(&[1, 2, 3, 4, 5], String::from("header"));
      let array = handle.value();

      assert_eq!(array.len(), 5);
      assert_eq!(array[0], 1);
      assert_eq!(array[1], 2);
      assert_eq!(array[2], 3);
      assert_eq!(array[3], 4);
      assert_eq!(array[4], 5);

      drop(array);

      assert_eq!(handle.len(), 5);
    }
  }
}
