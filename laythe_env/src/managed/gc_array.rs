extern crate alloc;

use crate::managed::{Mark, Marked, Trace, Unmark};
use alloc::alloc::Layout;
use ptr::NonNull;
use std::{
  cmp,
  fmt::Debug,
  marker::PhantomData,
  mem,
  ops::{Deref, DerefMut},
  ptr, slice,
  sync::atomic::{AtomicBool, Ordering},
};

/// For a given offset determine the total offset until the next alignment
pub const fn next_aligned(num_bytes: usize, alignment: usize) -> usize {
  let remaining = num_bytes % alignment;
  if remaining == 0 {
    num_bytes
  } else {
    num_bytes + (alignment - remaining)
  }
}

/// Get the offset to the start of the `GcArray<T>` data
const fn get_offset<T>() -> usize {
  next_aligned(mem::size_of::<Header>(), mem::align_of::<T>())
}

/// Determine the max alignment between the item `T`
/// and the `GcArray` header `Header`
pub fn max_align<T>() -> usize {
  let align_t = mem::align_of::<T>();
  let header_align = mem::align_of::<Header>();
  cmp::max(align_t, header_align)
}

/// Create a rust `Layout` for a `GcArray` of `len` length.
pub fn make_layout<T>(len: usize) -> Layout {
  let alignment = max_align::<T>();

  let header_size = mem::size_of::<Header>();
  let num_bytes = if len == 0 {
    header_size
  } else {
    next_aligned(header_size, mem::align_of::<T>()) + len * mem::size_of::<T>()
  };
  Layout::from_size_align(num_bytes, alignment).unwrap()
}

/// A non owning reference to a Garbage collector
/// allocated array. Note this array is the same size
/// as a single pointer.
///
/// ## Example
/// ```
/// use laythe_env::managed::{GcArray, GcArrayHandle};
/// use std::mem;
///
/// let data: &[u32] = &[1, 2, 3, 4];
/// let handle = GcArrayHandle::from(data);
/// let array = handle.value();
///
/// assert_eq!(mem::size_of::<GcArray<u32>>(), mem::size_of::<&u32>());
/// assert_eq!(data[0], array[0]);
/// assert_eq!(data.len(), array.len());
/// ```
pub struct GcArray<T> {
  /// Pointer to the header of the array
  buf: NonNull<u8>,

  /// Phantom data to hold the type parameter
  phantom: PhantomData<T>,
}

/// The `Header` meta data for `GcArray<T>`. This struct
/// is positioned at the front of the array such that the layout looks like
/// this
/// ```markdown
/// [Header (potential padding)| T | T | T | ...]
/// ```
struct Header {
  // Has this array allocation been marked by the garbage collector
  marked: AtomicBool,

  // What is the length of this array
  len: usize,
}

impl<T> GcArray<T> {
  /// Retrieve the header from this array
  #[inline]
  fn header(&self) -> &Header {
    #[allow(clippy::cast_ptr_alignment)]
    unsafe {
      &*(self.buf.as_ptr() as *const Header)
    }
  }

  /// Retrieve a pointer data array
  #[inline]
  fn data(&self) -> *mut T {
    let count = get_offset::<T>();
    unsafe { self.buf.as_ptr().add(count) as *mut T }
  }

  /// Get a raw pointer to allocation
  #[inline]
  pub fn as_alloc_ptr(&self) -> *const u8 {
    self.buf.as_ptr()
  }

  /// Construct a `GcArray<T>` from `NonNull<u8>`
  ///
  /// ## Safety
  /// This should only be constructed from a box value
  pub unsafe fn from_alloc_ptr(buf: NonNull<u8>) -> Self {
    Self {
      buf,
      phantom: PhantomData,
    }
  }
}

impl<T> Mark for GcArray<T> {
  #[inline]
  fn mark(&self) -> bool {
    self.header().marked.swap(true, Ordering::Release)
  }
}

impl<T> Marked for GcArray<T> {
  #[inline]
  fn marked(&self) -> bool {
    self.header().marked.load(Ordering::Acquire)
  }
}

impl<T: Trace> Trace for GcArray<T> {
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

impl<T> Copy for GcArray<T> {}
impl<T> Clone for GcArray<T> {
  fn clone(&self) -> Self {
    *self
  }
}

impl<T> Deref for GcArray<T> {
  type Target = [T];

  fn deref(&self) -> &Self::Target {
    let header = self.header();
    let data = self.data();
    let len = header.len;
    unsafe { slice::from_raw_parts(data, len) }
  }
}

impl<T> DerefMut for GcArray<T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    let header = self.header();
    let data = self.data();
    let len = header.len;
    unsafe { slice::from_raw_parts_mut(data, len) }
  }
}

impl<T> PartialEq<GcArray<T>> for GcArray<T> {
  #[inline]
  fn eq(&self, other: &GcArray<T>) -> bool {
    ptr::eq(self.as_alloc_ptr(), other.as_alloc_ptr())
  }
}

impl<T: Debug> Debug for GcArray<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_list().entry(self).finish()
  }
}

unsafe impl<T: Send> Send for GcArray<T> {}
unsafe impl<T: Sync> Sync for GcArray<T> {}

/// A owning reference to a Garbage collector
/// allocated array. Note this array is the same size
/// as a single pointer.
///
/// ## Example
/// ```
/// use laythe_env::managed::GcArrayHandle;
/// use std::mem;
///
/// let data: &[u32] = &[1, 2, 3, 4];
/// let handle = GcArrayHandle::from(data);
///
/// assert_eq!(mem::size_of::<GcArrayHandle<u32>>(), mem::size_of::<&u32>());
/// ```
pub struct GcArrayHandle<T>(GcArray<T>);

impl<T> GcArrayHandle<T> {
  /// Create a non owning reference to this array.
  ///
  /// ## Examples
  /// ```
  /// use laythe_env::managed::GcArrayHandle;
  /// use std::mem;
  ///
  /// let data: &[u32] = &[1, 2, 3, 4];
  /// let handle = GcArrayHandle::from(data);
  ///
  /// let array1 = handle.value();
  /// let array2 = handle.value();
  ///
  /// assert_eq!(array1, array2);
  /// assert_eq!(handle[0], array1[0]);
  /// ```
  pub fn value(&self) -> GcArray<T> {
    self.0
  }
}

impl<T> Unmark for GcArrayHandle<T> {
  /// Unmark this allocation as visited, returning
  /// the existing marked status
  #[inline]
  fn unmark(&self) -> bool {
    self.0.header().marked.swap(false, Ordering::Release)
  }
}

impl<T> Marked for GcArrayHandle<T> {
  /// Is this allocation marked
  #[inline]
  fn marked(&self) -> bool {
    self.0.marked()
  }
}

impl<T> Deref for GcArrayHandle<T> {
  type Target = [T];

  fn deref(&self) -> &Self::Target {
    self.0.deref()
  }
}

impl<T> DerefMut for GcArrayHandle<T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    self.0.deref_mut()
  }
}

impl<T: Copy> From<&[T]> for GcArrayHandle<T> {
  fn from(slice: &[T]) -> Self {
    assert!(mem::size_of::<T>() > 0, "ZSTs currently not supported");

    let len = slice.len();
    let new_layout = make_layout::<T>(len);
    let buf = unsafe { alloc::alloc::alloc(new_layout) };

    if buf.is_null() {
      alloc::alloc::handle_alloc_error(new_layout);
    }

    let header = Header {
      marked: AtomicBool::new(false),
      len,
    };

    #[allow(clippy::cast_ptr_alignment)]
    let mut array = unsafe {
      ptr::write(buf as *mut Header, header);

      GcArrayHandle(GcArray {
        buf: NonNull::new_unchecked(buf),
        phantom: PhantomData,
      })
    };

    array.0.copy_from_slice(slice);
    array
  }
}

impl<T> Drop for GcArrayHandle<T> {
  fn drop(&mut self) {
    #[allow(clippy::cast_ptr_alignment)]
    let header = unsafe { ptr::read(self.0.buf.as_ptr() as *const Header) };

    for i in 0..header.len {
      unsafe { ptr::read(self.0.data().add(i)) };
    }

    unsafe { alloc::alloc::dealloc(self.0.buf.as_ptr(), make_layout::<T>(header.len)) };
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  #[test]
  fn next_aligned_test() {
    assert_eq!(next_aligned(9, 4), 12);
    assert_eq!(next_aligned(13, 4), 16);
    assert_eq!(next_aligned(12, 4), 12);
    assert_eq!(next_aligned(13, 1), 13);
    assert_eq!(next_aligned(8, 8), 8);
    assert_eq!(next_aligned(16, 32), 32);
    assert_eq!(next_aligned(16, 512), 512);
  }

  #[repr(align(512))]
  struct OverAligned {
    _data: [u8; 512],
  }

  #[test]
  fn max_align_test() {
    let header_alignment = mem::align_of::<Header>();

    assert!(mem::align_of::<i32>() <= mem::align_of::<Header>());
    assert_eq!(max_align::<i32>(), header_alignment);

    assert!(mem::align_of::<u8>() <= mem::align_of::<Header>());
    assert_eq!(max_align::<u8>(), header_alignment);

    assert!(mem::align_of::<OverAligned>() > mem::align_of::<Header>());
    assert_eq!(max_align::<OverAligned>(), mem::align_of::<OverAligned>());
  }

  #[test]
  fn make_layout_test() {
    // empty
    //
    let layout = make_layout::<i32>(0);

    assert_eq!(layout.align(), mem::align_of::<Header>());
    assert_eq!(layout.size(), mem::size_of::<Header>());

    // non-empty, less than
    //
    let layout = make_layout::<i32>(512);
    assert!(mem::align_of::<i32>() < mem::align_of::<Header>());
    assert_eq!(layout.align(), mem::align_of::<Header>());
    assert_eq!(
      layout.size(),
      mem::size_of::<Header>() + 512 * mem::size_of::<i32>()
    );

    // non-empty, equal
    //
    let layout = make_layout::<i64>(512);
    assert_eq!(mem::align_of::<i64>(), mem::align_of::<Header>());
    assert_eq!(layout.align(), mem::align_of::<Header>());
    assert_eq!(
      layout.size(),
      mem::size_of::<Header>() + 512 * mem::size_of::<i64>()
    );

    // non-empty, greater
    let layout = make_layout::<OverAligned>(512);
    assert!(mem::align_of::<OverAligned>() > mem::align_of::<Header>());
    assert_eq!(layout.align(), mem::align_of::<OverAligned>());
    assert_eq!(
      layout.size(),
      next_aligned(mem::size_of::<Header>(), mem::align_of::<OverAligned>())
        + 512 * mem::size_of::<OverAligned>()
    );
  }
}
