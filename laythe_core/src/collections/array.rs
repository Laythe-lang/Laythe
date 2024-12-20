use ptr::NonNull;
use std::{
  alloc::{alloc, dealloc, handle_alloc_error},
  fmt::{self, Debug},
  marker::PhantomData,
  mem,
  ops::{Deref, DerefMut},
  ptr,
  slice::{self},
};

use crate::{
  align_utils::{get_array_len_offset, get_array_offset, make_array_layout},
  managed::{AllocResult, Allocate, DebugHeap, DebugWrap, Manage, Mark, Marked, Trace, Unmark},
};

/// A non owning reference to a Garbage collector
/// allocated array. Note this array is the same size
/// as a single pointer.
pub struct Array<T, H> {
  /// Pointer to the header of the array
  ptr: NonNull<u8>,

  /// Phantom data to hold the array data type
  data: PhantomData<T>,

  /// Phantom data to hold the array header type
  header: PhantomData<H>,
}

impl<T, H> Array<T, H> {
  pub fn ptr(&self) -> NonNull<u8> {
    self.ptr
  }

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

  /// Is this array empty
  #[inline]
  pub fn is_empty(&self) -> bool {
    self.len() == 0
  }

  /// Get a static lifetime reference to the underlying slice.
  ///
  /// ## Safety
  /// This method is truly only safe in the context for an alice
  /// allocated by the allocator. This method also assumes that
  /// whichever object hold this slice also hold a reference to
  /// the Array itself in order to keep it alive. This is primarily
  /// to use rust method requiring a lifetime, typically for iterators
  pub unsafe fn deref_static(&self) -> &'static [T] {
    slice::from_raw_parts(self.as_ptr(), self.len())
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

  /// Create a usize from the buffer pointer. This is used
  /// when the value is boxed
  pub fn to_usize(self) -> usize {
    self.as_alloc_ptr() as *const () as usize
  }

  /// Construct a `Array<T>` from `NonNull<u8>`
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

impl<T, H: Mark> Mark for Array<T, H> {
  /// Mark the array itself as visited
  #[inline]
  fn mark(&self) -> bool {
    self.header().mark()
  }
}

impl<T, H: Marked> Marked for Array<T, H> {
  /// Is this array marked
  #[inline]
  fn marked(&self) -> bool {
    self.header().marked()
  }
}

impl<T: Trace + DebugHeap, H: Send + Mark + Trace> Trace for Array<T, H> {
  #[inline]
  fn trace(&self) {
    if self.mark() {
      return;
    }

    self.header().trace();
    self.iter().for_each(|i| i.trace());
  }

  fn trace_debug(&self, log: &mut dyn std::io::Write) {
    if self.mark() {
      return;
    }

    log
      .write_fmt(format_args!(
        "{:p} mark {:?}\n",
        self.ptr,
        DebugWrap(self, 1)
      ))
      .expect("unable to write to stdout");
    log.flush().expect("unable to flush stdout");

    self.header().trace_debug(log);
    self.iter().for_each(|i| i.trace_debug(log));
  }
}

impl<T: DebugHeap, H> DebugHeap for Array<T, H> {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    if depth == 0 {
      f.write_fmt(format_args!("{:p}", self.ptr))
    } else {
      f.debug_list()
        .entries(self.iter().map(|x| DebugWrap(x, depth.saturating_sub(1))))
        .finish()
    }
  }
}

impl<T, H> fmt::Pointer for Array<T, H> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Pointer::fmt(&self.ptr, f)
  }
}

impl<T, H> Copy for Array<T, H> {}
impl<T, H> Clone for Array<T, H> {
  fn clone(&self) -> Self {
    *self
  }
}

impl<T, H> Deref for Array<T, H> {
  type Target = [T];

  #[inline]
  fn deref(&self) -> &Self::Target {
    let data = self.data();
    let len = self.len();
    unsafe { slice::from_raw_parts(data, len) }
  }
}

impl<T, H> DerefMut for Array<T, H> {
  #[inline]
  fn deref_mut(&mut self) -> &mut Self::Target {
    let data = self.data();
    let len = self.len();
    unsafe { slice::from_raw_parts_mut(data, len) }
  }
}

impl<T, H> PartialEq<Array<T, H>> for Array<T, H> {
  #[inline]
  fn eq(&self, other: &Array<T, H>) -> bool {
    ptr::eq(self.as_alloc_ptr(), other.as_alloc_ptr())
  }
}
impl<T, H> Eq for Array<T, H> {}

impl<T: Debug, H> Debug for Array<T, H> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_list().entries(self.iter()).finish()
  }
}

unsafe impl<T: Send, H: Send> Send for Array<T, H> {}
unsafe impl<T: Sync, H: Sync> Sync for Array<T, H> {}

/// A owning reference to a Garbage collector
/// allocated array. Note this array is the same size
/// as a single pointer.
pub struct ArrayHandle<T, H>(Array<T, H>);

impl<T, H> ArrayHandle<T, H> {
  /// Create a non owning reference to this array.
  pub fn value(&self) -> Array<T, H> {
    self.0
  }

  /// Determine the size of the handle and the pointed to
  /// allocation
  #[inline]
  pub fn size(&self) -> usize {
    make_array_layout::<H, T>(self.0.len()).size()
  }
}

impl<T: Copy, H> ArrayHandle<T, H> {
  /// Create a new `ArrayHandle` from the provided header
  /// and a copyable slice
  pub fn from_slice(slice: &[T], header: H) -> Self {
    assert!(mem::size_of::<T>() > 0, "ZSTs currently not supported");

    let len = slice.len();
    let new_layout = make_array_layout::<H, T>(len);
    let buf = unsafe { alloc(new_layout) };

    if buf.is_null() {
      handle_alloc_error(new_layout);
    }

    #[allow(clippy::cast_ptr_alignment)]
    let mut array = unsafe {
      ptr::write(buf as *mut H, header);
      ptr::write(buf.add(get_array_len_offset::<H>()) as *mut usize, len);

      ArrayHandle(Array {
        ptr: NonNull::new_unchecked(buf),
        data: PhantomData,
        header: PhantomData,
      })
    };

    array.0.copy_from_slice(slice);
    array
  }
}

impl<T, H: Unmark> Unmark for ArrayHandle<T, H> {
  /// Unmark this allocation as visited, returning
  /// the existing marked status
  #[inline]
  fn unmark(&self) -> bool {
    self.0.header().unmark()
  }
}

impl<T, H: Marked> Marked for ArrayHandle<T, H> {
  /// Is this allocation marked
  #[inline]
  fn marked(&self) -> bool {
    self.0.marked()
  }
}

impl<T, H> Deref for ArrayHandle<T, H> {
  type Target = [T];

  fn deref(&self) -> &Self::Target {
    self.0.deref()
  }
}

impl<T, H> DerefMut for ArrayHandle<T, H> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    self.0.deref_mut()
  }
}

impl<T: DebugHeap, H> DebugHeap for ArrayHandle<T, H> {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    self.0.fmt_heap(f, depth)
  }
}

impl<T, H> fmt::Pointer for ArrayHandle<T, H> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.0.fmt(f)
  }
}

impl<T: Copy, H: Default> From<&[T]> for ArrayHandle<T, H> {
  fn from(slice: &[T]) -> Self {
    ArrayHandle::from_slice(slice, H::default())
  }
}

impl<T: DebugHeap, H: Unmark + Marked> Manage for ArrayHandle<T, H> {
  fn size(&self) -> usize {
    self.size()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }

  fn loc(&self) -> *const u8 {
    self.0.as_alloc_ptr()
  }
}

impl<T, H> Drop for ArrayHandle<T, H> {
  fn drop(&mut self) {
    unsafe {
      #[allow(clippy::cast_ptr_alignment)]
      ptr::read(self.0.ptr.as_ptr() as *const H);
      let len = self.0.len();

      for i in 0..len {
        ptr::read(self.0.data().add(i));
      }

      dealloc(self.0.ptr.as_ptr(), make_array_layout::<H, T>(len));
    }
  }
}

impl<T, H> Allocate<Array<T, H>> for &[T]
where
  T: 'static + Trace + Copy + DebugHeap,
  H: 'static + Send + Mark + Unmark + Trace + Default,
{
  fn alloc(self) -> AllocResult<Array<T, H>> {
    let handle = ArrayHandle::from_slice(self, H::default());
    let reference = handle.value();
    let size = handle.size();

    let handle = Box::new(handle);
    let handle = handle as Box<dyn Manage>;

    AllocResult {
      handle,
      reference,
      size,
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
      let handle = ArrayHandle::from_slice(&[1, 2, 3, 4, 5], String::from("header"));
      let array = handle.value();

      assert_eq!(array.header(), "header");
    }

    #[test]
    fn len() {
      let handle = ArrayHandle::from_slice(&[1, 2, 3, 4, 5], String::from("header"));
      let array = handle.value();

      assert_eq!(array.len(), 5);
    }
  }

  mod handle {
    use super::*;

    #[test]
    fn from_slice() {
      let handle = ArrayHandle::from_slice(&[1, 2, 3, 4, 5], String::from("header"));

      assert_eq!(handle.len(), 5);
      assert_eq!(handle[0], 1);
      assert_eq!(handle[1], 2);
      assert_eq!(handle[2], 3);
      assert_eq!(handle[3], 4);
      assert_eq!(handle[4], 5);
    }

    #[test]
    fn from_value() {
      let handle = ArrayHandle::from_slice(&[1, 2, 3, 4, 5], String::from("header"));
      let array = handle.value();

      assert_eq!(array.len(), 5);
      assert_eq!(array[0], 1);
      assert_eq!(array[1], 2);
      assert_eq!(array[2], 3);
      assert_eq!(array[3], 4);
      assert_eq!(array[4], 5);

      assert_eq!(handle.len(), 5);
    }
  }
}
