use crate::{
  align_utils::{
    get_vector_cap_offset, get_vector_len_offset, get_vector_offset, make_vector_layout,
  },
  managed::{AllocResult, Allocate, DebugHeap, DebugWrap, Manage, Mark, Marked, Trace, Unmark},
  VecBuilder,
};
use ptr::NonNull;
use std::{
  alloc::{alloc, dealloc, handle_alloc_error},
  fmt::{self, Debug, Display},
  marker::PhantomData,
  mem,
  ops::{Deref, DerefMut},
  ptr,
  slice::{self},
};

/// A unique reference to a Garbage collector
/// allocated vector. Note this vector is the same size
/// as a single pointer.
pub struct RawUniqueVector<T, H> {
  /// Pointer to the header of the unique vector
  ptr: NonNull<u8>,

  /// Phantom data to hold the unique vector data type
  data: PhantomData<T>,

  /// Phantom data to hold the unique vector header type
  header: PhantomData<H>,
}

impl<T, H> RawUniqueVector<T, H> {
  /// Retrieve the len from this vector
  #[inline]
  pub fn len(&self) -> usize {
    let count = get_vector_len_offset::<H>();
    unsafe { *(self.ptr.as_ptr().add(count) as *mut usize) }
  }

  /// Retrieve the underlying non Null
  pub fn ptr(&self) -> NonNull<u8> {
    self.ptr
  }

  /// Create a usize from the buffer pointer. This is used
  /// when the value is boxed
  pub fn to_usize(self) -> usize {
    self.as_alloc_ptr() as *const () as usize
  }

  /// Construct a `Vector<T>` from `NonNull<u8>`
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

  /// Retrieve the header from this vector
  #[inline]
  fn header(&self) -> &H {
    #[allow(clippy::cast_ptr_alignment)]
    unsafe {
      &*(self.ptr.as_ptr() as *const H)
    }
  }

  /// Retrieve the capacity from this vector
  pub fn cap(&self) -> usize {
    let count: usize = get_vector_cap_offset::<H>();
    unsafe { *(self.ptr.as_ptr().add(count) as *mut usize) }
  }

  /// Get a raw pointer to allocation
  fn as_alloc_ptr(&self) -> *const u8 {
    self.ptr.as_ptr()
  }

  /// Write a length to to the length slot. This assumes the
  pub unsafe fn write_len(&mut self, len: usize) {
    #[allow(clippy::cast_ptr_alignment)]
    let count = get_vector_len_offset::<H>();
    ptr::write(self.ptr.as_ptr().add(count) as *mut usize, len);
  }

  /// Write a value at the provided index
  /// Note this function does not do bounds checks and is
  /// expected that the caller has already checked the bounds
  /// of len
  pub unsafe fn write_value(&mut self, value: T, index: usize) {
    ptr::write(self.item_mut(index), value);
  }

  /// Read a value at the provided index
  /// Note this function does not do bounds checks and is
  /// expected that the caller has already checked the bounds
  /// of len
  pub unsafe fn read_value(&mut self, index: usize) -> T {
    ptr::read(self.item_mut(index))
  }

  /// Get a mutable pointer to a index into the collection
  ///
  /// ## Safety
  /// This method does no bounds checks so the caller will need to ensure
  /// that this is only called within bounds
  pub unsafe fn item_mut(&self, index: usize) -> *mut T {
    self.ptr.as_ptr().add(self.offset_item(index)) as *mut T
  }

  /// Get a const pointer to a index into the collection
  ///
  /// ## Safety
  /// This method does no bounds checks so the caller will need to ensure
  /// that this is only called within bounds
  pub unsafe fn item_ptr(&self, index: usize) -> *const T {
    self.ptr.as_ptr().add(self.offset_item(index)) as *const T
  }

  /// Determine the byte offset of an item in the collection
  const fn offset_item(&self, index: usize) -> usize {
    let item_offset = get_vector_offset::<H, T>();
    item_offset + index * mem::size_of::<T>()
  }
}

impl<T, H: Mark> Mark for RawUniqueVector<T, H> {
  /// Mark the vector itself as visited
  #[inline]
  fn mark(&self) -> bool {
    self.header().mark()
  }
}

impl<T, H: Marked> Marked for RawUniqueVector<T, H> {
  /// Is this vector marked
  #[inline]
  fn marked(&self) -> bool {
    self.header().marked()
  }
}

impl<T: Trace + DebugHeap, H: Send + Mark + Trace> Trace for RawUniqueVector<T, H> {
  #[inline]
  fn trace(&self) {
    if self.mark() {
      return;
    }

    self.header().trace();
    self.iter().for_each(|i| i.trace())
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
    self.iter().for_each(|i| i.trace_debug(log))
  }
}

impl<T: DebugHeap, H> DebugHeap for RawUniqueVector<T, H> {
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

impl<T, H> fmt::Pointer for RawUniqueVector<T, H> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Pointer::fmt(&self.ptr, f)
  }
}

// TODO we need this for now
// As of today the allocator needs to hold a
// boxed version of everything that is allocated
// as a temporary root
impl<T, H> Copy for RawUniqueVector<T, H> {}
impl<T, H> Clone for RawUniqueVector<T, H> {
  fn clone(&self) -> Self {
    *self
  }
}

impl<T, H> Deref for RawUniqueVector<T, H> {
  type Target = [T];

  #[inline]
  fn deref(&self) -> &Self::Target {
    unsafe {
      let data = self.item_ptr(0);
      let len = self.len();
      slice::from_raw_parts(data, len)
    }
  }
}

impl<T, H> DerefMut for RawUniqueVector<T, H> {
  #[inline]
  fn deref_mut(&mut self) -> &mut Self::Target {
    unsafe {
      let data = self.item_mut(0);
      let len = self.len();
      slice::from_raw_parts_mut(data, len)
    }
  }
}

impl<T, H> PartialEq<RawUniqueVector<T, H>> for RawUniqueVector<T, H> {
  #[inline]
  fn eq(&self, other: &RawUniqueVector<T, H>) -> bool {
    ptr::eq(self.as_alloc_ptr(), other.as_alloc_ptr())
  }
}
impl<T, H> Eq for RawUniqueVector<T, H> {}

impl<T: Display, H> Display for RawUniqueVector<T, H> {
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

impl<T: Debug, H> Debug for RawUniqueVector<T, H> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_list().entries(self.iter()).finish()
  }
}

unsafe impl<T: Send, H: Send> Send for RawUniqueVector<T, H> {}
unsafe impl<T: Sync, H: Sync> Sync for RawUniqueVector<T, H> {}

impl<'a, T, H> Allocate<RawUniqueVector<T, H>> for VecBuilder<'a, T>
where
  T: Trace + DebugHeap + Copy + 'static,
  H: Trace + Mark + Unmark + Send + Default + 'static,
{
  fn alloc(self) -> AllocResult<RawUniqueVector<T, H>> {
    debug_assert!(self.slice().len() <= self.cap());
    let handle = RawUniqueVectorHandle::from_slice(self.slice(), self.cap(), H::default());

    let size = handle.size();
    let reference = handle.value();

    let handle = Box::new(handle);
    let handle = handle as Box<dyn Manage>;

    AllocResult {
      handle,
      size,
      reference,
    }
  }
}

/// A owning reference to a Garbage collector
/// allocated vector. Note this vector is the same size
/// as a single pointer.
pub struct RawUniqueVectorHandle<T, H>(RawUniqueVector<T, H>);

impl<T, H> RawUniqueVectorHandle<T, H> {
  /// Create a non owning unique reference to this vector.
  ///
  /// ## Examples
  pub fn value(&self) -> RawUniqueVector<T, H> {
    RawUniqueVector {
      ptr: self.0.ptr,
      data: PhantomData,
      header: PhantomData,
    }
  }

  /// Determine the size of the handle and the pointed to
  /// allocation
  #[inline]
  pub fn size(&self) -> usize {
    make_vector_layout::<H, T>(self.0.cap()).size()
  }
}

impl<T: Copy, H> RawUniqueVectorHandle<T, H> {
  /// Create a new `VectorHandle` from the provided header
  /// and a copyable slice
  pub fn from_slice(slice: &[T], cap: usize, header: H) -> Self {
    debug_assert!(slice.len() <= cap);
    assert!(mem::size_of::<T>() > 0, "ZSTs currently not supported");

    let new_layout = make_vector_layout::<H, T>(cap);
    let buf = unsafe { alloc(new_layout) };

    if buf.is_null() {
      handle_alloc_error(new_layout);
    }

    #[allow(clippy::cast_ptr_alignment)]
    let mut vector = unsafe {
      ptr::write(buf as *mut H, header);
      ptr::write(
        buf.add(get_vector_len_offset::<H>()) as *mut usize,
        slice.len(),
      );
      ptr::write(buf.add(get_vector_cap_offset::<H>()) as *mut usize, cap);

      RawUniqueVectorHandle(RawUniqueVector {
        ptr: NonNull::new_unchecked(buf),
        data: PhantomData,
        header: PhantomData,
      })
    };

    vector.0.copy_from_slice(slice);
    vector
  }
}

impl<T, H: Unmark> Unmark for RawUniqueVectorHandle<T, H> {
  /// Unmark this allocation as visited, returning
  /// the existing marked status
  #[inline]
  fn unmark(&self) -> bool {
    self.0.header().unmark()
  }
}

impl<T, H: Marked> Marked for RawUniqueVectorHandle<T, H> {
  /// Is this allocation marked
  #[inline]
  fn marked(&self) -> bool {
    self.0.marked()
  }
}

impl<T, H> Deref for RawUniqueVectorHandle<T, H> {
  type Target = [T];

  fn deref(&self) -> &Self::Target {
    self.0.deref()
  }
}

impl<T, H> DerefMut for RawUniqueVectorHandle<T, H> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    self.0.deref_mut()
  }
}

impl<T: DebugHeap, H> DebugHeap for RawUniqueVectorHandle<T, H> {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    self.0.fmt_heap(f, depth)
  }
}

impl<T, H> fmt::Pointer for RawUniqueVectorHandle<T, H> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.0.fmt(f)
  }
}

impl<T: Copy, H: Default> From<&[T]> for RawUniqueVectorHandle<T, H> {
  fn from(slice: &[T]) -> Self {
    RawUniqueVectorHandle::from_slice(slice, slice.len(), H::default())
  }
}

impl<T: DebugHeap, H: Unmark + Marked> Manage for RawUniqueVectorHandle<T, H> {
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

impl<T, H> Drop for RawUniqueVectorHandle<T, H> {
  fn drop(&mut self) {
    unsafe {
      #[allow(clippy::cast_ptr_alignment)]
      ptr::read(self.0.ptr.as_ptr() as *const H);
      let cap = self.0.cap();

      for i in 0..cap {
        ptr::read(self.0.item_ptr(i));
      }

      dealloc(self.0.ptr.as_ptr(), make_vector_layout::<H, T>(cap));
    }
  }
}

#[cfg(test)]
mod test {

  use super::*;
  mod raw_shared_vector {
    use super::*;

    #[test]
    fn header() {
      let handle = RawUniqueVectorHandle::from_slice(&[1, 2, 3, 4, 5], 5, String::from("header"));
      let vector = handle.value();

      assert_eq!(vector.header(), "header");
    }

    #[test]
    fn len() {
      let handle = RawUniqueVectorHandle::from_slice(&[1, 2, 3, 4, 5], 5, String::from("header"));
      let vector = handle.value();

      assert_eq!(vector.len(), 5);
    }

    #[test]
    fn is_empty() {
      let handle1 = RawUniqueVectorHandle::from_slice(&[1, 2, 3, 4, 5], 5, String::from("header"));
      let handle2 =
        RawUniqueVectorHandle::<i32, String>::from_slice(&[], 5, String::from("header"));
      let vector1 = handle1.value();
      let vector2 = handle2.value();

      assert!(!vector1.is_empty());
      assert!(vector2.is_empty());
    }

    #[test]
    fn cap() {
      let handle = RawUniqueVectorHandle::from_slice(&[1, 2, 3, 4, 5], 10, String::from("header"));
      let vector = handle.value();

      assert_eq!(vector.cap(), 10);
    }
  }

  mod raw_shared_vector_handle {
    use super::*;

    #[test]
    fn from_slice() {
      let vector_handle =
        RawUniqueVectorHandle::from_slice(&[1, 2, 3, 4, 5], 6, String::from("header"));
      let vector = vector_handle.value();

      assert_eq!(vector.len(), 5);
      assert_eq!(vector.cap(), 6);
      assert_eq!(vector[0], 1);
      assert_eq!(vector[1], 2);
      assert_eq!(vector[2], 3);
      assert_eq!(vector[3], 4);
      assert_eq!(vector[4], 5);
    }

    #[test]
    #[should_panic]
    fn from_slice_bad_cap() {
      RawUniqueVectorHandle::from_slice(&[1, 2, 3, 4, 5], 3, String::from("header"));
    }
  }
}
