use crate::{
  align_utils::{get_list_cap_offset, get_list_len_offset, get_list_offset, make_list_layout},
  managed::{AllocResult, Allocate, DebugHeap, DebugWrap, Manage, Mark, Marked, Trace, Unmark},
  utils::{msb_set, set_msb, strip_msb},
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

use super::vec_builder::VecBuilder;

pub enum RawVecLocation<T, H> {
  /// The list is in it's default state which returns it's capacity
  Here(usize),

  /// The list has been forwarded to a new location
  Forwarded(RawVector<T, H>),
}

/// The result of an indexed Operation
#[derive(PartialEq, Eq, Debug)]
pub enum IndexedResult<T = ()> {
  /// The result was completed successfully
  Ok(T),

  /// The operation was out of bounds
  OutOfBounds,
}

/// A non owning reference to a Garbage collector
/// allocated list. Note this list is the same size
/// as a single pointer.
pub struct RawVector<T, H> {
  /// Pointer to the header of the list
  ptr: NonNull<u8>,

  /// Phantom data to hold the list data type
  data: PhantomData<T>,

  /// Phantom data to hold the list header type
  header: PhantomData<H>,
}

impl<T, H> RawVector<T, H> {
  /// Retrieve the len from this list
  #[inline]
  pub fn len(&self) -> usize {
    match self.state() {
      RawVecLocation::Here(_) => unsafe { self.read_len() },
      RawVecLocation::Forwarded(gc_list) => gc_list.len(),
    }
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

  /// Has this list moved
  #[inline]
  pub fn has_moved(&self) -> bool {
    msb_set(self.read_cap())
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

  /// Determine the state of this list
  pub fn state(&self) -> RawVecLocation<T, H> {
    let cap: usize = self.read_cap();

    // if capacity is a usize max we treat that as a flag that
    // the array has actually moved elsewhere
    if msb_set(cap) {
      RawVecLocation::Forwarded(self.relocated_list())
    } else {
      RawVecLocation::Here(cap)
    }
  }

  /// Retrieve the header from this list
  #[inline]
  fn header(&self) -> &H {
    #[allow(clippy::cast_ptr_alignment)]
    unsafe {
      &*(self.ptr.as_ptr() as *const H)
    }
  }

  /// Retrieve the capacity from this list
  pub fn cap(&self) -> usize {
    match self.state() {
      RawVecLocation::Here(cap) => cap,
      RawVecLocation::Forwarded(gc_list) => gc_list.cap(),
    }
  }

  /// Get a raw pointer to allocation
  fn as_alloc_ptr(&self) -> *const u8 {
    match self.state() {
      RawVecLocation::Here(_) => self.ptr.as_ptr(),
      RawVecLocation::Forwarded(gc_list) => gc_list.as_alloc_ptr(),
    }
  }

  /// Read the length slot as a usize. When the length has not
  /// moved this will be the actual length. If it has move it
  /// is undefined behavior
  pub unsafe fn read_len<L: Copy>(&self) -> L {
    #[allow(clippy::cast_ptr_alignment)]
    let count = get_list_len_offset::<H>();
    *(self.ptr.as_ptr().add(count) as *mut L)
  }

  /// Write a length to to the length slot. This assumes the
  pub unsafe fn write_len<L>(&mut self, len: L) {
    #[allow(clippy::cast_ptr_alignment)]
    let count = get_list_len_offset::<H>();
    ptr::write(self.ptr.as_ptr().add(count) as *mut L, len);
  }

  /// Read the capacity field on this list. This may indicate a forwarded pointer
  fn read_cap(&self) -> usize {
    #[allow(clippy::cast_ptr_alignment)]
    let count: usize = get_list_cap_offset::<H>();
    unsafe { *(self.ptr.as_ptr().add(count) as *mut usize) }
  }

  /// Make this list as moved
  pub fn mark_moved(&mut self, cap: usize) {
    #[allow(clippy::cast_ptr_alignment)]
    let count: usize = get_list_cap_offset::<H>();
    unsafe { ptr::write(self.ptr.as_ptr().add(count) as *mut usize, set_msb(cap)) };
  }

  /// The pointer to the moved list
  fn relocated_list(&self) -> RawVector<T, H> {
    let ptr = unsafe { NonNull::new_unchecked(self.read_len()) };

    RawVector {
      ptr,
      data: PhantomData,
      header: PhantomData,
    }
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
    match self.state() {
      RawVecLocation::Here(_) => self.ptr.as_ptr().add(self.offset_item(index)) as *mut T,
      RawVecLocation::Forwarded(gc_list) => gc_list.item_mut(index),
    }
  }

  /// Get a const pointer to a index into the collection
  ///
  /// ## Safety
  /// This method does no bounds checks so the caller will need to ensure
  /// that this is only called within bounds
  pub unsafe fn item_ptr(&self, index: usize) -> *const T {
    match self.state() {
      RawVecLocation::Here(_) => self.ptr.as_ptr().add(self.offset_item(index)) as *const T,
      RawVecLocation::Forwarded(gc_list) => gc_list.item_ptr(index),
    }
  }

  /// Determine the byte offset of an item in the collection
  const fn offset_item(&self, index: usize) -> usize {
    let offset_list_start = get_list_offset::<H, T>();
    offset_list_start + index * mem::size_of::<T>()
  }
}

impl<T, H: Mark> Mark for RawVector<T, H> {
  /// Mark the list itself as visited
  #[inline]
  fn mark(&self) -> bool {
    self.header().mark()
  }
}

impl<T, H: Marked> Marked for RawVector<T, H> {
  /// Is this list marked
  #[inline]
  fn marked(&self) -> bool {
    self.header().marked()
  }
}

impl<T: Trace + DebugHeap, H: Send + Mark + Trace> Trace for RawVector<T, H> {
  #[inline]
  fn trace(&self) {
    if self.mark() {
      return;
    }

    self.header().trace();
    match self.state() {
      RawVecLocation::Here(_) => self.iter().for_each(|i| i.trace()),
      RawVecLocation::Forwarded(gc_list) => gc_list.trace(),
    };
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
    match self.state() {
      RawVecLocation::Here(_) => self.iter().for_each(|i| i.trace_debug(log)),
      RawVecLocation::Forwarded(gc_list) => gc_list.trace_debug(log),
    };
  }
}

impl<T: DebugHeap, H> DebugHeap for RawVector<T, H> {
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

impl<T, H> fmt::Pointer for RawVector<T, H> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Pointer::fmt(&self.ptr, f)
  }
}

impl<T, H> Copy for RawVector<T, H> {}
impl<T, H> Clone for RawVector<T, H> {
  fn clone(&self) -> Self {
    *self
  }
}

impl<T, H> Deref for RawVector<T, H> {
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

impl<T, H> DerefMut for RawVector<T, H> {
  #[inline]
  fn deref_mut(&mut self) -> &mut Self::Target {
    unsafe {
      let data = self.item_mut(0);
      let len = self.len();
      slice::from_raw_parts_mut(data, len)
    }
  }
}

impl<T, H> PartialEq<RawVector<T, H>> for RawVector<T, H> {
  #[inline]
  fn eq(&self, other: &RawVector<T, H>) -> bool {
    ptr::eq(self.as_alloc_ptr(), other.as_alloc_ptr())
  }
}
impl<T, H> Eq for RawVector<T, H> {}

impl<T: Display, H> Display for RawVector<T, H> {
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

impl<T: Debug, H> Debug for RawVector<T, H> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_list().entries(self.iter()).finish()
  }
}

unsafe impl<T: Send, H: Send> Send for RawVector<T, H> {}
unsafe impl<T: Sync, H: Sync> Sync for RawVector<T, H> {}

impl<'a, T, H> Allocate<RawVector<T, H>> for VecBuilder<'a, T>
where
  T: Trace + DebugHeap + Copy + 'static,
  H: Trace + Mark + Unmark + Send + Default + 'static,
{
  fn alloc(self) -> AllocResult<RawVector<T, H>> {
    debug_assert!(self.slice().len() <= self.cap());
    let handle = VectorHandle::from_slice(self.slice(), self.cap(), H::default());

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
/// allocated list. Note this list is the same size
/// as a single pointer.
pub struct VectorHandle<T, H>(RawVector<T, H>);

impl<T, H> VectorHandle<T, H> {
  /// Create a non owning reference to this list.
  ///
  /// ## Examples
  pub fn value(&self) -> RawVector<T, H> {
    self.0
  }

  /// Determine the size of the handle and the pointed to
  /// allocation
  #[inline]
  pub fn size(&self) -> usize {
    make_list_layout::<H, T>(strip_msb(self.0.read_cap())).size()
  }
}

impl<T: Copy, H> VectorHandle<T, H> {
  /// Create a new `VectorHandle` from the provided header
  /// and a copyable slice
  pub fn from_slice(slice: &[T], cap: usize, header: H) -> Self {
    debug_assert!(slice.len() <= cap);
    assert!(mem::size_of::<T>() > 0, "ZSTs currently not supported");

    let new_layout = make_list_layout::<H, T>(cap);
    let buf = unsafe { alloc(new_layout) };

    if buf.is_null() {
      handle_alloc_error(new_layout);
    }

    #[allow(clippy::cast_ptr_alignment)]
    let mut list = unsafe {
      ptr::write(buf as *mut H, header);
      ptr::write(
        buf.add(get_list_len_offset::<H>()) as *mut usize,
        slice.len(),
      );
      ptr::write(buf.add(get_list_cap_offset::<H>()) as *mut usize, cap);

      VectorHandle(RawVector {
        ptr: NonNull::new_unchecked(buf),
        data: PhantomData,
        header: PhantomData,
      })
    };

    list.0.copy_from_slice(slice);
    list
  }
}

impl<T, H: Unmark> Unmark for VectorHandle<T, H> {
  /// Unmark this allocation as visited, returning
  /// the existing marked status
  #[inline]
  fn unmark(&self) -> bool {
    self.0.header().unmark()
  }
}

impl<T, H: Marked> Marked for VectorHandle<T, H> {
  /// Is this allocation marked
  #[inline]
  fn marked(&self) -> bool {
    self.0.marked()
  }
}

impl<T, H> Deref for VectorHandle<T, H> {
  type Target = [T];

  fn deref(&self) -> &Self::Target {
    self.0.deref()
  }
}

impl<T, H> DerefMut for VectorHandle<T, H> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    self.0.deref_mut()
  }
}

impl<T: DebugHeap, H> DebugHeap for VectorHandle<T, H> {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    self.0.fmt_heap(f, depth)
  }
}

impl<T, H> fmt::Pointer for VectorHandle<T, H> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.0.fmt(f)
  }
}

impl<T: Copy, H: Default> From<&[T]> for VectorHandle<T, H> {
  fn from(slice: &[T]) -> Self {
    VectorHandle::from_slice(slice, slice.len(), H::default())
  }
}

impl<T: DebugHeap, H: Unmark + Marked> Manage for VectorHandle<T, H> {
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

impl<T, H> Drop for VectorHandle<T, H> {
  fn drop(&mut self) {
    unsafe {
      #[allow(clippy::cast_ptr_alignment)]
      ptr::read(self.0.ptr.as_ptr() as *const H);
      let cap = strip_msb(self.0.read_cap());

      for i in 0..cap {
        ptr::read(self.0.item_ptr(i));
      }

      dealloc(self.0.ptr.as_ptr(), make_list_layout::<H, T>(cap));
    }
  }
}

#[cfg(test)]
mod test {

  use super::*;
  mod raw_vector {
    use super::*;

    #[test]
    fn header() {
      let handle = VectorHandle::from_slice(&[1, 2, 3, 4, 5], 5, String::from("header"));
      let list = handle.value();

      assert_eq!(list.header(), "header");
    }

    #[test]
    fn len() {
      let handle = VectorHandle::from_slice(&[1, 2, 3, 4, 5], 5, String::from("header"));
      let list = handle.value();

      assert_eq!(list.len(), 5);
    }

    #[test]
    fn is_empty() {
      let handle1 = VectorHandle::from_slice(&[1, 2, 3, 4, 5], 5, String::from("header"));
      let handle2 = VectorHandle::<i32, String>::from_slice(&[], 5, String::from("header"));
      let list1 = handle1.value();
      let list2 = handle2.value();

      assert!(!list1.is_empty());
      assert!(list2.is_empty());
    }

    #[test]
    fn cap() {
      let handle = VectorHandle::from_slice(&[1, 2, 3, 4, 5], 10, String::from("header"));
      let list = handle.value();

      assert_eq!(list.cap(), 10);
    }
  }

  mod handle {
    use super::*;

    #[test]
    fn from_slice() {
      let list_handle = VectorHandle::from_slice(&[1, 2, 3, 4, 5], 6, String::from("header"));
      let list = list_handle.value();

      assert_eq!(list.len(), 5);
      assert_eq!(list.cap(), 6);
      assert_eq!(list[0], 1);
      assert_eq!(list[1], 2);
      assert_eq!(list[2], 3);
      assert_eq!(list[3], 4);
      assert_eq!(list[4], 5);
    }

    #[test]
    #[should_panic]
    fn from_slice_bad_cap() {
      VectorHandle::from_slice(&[1, 2, 3, 4, 5], 3, String::from("header"));
    }
  }
}
