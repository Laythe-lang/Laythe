use super::{
  allocate::AllocObjResult,
  header::ObjHeader,
  utils::{get_list_cap_offset, get_list_len_offset, get_list_offset},
  AllocateObj, DebugHeap, DebugWrap, GcObject, GcObjectHandle, Manage, Mark, Marked, Trace, Unmark,
};
use crate::{hooks::GcHooks, managed::utils::make_list_layout, object::ObjectKind, value::Value};
use ptr::NonNull;
use std::{
  alloc::{alloc, dealloc, handle_alloc_error},
  fmt::{self, Debug, Display},
  marker::PhantomData,
  mem,
  ops::{Deref, DerefMut},
  ptr,
  slice::{self},
  usize,
};

pub type List = GcList<Value, ObjHeader>;

/// A dummy array so we can get a slice to nothing for free
const DUMMY_ARRAY: [Value; 0] = [];

/// the bit position
const BIT_POSITION: u32 = usize::BITS - 1;
const TOP_BIT: usize = 1 << BIT_POSITION;
const TOP_BIT_MASK: usize = !TOP_BIT;

fn msb_set(value: usize) -> bool {
  (value & TOP_BIT) != 0
}

fn set_msb(value: usize) -> usize {
  TOP_BIT | value
}

pub fn strip_msb(value: usize) -> usize {
  value & TOP_BIT_MASK
}

/// A structure used to construct a list via the allocator
pub struct ListBuilder<'a> {
  /// The source slice from which the list is copied from
  slice: &'a [Value],

  /// The requested capacity of the list
  cap: usize,
}

impl<'a> ListBuilder<'a> {
  /// Create a list builder with a dummy slice and capacity
  pub fn cap_only(cap: usize) -> Self {
    Self {
      slice: &DUMMY_ARRAY,
      cap,
    }
  }

  /// Create a list builder with a slice and capacity
  pub fn new(slice: &'a [Value], cap: usize) -> Self {
    assert!(slice.len() <= cap);

    Self { slice, cap }
  }

  /// The underlying slice for the builder
  pub fn slice(&self) -> &[Value] {
    &self.slice
  }

  /// The requested capacity
  pub fn cap(&self) -> usize {
    self.cap
  }
}

/// The state of the list
enum ListState<T, H> {
  /// The list is in it's default state which returns it's capacity
  Default(usize),

  /// The list has been forwarded to a new location
  Forwarded(GcList<T, H>),
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
pub struct GcList<T, H> {
  /// Pointer to the header of the list
  ptr: NonNull<u8>,

  /// Phantom data to hold the list data type
  data: PhantomData<T>,

  /// Phantom data to hold the list header type
  header: PhantomData<H>,
}

impl<T, H> GcList<T, H> {
  /// Retrieve the len from this list
  #[inline]
  pub fn len(&self) -> usize {
    match self.state() {
      ListState::Default(_) => unsafe { self.read_len() },
      ListState::Forwarded(gc_list) => gc_list.len(),
    }
  }

  /// Retrieve the capacity from this list
  #[inline]
  pub fn cap(&self) -> usize {
    match self.state() {
      ListState::Default(cap) => cap,
      ListState::Forwarded(gc_list) => gc_list.cap(),
    }
  }

  /// Is this list empty
  #[inline]
  pub fn is_empty(&self) -> bool {
    self.len() == 0
  }

  /// Get a raw pointer to allocation
  #[inline]
  pub fn as_alloc_ptr(&self) -> *const u8 {
    match self.state() {
      ListState::Default(_) => self.ptr.as_ptr(),
      ListState::Forwarded(gc_list) => gc_list.as_alloc_ptr(),
    }
  }

  /// Create a usize from the buffer pointer. This is used
  /// when the value is boxed
  pub fn to_usize(self) -> usize {
    self.as_alloc_ptr() as *const () as usize
  }

  /// Construct a `GcList<T>` from `NonNull<u8>`
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

  /// Retrieve the header from this list
  #[inline]
  fn header(&self) -> &H {
    #[allow(clippy::cast_ptr_alignment)]
    unsafe {
      &*(self.ptr.as_ptr() as *const H)
    }
  }

  /// Determine the state of this list
  fn state(&self) -> ListState<T, H> {
    let cap: usize = self.read_cap();

    // if capacity is a usize max we treat that as a flag that
    // the array has actually moved elsewhere
    if msb_set(cap) {
      ListState::Forwarded(self.relocated_list())
    } else {
      ListState::Default(cap)
    }
  }

  /// Read the length slot as a usize. When the length has not
  /// moved this will be the actual length. If it has move it
  /// is undefined behavior
  unsafe fn read_len<L: Copy>(&self) -> L {
    #[allow(clippy::cast_ptr_alignment)]
    let count = get_list_len_offset::<H>();
    *(self.ptr.as_ptr().add(count) as *mut L)
  }

  /// Write a length to to the length slot. This assumes the
  unsafe fn write_len<L>(&mut self, len: L) {
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
  fn mark_moved(&mut self, cap: usize) {
    #[allow(clippy::cast_ptr_alignment)]
    let count: usize = get_list_cap_offset::<H>();
    unsafe { ptr::write(self.ptr.as_ptr().add(count) as *mut usize, set_msb(cap)) };
  }

  /// The pointer to the moved list
  fn relocated_list(&self) -> GcList<T, H> {
    let ptr = unsafe { NonNull::new_unchecked(self.read_len()) };

    GcList {
      ptr,
      data: PhantomData,
      header: PhantomData,
    }
  }

  /// Write a value at the provided index
  /// Note this function does not do bounds checks and is
  /// expected that the caller has already checked the bounds
  /// of len
  unsafe fn write_value(&mut self, value: T, index: usize) {
    ptr::write(self.item_mut(index), value);
  }

  /// Read a value at the provided index
  /// Note this function does not do bounds checks and is
  /// expected that the caller has already checked the bounds
  /// of len
  unsafe fn read_value(&mut self, index: usize) -> T {
    ptr::read(self.item_mut(index))
  }

  /// Get a mutable pointer to a index into the collection
  ///
  /// ## Safety
  /// This method does no bounds checks so the caller will need to ensure
  /// that this is only called within bounds
  unsafe fn item_mut(&self, index: usize) -> *mut T {
    match self.state() {
      ListState::Default(_) => self.ptr.as_ptr().add(self.offset_item(index)) as *mut T,
      ListState::Forwarded(gc_list) => gc_list.item_mut(index),
    }
  }

  /// Get a const pointer to a index into the collection
  ///
  /// ## Safety
  /// This method does no bounds checks so the caller will need to ensure
  /// that this is only called within bounds
  unsafe fn item_ptr(&self, index: usize) -> *const T {
    match self.state() {
      ListState::Default(_) => self.ptr.as_ptr().add(self.offset_item(index)) as *const T,
      ListState::Forwarded(gc_list) => gc_list.item_ptr(index),
    }
  }

  /// Determine the byte offset of an item in the collection
  const fn offset_item(&self, index: usize) -> usize {
    let offset_list_start = get_list_offset::<H, T>();
    offset_list_start + index * mem::size_of::<T>()
  }
}

impl GcList<Value, ObjHeader> {
  /// Push a new element onto this list. If a resize is needed
  /// a new list will be allocated and elements will be transferred
  pub fn push(&mut self, value: Value, hooks: &GcHooks) {
    match self.state() {
      ListState::Default(cap) => {
        let len = unsafe { self.read_len() };

        // determine if we need to grow the list then
        // persist the value
        let mut list = self.ensure_capacity(len + 1, cap, hooks);
        unsafe {
          list.write_value(value, len);
          list.write_len(len + 1);
        }
      },
      ListState::Forwarded(mut gc_list) => gc_list.push(value, hooks),
    }
  }

  /// Pop and element off the list.
  pub fn pop(&mut self) -> Option<Value> {
    match self.state() {
      ListState::Default(_) => {
        let len: usize = unsafe { self.read_len() };
        if len == 0 {
          None
        } else {
          unsafe {
            self.write_len(len - 1);
            let offset_list_start = get_list_offset::<ObjHeader, Value>();

            let slot = self
              .ptr
              .as_ptr()
              .add(offset_list_start + (len - 1) * mem::size_of::<Value>())
              as *mut Value;

            Some(ptr::read(slot))
          }
        }
      },
      ListState::Forwarded(mut gc_list) => gc_list.pop(),
    }
  }

  pub fn insert(&mut self, index: usize, value: Value, hooks: &GcHooks) -> IndexedResult {
    match self.state() {
      ListState::Default(cap) => {
        let len = unsafe { self.read_len() };
        if index > len {
          return IndexedResult::OutOfBounds;
        }

        // determine if we need to grow the list then
        // persist the value
        let mut list = self.ensure_capacity(len + 1, cap, hooks);

        unsafe {
          ptr::copy(list.item_ptr(index), list.item_mut(index + 1), len - index);
          list.write_value(value, index);
          list.write_len(len + 1);
        }

        IndexedResult::Ok(())
      },
      ListState::Forwarded(mut gc_list) => gc_list.insert(index, value, hooks),
    }
  }

  pub fn remove(&mut self, index: usize) -> IndexedResult<Value> {
    match self.state() {
      ListState::Default(_) => {
        let len = unsafe { self.read_len() };
        if index >= len {
          return IndexedResult::OutOfBounds;
        }

        unsafe {
          let value = self.read_value(index);
          ptr::copy(
            self.item_ptr(index + 1),
            self.item_mut(index),
            len - index - 1,
          );

          self.write_len(len - 1);
          IndexedResult::Ok(value)
        }

      },
      ListState::Forwarded(mut gc_list) => gc_list.remove(index),
    }
  }

  /// Degrade this GcList into the more generic GcObject.
  /// This allows the list to meet the same interface
  /// as the other managed objects
  pub fn degrade(self) -> GcObject {
    GcObject::new(self.ptr)
  }

  /// Ensure this list has enough capacity for the operation
  /// If it does it returns itself. Otherwise it returns a new list
  /// which it will have just allocated
  fn ensure_capacity(
    &mut self,
    needed: usize,
    cap: usize,
    hooks: &GcHooks,
  ) -> GcList<Value, ObjHeader> {
    if needed > cap {
      self.grow(cap, cap * 2, hooks)
    } else {
      *self
    }
  }

  /// Allocate a new list which the specified capacity. Mark the existing list as moved
  /// by setting the MSB for capacity and replacing len with a pointer to the
  /// new list
  fn grow(&mut self, cap: usize, new_cap: usize, hooks: &GcHooks) -> GcList<Value, ObjHeader> {
    let new_list = hooks.manage_obj(ListBuilder {
      slice: &self,
      cap: new_cap,
    });

    unsafe {
      self.write_len(new_list);
      self.mark_moved(cap);
    }
    new_list
  }
}

impl<T, H: Mark> Mark for GcList<T, H> {
  /// Mark the list itself as visited
  #[inline]
  fn mark(&self) -> bool {
    self.header().mark()
  }
}

impl<T, H: Marked> Marked for GcList<T, H> {
  /// Is this list marked
  #[inline]
  fn marked(&self) -> bool {
    self.header().marked()
  }
}

impl<T: Trace + DebugHeap, H: Send + Mark + Trace> Trace for GcList<T, H> {
  #[inline]
  fn trace(&self) {
    if self.mark() {
      return;
    }

    self.header().trace();
    match self.state() {
      ListState::Default(_) => self.iter().for_each(|i| i.trace()),
      ListState::Forwarded(gc_list) => gc_list.trace(),
    };
  }

  #[inline]
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
      ListState::Default(_) => self.iter().for_each(|i| i.trace_debug(log)),
      ListState::Forwarded(gc_list) => gc_list.trace_debug(log),
    };
  }
}

impl<T: DebugHeap, H> DebugHeap for GcList<T, H> {
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

impl<T, H> fmt::Pointer for GcList<T, H> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Pointer::fmt(&self.ptr, f)
  }
}

impl<T, H> Copy for GcList<T, H> {}
impl<T, H> Clone for GcList<T, H> {
  fn clone(&self) -> Self {
    *self
  }
}

impl<T, H> Deref for GcList<T, H> {
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

impl<T, H> DerefMut for GcList<T, H> {
  #[inline]
  fn deref_mut(&mut self) -> &mut Self::Target {
    unsafe {
      let data = self.item_mut(0);
      let len = self.len();
      slice::from_raw_parts_mut(data, len)
    }
  }
}

impl<T, H> PartialEq<GcList<T, H>> for GcList<T, H> {
  #[inline]
  fn eq(&self, other: &GcList<T, H>) -> bool {
    ptr::eq(self.as_alloc_ptr(), other.as_alloc_ptr())
  }
}
impl<T, H> Eq for GcList<T, H> {}

impl Display for GcList<Value, ObjHeader> {
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

impl<T: Debug, H> Debug for GcList<T, H> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_list().entries(self.iter()).finish()
  }
}

unsafe impl<T: Send, H: Send> Send for GcList<T, H> {}
unsafe impl<T: Sync, H: Sync> Sync for GcList<T, H> {}

/// A owning reference to a Garbage collector
/// allocated list. Note this list is the same size
/// as a single pointer.
pub struct GcListHandle<T, H>(GcList<T, H>);

impl<T, H> GcListHandle<T, H> {
  /// Create a non owning reference to this list.
  ///
  /// ## Examples
  pub fn value(&self) -> GcList<T, H> {
    self.0
  }

  /// Determine the size of the handle and the pointed to
  /// allocation
  #[inline]
  pub fn size(&self) -> usize {
    make_list_layout::<H, T>(self.0.cap()).size()
  }
}

impl<T> GcListHandle<T, ObjHeader> {
  /// Degrade this handle into
  pub fn degrade(self) -> GcObjectHandle {
    let handle = GcObjectHandle { ptr: self.0.ptr };
    mem::forget(self);
    handle
  }
}

impl<T: Copy, H> GcListHandle<T, H> {
  /// Create a new `GcListHandle` from the provided header
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

      GcListHandle(GcList {
        ptr: NonNull::new_unchecked(buf),
        data: PhantomData,
        header: PhantomData,
      })
    };

    list.0.copy_from_slice(slice);
    list
  }
}

impl<T, H: Unmark> Unmark for GcListHandle<T, H> {
  /// Unmark this allocation as visited, returning
  /// the existing marked status
  #[inline]
  fn unmark(&self) -> bool {
    self.0.header().unmark()
  }
}

impl<T, H: Marked> Marked for GcListHandle<T, H> {
  /// Is this allocation marked
  #[inline]
  fn marked(&self) -> bool {
    self.0.marked()
  }
}

impl<T, H> Deref for GcListHandle<T, H> {
  type Target = [T];

  fn deref(&self) -> &Self::Target {
    self.0.deref()
  }
}

impl<T, H> DerefMut for GcListHandle<T, H> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    self.0.deref_mut()
  }
}

impl<T: DebugHeap, H> DebugHeap for GcListHandle<T, H> {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    self.0.fmt_heap(f, depth)
  }
}

impl<T, H> fmt::Pointer for GcListHandle<T, H> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.0.fmt(f)
  }
}

impl<T: Copy, H: Default> From<&[T]> for GcListHandle<T, H> {
  fn from(slice: &[T]) -> Self {
    GcListHandle::from_slice(slice, slice.len(), H::default())
  }
}

impl<T: DebugHeap, H: Unmark + Marked> Manage for GcListHandle<T, H> {
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

impl<T, H> Drop for GcListHandle<T, H> {
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

impl<'a> AllocateObj<List> for ListBuilder<'a> {
  fn alloc(self) -> AllocObjResult<List> {
    debug_assert!(self.slice.len() <= self.cap);
    let handle = GcListHandle::from_slice(&self.slice, self.cap, ObjHeader::new(ObjectKind::List));

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

  mod list {
    use crate::{hooks::NoContext, val};

    use super::*;

    #[test]
    fn header() {
      let handle = GcListHandle::from_slice(&[1, 2, 3, 4, 5], 5, String::from("header"));
      let list = handle.value();

      assert_eq!(list.header(), "header");
    }

    #[test]
    fn len() {
      let handle = GcListHandle::from_slice(&[1, 2, 3, 4, 5], 5, String::from("header"));
      let list = handle.value();

      assert_eq!(list.len(), 5);
    }

    #[test]
    fn is_empty() {
      let handle1 = GcListHandle::from_slice(&[1, 2, 3, 4, 5], 5, String::from("header"));
      let handle2 = GcListHandle::<i32, String>::from_slice(&[], 5, String::from("header"));
      let list1 = handle1.value();
      let list2 = handle2.value();

      assert!(!list1.is_empty());
      assert!(list2.is_empty());
    }

    #[test]
    fn cap() {
      let handle = GcListHandle::from_slice(&[1, 2, 3, 4, 5], 10, String::from("header"));
      let list = handle.value();

      assert_eq!(list.cap(), 10);
    }

    #[test]
    fn push_with_capacity() {
      let mut context = NoContext::default();
      let hooks = GcHooks::new(&mut context);

      let handle = GcListHandle::from_slice(&[val!(1.0)], 2, ObjHeader::new(ObjectKind::List));
      let mut list = handle.value();

      list.push(val!(3.0), &hooks);
      assert_eq!(list[0], val!(1.0));
      assert_eq!(list[1], val!(3.0));
      assert_eq!(list.len(), 2);
      assert_eq!(list.cap(), 2);
    }

    #[test]
    fn push_without_capacity() {
      let mut context = NoContext::default();
      let hooks = GcHooks::new(&mut context);

      let handle = GcListHandle::from_slice(&[val!(1.0)], 1, ObjHeader::new(ObjectKind::List));
      let mut list = handle.value();

      list.push(val!(3.0), &hooks);
      list.push(val!(5.0), &hooks);
      assert_eq!(list[0], val!(1.0));
      assert_eq!(list[1], val!(3.0));
      assert_eq!(list[2], val!(5.0));
      assert_eq!(list.len(), 3);
      assert_eq!(list.cap(), 4);
    }

    #[test]
    fn pop() {
      let handle = GcListHandle::from_slice(
        &[val!(1.0), val!(2.0), val!(true)],
        3,
        ObjHeader::new(ObjectKind::List),
      );
      let mut list = handle.value();

      assert_eq!(list.pop(), Some(val!(true)));
      assert_eq!(list.len(), 2);

      assert_eq!(list.pop(), Some(val!(2.0)));
      assert_eq!(list.len(), 1);

      assert_eq!(list.pop(), Some(val!(1.0)));
      assert_eq!(list.len(), 0);

      assert_eq!(list.pop(), None);
      assert_eq!(list.len(), 0);
    }

    #[test]
    fn insert_with_capacity() {
      let mut context = NoContext::default();
      let hooks = GcHooks::new(&mut context);

      let handle = GcListHandle::from_slice(&[val!(1.0)], 4, ObjHeader::new(ObjectKind::List));
      let mut list = handle.value();

      assert_eq!(list.insert(0, val!(2.0), &hooks), IndexedResult::Ok(()));

      assert_eq!(list.len(), 2);
      assert_eq!(list.cap(), 4);
      assert_eq!(list[0], val!(2.0));
      assert_eq!(list[1], val!(1.0));

      assert_eq!(list.insert(2, val!(3.0), &hooks), IndexedResult::Ok(()));

      assert_eq!(list.len(), 3);
      assert_eq!(list.cap(), 4);
      assert_eq!(list[0], val!(2.0));
      assert_eq!(list[1], val!(1.0));
      assert_eq!(list[2], val!(3.0));

      assert_eq!(
        list.insert(5, val!(5.0), &hooks),
        IndexedResult::OutOfBounds
      );

      assert_eq!(list.len(), 3);
      assert_eq!(list.cap(), 4);
      assert_eq!(list[0], val!(2.0));
      assert_eq!(list[1], val!(1.0));
      assert_eq!(list[2], val!(3.0));
    }

    #[test]
    fn insert_without_capacity() {
      let mut context = NoContext::default();
      let hooks = GcHooks::new(&mut context);

      let handle = GcListHandle::from_slice(&[val!(1.0)], 1, ObjHeader::new(ObjectKind::List));
      let mut list = handle.value();

      assert_eq!(list.insert(0, val!(2.0), &hooks), IndexedResult::Ok(()));

      assert_eq!(list.len(), 2);
      assert_eq!(list.cap(), 2);
      assert_eq!(list[0], val!(2.0));
      assert_eq!(list[1], val!(1.0));

      assert_eq!(list.insert(2, val!(3.0), &hooks), IndexedResult::Ok(()));

      assert_eq!(list.len(), 3);
      assert_eq!(list.cap(), 4);
      assert_eq!(list[0], val!(2.0));
      assert_eq!(list[1], val!(1.0));
      assert_eq!(list[2], val!(3.0));

      assert_eq!(
        list.insert(5, val!(5.0), &hooks),
        IndexedResult::OutOfBounds
      );

      assert_eq!(list.len(), 3);
      assert_eq!(list.cap(), 4);
      assert_eq!(list[0], val!(2.0));
      assert_eq!(list[1], val!(1.0));
      assert_eq!(list[2], val!(3.0));
    }

    #[test]
    fn remove() {
      let handle = GcListHandle::from_slice(&[val!(1.0), val!(3.0), val!(false)], 3, ObjHeader::new(ObjectKind::List));
      let mut list = handle.value();

      assert_eq!(list.remove(3), IndexedResult::OutOfBounds);
      assert_eq!(list.remove(1), IndexedResult::Ok(val!(3.0)));

      assert_eq!(list.len(), 2);
      assert_eq!(list.cap(), 3);
      assert_eq!(list[0], val!(1.0));
      assert_eq!(list[1], val!(false));

      assert_eq!(list.remove(1), IndexedResult::Ok(val!(false)));

      assert_eq!(list.len(), 1);
      assert_eq!(list.cap(), 3);
      assert_eq!(list[0], val!(1.0));

      assert_eq!(list.remove(0), IndexedResult::Ok(val!(1.0)));

      assert_eq!(list.len(), 0);
      assert_eq!(list.cap(), 3);
    }

    #[test]
    fn remove_forwarded() {
      let mut context = NoContext::default();
      let hooks = GcHooks::new(&mut context);

      let handle = GcListHandle::from_slice(&[val!(1.0)], 1, ObjHeader::new(ObjectKind::List));
      let mut list = handle.value();

      list.push(val!(3.0), &hooks);
      list.push(val!(false), &hooks);

      assert_eq!(list.remove(3), IndexedResult::OutOfBounds);
      assert_eq!(list.remove(1), IndexedResult::Ok(val!(3.0)));

      assert_eq!(list.len(), 2);
      assert_eq!(list.cap(), 4);
      assert_eq!(list[0], val!(1.0));
      assert_eq!(list[1], val!(false));

      assert_eq!(list.remove(1), IndexedResult::Ok(val!(false)));

      assert_eq!(list.len(), 1);
      assert_eq!(list.cap(), 4);
      assert_eq!(list[0], val!(1.0));

      assert_eq!(list.remove(0), IndexedResult::Ok(val!(1.0)));

      assert_eq!(list.len(), 0);
      assert_eq!(list.cap(), 4);
    }
  }

  mod handle {
    use super::*;

    #[test]
    fn from_slice() {
      let list_handle = GcListHandle::from_slice(&[1, 2, 3, 4, 5], 6, String::from("header"));
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
      GcListHandle::from_slice(&[1, 2, 3, 4, 5], 3, String::from("header"));
    }
  }
}
