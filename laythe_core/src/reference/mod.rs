mod obj_reference;

use crate::managed::{
  AllocResult, Allocation, DebugHeap, DebugWrap, Manage, Mark, Trace, TraceAny,
};
pub use obj_reference::{ObjRef, Object, ObjectHandle, ObjectRef};
use std::{
  cmp::Ordering,
  fmt,
  hash::{Hash, Hasher},
  io::Write,
  ops::{Deref, DerefMut},
  ptr::{self, NonNull},
};

pub struct Ref<T: 'static> {
  ptr: NonNull<Allocation<T>>,
}

impl<T: 'static + Trace + DebugHeap> Ref<T> {
  pub fn alloc_result(data: T) -> AllocResult<Ref<T>> {
    let mut handle = Box::new(Allocation::new(data));
    let ptr = unsafe { NonNull::new_unchecked(&mut *handle) };
    let reference = Ref::from(ptr);
    let size = handle.size();

    let handle = handle as Box<dyn Manage>;
    AllocResult {
      handle,
      size,
      reference,
    }
  }
}

impl<T: 'static> Ref<T> {
  /// Get a static reference to the underlying data
  ///
  /// # Safety
  /// This object must be keep alive otherwise this can
  /// lead to dangling pointer error. This effectively
  /// completely circumvents rust type system completely
  pub unsafe fn deref_static(&self) -> &'static T { unsafe {
    &(*self.ptr.as_ptr()).data
  }}

  /// Return the underlying pointer as a usize. This is
  /// used by the nan boxing functionality
  pub fn to_usize(self) -> usize {
    self.ptr.as_ptr() as *const () as usize
  }

  /// Return an immutable reference to the pointed
  /// to allocation.
  fn obj(&self) -> &Allocation<T> {
    unsafe { self.ptr.as_ref() }
  }

  /// Return a mutable reference to the pointed
  /// to allocation.
  fn obj_mut(&mut self) -> &mut Allocation<T> {
    unsafe { self.ptr.as_mut() }
  }
}

impl<T: 'static + Trace + DebugHeap> Trace for Ref<T> {
  #[inline]
  fn trace(&self) {
    if self.obj().mark() {
      return;
    }

    self.obj().data.trace();
  }

  fn trace_debug(&self, log: &mut dyn Write) {
    if self.obj().mark() {
      return;
    }

    log
      .write_fmt(format_args!(
        "{:p} mark {:?}\n",
        self.obj(),
        DebugWrap(self, 1)
      ))
      .expect("unable to write to stdout");
    log.flush().expect("unable to flush stdout");

    self.obj().data.trace_debug(log);
  }
}

impl<T: 'static + Trace + DebugHeap> TraceAny for Ref<T> {
  fn as_trace(&self) -> &dyn Trace {
    self
  }

  fn as_any(&self) -> &dyn std::any::Any {
    self
  }

  fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
    self
  }
}

impl<T: 'static + DebugHeap> DebugHeap for Ref<T> {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    if depth == 0 {
      f.write_fmt(format_args!("{:p}", self.ptr))
    } else {
      f.write_fmt(format_args!(
        "{:?}",
        DebugWrap(self.obj(), depth.saturating_sub(1))
      ))
    }
  }
}

impl<T> fmt::Pointer for Ref<T> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.ptr.fmt(f)
  }
}

unsafe impl<T: 'static + Trace> Send for Ref<T> {}
unsafe impl<T: 'static + Trace> Sync for Ref<T> {}

impl<T: 'static + Trace> From<NonNull<Allocation<T>>> for Ref<T> {
  fn from(ptr: NonNull<Allocation<T>>) -> Self {
    Self { ptr }
  }
}

impl<T: 'static> Copy for Ref<T> {}
impl<T: 'static> Clone for Ref<T> {
  fn clone(&self) -> Ref<T> {
    *self
  }
}

impl<T: 'static> Deref for Ref<T> {
  type Target = T;

  #[inline]
  fn deref(&self) -> &T {
    &self.obj().data
  }
}

impl<T: 'static> DerefMut for Ref<T> {
  #[inline]
  fn deref_mut(&mut self) -> &mut T {
    &mut self.obj_mut().data
  }
}

impl<T> PartialEq for Ref<T> {
  #[inline]
  fn eq(&self, other: &Ref<T>) -> bool {
    let left_inner: &T = self;
    let right_inner: &T = other;

    ptr::eq(left_inner, right_inner)
  }
}

impl<T> Eq for Ref<T> {}

impl<T> Hash for Ref<T> {
  #[inline]
  fn hash<H: Hasher>(&self, state: &mut H) {
    ptr::hash(self.ptr.as_ptr(), state)
  }
}

impl<T> PartialOrd for Ref<T> {
  #[inline]
  fn partial_cmp(&self, other: &Ref<T>) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl<T> Ord for Ref<T> {
  #[inline]
  fn cmp(&self, other: &Ref<T>) -> Ordering {
    self.ptr.cmp(&other.ptr)
  }
}

impl<T: fmt::Display> fmt::Display for Ref<T> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let inner: &T = self;
    write!(f, "{inner}")
  }
}

impl<T: fmt::Debug> fmt::Debug for Ref<T> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let inner: &T = self;

    f.debug_struct("Gc").field("ptr", inner).finish()
  }
}
