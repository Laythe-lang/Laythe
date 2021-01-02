use std::{
  cmp::Ordering,
  fmt,
  hash::{Hash, Hasher},
  io::Write,
  ops::{Deref, DerefMut},
  ptr::{self, NonNull},
};

use super::{
  allocation::Allocation,
  manage::{DebugHeap, DebugWrap, Manage, Trace},
};

pub struct Gc<T: 'static + Manage + ?Sized> {
  ptr: NonNull<Allocation<T>>,
}

impl<T: 'static + Manage + ?Sized> Gc<T> {
  /// Get a static reference to the underlying data
  ///
  /// # Safety
  /// This object must be keep alive otherwise this can
  /// lead to dangling pointer error. This effectively
  /// completely circumvents rust type system completely
  pub unsafe fn deref_static(&self) -> &'static T {
    &(*self.ptr.as_ptr()).data
  }

  /// Return the underlying pointer as a usize. This is
  /// used by the nan boxing functionality
  pub fn to_usize(&self) -> usize {
    self.ptr.as_ptr() as *const () as usize
  }

  /// Return an immutable reference to the pointed
  /// to allocation.
  pub(crate) fn obj(&self) -> &Allocation<T> {
    unsafe { self.ptr.as_ref() }
  }

  /// Return a mutable reference to the pointed
  /// to allocation.
  fn obj_mut(&mut self) -> &mut Allocation<T> {
    unsafe { self.ptr.as_mut() }
  }
}

impl<T: 'static + Manage> Gc<T> {
  /// Create a new Gc pointer that is dangling but well-aligned
  pub fn dangling() -> Gc<T> {
    Self {
      ptr: NonNull::dangling(),
    }
  }
}

impl<T: 'static + Manage + Send> Trace for Gc<T> {
  fn trace(&self) {
    if self.obj().mark() {
      return;
    }

    self.obj().data.trace();
  }

  fn trace_debug(&self, stdout: &mut dyn Write) {
    if self.obj().mark() {
      return;
    }

    stdout
      .write_fmt(format_args!(
        "{:p} mark {:?}\n",
        &*self.obj(),
        DebugWrap(self, 1)
      ))
      .expect("unable to write to stdout");
    stdout.flush().expect("unable to flush stdout");

    self.obj().data.trace_debug(stdout);
  }
}

impl<T: 'static + Manage> DebugHeap for Gc<T> {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    if depth == 0 {
      f.write_str("*()")
    } else {
      f.write_fmt(format_args!("*{:?}", DebugWrap(self.obj(), depth)))
    }
  }
}

impl<T: 'static + Manage> Manage for Gc<T> {
  fn size(&self) -> usize {
    self.obj().size()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

unsafe impl<T: 'static + Manage> Send for Gc<T> {}

// impl Trace for Gc<dyn Manage> {
//   fn trace(&self) {
//     if self.obj().mark() {
//       return;
//     }

//     self.obj().data.trace();
//   }

//   fn trace_debug(&self, stdout: &mut dyn Write) {
//     if self.obj().mark() {
//       return;
//     }

//     stdout
//       .write_fmt(format_args!(
//         "{:p} mark {:?}\n",
//         &*self.obj(),
//         DebugWrap(self, 1)
//       ))
//       .expect("unable to write to stdout");
//     stdout.flush().expect("unable to flush stdout");

//     self.obj().data.trace_debug(stdout);
//   }
// }

// impl DebugHeap for Gc<dyn Manage> {
//   fn fmt_heap(&self, f: &mut fmt::Formatter, _: usize) -> fmt::Result {
//     f.write_str("*()")
//   }
// }

// impl Manage for Gc<dyn Manage> {
//   fn size(&self) -> usize {
//     self.obj().size()
//   }

//   fn as_debug(&self) -> &dyn DebugHeap {
//     self
//   }
// }

impl<T: 'static + Manage + ?Sized> From<NonNull<Allocation<T>>> for Gc<T> {
  fn from(ptr: NonNull<Allocation<T>>) -> Self {
    Self { ptr }
  }
}

impl<T: 'static + Manage + ?Sized> Copy for Gc<T> {}
impl<T: 'static + Manage + ?Sized> Clone for Gc<T> {
  fn clone(&self) -> Gc<T> {
    *self
  }
}

impl<T: 'static + Manage> Deref for Gc<T> {
  type Target = T;

  #[inline]
  fn deref(&self) -> &T {
    &self.obj().data
  }
}

impl<T: 'static + Manage> DerefMut for Gc<T> {
  #[inline]
  fn deref_mut(&mut self) -> &mut T {
    &mut self.obj_mut().data
  }
}

impl<T: 'static + Manage> PartialEq for Gc<T> {
  #[inline]
  fn eq(&self, other: &Gc<T>) -> bool {
    let left_inner: &T = &*self;
    let right_inner: &T = &*other;

    ptr::eq(left_inner, right_inner)
  }
}

impl<T: 'static + Manage> Eq for Gc<T> {}

impl<T: 'static + Manage> Hash for Gc<T> {
  #[inline]
  fn hash<H: Hasher>(&self, state: &mut H) {
    ptr::hash(self.ptr.as_ptr(), state)
  }
}

impl<T: 'static + Manage> PartialOrd for Gc<T> {
  #[inline]
  fn partial_cmp(&self, other: &Gc<T>) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl<T: 'static + Manage> Ord for Gc<T> {
  #[inline]
  fn cmp(&self, other: &Gc<T>) -> Ordering {
    self.ptr.cmp(&other.ptr)
  }
}

impl<T: 'static + Manage + fmt::Display> fmt::Display for Gc<T> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let inner: &T = &*self;
    write!(f, "{}", inner)
  }
}

impl<T: 'static + Manage + fmt::Debug> fmt::Debug for Gc<T> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let inner: &T = &*self;

    f.debug_struct("Gc").field("ptr", inner).finish()
  }
}
