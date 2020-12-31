use io::Write;
pub use laythe_macro::*;
use std::{
  cell::Cell,
  cmp::Ordering,
  fmt,
  hash::{Hash, Hasher},
  io,
  ops::{Deref, DerefMut},
  ptr::{self, NonNull},
};

pub struct DebugWrap<'a, T>(pub &'a T, pub usize);

impl<'a, T: DebugHeap> fmt::Debug for DebugWrap<'a, T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.0.fmt_heap(f, self.1)
  }
}

pub struct DebugWrapDyn<'a>(pub &'a dyn DebugHeap, pub usize);

impl<'a> fmt::Debug for DebugWrapDyn<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.0.fmt_heap(f, self.1)
  }
}

/// A utility to print debug information to a fixed depth in the Laythe heap
pub trait DebugHeap {
  /// A debugging string for this managed object. Typically just wrapping
  /// wrapping fmt::Debug so we can have dyn Manage
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result;
}

/// An entity that is traceable by the garbage collector
pub trait Trace {
  /// Mark all objects that are reachable from this object
  fn trace(&self);

  /// Mark all objects that are reachable printing debugging information
  /// for each object
  fn trace_debug(&self, log: &mut dyn Write);
}

/// An entity that can provide tracing roots to the garbage collector
pub trait RootTrace {
  /// Mark all objects that are reachable from this object
  fn trace(&self) -> bool;

  /// Mark all objects that are reachable printing debugging information
  /// for each object
  fn trace_debug(&self, log: &mut dyn Write) -> bool;
}

/// An entity that can be managed and collected by the garbage collector.
/// This trait provided debugging capabilities and statistics for the gc.
pub trait Manage: Trace + DebugHeap {
  /// What is the size of this allocation
  fn size(&self) -> usize;

  /// Helper function to get a trait object for Debug Heap
  fn as_debug(&self) -> &dyn DebugHeap;
}

/// The header of an allocation indicate meta data about the object
#[derive(Debug, Default)]
pub struct Header {
  marked: Cell<bool>,
}

#[derive(Debug)]
pub struct Allocation<T: 'static + Trace + ?Sized> {
  header: Header,
  data: T,
}

impl<T: 'static + Manage> Allocation<T> {
  pub fn new(data: T) -> Self {
    Self {
      data,
      header: Header {
        marked: Cell::new(false),
      },
    }
  }

  pub fn size(&self) -> usize {
    self.data.size()
  }

  pub fn as_debug(&self) -> &dyn DebugHeap {
    self.data.as_debug()
  }
}

impl Allocation<dyn Manage> {
  pub fn size(&self) -> usize {
    self.data.size()
  }

  pub fn as_debug(&self) -> &dyn DebugHeap {
    self.data.as_debug()
  }
}

impl<T: 'static + Manage + ?Sized> Allocation<T> {
  pub fn mark(&self) -> bool {
    self.header.marked.replace(true)
  }

  pub fn unmark(&self) -> bool {
    self.header.marked.replace(false)
  }

  pub fn marked(&self) -> bool {
    self.header.marked.get()
  }
}

impl<T: 'static + Manage + ?Sized> DebugHeap for Allocation<T> {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    self.data.fmt_heap(f, depth)
  }
}

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

  pub fn to_usize(&self) -> usize {
    self.ptr.as_ptr() as *const () as usize
  }

  pub fn obj(&self) -> &Allocation<T> {
    unsafe { self.ptr.as_ref() }
  }

  pub fn obj_mut(&mut self) -> &mut Allocation<T> {
    unsafe { self.ptr.as_mut() }
  }
}

impl<T: 'static + Manage> Gc<T> {
  pub fn dangling() -> Gc<T> {
    Self {
      ptr: NonNull::dangling(),
    }
  }
}

impl<T: 'static + Manage> Trace for Gc<T> {
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

impl Trace for Gc<dyn Manage> {
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

impl DebugHeap for Gc<dyn Manage> {
  fn fmt_heap(&self, f: &mut fmt::Formatter, _: usize) -> fmt::Result {
    f.write_str("*()")
  }
}

impl Manage for Gc<dyn Manage> {
  fn size(&self) -> usize {
    self.obj().size()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

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

  fn deref(&self) -> &T {
    &self.obj().data
  }
}

impl<T: 'static + Manage> DerefMut for Gc<T> {
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
  fn partial_cmp(&self, other: &Gc<T>) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl<T: 'static + Manage> Ord for Gc<T> {
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

#[cfg(test)]
mod test {
  use super::*;
  mod fmt_heap {
    use std::mem;

    use super::*;

    struct Test {
      val: usize,
      next: Option<Gc<Test>>,
    }

    impl Manage for Test {
      fn size(&self) -> usize {
        mem::size_of::<Self>()
      }

      fn as_debug(&self) -> &dyn DebugHeap {
        todo!()
      }
    }

    impl Trace for Test {
      fn trace(&self) {}

      fn trace_debug(&self, _: &mut dyn Write) {}
    }

    impl DebugHeap for Test {
      fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
        f.debug_struct("Test")
          .field("next", &DebugWrap(&self.next, depth - 1))
          .field("val", &self.val)
          .finish()
      }
    }

    #[test]
    fn managed() {
      let inner = Test { next: None, val: 1 };
      let mut alloc_inner = Box::new(Allocation::new(inner));
      let managed_inner = Gc::from(unsafe { NonNull::new_unchecked(&mut *alloc_inner) });

      let outer = Test {
        next: Some(managed_inner),
        val: 2,
      };
      let mut alloc_outer = Box::new(Allocation::new(outer));
      let managed_outer = Gc::from(unsafe { NonNull::new_unchecked(&mut *alloc_outer) });

      let mut output = String::new();
      fmt::write(
        &mut output,
        format_args!("{:?}", DebugWrap(&managed_outer, 0)),
      )
      .expect("failure");

      assert_eq!(output, "*()");

      let mut output = String::new();
      fmt::write(
        &mut output,
        format_args!("{:?}", DebugWrap(&managed_outer, 1)),
      )
      .expect("failure");

      assert_eq!(output, "*Test { next: Some(*()), val: 2 }");

      let mut output = String::new();
      fmt::write(
        &mut output,
        format_args!("{:?}", DebugWrap(&managed_outer, 3)),
      )
      .expect("failure");

      assert_eq!(
        output,
        "*Test { next: Some(*Test { next: None, val: 1 }), val: 2 }"
      )
    }
  }
}
