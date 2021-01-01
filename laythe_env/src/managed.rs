use io::Write;
pub use laythe_macro::*;
use std::{
  cmp::Ordering,
  fmt,
  hash::{Hash, Hasher},
  io, mem,
  ops::{Deref, DerefMut},
  ptr::{self, NonNull},
  sync::atomic::{self, AtomicBool},
};

/// A wrapper struct to print debug information to
/// a provided depth. This is to handled cycles in
/// managed Gc pointers
pub struct DebugWrap<'a, T>(pub &'a T, pub usize);

impl<'a, T: DebugHeap> fmt::Debug for DebugWrap<'a, T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.0.fmt_heap(f, self.1)
  }
}

/// A wrapper struct to print debug information to
/// a provided depth. This is to handled cycles in
/// managed Gc pointers
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
  // has this allocation been marked by the garbage collector
  marked: AtomicBool,
}

#[derive(Debug)]
/// An allocation onto the Laythe Heap. This struct
/// attaches a header to all objects the GC uses to
/// manage the object
pub struct Allocation<T: 'static + Trace + ?Sized> {
  // The object header
  header: Header,

  // The underlying date being managed
  data: T,
}

impl<T: 'static + Manage> Allocation<T> {
  /// Create a new allocation from a struct that is Manage
  ///
  /// # Examples
  /// ```
  /// use laythe_env::Managed::Allocation;
  /// use smol_str::SmolStr;
  ///
  /// let s = SmolStr::from("example");
  /// let alloc = Allocation::new(s);
  /// ```
  pub fn new(data: T) -> Self {
    Self {
      data,
      header: Header {
        marked: AtomicBool::new(false),
      },
    }
  }

  /// What is the size of this allocation in bytes
  pub fn size(&self) -> usize {
    self.data.size() + mem::size_of::<Header>()
  }

  /// Get a trait object is can be logged for the heap
  pub fn as_debug(&self) -> &dyn DebugHeap {
    self.data.as_debug()
  }
}

impl Allocation<dyn Manage> {
  /// What is the size of this allocation in bytes
  pub fn size(&self) -> usize {
    self.data.size() + mem::size_of::<Header>()
  }

  /// Get a trait object is can be logged for the heap
  pub fn as_debug(&self) -> &dyn DebugHeap {
    self.data.as_debug()
  }
}

impl<T: 'static + Manage + ?Sized> Allocation<T> {
  /// Mark this allocation as visited, returning
  /// the existing marked status
  pub fn mark(&self) -> bool {
    self
      .header
      .marked
      .compare_and_swap(false, true, atomic::Ordering::Release)
  }

  /// Unmark this allocation as visited, returning 
  /// the existing marked status
  pub fn unmark(&self) -> bool {
    self
      .header
      .marked
      .compare_and_swap(true, false, atomic::Ordering::Release)
  }

  /// Is this allocation marked
  pub fn marked(&self) -> bool {
    self.header.marked.load(atomic::Ordering::Acquire)
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
