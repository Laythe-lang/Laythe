extern crate proc_macro;

use crate::io::StdIo;
pub use spacelox_macro::*;
use std::{
  cell::Cell,
  cmp::Ordering,
  fmt,
  hash::{Hash, Hasher},
  ops::{Deref, DerefMut},
  ptr::{self, NonNull},
};

/// An entity that is traceable by the garbage collector
pub trait Trace {
  /// Mark all objects that are reachable from this object
  fn trace(&self) -> bool;

  /// Mark all objects that are reachable printing debugging information
  /// for each object
  fn trace_debug(&self, stdio: &dyn StdIo) -> bool;
}

/// An entity that can be managed and collected by the garbage collector.
/// This trait provided debugging capabilities and statistics for the gc.
pub trait Manage: Trace {
  /// What allocation type is
  fn alloc_type(&self) -> &str;

  /// A debugging string for this managed object. Typically just wrapping
  /// wrapping fmt::Debug so we can have dyn Manage
  fn debug(&self) -> String;

  /// A debugging string that doesn't use any other `Manage` objects.
  /// This is to avoid issues where the child object has already been freed
  fn debug_free(&self) -> String;

  /// What is the size of this allocation
  fn size(&self) -> usize;
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
}

impl Allocation<dyn Manage> {
  pub fn size(&self) -> usize {
    self.data.size()
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

  pub fn alloc_type(&self) -> &str {
    self.data.alloc_type()
  }

  pub fn debug(&self) -> String {
    self.data.debug()
  }

  pub fn debug_free(&self) -> String {
    self.data.debug_free()
  }
}

pub struct Managed<T: 'static + Manage + ?Sized> {
  ptr: NonNull<Allocation<T>>,
}

impl<T: 'static + Manage + ?Sized> Managed<T> {
  /// Note this method is hilariously unsafe
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

impl<T: 'static + Manage> Managed<T> {
  pub fn clone_dyn(self) -> Managed<dyn Manage> {
    Managed {
      ptr: NonNull::from(self.obj()) as NonNull<Allocation<dyn Manage>>,
    }
  }

  pub fn size(self) -> usize {
    self.obj().size()
  }
}

impl<T: 'static + Manage> Trace for Managed<T> {
  fn trace(&self) -> bool {
    if self.obj().mark() {
      return true;
    }

    self.obj().data.trace();
    true
  }

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    if self.obj().mark() {
      return true;
    }

    stdio.println(&format!("{:p} mark {}", &*self.obj(), self.debug()));

    self.obj().data.trace_debug(stdio);
    true
  }
}

impl<T: 'static + Manage> Manage for Managed<T> {
  fn alloc_type(&self) -> &str {
    self.obj().data.alloc_type()
  }

  fn debug(&self) -> String {
    self.obj().data.debug()
  }

  fn debug_free(&self) -> String {
    self.obj().data.debug_free()
  }

  fn size(&self) -> usize {
    self.obj().size()
  }
}

impl Trace for Managed<dyn Manage> {
  fn trace(&self) -> bool {
    if self.obj().mark() {
      return true;
    }

    self.obj().data.trace();
    true
  }

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    if self.obj().mark() {
      return true;
    }

    stdio.println(&format!("{:p} mark {}", &*self.obj(), self.debug()));

    self.obj().data.trace_debug(stdio);
    true
  }
}

impl Manage for Managed<dyn Manage> {
  fn alloc_type(&self) -> &str {
    self.obj().data.alloc_type()
  }

  fn debug(&self) -> String {
    self.obj().data.debug()
  }

  fn debug_free(&self) -> String {
    self.obj().data.debug_free()
  }

  fn size(&self) -> usize {
    self.obj().size()
  }
}

impl<T: 'static + Manage + ?Sized> From<NonNull<Allocation<T>>> for Managed<T> {
  fn from(ptr: NonNull<Allocation<T>>) -> Self {
    Self { ptr }
  }
}

impl<T: 'static + Manage + ?Sized> Copy for Managed<T> {}
impl<T: 'static + Manage + ?Sized> Clone for Managed<T> {
  fn clone(&self) -> Managed<T> {
    *self
  }
}

impl<T: 'static + Manage> Deref for Managed<T> {
  type Target = T;

  fn deref(&self) -> &T {
    &self.obj().data
  }
}

impl<T: 'static + Manage> DerefMut for Managed<T> {
  fn deref_mut(&mut self) -> &mut T {
    &mut self.obj_mut().data
  }
}

impl<T: 'static + Manage> PartialEq for Managed<T> {
  #[inline]
  fn eq(&self, other: &Managed<T>) -> bool {
    let left_inner: &T = &*self;
    let right_inner: &T = &*other;

    ptr::eq(left_inner, right_inner)
  }
}

impl<T: 'static + Manage> Eq for Managed<T> {}

impl<T: 'static + Manage> Hash for Managed<T> {
  #[inline]
  fn hash<H: Hasher>(&self, state: &mut H) {
    ptr::hash(self.ptr.as_ptr(), state)
  }
}

impl<T: 'static + Manage> PartialOrd for Managed<T> {
  fn partial_cmp(&self, other: &Managed<T>) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl<T: 'static + Manage> Ord for Managed<T> {
  fn cmp(&self, other: &Managed<T>) -> Ordering {
    self.ptr.cmp(&other.ptr)
  }
}

impl<T: 'static + Manage + fmt::Display> fmt::Display for Managed<T> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let inner: &T = &*self;
    write!(f, "{}", inner)
  }
}

impl<T: 'static + Manage + fmt::Debug> fmt::Debug for Managed<T> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let inner: &T = &*self;

    f.debug_struct("Managed").field("ptr", inner).finish()
  }
}

pub fn make_managed<T: 'static + Manage>(data: T) -> (Managed<T>, Box<Allocation<T>>) {
  let mut alloc = Box::new(Allocation::new(data));
  let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };

  (Managed::from(ptr), alloc)
}
