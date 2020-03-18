use std::{
  cell::Cell,
  fmt,
  hash::{Hash, Hasher},
  ops::{Deref, DerefMut},
  ptr::{self, NonNull},
};

/// An entity that is traceable by the garbage collector
pub trait Trace {
  /// Mark all objects that are reachable from this object
  fn trace(&self, mark_obj: &mut dyn FnMut(Managed<dyn Manageable>)) -> bool;
}

pub trait Manageable: Trace {
  /// What allocation type is
  fn alloc_type(&self) -> &str;

  /// What allocation type is
  fn debug(&self) -> String;
}

#[derive(Debug, Default)]
pub struct Header {
  marked: Cell<bool>,
}

#[derive(Debug)]
pub struct Allocation<T: 'static + Trace + ?Sized> {
  header: Header,
  data: T,
}

impl<T: 'static + Trace> Allocation<T> {
  pub fn new(data: T) -> Self {
    Self {
      data,
      header: Header::default(),
    }
  }
}

impl<T: 'static + Manageable + ?Sized> Allocation<T> {
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
}

pub struct Managed<T: 'static + Manageable + ?Sized> {
  ptr: NonNull<Allocation<T>>,
}

impl<T: 'static + Manageable + ?Sized> Managed<T> {
  pub fn obj(&self) -> &Allocation<T> {
    unsafe { self.ptr.as_ref() }
  }

  pub fn obj_mut(&mut self) -> &mut Allocation<T> {
    unsafe { self.ptr.as_mut() }
  }
}

impl<T: 'static + Manageable> Managed<T> {
  pub fn clone_dyn(&self) -> Managed<dyn Manageable> {
    Managed {
      ptr: NonNull::from(self.obj()) as NonNull<Allocation<dyn Manageable>>,
    }
  }
}

impl<T: 'static + Manageable> Trace for Managed<T> {
  fn trace(&self, mark: &mut dyn FnMut(Managed<dyn Manageable>)) -> bool {
    self.obj().data.trace(mark);
    true
  }
}

impl<T: 'static + Manageable> Manageable for Managed<T> {
  fn alloc_type(&self) -> &str {
    self.obj().data.alloc_type()
  }

  fn debug(&self) -> String {
    self.obj().data.debug()
  }
}

impl Trace for Managed<dyn Manageable> {
  fn trace(&self, mark: &mut dyn FnMut(Managed<dyn Manageable>)) -> bool {
    self.obj().data.trace(mark);
    true
  }
}

impl Manageable for Managed<dyn Manageable> {
  fn alloc_type(&self) -> &str {
    self.obj().data.alloc_type()
  }

  fn debug(&self) -> String {
    self.obj().data.debug()
  }
}

impl<T: 'static + Manageable + ?Sized> From<NonNull<Allocation<T>>> for Managed<T> {
  fn from(fun: NonNull<Allocation<T>>) -> Self {
    Self { ptr: fun }
  }
}

impl<T: 'static + Manageable + ?Sized> Copy for Managed<T> {}
impl<T: 'static + Manageable + ?Sized> Clone for Managed<T> {
  fn clone(&self) -> Managed<T> {
    *self
  }
}

impl<T: 'static + Manageable> Deref for Managed<T> {
  type Target = T;

  fn deref(&self) -> &T {
    &self.obj().data
  }
}

impl<T: 'static + Manageable> DerefMut for Managed<T> {
  fn deref_mut(&mut self) -> &mut T {
    &mut self.obj_mut().data
  }
}

impl<T: 'static + PartialEq + Manageable> PartialEq for Managed<T> {
  fn eq(&self, other: &Managed<T>) -> bool {
    let left_inner: &T = &*self;
    let right_inner: &T = &*other;

    if ptr::eq(left_inner, right_inner) {
      return true;
    }

    left_inner.eq(right_inner)
  }
}
impl<T: 'static + Eq + Manageable> Eq for Managed<T> {}

impl<T: 'static + Hash + Manageable> Hash for Managed<T> {
  fn hash<H: Hasher>(&self, state: &mut H) {
    let inner: &T = &*self;
    inner.hash(state);
  }
}

impl<T: 'static + Manageable + fmt::Debug> fmt::Debug for Managed<T> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let inner: &T = &*self;

    f.debug_struct("Managed").field("ptr", inner).finish()
  }
}
