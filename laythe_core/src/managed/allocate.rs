use crate::reference::ObjectHandle;

use super::{Manage, Trace};

/// Define how a struct should be allocated by the garbage collector
pub trait Allocate<R: Trace> {
  // The handle to manage this allocated object
  fn alloc(self) -> AllocResult<R>;
}

pub struct AllocResult<R> {
  pub handle: Box<dyn Manage>,
  pub size: usize,
  pub reference: R,
}

impl<R> AllocResult<R> {
  pub fn new(handle: Box<dyn Manage>, size: usize, reference: R) -> Self {
    Self {
      handle,
      size,
      reference,
    }
  }
}

/// Define how a object should be allocated by the garbage collector
pub trait AllocateObj<R: Trace> {
  // The handle to manage this allocated object
  fn alloc(self) -> AllocObjResult<R>;
}

/// The result of an allocation including it's handle
/// it's size and a client reference
pub struct AllocObjResult<R> {
  pub handle: ObjectHandle,
  pub size: usize,
  pub reference: R,
}

impl<R> AllocObjResult<R> {
  pub fn new(handle: ObjectHandle, size: usize, reference: R) -> Self {
    Self {
      handle,
      size,
      reference,
    }
  }
}
