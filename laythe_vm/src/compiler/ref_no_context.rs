use std::{
  cell::{RefCell, RefMut},
  io::Write,
};

use laythe_core::{
  hooks::GcContext,
  managed::{Allocator, TraceRoot},
};

pub struct RefNoContext<'a> {
  /// A reference to a gc just to allocate
  gc: &'a RefCell<Allocator>,
}

impl<'a> RefNoContext<'a> {
  /// Create a new instance of no context
  pub fn new(gc: &'a RefCell<Allocator>) -> Self {
    Self { gc }
  }
}

impl<'a> TraceRoot for RefNoContext<'a> {
  fn trace(&self) {}

  fn trace_debug(&self, _stdout: &mut dyn Write) {}

  fn can_collect(&self) -> bool {
    false
  }
}

impl<'a> GcContext for RefNoContext<'a> {
  fn gc(&self) -> RefMut<'_, Allocator> {
    self.gc.borrow_mut()
  }
}
