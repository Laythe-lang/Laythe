use std::fmt;

use crate::{
  collections::Array,
  hooks::GcHooks,
  managed::{DebugHeap, DebugWrap, Header, Trace},
  object::LyBox,
  reference::ObjRef,
  value::Value,
};

#[derive(PartialEq, Eq, Clone, Copy)]
pub struct Captures(Array<ObjRef<LyBox>, Header>);

impl Captures {
  pub fn new(captures: Array<ObjRef<LyBox>, Header>) -> Self {
    Self(captures)
  }

  pub fn build(hooks: &GcHooks, captures: &[ObjRef<LyBox>]) -> Self {
    Self(hooks.manage(captures))
  }

  #[inline]
  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }

  #[inline]
  pub fn len(&self) -> usize {
    self.0.len()
  }

  #[inline]
  pub fn get_capture(&self, index: usize) -> ObjRef<LyBox> {
    self.0[index]
  }

  #[inline]
  pub fn get_capture_value(&self, index: usize) -> Value {
    self.0[index].value
  }

  #[inline]
  pub fn set_capture_value(&mut self, index: usize, value: Value) {
    self.0[index].value = value;
  }
}

impl Trace for Captures {
  #[inline]
  fn trace(&self) {
    self.0.trace()
  }

  fn trace_debug(&self, log: &mut dyn std::io::Write) {
    self.0.trace_debug(log)
  }
}

impl DebugHeap for Captures {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    f.debug_tuple("Captures")
      .field(&DebugWrap(&&*self.0, depth))
      .finish()
  }
}

impl fmt::Debug for Captures {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.fmt_heap(f, 2)
  }
}
