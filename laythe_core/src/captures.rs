use std::{fmt, mem};

use crate::{
  hooks::GcHooks,
  managed::{DebugHeap, DebugWrap, GcObj, Trace, Tuple, Manage},
  object::LyBox,
  val,
  value::Value,
};

#[derive(PartialEq, Debug, Clone, Copy)]
pub struct Captures(Tuple);

impl Captures {
  pub fn new(hooks: &GcHooks, captures: &[GcObj<LyBox>]) -> Self {
    Self(hooks.manage_tuple(&captures.iter().map(|c| val!(*c)).collect::<Vec<Value>>()))
  }

  #[inline]
  pub fn len(&self) -> usize {
    self.0.len()
  }

  #[inline]
  pub fn get_capture(&self, index: usize) -> GcObj<LyBox> {
    self.0[index].to_obj().to_box()
  }

  #[inline]
  pub fn get_capture_value(&self, index: usize) -> Value {
    self.0[index].to_obj().to_box().value
  }

  #[inline]
  pub fn set_capture_value(&mut self, index: usize, value: Value) {
    self.0[index].to_obj().to_box().value = value;
  }
}

impl Trace for Captures {
  fn trace(&self) {
    self.0.trace()
  }

  fn trace_debug(&self, log: &mut dyn std::io::Write) {
    self.0.trace_debug(log)
  }
}

impl DebugHeap for Captures {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    f.debug_struct("Captures")
      .field("0", &DebugWrap(&&*self.0, depth))
      .finish()
  }
}


impl Manage for Captures {
  fn size(&self) -> usize {
    mem::size_of::<Self>() + mem::size_of::<Value>() * self.len()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}