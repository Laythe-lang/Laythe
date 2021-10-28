use fmt::Display;

use crate::{
  managed::{DebugHeap, DebugWrap, Manage, Object, Trace},
  value::Value,
};
use std::{fmt, io::Write, mem};

use super::ObjectKind;

#[derive(PartialEq, Clone, Debug)]
pub struct Capture {
  pub value: Value,
}

impl Capture {
  pub fn new(value: Value) -> Self {
    Self { value }
  }
}

impl Display for Capture {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    // TODO which we indicate this is an capture somehow
    write!(f, "{}", self.value)
  }
}

impl Trace for Capture {
  fn trace(&self) {
    self.value.trace();
  }

  fn trace_debug(&self, stdio: &mut dyn Write) {
    self.value.trace_debug(stdio);
  }
}

impl DebugHeap for Capture {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    f.debug_struct("Capture")
      .field("value", &DebugWrap(&self.value, depth))
      .finish()
  }
}

impl Manage for Capture {
  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

unsafe impl Send for Capture {}

impl Object for Capture {
  fn kind(&self) -> ObjectKind {
    ObjectKind::Capture
  }
}
