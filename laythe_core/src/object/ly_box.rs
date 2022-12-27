use fmt::Display;

use crate::{
  managed::{DebugHeap, DebugWrap, Object, Trace},
  value::{Value, VALUE_NIL},
};
use std::{fmt, io::Write};

use super::ObjectKind;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct LyBox {
  pub value: Value,
}

impl LyBox {
  pub fn new(value: Value) -> Self {
    Self { value }
  }
}

impl Default for LyBox {
  fn default() -> Self {
    Self { value: VALUE_NIL }
  }
}

impl Display for LyBox {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<*{} {:p}>", self.value, self)
  }
}

impl Trace for LyBox {
  fn trace(&self) {
    self.value.trace();
  }

  fn trace_debug(&self, stdio: &mut dyn Write) {
    self.value.trace_debug(stdio);
  }
}

impl DebugHeap for LyBox {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    f.debug_struct("Capture")
      .field("value", &DebugWrap(&self.value, depth))
      .finish()
  }
}

unsafe impl Send for LyBox {}

impl Object for LyBox {
  fn kind(&self) -> ObjectKind {
    ObjectKind::LyBox
  }
}
