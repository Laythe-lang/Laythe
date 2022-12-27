use crate::{
  managed::{DebugHeap, DebugWrap, Object, Trace},
  value::Value,
};
use std::{
  fmt::{self, Display},
  io::Write,
};

use super::ObjectKind;

#[derive(PartialEq, Eq, Clone)]
pub struct Method {
  receiver: Value,
  method: Value,
}

impl Method {
  #[inline]
  pub fn new(receiver: Value, method: Value) -> Self {
    Self { receiver, method }
  }

  #[inline]
  pub fn receiver(&self) -> Value {
    self.receiver
  }

  #[inline]
  pub fn method(&self) -> Value {
    self.method
  }
}

impl Display for Method {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}.{}", self.receiver(), self.method())
  }
}

impl fmt::Debug for Method {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.fmt_heap(f, 2)
  }
}

impl Trace for Method {
  fn trace(&self) {
    self.receiver.trace();
    self.method.trace();
  }

  fn trace_debug(&self, stdio: &mut dyn Write) {
    self.receiver.trace_debug(stdio);
    self.method.trace_debug(stdio);
  }
}

impl DebugHeap for Method {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    f.debug_struct("Method")
      .field("receiver", &DebugWrap(&self.receiver, depth))
      .field("method", &DebugWrap(&self.method, depth))
      .finish()
  }
}

impl Object for Method {
  fn kind(&self) -> ObjectKind {
    ObjectKind::Method
  }
}
