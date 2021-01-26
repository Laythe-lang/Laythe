use crate::value::Value;
use laythe_env::managed::{DebugHeap, DebugWrap, Manage, Trace};
use std::{fmt, io::Write, mem};

#[derive(PartialEq, Clone)]
pub struct Method {
  receiver: Value,
  method: Value,
}

impl Method {
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

impl fmt::Debug for Method {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.fmt_heap(f, 1)
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
    let depth = depth.saturating_sub(1);

    f.debug_struct("Method")
      .field("receiver", &DebugWrap(&self.receiver, depth))
      .field("method", &DebugWrap(&self.method, depth))
      .finish()
  }
}

impl Manage for Method {
  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}
