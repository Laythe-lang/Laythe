use crate::{
  managed::{AllocResult, Allocate, DebugHeap, Trace},
  Ref,
};

/// A single marker for an entity that is waiting on a channel
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ChannelWaiter {
  /// Is this waiter runnable
  runnable: bool,
}

impl ChannelWaiter {
  /// Create a new waiter
  pub fn new(runnable: bool) -> Self {
    Self { runnable }
  }

  /// Is this waiter runnable
  pub fn is_runnable(&self) -> bool {
    self.runnable
  }

  /// Set this waiters are potentially runnable
  pub fn set_runnable(&mut self, runnable: bool) {
    self.runnable = runnable
  }
}

impl Trace for ChannelWaiter {}
impl DebugHeap for ChannelWaiter {
  fn fmt_heap(&self, f: &mut std::fmt::Formatter, _: usize) -> std::fmt::Result {
    f.debug_struct("ChannelWaiter")
      .field("runnable", &self.runnable)
      .finish()
  }
}

impl Allocate<Ref<Self>> for ChannelWaiter {
  fn alloc(self) -> AllocResult<Ref<Self>> {
    Ref::alloc_result(self)
  }
}
