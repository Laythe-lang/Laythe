use crate::{
  managed::{AllocResult, Allocate, DebugHeap, Trace, TraceAny},
  Ref,
};
use std::fmt::Debug;

/// A single marker for an entity that is waiting on a channel
pub struct ChannelWaiter {
  /// Is this waiter runnable
  runnable: bool,

  /// What is waiting on this channel
  waiter: Option<Box<dyn TraceAny>>,
}

impl ChannelWaiter {
  /// Create a new waiter
  pub fn new(runnable: bool) -> Self {
    Self {
      runnable,
      waiter: None,
    }
  }

  /// Is this waiter runnable
  pub fn is_runnable(&self) -> bool {
    self.runnable
  }

  /// Set this waiters are potentially runnable
  pub fn set_runnable(&mut self, runnable: bool) {
    self.runnable = runnable
  }

  pub fn set_waiter<T: 'static + TraceAny>(&mut self, value: T) {
    self.waiter = Some(Box::new(value))
  }

  pub fn get_waiter<T: 'static + TraceAny>(&self) -> Option<&T> {
    self
      .waiter
      .as_ref()
      .and_then(|waiter| waiter.as_any().downcast_ref())
  }

  pub fn get_waiter_mut<T: 'static + TraceAny>(&mut self) -> Option<&mut T> {
    self
      .waiter
      .as_mut()
      .and_then(|waiter| waiter.as_any_mut().downcast_mut())
  }
}

impl Trace for ChannelWaiter {
  fn trace(&self) {
    if let Some(waiter) = self.waiter.as_ref() {
      waiter.as_trace().trace()
    }
  }

  fn trace_debug(&self, log: &mut dyn std::io::Write) {
    if let Some(waiter) = self.waiter.as_ref() {
      waiter.as_trace().trace_debug(log)
    }
  }
}
impl DebugHeap for ChannelWaiter {
  fn fmt_heap(&self, f: &mut std::fmt::Formatter, _: usize) -> std::fmt::Result {
    f.debug_struct("ChannelWaiter")
      .field("runnable", &self.runnable)
      .field("waiter", &self.waiter.as_ref().map(|w| w.as_any()))
      .finish()
  }
}

impl Debug for ChannelWaiter {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("ChannelWaiter")
      .field("runnable", &self.runnable)
      .field("waiter", &self.waiter.as_ref().map(|w| w.as_any()))
      .finish()
  }
}

impl Allocate<Ref<Self>> for ChannelWaiter {
  fn alloc(self) -> AllocResult<Ref<Self>> {
    Ref::alloc_result(self)
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use std::any::Any;

  impl TraceAny for u16 {
    fn as_trace(&self) -> &dyn Trace {
      self
    }

    fn as_any(&self) -> &dyn Any {
      self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
      self
    }
  }

  impl TraceAny for u8 {
    fn as_trace(&self) -> &dyn Trace {
      self
    }

    fn as_any(&self) -> &dyn Any {
      self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
      self
    }
  }

  #[test]
  fn get_waiter_none() {
    let waiter = ChannelWaiter::new(true);

    assert_eq!(waiter.get_waiter::<u8>(), None)
  }

  #[test]
  fn get_waiter_wrong_type() {
    let mut waiter = ChannelWaiter::new(true);
    waiter.set_waiter(10u16);

    assert_eq!(waiter.get_waiter::<u8>(), None)
  }

  #[test]
  fn get_waiter_right_type() {
    let mut waiter = ChannelWaiter::new(true);
    waiter.set_waiter::<u16>(10u16);

    assert_eq!(waiter.get_waiter::<u16>(), Some(&10u16))
  }
}
