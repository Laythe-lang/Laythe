mod channel_queue;
mod channel_waiter;

use channel_queue::ChannelQueue;

use super::{Fiber, ObjectKind};
use crate::{
  hooks::GcHooks, managed::{AllocResult, Allocate, DebugHeap, DebugWrap, Trace}, reference::{ObjRef, Object, Ref}, value::Value
};
use std::{fmt, io::Write};

/// What type of channel is this
#[derive(PartialEq, Clone, Debug)]
enum ChannelKind {
  /// This channel and both send and receive
  BiDirectional,

  /// This channel can only receive
  ReceiveOnly,

  /// This channel can only send
  SendOnly,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum SendResult {
  Ok,
  NoSendAccess,
  FullBlock(Option<ObjRef<Fiber>>),
  Full(Option<ObjRef<Fiber>>),
  Closed,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ReceiveResult {
  Ok(Value),
  NoReceiveAccess,
  EmptyBlock(Option<ObjRef<Fiber>>),
  Empty(Option<ObjRef<Fiber>>),
  Closed,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum CloseResult {
  Ok,
  AlreadyClosed,
}

impl Allocate<Ref<Self>> for ChannelQueue {
  fn alloc(self) -> AllocResult<Ref<Self>> {
    Ref::alloc_result(self)
  }
}

#[derive(PartialEq, Clone)]
pub struct Channel {
  queue: Ref<ChannelQueue>,
  kind: ChannelKind,
}

impl Channel {
  /// Create a synchronous channel
  pub fn sync(hooks: &GcHooks) -> Self {
    Self {
      queue: hooks.manage(ChannelQueue::sync()),
      kind: ChannelKind::BiDirectional,
    }
  }

  /// Create a channel with a fixed capacity
  pub fn with_capacity(hooks: &GcHooks, capacity: usize) -> Self {
    Self {
      queue: hooks.manage(ChannelQueue::with_capacity(capacity)),
      kind: ChannelKind::BiDirectional,
    }
  }

  /// Create a read only channel
  pub fn read_only(&self) -> Option<Self> {
    match self.kind {
      ChannelKind::BiDirectional | ChannelKind::ReceiveOnly => Some(Self {
        queue: self.queue,
        kind: ChannelKind::ReceiveOnly,
      }),
      ChannelKind::SendOnly => None,
    }
  }

  /// Create a write only channel
  pub fn write_only(&self) -> Option<Self> {
    match self.kind {
      ChannelKind::BiDirectional | ChannelKind::SendOnly => Some(Self {
        queue: self.queue,
        kind: ChannelKind::SendOnly,
      }),
      ChannelKind::ReceiveOnly => None,
    }
  }

  /// Is this queue empty
  #[inline]
  pub fn is_empty(&self) -> bool {
    self.queue.is_empty()
  }

  /// The capacity of the associated channel queue
  #[inline]
  pub fn len(&self) -> usize {
    self.queue.len()
  }

  /// The capacity of the associated channel queue
  #[inline]
  pub fn capacity(&self) -> usize {
    self.queue.capacity()
  }

  /// Close this channel queue
  pub fn close(&mut self) -> CloseResult {
    self.queue.close()
  }

  /// Is this channel queue closed
  #[inline]
  pub fn is_closed(&self) -> bool {
    self.queue.is_closed()
  }

  /// Attempt to send a value into this channel.
  /// the value can be reject either because the
  /// channel is saturated or because the channel
  /// has already been closed
  #[inline]
  pub fn send(&mut self, fiber: ObjRef<Fiber>, val: Value) -> SendResult {
    match self.kind {
      ChannelKind::BiDirectional | ChannelKind::SendOnly => self.queue.send(fiber, val),
      ChannelKind::ReceiveOnly => SendResult::NoSendAccess,
    }
  }

  /// Attempt to receive a value from this channel.
  /// If the channel is empty
  #[inline]
  pub fn receive(&mut self, fiber: ObjRef<Fiber>) -> ReceiveResult {
    match self.kind {
      ChannelKind::BiDirectional | ChannelKind::ReceiveOnly => self.queue.receive(fiber),
      ChannelKind::SendOnly => ReceiveResult::NoReceiveAccess,
    }
  }

  /// Attempt to get a runnable waiter
  /// from this channel
  pub fn runnable_waiter(&mut self) -> Option<ObjRef<Fiber>> {
    self.queue.runnable_waiter()
  }
}

impl fmt::Display for Channel {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<Channel {self:p}>")
  }
}

impl fmt::Debug for Channel {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.fmt_heap(f, 2)
  }
}

impl Trace for Channel {
  #[inline]
  fn trace(&self) {
    self.queue.trace()
  }

  fn trace_debug(&self, log: &mut dyn Write) {
    self.queue.trace_debug(log)
  }
}

impl DebugHeap for Channel {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    f.debug_struct("Channel")
      .field("queue", &DebugWrap(&self.queue, depth))
      .field("kind", &self.kind)
      .finish()
  }
}

impl Object for Channel {
  fn kind(&self) -> ObjectKind {
    ObjectKind::Channel
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod channel {
    use crate::{
      hooks::{GcHooks, NoContext},
      support::FiberBuilder,
      val,
    };

    use super::*;

    #[test]
    fn sync() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let channel = Channel::sync(&hooks);
      assert_eq!(channel.capacity(), 1);
      assert_eq!(channel.len(), 0);
    }

    #[test]
    #[should_panic]
    fn with_zero_sized_capacity() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      Channel::with_capacity(&hooks, 0);
    }

    #[test]
    fn with_nonzero_sized_capacity() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let channel = Channel::with_capacity(&hooks, 5);
      assert_eq!(channel.capacity(), 5);
      assert_eq!(channel.len(), 0);
    }

    #[test]
    fn len() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let fiber = FiberBuilder::default().build(&hooks).unwrap();
      let mut channel = Channel::with_capacity(&hooks, 5);

      assert_eq!(channel.len(), 0);

      channel.send(fiber, val!(1.0));
      assert_eq!(channel.len(), 1);

      channel.send(fiber, val!(1.0));
      channel.send(fiber, val!(1.0));
      channel.send(fiber, val!(1.0));
      assert_eq!(channel.len(), 4);
    }

    #[test]
    fn capacity() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let channel = Channel::with_capacity(&hooks, 5);

      assert_eq!(channel.capacity(), 5);
    }

    #[test]
    fn close() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let mut channel = Channel::with_capacity(&hooks, 1);

      assert_eq!(channel.close(), CloseResult::Ok);
      assert!(channel.is_closed());

      assert_eq!(channel.close(), CloseResult::AlreadyClosed);
      assert!(channel.is_closed())
    }

    #[test]
    fn enqueue_with_no_write_access() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let fiber = FiberBuilder::default().build(&hooks).unwrap();
      let mut channel = Channel::sync(&hooks).read_only().unwrap();

      let r = channel.send(fiber, val!(10.0));

      assert_eq!(r, SendResult::NoSendAccess);
      assert_eq!(channel.len(), 0);
    }

    #[test]
    fn dequeue_when_no_read_access() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let fiber = FiberBuilder::default().build(&hooks).unwrap();
      let mut channel = Channel::sync(&hooks).write_only().unwrap();

      let r = channel.receive(fiber);

      assert_eq!(r, ReceiveResult::NoReceiveAccess);
      assert_eq!(channel.len(), 0);
    }

    #[test]
    fn bi_directional_to_read_only() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      assert!(Channel::with_capacity(&hooks, 1).read_only().is_some())
    }

    #[test]
    fn bi_directional_to_write_only() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      assert!(Channel::with_capacity(&hooks, 1).write_only().is_some())
    }

    #[test]
    fn read_only_to_read_only() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let read_only = Channel::with_capacity(&hooks, 1).read_only().unwrap();
      assert!(read_only.read_only().is_some())
    }

    #[test]
    fn read_only_to_write_only() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let read_only = Channel::with_capacity(&hooks, 1).read_only().unwrap();
      assert!(read_only.write_only().is_none())
    }

    #[test]
    fn write_only_to_read_only() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let write_only = Channel::with_capacity(&hooks, 1).write_only().unwrap();
      assert!(write_only.read_only().is_none())
    }

    #[test]
    fn write_only_to_write_only() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let write_only = Channel::with_capacity(&hooks, 1).write_only().unwrap();
      assert!(write_only.write_only().is_some())
    }
  }
}
