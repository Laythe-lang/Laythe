use super::{Fiber, ObjectKind};
use crate::{
  hooks::GcHooks,
  managed::{DebugHeap, DebugWrap, Gc, GcObj, Manage, Object, Trace},
  value::Value,
};
use std::{collections::VecDeque, usize};
use std::{fmt, io::Write, mem};

#[derive(PartialEq, Clone, Debug)]
enum ChannelKind {
  BiDirectional,
  ReadOnly,
  WriteOnly,
}

#[derive(PartialEq, Clone, Debug)]
enum ChannelQueueState {
  Ready,
  Closed,
  ClosedEmpty,
}

#[derive(PartialEq, Clone, Debug)]
pub enum EnqueueResult {
  Ok,
  NoWriteAccess,
  Full(Option<GcObj<Fiber>>),
  Closed,
}

#[derive(PartialEq, Clone, Debug)]
pub enum DequeueResult {
  Ok(Value),
  NoReadAccess,
  Empty(Option<GcObj<Fiber>>),
  Closed,
}

#[derive(PartialEq, Clone, Debug)]
pub enum CloseResult {
  Ok,
  AlreadyClosed,
}

#[derive(PartialEq, Clone)]
struct ChannelQueue {
  queue: VecDeque<Value>,
  capacity: usize,
  state: ChannelQueueState,

  send_waiters: VecDeque<GcObj<Fiber>>,
  receive_waiters: VecDeque<GcObj<Fiber>>,
}

impl ChannelQueue {
  /// Create with a fixed capacity
  /// The channel queue will immediately be ready
  fn with_capacity(capacity: usize) -> Self {
    assert!(capacity > 0, "ChannelQueue must be positive");

    Self {
      queue: VecDeque::with_capacity(capacity),
      capacity,
      state: ChannelQueueState::Ready,

      send_waiters: VecDeque::new(),
      receive_waiters: VecDeque::new(),
    }
  }

  /// How many items are currently in the queue
  fn len(&self) -> usize {
    self.queue.len()
  }

  /// How many items can this queue hold
  fn capacity(&self) -> usize {
    self.capacity
  }

  /// Is this channel queue closed
  fn is_closed(&self) -> bool {
    matches!(
      self.state,
      ChannelQueueState::Closed | ChannelQueueState::ClosedEmpty
    )
  }

  /// Close this channel queue
  fn close(&mut self) -> CloseResult {
    if self.is_closed() {
      return CloseResult::AlreadyClosed;
    }

    if self.queue.is_empty() {
      self.state = ChannelQueueState::ClosedEmpty
    } else {
      self.state = ChannelQueueState::Closed
    }
    CloseResult::Ok
  }

  /// Attempt to enqueue a value into this channel.
  /// the value can be reject either because the
  /// channel is saturated or because the channel
  /// has already been closed
  fn enqueue(&mut self, fiber: GcObj<Fiber>, val: Value) -> EnqueueResult {
    match self.state {
      ChannelQueueState::Ready => {
        if self.queue.len() < self.capacity {
          self.queue.push_back(val);
          EnqueueResult::Ok
        } else {
          self.send_waiters.push_back(fiber);
          EnqueueResult::Full(self.receive_waiters.pop_front())
        }
      }
      ChannelQueueState::Closed | ChannelQueueState::ClosedEmpty => EnqueueResult::Closed,
    }
  }

  /// Attempt to dequeue a value from this channel.
  /// If the channel is empty
  fn dequeue(&mut self, fiber: GcObj<Fiber>) -> DequeueResult {
    match self.state {
      ChannelQueueState::Ready => match self.queue.pop_front() {
        Some(value) => DequeueResult::Ok(value),
        None => {
          self.receive_waiters.push_back(fiber);
          DequeueResult::Empty(self.send_waiters.pop_front())
        }
      },
      ChannelQueueState::Closed => match self.queue.pop_front() {
        Some(value) => DequeueResult::Ok(value),
        None => {
          self.state = ChannelQueueState::ClosedEmpty;
          DequeueResult::Closed
        }
      },
      ChannelQueueState::ClosedEmpty => DequeueResult::Closed,
    }
  }
}

impl Trace for ChannelQueue {
  fn trace(&self) {
    for value in &self.queue {
      value.trace()
    }
  }

  fn trace_debug(&self, log: &mut dyn Write) {
    for value in &self.queue {
      value.trace_debug(log)
    }
  }
}

impl DebugHeap for ChannelQueue {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    f.debug_struct("ChannelQueue")
      .field("queue", &DebugWrap(&self.queue, depth))
      .field("kind", &self.state)
      .finish()
  }
}

impl Manage for ChannelQueue {
  fn size(&self) -> usize {
    mem::size_of::<Self>() + mem::size_of::<Value>() * self.queue.capacity()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

#[derive(PartialEq, Clone)]
pub struct Channel {
  queue: Gc<ChannelQueue>,
  kind: ChannelKind,
}

impl Channel {
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
      ChannelKind::BiDirectional | ChannelKind::ReadOnly => Some(Self {
        queue: self.queue,
        kind: ChannelKind::ReadOnly,
      }),
      ChannelKind::WriteOnly => None,
    }
  }

  /// Create a write only channel
  pub fn write_only(&self) -> Option<Self> {
    match self.kind {
      ChannelKind::BiDirectional | ChannelKind::WriteOnly => Some(Self {
        queue: self.queue,
        kind: ChannelKind::WriteOnly,
      }),
      ChannelKind::ReadOnly => None,
    }
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
  pub fn is_closed(&mut self) -> bool {
    self.queue.is_closed()
  }

  /// Attempt to enqueue a value into this channel.
  /// the value can be reject either because the
  /// channel is saturated or because the channel
  /// has already been closed
  #[inline]
  pub fn enqueue(&mut self, fiber: GcObj<Fiber>, val: Value) -> EnqueueResult {
    match self.kind {
      ChannelKind::BiDirectional | ChannelKind::WriteOnly => self.queue.enqueue(fiber, val),
      ChannelKind::ReadOnly => EnqueueResult::NoWriteAccess,
    }
  }

  /// Attempt to dequeue a value from this channel.
  /// If the channel is empty
  #[inline]
  pub fn dequeue(&mut self, fiber: GcObj<Fiber>) -> DequeueResult {
    match self.kind {
      ChannelKind::BiDirectional | ChannelKind::ReadOnly => self.queue.dequeue(fiber),
      ChannelKind::WriteOnly => DequeueResult::NoReadAccess,
    }
  }
}

impl fmt::Display for Channel {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<Channel {:p}>", &self)
  }
}

impl fmt::Debug for Channel {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.fmt_heap(f, 2)
  }
}

impl Trace for Channel {
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

impl Manage for Channel {
  fn size(&self) -> usize {
    mem::size_of::<Channel>()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
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

  mod channel_queue {
    use crate::{
      hooks::{GcHooks, NoContext},
      support::FiberBuilder,
      val,
    };

    use super::*;

    #[test]
    #[should_panic]
    fn new_zero_sized() {
      ChannelQueue::with_capacity(0);
    }

    #[test]
    fn new_nonzero_sized() {
      let queue = ChannelQueue::with_capacity(5);

      assert_eq!(queue.len(), 0);
      assert!(!queue.is_closed())
    }

    #[test]
    fn len() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let fiber = FiberBuilder::<u8>::default().build(&hooks).unwrap();
      let mut queue = ChannelQueue::with_capacity(5);

      assert_eq!(queue.len(), 0);
      queue.enqueue(fiber, val!(0.0));
      queue.enqueue(fiber, val!(0.0));

      assert_eq!(queue.len(), 2);
      queue.dequeue(fiber);

      assert_eq!(queue.len(), 1);
    }

    #[test]
    fn close() {
      let mut queue = ChannelQueue::with_capacity(5);

      assert_eq!(queue.close(), CloseResult::Ok);
      assert_eq!(queue.close(), CloseResult::AlreadyClosed);
    }

    #[test]
    fn enqueue_when_empty() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let fiber = FiberBuilder::<u8>::default().build(&hooks).unwrap();
      let mut queue = ChannelQueue::with_capacity(3);

      let r = queue.enqueue(fiber, val!(1.0));
      assert_eq!(r, EnqueueResult::Ok);
      assert_eq!(queue.len(), 1);
    }

    #[test]
    fn enqueue_when_full() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let fiber = FiberBuilder::<u8>::default().build(&hooks).unwrap();
      let mut queue = ChannelQueue::with_capacity(1);

      queue.enqueue(fiber, val!(1.0));
      let r = queue.enqueue(fiber, val!(1.0));
      assert_eq!(r, EnqueueResult::Full(None));
      assert_eq!(queue.len(), 1);
    }

    #[test]
    fn enqueue_when_full_with_waiter() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let fiber = FiberBuilder::<u8>::default().build(&hooks).unwrap();
      let fiber_waiter = FiberBuilder::<u8>::default().build(&hooks).unwrap();

      let mut queue = ChannelQueue::with_capacity(1);
      queue.dequeue(fiber_waiter);
      queue.enqueue(fiber, val!(1.0));
      let r = queue.enqueue(fiber, val!(1.0));

      assert_eq!(r, EnqueueResult::Full(Some(fiber_waiter)));
      assert_eq!(queue.len(), 1);
    }

    #[test]
    fn enqueue_when_closed() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let fiber = FiberBuilder::<u8>::default().build(&hooks).unwrap();
      let mut queue = ChannelQueue::with_capacity(1);

      assert_eq!(queue.close(), CloseResult::Ok);
      let r = queue.enqueue(fiber, val!(10.0));

      assert_eq!(r, EnqueueResult::Closed);
      assert_eq!(queue.len(), 0);
    }

    #[test]
    fn dequeue_when_empty() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let fiber = FiberBuilder::<u8>::default().build(&hooks).unwrap();
      let mut queue = ChannelQueue::with_capacity(3);

      let r = queue.dequeue(fiber);
      assert_eq!(r, DequeueResult::Empty(None));
      assert_eq!(queue.len(), 0);
    }

    #[test]
    fn dequeue_when_empty_with_waiter() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let fiber = FiberBuilder::<u8>::default().build(&hooks).unwrap();
      let fiber_waiter = FiberBuilder::<u8>::default().build(&hooks).unwrap();

      let mut queue = ChannelQueue::with_capacity(1);
      queue.enqueue(fiber_waiter, val!(1.0));
      queue.enqueue(fiber_waiter, val!(1.0));

      queue.dequeue(fiber);
      let r = queue.dequeue(fiber);
      assert_eq!(r, DequeueResult::Empty(Some(fiber_waiter)));
      assert_eq!(queue.len(), 0);
    }

    #[test]
    fn dequeue_when_nonempty() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let fiber = FiberBuilder::<u8>::default().build(&hooks).unwrap();
      let mut queue = ChannelQueue::with_capacity(3);

      queue.enqueue(fiber, val!(1.0));
      queue.enqueue(fiber, val!(2.0));
      let r = queue.dequeue(fiber);
      assert_eq!(r, DequeueResult::Ok(val!(1.0)));
      assert_eq!(queue.len(), 1);
    }

    #[test]
    fn dequeue_when_closed() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let fiber = FiberBuilder::<u8>::default().build(&hooks).unwrap();
      let mut queue = ChannelQueue::with_capacity(3);

      queue.enqueue(fiber, val!(10.0));
      assert_eq!(queue.close(), CloseResult::Ok);
      let r = queue.dequeue(fiber);

      assert_eq!(r, DequeueResult::Ok(val!(10.0)));
      assert_eq!(queue.len(), 0);
    }

    #[test]
    fn dequeue_when_closed_empty() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let fiber = FiberBuilder::<u8>::default().build(&hooks).unwrap();
      let mut queue = ChannelQueue::with_capacity(1);

      assert_eq!(queue.close(), CloseResult::Ok);
      let r = queue.dequeue(fiber);

      assert_eq!(r, DequeueResult::Closed);
      assert_eq!(queue.len(), 0);
    }
  }

  mod channel {
    use crate::{
      hooks::{GcHooks, NoContext},
      support::FiberBuilder,
      val,
    };

    use super::*;
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

      let fiber = FiberBuilder::<u8>::default().build(&hooks).unwrap();
      let mut channel = Channel::with_capacity(&hooks, 5);

      assert_eq!(channel.len(), 0);

      channel.enqueue(fiber, val!(1.0));
      assert_eq!(channel.len(), 1);

      channel.enqueue(fiber, val!(1.0));
      channel.enqueue(fiber, val!(1.0));
      channel.enqueue(fiber, val!(1.0));
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
    fn enqueue_when_empty() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let fiber = FiberBuilder::<u8>::default().build(&hooks).unwrap();
      let mut channel = Channel::with_capacity(&hooks, 5);

      let r = channel.enqueue(fiber, val!(1.0));
      assert_eq!(r, EnqueueResult::Ok);
    }

    #[test]
    fn enqueue_when_full() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let fiber = FiberBuilder::<u8>::default().build(&hooks).unwrap();
      let mut channel = Channel::with_capacity(&hooks, 1);

      channel.enqueue(fiber, val!(1.0));
      let r = channel.enqueue(fiber, val!(1.0));
      assert_eq!(r, EnqueueResult::Full(None));
      assert_eq!(channel.len(), 1);
    }

    #[test]
    fn enqueue_when_full_with_waiter() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let fiber = FiberBuilder::<u8>::default().build(&hooks).unwrap();
      let fiber_waiter = FiberBuilder::<u8>::default().build(&hooks).unwrap();

      let mut channel = Channel::with_capacity(&hooks, 1);
      channel.dequeue(fiber_waiter);
      channel.enqueue(fiber, val!(1.0));
      let r = channel.enqueue(fiber, val!(1.0));

      assert_eq!(r, EnqueueResult::Full(Some(fiber_waiter)));
      assert_eq!(channel.len(), 1);
    }

    #[test]
    fn enqueue_when_closed() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let fiber = FiberBuilder::<u8>::default().build(&hooks).unwrap();
      let mut channel = Channel::with_capacity(&hooks, 1);

      assert_eq!(channel.close(), CloseResult::Ok);
      let r = channel.enqueue(fiber, val!(10.0));

      assert_eq!(r, EnqueueResult::Closed);
      assert_eq!(channel.len(), 0);
    }

    #[test]
    fn enqueue_with_no_write_access() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let fiber = FiberBuilder::<u8>::default().build(&hooks).unwrap();
      let mut channel = Channel::with_capacity(&hooks, 1).read_only().unwrap();

      let r = channel.enqueue(fiber, val!(10.0));

      assert_eq!(r, EnqueueResult::NoWriteAccess);
      assert_eq!(channel.len(), 0);
    }

    #[test]
    fn dequeue_when_empty() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let fiber = FiberBuilder::<u8>::default().build(&hooks).unwrap();
      let mut channel = Channel::with_capacity(&hooks, 3);

      let r = channel.dequeue(fiber);
      assert_eq!(r, DequeueResult::Empty(None));
      assert_eq!(channel.len(), 0);
    }

    #[test]
    fn dequeue_when_empty_with_waiter() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let fiber = FiberBuilder::<u8>::default().build(&hooks).unwrap();
      let fiber_waiter = FiberBuilder::<u8>::default().build(&hooks).unwrap();

      let mut channel = Channel::with_capacity(&hooks, 1);
      channel.enqueue(fiber_waiter, val!(1.0));
      channel.enqueue(fiber_waiter, val!(1.0));

      channel.dequeue(fiber);
      let r = channel.dequeue(fiber);
      assert_eq!(r, DequeueResult::Empty(Some(fiber_waiter)));
      assert_eq!(channel.len(), 0);
    }

    #[test]
    fn dequeue_when_nonempty() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let fiber = FiberBuilder::<u8>::default().build(&hooks).unwrap();
      let mut channel = Channel::with_capacity(&hooks, 3);

      channel.enqueue(fiber, val!(1.0));
      channel.enqueue(fiber, val!(2.0));
      let r = channel.dequeue(fiber);
      assert_eq!(r, DequeueResult::Ok(val!(1.0)));
      assert_eq!(channel.len(), 1);
    }

    #[test]
    fn dequeue_when_closed() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let fiber = FiberBuilder::<u8>::default().build(&hooks).unwrap();
      let mut channel = Channel::with_capacity(&hooks, 3);

      channel.enqueue(fiber, val!(10.0));
      assert_eq!(channel.close(), CloseResult::Ok);
      let r = channel.dequeue(fiber);

      assert_eq!(r, DequeueResult::Ok(val!(10.0)));
      assert_eq!(channel.len(), 0);
    }

    #[test]
    fn dequeue_when_closed_empty() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let fiber = FiberBuilder::<u8>::default().build(&hooks).unwrap();
      let mut queue = Channel::with_capacity(&hooks, 1);

      assert_eq!(queue.close(), CloseResult::Ok);
      let r = queue.dequeue(fiber);

      assert_eq!(r, DequeueResult::Closed);
      assert_eq!(queue.len(), 0);
    }

    #[test]
    fn dequeue_when_no_read_access() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let fiber = FiberBuilder::<u8>::default().build(&hooks).unwrap();
      let mut channel = Channel::with_capacity(&hooks, 1).write_only().unwrap();

      let r = channel.dequeue(fiber);

      assert_eq!(r, DequeueResult::NoReadAccess);
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
  }
}
