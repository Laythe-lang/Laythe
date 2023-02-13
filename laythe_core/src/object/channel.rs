use super::{Fiber, ObjectKind};
use crate::{
  hooks::GcHooks,
  managed::{AllocResult, Allocate, DebugHeap, DebugWrap, Gc, GcObj, Object, Trace},
  value::Value,
  LyHashSet,
};
use std::{collections::VecDeque, usize};
use std::{fmt, io::Write};

#[derive(PartialEq, Clone, Debug)]
enum ChannelKind {
  BiDirectional,
  ReceiveOnly,
  SendOnly,
}

#[derive(PartialEq, Clone, Debug)]
enum ChannelQueueState {
  Ready,
  Closed,
  ClosedEmpty,
}

#[derive(PartialEq, Clone, Debug)]
enum ChannelQueueKind {
  Sync,
  Buffered,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum SendResult {
  Ok,
  NoSendAccess,
  FullBlock(Option<GcObj<Fiber>>),
  Full(Option<GcObj<Fiber>>),
  Closed,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ReceiveResult {
  Ok(Value),
  NoReceiveAccess,
  EmptyBlock(Option<GcObj<Fiber>>),
  Empty(Option<GcObj<Fiber>>),
  Closed,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum CloseResult {
  Ok,
  AlreadyClosed,
}

#[derive(PartialEq, Clone)]
struct ChannelQueue {
  queue: VecDeque<Value>,
  capacity: usize,
  state: ChannelQueueState,
  kind: ChannelQueueKind,

  send_waiters: LyHashSet<GcObj<Fiber>>,
  receive_waiters: LyHashSet<GcObj<Fiber>>,
}

impl ChannelQueue {
  /// Create a synchronous channel queue
  /// that is immediately ready
  fn sync() -> Self {
    Self {
      queue: VecDeque::with_capacity(1),
      capacity: 1,
      state: ChannelQueueState::Ready,
      kind: ChannelQueueKind::Sync,

      send_waiters: LyHashSet::default(),
      receive_waiters: LyHashSet::default(),
    }
  }

  /// Create with a fixed capacity
  /// The channel queue will immediately be ready
  fn with_capacity(capacity: usize) -> Self {
    assert!(capacity > 0, "ChannelQueue must be positive");

    Self {
      queue: VecDeque::with_capacity(capacity),
      capacity,
      state: ChannelQueueState::Ready,
      kind: ChannelQueueKind::Buffered,

      send_waiters: LyHashSet::default(),
      receive_waiters: LyHashSet::default(),
    }
  }

  /// Is this queue sync
  fn is_sync(&self) -> bool {
    self.kind == ChannelQueueKind::Sync
  }

  /// Is this queue empty
  fn is_empty(&self) -> bool {
    self.queue.is_empty()
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

  /// Attempt to send a value into this channel.
  /// the value can be reject either because the
  /// channel is saturated or because the channel
  /// has already been closed
  fn send(&mut self, fiber: GcObj<Fiber>, val: Value) -> SendResult {
    match self.state {
      ChannelQueueState::Ready => {
        if self.is_sync() && self.queue.is_empty() {
          self.queue.push_back(val);
          self.send_waiters.insert(fiber);
          return SendResult::FullBlock(get_runnable_from_set(&mut self.receive_waiters));
        }

        if self.queue.len() < self.capacity {
          self.queue.push_back(val);
          SendResult::Ok
        } else {
          self.send_waiters.insert(fiber);
          SendResult::Full(get_runnable_from_set(&mut self.receive_waiters))
        }
      }
      ChannelQueueState::Closed | ChannelQueueState::ClosedEmpty => SendResult::Closed,
    }
  }

  /// Attempt to receive a value from this channel.
  /// If the channel is empty return potentially a send
  /// waiter. If
  fn receive(&mut self, fiber: GcObj<Fiber>) -> ReceiveResult {
    match self.state {
      ChannelQueueState::Ready => match self.queue.pop_front() {
        Some(value) => ReceiveResult::Ok(value),
        None => {
          self.receive_waiters.insert(fiber);

          if self.is_sync() {
            ReceiveResult::EmptyBlock(get_runnable_from_set(&mut self.send_waiters))
          } else {
            ReceiveResult::Empty(get_runnable_from_set(&mut self.send_waiters))
          }
        }
      },
      ChannelQueueState::Closed => match self.queue.pop_front() {
        Some(value) => ReceiveResult::Ok(value),
        None => {
          self.state = ChannelQueueState::ClosedEmpty;
          ReceiveResult::Closed
        }
      },
      ChannelQueueState::ClosedEmpty => ReceiveResult::Closed,
    }
  }

  /// Attempt to get a runnable waiter
  /// from this channel
  fn runnable_waiter(&mut self) -> Option<GcObj<Fiber>> {
    match self.kind {
      ChannelQueueKind::Sync => {
        if self.is_empty() && !self.is_closed() {
          get_runnable_from_set(&mut self.send_waiters)
        } else {
          get_runnable_from_set(&mut self.receive_waiters)
        }
      }
      ChannelQueueKind::Buffered => {
        if self.is_empty() && !self.is_closed() {
          get_runnable_from_set(&mut self.send_waiters)
        } else if self.len() == self.capacity || self.is_closed() {
          get_runnable_from_set(&mut self.receive_waiters)
        } else {
          get_runnable_from_set(&mut self.receive_waiters)
            .or_else(|| get_runnable_from_set(&mut self.send_waiters))
        }
      }
    }
  }

  /// Remove waiter from this channel
  fn remove_waiter(&mut self, fiber: GcObj<Fiber>) {
    self.send_waiters.remove(&fiber);
    self.receive_waiters.remove(&fiber);
  }
}

impl Trace for ChannelQueue {
  fn trace(&self) {
    for value in &self.queue {
      value.trace()
    }
    for waiter in &self.send_waiters {
      waiter.trace();
    }
    for waiter in &self.receive_waiters {
      waiter.trace();
    }
  }

  fn trace_debug(&self, log: &mut dyn Write) {
    for value in &self.queue {
      value.trace_debug(log);
    }
    for waiter in &self.send_waiters {
      waiter.trace_debug(log);
    }
    for waiter in &self.receive_waiters {
      waiter.trace_debug(log);
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

impl Allocate<Gc<Self>> for ChannelQueue {
  fn alloc(self) -> AllocResult<Gc<Self>> {
    Gc::alloc_result(self)
  }
}

#[derive(PartialEq, Clone)]
pub struct Channel {
  queue: Gc<ChannelQueue>,
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
  pub fn send(&mut self, fiber: GcObj<Fiber>, val: Value) -> SendResult {
    match self.kind {
      ChannelKind::BiDirectional | ChannelKind::SendOnly => self.queue.send(fiber, val),
      ChannelKind::ReceiveOnly => SendResult::NoSendAccess,
    }
  }

  /// Attempt to receive a value from this channel.
  /// If the channel is empty
  #[inline]
  pub fn receive(&mut self, fiber: GcObj<Fiber>) -> ReceiveResult {
    match self.kind {
      ChannelKind::BiDirectional | ChannelKind::ReceiveOnly => self.queue.receive(fiber),
      ChannelKind::SendOnly => ReceiveResult::NoReceiveAccess,
    }
  }

  /// Attempt to get a runnable waiter
  /// from this channel
  pub fn runnable_waiter(&mut self) -> Option<GcObj<Fiber>> {
    self.queue.runnable_waiter()
  }

  /// Remove waiter from this channel
  pub fn remove_waiter(&mut self, fiber: GcObj<Fiber>) {
    self.queue.remove_waiter(fiber)
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

fn get_runnable_from_set(set: &mut LyHashSet<GcObj<Fiber>>) -> Option<GcObj<Fiber>> {
  if set.is_empty() {
    return None;
  }

  let fiber = set.iter().next().cloned();

  if let Some(fiber) = fiber {
    assert!(set.remove(&fiber));
  }

  fiber
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
    fn sync() {
      let queue = ChannelQueue::sync();

      assert_eq!(queue.len(), 0);
      assert_eq!(queue.capacity(), 1);
      assert!(!queue.is_closed())
    }

    #[test]
    #[should_panic]
    fn new_zero_sized_buffered() {
      ChannelQueue::with_capacity(0);
    }

    #[test]
    fn new_nonzero_sized_buffered() {
      let queue = ChannelQueue::with_capacity(5);

      assert_eq!(queue.len(), 0);
      assert_eq!(queue.capacity(), 5);
      assert!(!queue.is_closed());
    }

    #[test]
    fn len() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let fiber = FiberBuilder::default().build(&hooks).unwrap();
      let mut queue = ChannelQueue::with_capacity(5);

      assert_eq!(queue.len(), 0);
      queue.send(fiber, val!(0.0));
      queue.send(fiber, val!(0.0));

      assert_eq!(queue.len(), 2);
      queue.receive(fiber);

      assert_eq!(queue.len(), 1);
    }

    #[test]
    fn capacity() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let fiber = FiberBuilder::default().build(&hooks).unwrap();
      let mut queue = ChannelQueue::with_capacity(5);

      assert_eq!(queue.capacity(), 5);
      queue.send(fiber, val!(0.0));

      assert_eq!(queue.capacity(), 5);
    }

    #[test]
    fn close() {
      let mut queue = ChannelQueue::with_capacity(5);

      assert_eq!(queue.close(), CloseResult::Ok);
      assert_eq!(queue.close(), CloseResult::AlreadyClosed);
    }

    mod sync {
      use super::*;

      #[test]
      fn enqueue_when_empty() {
        let context = NoContext::default();
        let hooks = GcHooks::new(&context);

        let fiber = FiberBuilder::default().build(&hooks).unwrap();
        let mut queue = ChannelQueue::sync();

        let r = queue.send(fiber, val!(1.0));
        assert_eq!(r, SendResult::FullBlock(None));
        assert_eq!(queue.len(), 1);
      }

      #[test]
      fn enqueue_when_empty_with_waiter() {
        let context = NoContext::default();
        let hooks = GcHooks::new(&context);

        let fiber = FiberBuilder::default().build(&hooks).unwrap();
        let fiber_waiter = FiberBuilder::default().build(&hooks).unwrap();
        let mut queue = ChannelQueue::sync();

        queue.receive(fiber_waiter);

        let r = queue.send(fiber, val!(1.0));
        assert_eq!(r, SendResult::FullBlock(Some(fiber_waiter)));
        assert_eq!(queue.len(), 1);
      }

      #[test]
      fn enqueue_when_full() {
        let context = NoContext::default();
        let hooks = GcHooks::new(&context);

        let fiber = FiberBuilder::default().build(&hooks).unwrap();
        let mut queue = ChannelQueue::sync();

        queue.send(fiber, val!(1.0));
        let r = queue.send(fiber, val!(1.0));
        assert_eq!(r, SendResult::Full(None));
        assert_eq!(queue.len(), 1);
      }

      #[test]
      fn enqueue_when_full_with_waiter() {
        let context = NoContext::default();
        let hooks = GcHooks::new(&context);

        let fiber = FiberBuilder::default().build(&hooks).unwrap();
        let fiber_waiter1 = FiberBuilder::default().build(&hooks).unwrap();
        let fiber_waiter2 = FiberBuilder::default().build(&hooks).unwrap();

        let mut queue = ChannelQueue::sync();
        queue.receive(fiber_waiter1);
        queue.receive(fiber_waiter2);
        let r1 = queue.send(fiber, val!(1.0));
        let r2 = queue.send(fiber, val!(1.0));

        match r1 {
          SendResult::FullBlock(Some(waiter)) => {
            if waiter == fiber_waiter1 {
              match r2 {
                SendResult::Full(Some(waiter)) => assert_eq!(waiter, fiber_waiter2),
                _ => assert!(false),
              }
            } else if waiter == fiber_waiter2 {
              match r2 {
                SendResult::Full(Some(waiter)) => assert_eq!(waiter, fiber_waiter1),
                _ => assert!(false),
              }
            } else {
              assert!(false)
            }
          }
          _ => assert!(false),
        }

        assert_eq!(queue.len(), 1);
      }

      #[test]
      fn enqueue_when_closed() {
        let context = NoContext::default();
        let hooks = GcHooks::new(&context);

        let fiber = FiberBuilder::default().build(&hooks).unwrap();
        let mut queue = ChannelQueue::sync();

        assert_eq!(queue.close(), CloseResult::Ok);
        let r = queue.send(fiber, val!(10.0));

        assert_eq!(r, SendResult::Closed);
        assert_eq!(queue.len(), 0);
      }

      #[test]
      fn dequeue_when_empty() {
        let context = NoContext::default();
        let hooks = GcHooks::new(&context);

        let fiber = FiberBuilder::default().build(&hooks).unwrap();
        let mut queue = ChannelQueue::sync();

        let r = queue.receive(fiber);
        assert_eq!(r, ReceiveResult::EmptyBlock(None));
        assert_eq!(queue.len(), 0);
      }

      #[test]
      fn dequeue_when_empty_with_waiter() {
        let context = NoContext::default();
        let hooks = GcHooks::new(&context);

        let fiber = FiberBuilder::default().build(&hooks).unwrap();
        let mut queue = ChannelQueue::sync();

        let r = queue.receive(fiber);
        assert_eq!(r, ReceiveResult::EmptyBlock(None));
        assert_eq!(queue.len(), 0);
      }

      #[test]
      fn dequeue_when_full() {
        let context = NoContext::default();
        let hooks = GcHooks::new(&context);

        let fiber1 = FiberBuilder::default().build(&hooks).unwrap();
        let fiber2 = FiberBuilder::default().build(&hooks).unwrap();

        let mut queue = ChannelQueue::with_capacity(1);
        queue.send(fiber2, val!(1.0));

        let r = queue.receive(fiber1);
        assert_eq!(r, ReceiveResult::Ok(val!(1.0)));
        assert_eq!(queue.len(), 0);
      }

      #[test]
      fn dequeue_when_closed() {
        let context = NoContext::default();
        let hooks = GcHooks::new(&context);

        let fiber = FiberBuilder::default().build(&hooks).unwrap();
        let mut queue = ChannelQueue::sync();

        queue.send(fiber, val!(10.0));
        assert_eq!(queue.close(), CloseResult::Ok);
        let r = queue.receive(fiber);

        assert_eq!(r, ReceiveResult::Ok(val!(10.0)));
        assert_eq!(queue.len(), 0);
      }

      #[test]
      fn dequeue_when_closed_empty() {
        let context = NoContext::default();
        let hooks = GcHooks::new(&context);

        let fiber = FiberBuilder::default().build(&hooks).unwrap();
        let mut queue = ChannelQueue::sync();

        assert_eq!(queue.close(), CloseResult::Ok);
        let r = queue.receive(fiber);

        assert_eq!(r, ReceiveResult::Closed);
        assert_eq!(queue.len(), 0);
      }
    }

    mod buffered {
      use super::*;

      #[test]
      fn enqueue_when_not_full() {
        let context = NoContext::default();
        let hooks = GcHooks::new(&context);

        let fiber = FiberBuilder::default().build(&hooks).unwrap();
        let mut queue = ChannelQueue::with_capacity(3);

        let r = queue.send(fiber, val!(1.0));
        assert_eq!(r, SendResult::Ok);
        assert_eq!(queue.len(), 1);
      }

      #[test]
      fn enqueue_when_full() {
        let context = NoContext::default();
        let hooks = GcHooks::new(&context);

        let fiber = FiberBuilder::default().build(&hooks).unwrap();
        let mut queue = ChannelQueue::with_capacity(1);

        queue.send(fiber, val!(1.0));
        let r = queue.send(fiber, val!(1.0));
        assert_eq!(r, SendResult::Full(None));
        assert_eq!(queue.len(), 1);
      }

      #[test]
      fn enqueue_when_full_with_waiter() {
        let context = NoContext::default();
        let hooks = GcHooks::new(&context);

        let fiber = FiberBuilder::default().build(&hooks).unwrap();
        let fiber_waiter1 = FiberBuilder::default().build(&hooks).unwrap();

        let mut queue = ChannelQueue::with_capacity(1);
        queue.receive(fiber_waiter1);
        queue.send(fiber, val!(1.0));
        let r = queue.send(fiber, val!(1.0));

        assert_eq!(r, SendResult::Full(Some(fiber_waiter1)));
        assert_eq!(queue.len(), 1);
      }

      #[test]
      fn enqueue_when_closed() {
        let context = NoContext::default();
        let hooks = GcHooks::new(&context);

        let fiber = FiberBuilder::default().build(&hooks).unwrap();
        let mut queue = ChannelQueue::with_capacity(1);

        assert_eq!(queue.close(), CloseResult::Ok);
        let r = queue.send(fiber, val!(10.0));

        assert_eq!(r, SendResult::Closed);
        assert_eq!(queue.len(), 0);
      }

      #[test]
      fn dequeue_when_empty() {
        let context = NoContext::default();
        let hooks = GcHooks::new(&context);

        let fiber = FiberBuilder::default().build(&hooks).unwrap();
        let mut queue = ChannelQueue::with_capacity(3);

        let r = queue.receive(fiber);
        assert_eq!(r, ReceiveResult::Empty(None));
        assert_eq!(queue.len(), 0);
      }

      #[test]
      fn dequeue_when_empty_with_waiter() {
        let context = NoContext::default();
        let hooks = GcHooks::new(&context);

        let fiber = FiberBuilder::default().build(&hooks).unwrap();
        let fiber_waiter = FiberBuilder::default().build(&hooks).unwrap();

        let mut queue = ChannelQueue::with_capacity(1);
        queue.send(fiber_waiter, val!(1.0));
        queue.send(fiber_waiter, val!(1.0));

        queue.receive(fiber);
        let r = queue.receive(fiber);
        assert_eq!(r, ReceiveResult::Empty(Some(fiber_waiter)));
        assert_eq!(queue.len(), 0);
      }

      #[test]
      fn dequeue_when_nonempty() {
        let context = NoContext::default();
        let hooks = GcHooks::new(&context);

        let fiber = FiberBuilder::default().build(&hooks).unwrap();
        let mut queue = ChannelQueue::with_capacity(3);

        queue.send(fiber, val!(1.0));
        queue.send(fiber, val!(2.0));
        let r = queue.receive(fiber);
        assert_eq!(r, ReceiveResult::Ok(val!(1.0)));
        assert_eq!(queue.len(), 1);
      }

      #[test]
      fn dequeue_when_closed() {
        let context = NoContext::default();
        let hooks = GcHooks::new(&context);

        let fiber = FiberBuilder::default().build(&hooks).unwrap();
        let mut queue = ChannelQueue::with_capacity(3);

        queue.send(fiber, val!(10.0));
        assert_eq!(queue.close(), CloseResult::Ok);
        let r = queue.receive(fiber);

        assert_eq!(r, ReceiveResult::Ok(val!(10.0)));
        assert_eq!(queue.len(), 0);
      }

      #[test]
      fn dequeue_when_closed_empty() {
        let context = NoContext::default();
        let hooks = GcHooks::new(&context);

        let fiber = FiberBuilder::default().build(&hooks).unwrap();
        let mut queue = ChannelQueue::with_capacity(1);

        assert_eq!(queue.close(), CloseResult::Ok);
        let r = queue.receive(fiber);

        assert_eq!(r, ReceiveResult::Closed);
        assert_eq!(queue.len(), 0);
      }
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
