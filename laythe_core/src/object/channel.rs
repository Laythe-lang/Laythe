use super::{Fiber, ObjectKind};
use crate::{
  managed::{DebugHeap, DebugWrap, Gc, GcObj, Manage, Object, Trace},
  value::Value,
};
use std::collections::VecDeque;
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
}

enum EnqueueResult {
  Ok,
  Full(Value),
  Closed(Value),
}

enum DequeueResult {
  Ok(Value),
  Empty,
  Closed,
}

#[derive(PartialEq, Clone)]
struct ChannelQueue {
  queue: VecDeque<Value>,
  send_waiters: Vec<GcObj<Fiber>>,
  receive_waiters: Vec<GcObj<Fiber>>,
  state: ChannelQueueState,
}

impl ChannelQueue {
  /// Create with a fixed capacity
  /// The channel queue will immediately be ready
  pub fn new(capacity: usize) -> Self {
    Self {
      queue: VecDeque::with_capacity(capacity),
      send_waiters: vec![],
      receive_waiters: vec![],
      state: ChannelQueueState::Ready,
    }
  }

  /// Is this channel closed
  pub fn is_closed(&self) -> bool {
    matches!(self.state, ChannelQueueState::Closed)
  }

  /// Attempt to enqueue a value into this channel.
  /// the value can be reject either because the
  /// channel is saturated or because the channel
  /// has already been closed
  fn enqueue(&mut self, fiber: GcObj<Fiber>, val: Value) -> EnqueueResult {
    match self.state {
      ChannelQueueState::Ready => {
        if self.queue.len() == self.queue.capacity() {
          self.queue.push_back(val);
          EnqueueResult::Ok
        } else {
          self.send_waiters.push(fiber);
          EnqueueResult::Full(val)
        }
      },
      ChannelQueueState::Closed => EnqueueResult::Closed(val),
    }
  }

  /// Attempt to dequeue a value from this channel.
  /// If the channel is empty
  fn dequeue(&mut self, fiber: GcObj<Fiber>) -> DequeueResult {
    match self.state {
      ChannelQueueState::Ready => match self.queue.pop_front() {
        Some(value) => DequeueResult::Ok(value),
        None => {
          self.receive_waiters.push(fiber);
          DequeueResult::Empty
        },
      },
      ChannelQueueState::Closed => DequeueResult::Closed,
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
