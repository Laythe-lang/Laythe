use super::ObjectKind;
use crate::{
  managed::{DebugHeap, DebugWrap, Gc, Manage, Object, Trace},
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

#[derive(PartialEq, Clone)]
struct ChannelQueue {
  queue: VecDeque<Value>,
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
    f.debug_list()
      .entries(self.queue.iter().map(|x| DebugWrap(x, depth)))
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
