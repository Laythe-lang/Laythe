use crate::{
  managed::{DebugHeap, DebugWrap, DebugWrapDyn, Trace},
  value::Value,
  Ref,
};
use std::{collections::VecDeque, fmt, io::Write};

use super::{ChannelWaiter, CloseResult, ReceiveResult, SendResult};

#[derive(PartialEq, Clone, Debug)]
enum ChannelQueueState {
  Ready,
  Closed,
  ClosedEmpty,
}

/// What type of queue does
/// this channel possess
#[derive(PartialEq, Clone, Debug)]
enum ChannelQueueKind {
  /// This channel is sync and will context switch on each value
  Sync,

  /// This channel is buffered and may not context switch immediately
  Buffered,
}

pub struct ChannelQueue {
  queue: VecDeque<Value>,
  capacity: usize,
  state: ChannelQueueState,
  kind: ChannelQueueKind,

  send_waiters: VecDeque<Ref<ChannelWaiter>>,
  receive_waiters: VecDeque<Ref<ChannelWaiter>>,
}

impl ChannelQueue {
  /// Create a synchronous channel queue
  /// that is immediately ready
  pub fn sync() -> Self {
    Self {
      queue: VecDeque::with_capacity(1),
      capacity: 1,
      state: ChannelQueueState::Ready,
      kind: ChannelQueueKind::Sync,

      send_waiters: VecDeque::new(),
      receive_waiters: VecDeque::new(),
    }
  }

  /// Create with a fixed capacity
  /// The channel queue will immediately be ready
  pub fn with_capacity(capacity: usize) -> Self {
    assert!(capacity > 0, "ChannelQueue must be positive");

    Self {
      queue: VecDeque::with_capacity(capacity),
      capacity,
      state: ChannelQueueState::Ready,
      kind: ChannelQueueKind::Buffered,

      send_waiters: VecDeque::new(),
      receive_waiters: VecDeque::new(),
    }
  }

  /// Is this queue sync
  fn is_sync(&self) -> bool {
    self.kind == ChannelQueueKind::Sync
  }

  /// Is this queue empty
  pub fn is_empty(&self) -> bool {
    self.queue.is_empty()
  }

  /// How many items are currently in the queue
  pub fn len(&self) -> usize {
    self.queue.len()
  }

  /// How many items can this queue hold
  pub fn capacity(&self) -> usize {
    self.capacity
  }

  /// Is this channel queue closed
  pub fn is_closed(&self) -> bool {
    matches!(
      self.state,
      ChannelQueueState::Closed | ChannelQueueState::ClosedEmpty
    )
  }

  /// Close this channel queue
  pub fn close(&mut self) -> CloseResult {
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
  pub fn send(&mut self, waiter: Ref<ChannelWaiter>, val: Value) -> SendResult {
    match self.state {
      ChannelQueueState::Ready => {
        if self.is_sync() && self.queue.is_empty() {
          self.queue.push_back(val);
          self.send_waiters.push_back(waiter);
          return SendResult::FullBlock(find_runnable_waiter(&mut self.receive_waiters));
        }

        if self.queue.len() < self.capacity {
          self.queue.push_back(val);
          SendResult::Ok
        } else {
          self.send_waiters.push_back(waiter);
          SendResult::Full(find_runnable_waiter(&mut self.receive_waiters))
        }
      },
      ChannelQueueState::Closed | ChannelQueueState::ClosedEmpty => SendResult::Closed,
    }
  }

  /// Attempt to receive a value from this channel.
  /// If the channel is empty return potentially a send
  /// waiter. If
  pub fn receive(&mut self, waiter: Ref<ChannelWaiter>) -> ReceiveResult {
    match self.state {
      ChannelQueueState::Ready => match self.queue.pop_front() {
        Some(value) => ReceiveResult::Ok(value),
        None => {
          self.receive_waiters.push_back(waiter);

          let runnable = find_runnable_waiter(&mut self.send_waiters);

          if self.is_sync() {
            ReceiveResult::EmptyBlock(runnable)
          } else {
            ReceiveResult::Empty(runnable)
          }
        },
      },
      ChannelQueueState::Closed => match self.queue.pop_front() {
        Some(value) => ReceiveResult::Ok(value),
        None => {
          self.state = ChannelQueueState::ClosedEmpty;
          ReceiveResult::Closed
        },
      },
      ChannelQueueState::ClosedEmpty => ReceiveResult::Closed,
    }
  }

  /// Attempt to get a runnable waiter
  /// from this channel
  pub fn runnable_waiter(&mut self) -> Option<Ref<ChannelWaiter>> {
    match self.kind {
      ChannelQueueKind::Sync => {
        if self.is_empty() && !self.is_closed() {
          find_runnable_waiter(&mut self.send_waiters)
        } else {
          find_runnable_waiter(&mut self.receive_waiters)
        }
      },
      ChannelQueueKind::Buffered => {
        if self.is_empty() && !self.is_closed() {
          find_runnable_waiter(&mut self.send_waiters)
        } else if self.len() == self.capacity || self.is_closed() {
          find_runnable_waiter(&mut self.receive_waiters)
        } else {
          find_runnable_waiter(&mut self.send_waiters)
            .or_else(|| find_runnable_waiter(&mut self.receive_waiters))
        }
      },
    }
  }
}

fn find_runnable_waiter(queue: &mut VecDeque<Ref<ChannelWaiter>>) -> Option<Ref<ChannelWaiter>> {
  while let Some(waiter) = queue.pop_front() {
    if waiter.is_runnable() {
      return Some(waiter);
    }
  }

  None
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
      .field(
        "send_waiters",
        &DebugWrapDyn(&self.send_waiters as &dyn DebugHeap, depth),
      )
      .field(
        "receive_waiters",
        &DebugWrapDyn(&self.receive_waiters, depth),
      )
      .field("kind", &self.state)
      .finish()
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::{
    hooks::{GcHooks, NoContext},
    val,
  };

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

    let waiter = hooks.manage(ChannelWaiter::new(true));
    let mut queue = ChannelQueue::with_capacity(5);

    assert_eq!(queue.len(), 0);
    queue.send(waiter, val!(0.0));
    queue.send(waiter, val!(0.0));

    assert_eq!(queue.len(), 2);
    queue.receive(waiter);

    assert_eq!(queue.len(), 1);
  }

  #[test]
  fn capacity() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let waiter = hooks.manage(ChannelWaiter::new(true));
    let mut queue = ChannelQueue::with_capacity(5);

    assert_eq!(queue.capacity(), 5);
    queue.send(waiter, val!(0.0));

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

      let waiter = hooks.manage(ChannelWaiter::new(true));
      let mut queue = ChannelQueue::sync();

      let r = queue.send(waiter, val!(1.0));
      assert_eq!(r, SendResult::FullBlock(None));
      assert_eq!(queue.len(), 1);
    }

    #[test]
    fn enqueue_when_waiter_not_runnable() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let waiter1 = hooks.manage(ChannelWaiter::new(false));
      let waiter2 = hooks.manage(ChannelWaiter::new(true));
      let mut queue = ChannelQueue::sync();

      queue.receive(waiter1);

      let r = queue.send(waiter2, val!(1.0));
      assert_eq!(r, SendResult::FullBlock(None));
      assert_eq!(queue.len(), 1);
    }

    #[test]
    fn enqueue_when_empty_with_waiter() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let waiter1 = hooks.manage(ChannelWaiter::new(true));
      let waiter2 = hooks.manage(ChannelWaiter::new(true));
      let mut queue = ChannelQueue::sync();

      queue.receive(waiter1);

      let r = queue.send(waiter2, val!(1.0));
      assert_eq!(r, SendResult::FullBlock(Some(waiter1)));
      assert_eq!(queue.len(), 1);
    }

    #[test]
    fn enqueue_when_full() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let waiter = hooks.manage(ChannelWaiter::new(true));
      let mut queue = ChannelQueue::sync();

      queue.send(waiter, val!(1.0));
      let r = queue.send(waiter, val!(1.0));
      assert_eq!(r, SendResult::Full(None));
      assert_eq!(queue.len(), 1);
    }

    #[test]
    fn enqueue_when_full_with_waiter() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let waiter1 = hooks.manage(ChannelWaiter::new(true));
      let waiter2 = hooks.manage(ChannelWaiter::new(true));
      let waiter3 = hooks.manage(ChannelWaiter::new(true));

      let mut queue = ChannelQueue::sync();
      queue.receive(waiter1);
      queue.receive(waiter2);
      let r1 = queue.send(waiter3, val!(1.0));
      let r2 = queue.send(waiter3, val!(1.0));

      match r1 {
        SendResult::FullBlock(Some(waiter)) => {
          if waiter == waiter1 {
            match r2 {
              SendResult::Full(Some(waiter)) => assert_eq!(waiter, waiter2),
              _ => panic!(),
            }
          } else if waiter == waiter2 {
            match r2 {
              SendResult::Full(Some(waiter)) => assert_eq!(waiter, waiter1),
              _ => panic!(),
            }
          } else {
            panic!()
          }
        },
        _ => panic!(),
      }

      assert_eq!(queue.len(), 1);
    }

    #[test]
    fn enqueue_when_closed() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let waiter = hooks.manage(ChannelWaiter::new(true));

      let mut queue = ChannelQueue::sync();

      assert_eq!(queue.close(), CloseResult::Ok);
      let r = queue.send(waiter, val!(10.0));

      assert_eq!(r, SendResult::Closed);
      assert_eq!(queue.len(), 0);
    }

    #[test]
    fn dequeue_when_empty() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let waiter = hooks.manage(ChannelWaiter::new(true));

      let mut queue = ChannelQueue::sync();

      let r = queue.receive(waiter);
      assert_eq!(r, ReceiveResult::EmptyBlock(None));
      assert_eq!(queue.len(), 0);
    }

    #[test]
    fn dequeue_when_full() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let waiter1 = hooks.manage(ChannelWaiter::new(true));
      let waiter2 = hooks.manage(ChannelWaiter::new(true));

      let mut queue = ChannelQueue::with_capacity(1);
      queue.send(waiter2, val!(1.0));

      let r = queue.receive(waiter1);
      assert_eq!(r, ReceiveResult::Ok(val!(1.0)));
      assert_eq!(queue.len(), 0);
    }

    #[test]
    fn dequeue_when_full_with_waiter() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let waiter1 = hooks.manage(ChannelWaiter::new(true));
      let waiter2 = hooks.manage(ChannelWaiter::new(true));
      let mut queue = ChannelQueue::sync();

      queue.send(waiter1, val!(1.0));
      queue.send(waiter1, val!(2.0));

      assert_eq!(queue.receive(waiter2), ReceiveResult::Ok(val!(1.0)));
      assert_eq!(
        queue.receive(waiter2),
        ReceiveResult::EmptyBlock(Some(waiter1))
      );
      assert_eq!(queue.len(), 0);
    }

    #[test]
    fn dequeue_when_waiter_not_runnable() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let waiter1 = hooks.manage(ChannelWaiter::new(false));
      let waiter2 = hooks.manage(ChannelWaiter::new(true));
      let mut queue = ChannelQueue::sync();

      queue.send(waiter1, val!(1.0));
      queue.send(waiter1, val!(2.0));

      assert_eq!(queue.receive(waiter2), ReceiveResult::Ok(val!(1.0)));
      assert_eq!(queue.receive(waiter2), ReceiveResult::EmptyBlock(None));
      assert_eq!(queue.len(), 0);
    }

    #[test]
    fn dequeue_when_closed() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let waiter = hooks.manage(ChannelWaiter::new(true));
      let mut queue = ChannelQueue::sync();

      queue.send(waiter, val!(10.0));
      assert_eq!(queue.close(), CloseResult::Ok);
      let r = queue.receive(waiter);

      assert_eq!(r, ReceiveResult::Ok(val!(10.0)));
      assert_eq!(queue.len(), 0);
    }

    #[test]
    fn dequeue_when_closed_empty() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let waiter = hooks.manage(ChannelWaiter::new(true));
      let mut queue = ChannelQueue::sync();

      assert_eq!(queue.close(), CloseResult::Ok);
      let r = queue.receive(waiter);

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

      let waiter = hooks.manage(ChannelWaiter::new(true));
      let mut queue = ChannelQueue::with_capacity(3);

      let r = queue.send(waiter, val!(1.0));
      assert_eq!(r, SendResult::Ok);
      assert_eq!(queue.len(), 1);
    }

    #[test]
    fn enqueue_when_full() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let waiter = hooks.manage(ChannelWaiter::new(true));
      let mut queue = ChannelQueue::with_capacity(1);

      queue.send(waiter, val!(1.0));
      let r = queue.send(waiter, val!(1.0));
      assert_eq!(r, SendResult::Full(None));
      assert_eq!(queue.len(), 1);
    }

    #[test]
    fn enqueue_when_full_with_waiter() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let waiter1 = hooks.manage(ChannelWaiter::new(true));
      let waiter2 = hooks.manage(ChannelWaiter::new(true));

      let mut queue = ChannelQueue::with_capacity(1);
      queue.receive(waiter1);
      queue.send(waiter2, val!(1.0));
      let r = queue.send(waiter2, val!(1.0));

      assert_eq!(r, SendResult::Full(Some(waiter1)));
      assert_eq!(queue.len(), 1);
    }

    #[test]
    fn enqueue_when_waiter_not_runnable() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let waiter1 = hooks.manage(ChannelWaiter::new(false));
      let waiter2 = hooks.manage(ChannelWaiter::new(true));
      let mut queue = ChannelQueue::with_capacity(1);

      queue.receive(waiter1);

      assert_eq!(queue.send(waiter2, val!(1.0)), SendResult::Ok);
      assert_eq!(queue.send(waiter2, val!(1.0)), SendResult::Full(None));
      assert_eq!(queue.len(), 1);
    }

    #[test]
    fn enqueue_when_closed() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let waiter = hooks.manage(ChannelWaiter::new(true));
      let mut queue = ChannelQueue::with_capacity(1);

      assert_eq!(queue.close(), CloseResult::Ok);
      let r = queue.send(waiter, val!(10.0));

      assert_eq!(r, SendResult::Closed);
      assert_eq!(queue.len(), 0);
    }

    #[test]
    fn dequeue_when_empty() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let waiter = hooks.manage(ChannelWaiter::new(true));
      let mut queue = ChannelQueue::with_capacity(3);

      let r = queue.receive(waiter);
      assert_eq!(r, ReceiveResult::Empty(None));
      assert_eq!(queue.len(), 0);
    }

    #[test]
    fn dequeue_when_empty_with_waiter() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let waiter1 = hooks.manage(ChannelWaiter::new(true));
      let waiter2 = hooks.manage(ChannelWaiter::new(true));

      let mut queue = ChannelQueue::with_capacity(1);
      queue.send(waiter1, val!(1.0));
      queue.send(waiter1, val!(1.0));

      queue.receive(waiter2);
      let r = queue.receive(waiter2);
      assert_eq!(r, ReceiveResult::Empty(Some(waiter1)));
      assert_eq!(queue.len(), 0);
    }

    #[test]
    fn dequeue_when_waiter_not_runnable() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let waiter1 = hooks.manage(ChannelWaiter::new(false));
      let waiter2 = hooks.manage(ChannelWaiter::new(true));
      let mut queue = ChannelQueue::with_capacity(1);

      queue.send(waiter1, val!(1.0));
      queue.send(waiter1, val!(2.0));

      assert_eq!(queue.receive(waiter2), ReceiveResult::Ok(val!(1.0)));
      assert_eq!(queue.receive(waiter2), ReceiveResult::Empty(None));
      assert_eq!(queue.len(), 0);
    }

    #[test]
    fn dequeue_when_nonempty() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let waiter = hooks.manage(ChannelWaiter::new(true));
      let mut queue = ChannelQueue::with_capacity(3);

      queue.send(waiter, val!(1.0));
      queue.send(waiter, val!(2.0));
      let r = queue.receive(waiter);
      assert_eq!(r, ReceiveResult::Ok(val!(1.0)));
      assert_eq!(queue.len(), 1);
    }

    #[test]
    fn dequeue_when_closed() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let waiter = hooks.manage(ChannelWaiter::new(true));
      let mut queue = ChannelQueue::with_capacity(3);

      queue.send(waiter, val!(10.0));
      assert_eq!(queue.close(), CloseResult::Ok);
      let r = queue.receive(waiter);

      assert_eq!(r, ReceiveResult::Ok(val!(10.0)));
      assert_eq!(queue.len(), 0);
    }

    #[test]
    fn dequeue_when_closed_empty() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let waiter = hooks.manage(ChannelWaiter::new(true));
      let mut queue = ChannelQueue::with_capacity(1);

      assert_eq!(queue.close(), CloseResult::Ok);
      let r = queue.receive(waiter);

      assert_eq!(r, ReceiveResult::Closed);
      assert_eq!(queue.len(), 0);
    }
  }
}
