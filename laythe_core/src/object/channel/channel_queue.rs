use std::{collections::VecDeque, fmt, io::Write};

use crate::{
  managed::{DebugHeap, DebugWrap, Trace}, object::Fiber, reference::ObjRef, value::Value
};

use super::{CloseResult, ReceiveResult, SendResult};

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

#[derive(PartialEq, Clone)]
pub struct ChannelQueue {
  queue: VecDeque<Value>,
  capacity: usize,
  state: ChannelQueueState,
  kind: ChannelQueueKind,

  send_waiters: VecDeque<ObjRef<Fiber>>,
  receive_waiters: VecDeque<ObjRef<Fiber>>,
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
  pub fn send(&mut self, fiber: ObjRef<Fiber>, val: Value) -> SendResult {
    match self.state {
      ChannelQueueState::Ready => {
        if self.is_sync() && self.queue.is_empty() {
          self.queue.push_back(val);
          self.send_waiters.push_back(fiber);
          return SendResult::FullBlock(find_runnable_waiter(&mut self.receive_waiters));
        }

        if self.queue.len() < self.capacity {
          self.queue.push_back(val);
          SendResult::Ok
        } else {
          self.send_waiters.push_back(fiber);
          SendResult::Full(find_runnable_waiter(&mut self.receive_waiters))
        }
      },
      ChannelQueueState::Closed | ChannelQueueState::ClosedEmpty => SendResult::Closed,
    }
  }

  /// Attempt to receive a value from this channel.
  /// If the channel is empty return potentially a send
  /// waiter. If
  pub fn receive(&mut self, fiber: ObjRef<Fiber>) -> ReceiveResult {
    match self.state {
      ChannelQueueState::Ready => match self.queue.pop_front() {
        Some(value) => ReceiveResult::Ok(value),
        None => {
          self.receive_waiters.push_back(fiber);

          let waiter = find_runnable_waiter(&mut self.send_waiters);

          if self.is_sync() {
            ReceiveResult::EmptyBlock(waiter)
          } else {
            ReceiveResult::Empty(waiter)
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
  pub fn runnable_waiter(&mut self) -> Option<ObjRef<Fiber>> {
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

fn find_runnable_waiter(queue: &mut VecDeque<ObjRef<Fiber>>) -> Option<ObjRef<Fiber>> {
  while let Some(waiter) = queue.pop_front() {
    if !waiter.is_complete() {
      return Some(waiter)
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
      .field("send_waiters", &DebugWrap(&self.send_waiters, depth))
      .field("receive_waiters", &DebugWrap(&self.receive_waiters, depth))
      .field("kind", &self.state)
      .finish()
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
      fn enqueue_when_completed_waiter() {
        let context = NoContext::default();
        let hooks = GcHooks::new(&context);

        let fiber = FiberBuilder::default().build(&hooks).unwrap();
        let mut fiber_waiter = FiberBuilder::default().build(&hooks).unwrap();
        let mut queue = ChannelQueue::sync();

        queue.receive(fiber_waiter);

        fiber_waiter.activate();
        fiber_waiter.complete();

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
                _ => panic!(),
              }
            } else if waiter == fiber_waiter2 {
              match r2 {
                SendResult::Full(Some(waiter)) => assert_eq!(waiter, fiber_waiter1),
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
      fn dequeue_when_full_with_waiter() {
        let context = NoContext::default();
        let hooks = GcHooks::new(&context);

        let fiber = FiberBuilder::default().build(&hooks).unwrap();
        let fiber_waiter = FiberBuilder::default().build(&hooks).unwrap();
        let mut queue = ChannelQueue::sync();

        queue.send(fiber_waiter, val!(1.0));
        queue.send(fiber_waiter, val!(2.0));

        assert_eq!(queue.receive(fiber), ReceiveResult::Ok(val!(1.0)));
        assert_eq!(queue.receive(fiber), ReceiveResult::EmptyBlock(Some(fiber_waiter)));
        assert_eq!(queue.len(), 0);
      }

      #[test]
      fn dequeue_when_waiter_complete() {
        let context = NoContext::default();
        let hooks = GcHooks::new(&context);

        let fiber = FiberBuilder::default().build(&hooks).unwrap();
        let mut fiber_waiter = FiberBuilder::default().build(&hooks).unwrap();
        let mut queue = ChannelQueue::sync();

        queue.send(fiber_waiter, val!(1.0));
        queue.send(fiber_waiter, val!(2.0));

        fiber_waiter.activate();
        fiber_waiter.complete();

        assert_eq!(queue.receive(fiber), ReceiveResult::Ok(val!(1.0)));
        assert_eq!(queue.receive(fiber), ReceiveResult::EmptyBlock(None));
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
      fn enqueue_when_completed_waiter() {
        let context = NoContext::default();
        let hooks = GcHooks::new(&context);

        let fiber = FiberBuilder::default().build(&hooks).unwrap();
        let mut fiber_waiter = FiberBuilder::default().build(&hooks).unwrap();
        let mut queue = ChannelQueue::with_capacity(1);

        queue.receive(fiber_waiter);

        fiber_waiter.activate();
        fiber_waiter.complete();

        assert_eq!(queue.send(fiber, val!(1.0)), SendResult::Ok);
        assert_eq!(queue.send(fiber, val!(1.0)), SendResult::Full(None));
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
      fn dequeue_when_waiter_complete() {
        let context = NoContext::default();
        let hooks = GcHooks::new(&context);

        let fiber = FiberBuilder::default().build(&hooks).unwrap();
        let mut fiber_waiter = FiberBuilder::default().build(&hooks).unwrap();
        let mut queue = ChannelQueue::with_capacity(1);

        queue.send(fiber_waiter, val!(1.0));
        queue.send(fiber_waiter, val!(2.0));

        fiber_waiter.activate();
        fiber_waiter.complete();

        assert_eq!(queue.receive(fiber), ReceiveResult::Ok(val!(1.0)));
        assert_eq!(queue.receive(fiber), ReceiveResult::Empty(None));
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
}