#[cfg(test)]
mod builder;

mod call_frame;
mod exception_handler;

use self::{call_frame::CallFrame, exception_handler::ExceptionHandler};
use laythe_core::{
  constants::SCRIPT,
  if_let_obj,
  managed::{AllocResult, Allocate, DebugHeap, DebugWrap, Trace, TraceRoot},
  match_obj,
  object::{Channel, ChannelWaiter, Fun, Instance, List, ListLocation, ObjectKind},
  val,
  value::{Value, VALUE_NIL},
  Allocator, Captures, ObjRef, Ref,
};
use std::{cell::RefMut, fmt, io::Write, ptr};

const INITIAL_FRAME_SIZE: usize = 4;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum FiberState {
  Running,
  Unwinding,
  Pending,
  Blocked,
  Complete,
}

pub enum FiberPopResult {
  Empty,
  Emptied,
  Ok(ObjRef<Fun>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum UnwindResult<'a> {
  PotentiallyHandled(&'a mut CallFrame),
  Unhandled,
  UnwindStopped,
}

fn frame_line(fun: ObjRef<Fun>, offset: usize) -> String {
  match &*fun.name() {
    SCRIPT => format!(
      "{}:{} in script",
      fun.module().path(),
      fun.chunk().get_line(offset)
    )
    .to_string(),
    _ => format!(
      "{}:{} in {}()",
      fun.module().path(),
      fun.chunk().get_line(offset),
      fun.name()
    )
    .to_string(),
  }
}

pub struct Fiber {
  /// A stack holding all local variable currently in use
  stack: Vec<Value>,

  /// A stack holding call frames currently in use
  frames: Vec<CallFrame>,

  /// A stack of exception handlers active on this fiber
  exception_handlers: Vec<ExceptionHandler>,

  /// A list of channels executed on this fiber
  channels: Vec<ObjRef<Channel>>,

  /// The parent fiber to this fiber
  parent: Option<Ref<Self>>,

  /// The channel waiter for this fiber
  waiter: Ref<ChannelWaiter>,

  /// pointer to the top of the value stack
  stack_top: *mut Value,

  /// The current frame's closure
  frame: *mut CallFrame,

  /// The current state of this fiber
  state: FiberState,

  /// The current error context if one is active
  error: Option<Instance>,

  /// Backtrace's instruction pointers used during an unwind
  backtrace_ips: Vec<*const u8>,
}

impl Fiber {
  /// Create a new fiber from the provided closure. The fiber uses
  /// this initial closure to determine how much stack space to initially
  /// reserve
  pub fn new<C: TraceRoot>(
    allocator: &mut RefMut<'_, Allocator>,
    context: &C,
    parent: Option<Ref<Fiber>>,
    fun: ObjRef<Fun>,
    captures: Captures,
    stack_count: usize,
  ) -> Self {
    if fun.chunk().instructions().is_empty() {
      panic!("Fiber's launch function lacking instructions")
    }

    // reserve resources
    let mut frames = Vec::<CallFrame>::with_capacity(INITIAL_FRAME_SIZE);
    let mut stack = vec![VALUE_NIL; stack_count];

    let waiter = allocator.manage(ChannelWaiter::new(true), context);

    // push closure and frame onto fiber
    stack[0] = val!(fun);
    frames.push(CallFrame::new(fun, captures, stack.as_mut_ptr()));

    // get pointers to the call frame and stack top
    let current_frame = frames.as_mut_ptr();
    let stack_top = unsafe { stack.as_mut_ptr().offset(1) };

    Self {
      stack,
      waiter,
      frames,
      parent,
      channels: vec![],
      exception_handlers: vec![],
      state: FiberState::Pending,
      error: None,
      frame: current_frame,
      backtrace_ips: vec![],
      stack_top,
    }
  }

  /// Get the current slice call frame slice
  pub fn frames(&self) -> &[CallFrame] {
    &self.frames
  }

  pub fn scan_roots(&mut self) {
    for value in self.stack.iter_mut() {
      fn compact_slice(slice: &mut [Value]) {
        for value in slice {
          compact_value(value);
        }
      }

      fn compact_value(value: &mut Value) {
        if_let_obj!(ObjectKind::List(list) = (*value) {
          forward_list(value, list);
        });
      }

      fn forward_list(value: &mut Value, list: List) {
        if let ListLocation::Forwarded(list) = list.state() {
          *value = val!(list)
        }
      }

      if value.is_obj() {
        match_obj!((&value.to_obj()) {
          ObjectKind::List(mut list) => {
            forward_list(value, list);
            compact_slice(&mut list);
          },
          ObjectKind::Tuple(mut tuple) => {
            compact_slice(&mut tuple);
          },
          ObjectKind::Instance(mut instance) => {
            compact_slice(&mut instance);
          },
          ObjectKind::Map(mut map) => {
            for (_, value) in map.iter_mut() {
              compact_value(value);
            }
          },
          _ => (),
        })
      }
    }
  }

  pub fn waiter(&self) -> Ref<ChannelWaiter> {
    self.waiter
  }

  /// Get the current number of call frames on this fiber
  fn frame_count(&self) -> usize {
    self.frames.len()
  }

  /// Get the current frame's stack start
  pub fn stack_start(&self) -> *mut Value {
    self.frame().stack_start()
  }

  /// Get the current frame's current function
  pub fn fun(&self) -> ObjRef<Fun> {
    self.frame().fun()
  }

  /// Get the current frame's current captures
  pub fn captures(&self) -> Captures {
    self.frame().captures()
  }

  /// Is this fiber complete
  pub fn is_complete(&self) -> bool {
    self.state == FiberState::Complete
  }

  /// Is this fiber pending
  pub fn is_pending(&self) -> bool {
    self.state == FiberState::Pending
  }

  /// Activate this fiber
  pub fn activate(&mut self) {
    assert!(matches!(
      self.state,
      FiberState::Pending | FiberState::Unwinding
    ));
    self.state = FiberState::Running;
  }

  /// Put this fiber to sleep
  pub fn sleep(&mut self) {
    assert_eq!(self.state, FiberState::Running);
    self.state = FiberState::Pending;
    self.waiter.set_runnable(true);
  }

  /// Block this fiber
  pub fn block(&mut self) {
    assert_eq!(self.state, FiberState::Running);
    self.state = FiberState::Blocked;
  }

  /// Unblock this fiber
  pub fn unblock(&mut self) {
    assert!(matches!(
      self.state,
      FiberState::Blocked | FiberState::Pending
    ));
    if let FiberState::Blocked = self.state {
      self.state = FiberState::Pending;
    }
  }

  /// Activate this fiber
  pub fn complete(&mut self) -> Option<Ref<ChannelWaiter>> {
    assert_eq!(self.state, FiberState::Running);

    self.state = FiberState::Complete;
    self.waiter.set_runnable(false);

    // load from waiting fiber biases toward the parent fiber
    let waiter = self
      .parent
      .filter(|parent| parent.is_pending())
      .map(|parent| parent.waiter)
      .or_else(|| self.get_runnable());

    self.channels.clear();

    waiter
  }

  /// When an error occurs while handling an exception as in
  /// there is an error with the handler itself we need to
  /// readjust the backtrace to point back to the current
  /// instruction pointers
  pub fn error_while_handling(&mut self) {
    self.pop_exception_handler();
    self.backtrace_ips.clear();
  }

  /// pause unwind to search for handler
  fn pause_unwind(&mut self, new_frame_top: usize) {
    assert!(matches!(
      self.state,
      FiberState::Running | FiberState::Unwinding
    ));

    self.state = FiberState::Unwinding;

    let current_len = self.backtrace_ips.len();
    let additional_len = ((self.frames().len() - new_frame_top) + 1) - current_len;

    // extend backtraces instruction pointers
    self.backtrace_ips.extend(
      self
        .frames()
        .iter()
        .rev()
        .skip(current_len)
        .take(additional_len)
        .map(|frame| frame.ip())
        .collect::<Vec<*const u8>>(),
    );
  }

  /// Try to get a runnable fiber
  pub fn get_runnable(&mut self) -> Option<Ref<ChannelWaiter>> {
    if self.channels.is_empty() {
      return None;
    }

    self
      .channels
      .iter_mut()
      .map(|channel| channel.runnable_waiter())
      .find(|fiber| fiber.is_some())
      .unwrap_or(None)
  }

  /// Add a channel to the list of used channels by
  /// this fiber
  pub fn add_used_channel(&mut self, channel: ObjRef<Channel>) {
    if !self.channels.iter().any(|c| *c == channel) {
      self.channels.push(channel);
    }
  }

  /// push a value onto the stack
  ///
  /// ## Safety
  /// A stack push makes no bounds checks and it's possible
  /// to attempt to write a value past the end of the stack.
  /// ensure sufficient stack space has been allocated
  pub unsafe fn push(&mut self, value: Value) {
    self.set_val(0, value);
    self.stack_top = self.stack_top.offset(1);
  }

  /// pop a value off the stack
  ///
  /// ## Safety
  /// A stack pop makes no bounds checks and it's possible to
  /// attempt to write a value prior the start of the stack.
  /// ensure sufficient stack space has been allocated
  pub unsafe fn pop(&mut self) -> Value {
    self.drop();
    self.get_val(0)
  }

  /// drop a value off the stack
  ///
  /// ## Safety
  /// A stack drop makes not bounds checks and it's possible to
  /// move the current stack top prior to the beginning of the stack
  pub unsafe fn drop(&mut self) {
    self.stack_top = self.stack_top.offset(-1);

    #[cfg(debug_assertions)]
    self.assert_stack_inbounds();
  }

  /// drop n values off the stack
  ///
  /// ## Safety
  /// Dropping n values the stack makes not bounds checks and it' possible
  /// to move the current stack top prior to the beginning of the stack
  pub unsafe fn drop_n(&mut self, count: usize) {
    self.stack_top = self.stack_top.sub(count);

    #[cfg(debug_assertions)]
    self.assert_stack_inbounds();
  }

  /// Retrieve a value n slots from the stack head
  ///
  /// ## Safety
  /// Peeking n slot down make no bounds checks if distance is
  /// greater than the current stack length
  pub unsafe fn peek(&self, distance: usize) -> Value {
    self.get_val(distance + 1)
  }

  /// Set a value n slots from the stack head
  ///
  ///
  /// ## Safety
  /// Setting a value n slot down make no bounds checks if distance is
  /// greater than the current stack length
  pub unsafe fn peek_set(&mut self, distance: usize, val: Value) {
    self.set_val(distance + 1, val)
  }

  /// Push a new exception handler onto this fiber
  pub fn push_exception_handler(&mut self, offset: usize, slot_depth: usize) {
    debug_assert!(
      offset < self.fun().chunk().instructions().len(),
      "Offset past end of functions"
    );
    debug_assert!(
      slot_depth < self.fun().max_slots(),
      "Slot offset more than function maximum"
    );

    let call_frame_depth = self.frame_count();

    self
      .exception_handlers
      .push(ExceptionHandler::new(offset, call_frame_depth, slot_depth));
  }

  /// Pop an exception handler off this fiber
  pub fn pop_exception_handler(&mut self) {
    assert!(
      self.exception_handlers.pop().is_some(),
      "Attempted to pop an empty vec of exception handlers"
    );
  }

  /// Do we currently have an active exception handler
  fn exception_handler(&self) -> Option<ExceptionHandler> {
    self.exception_handlers.last().copied()
  }

  /// Load the instruction pointer from the current frame
  pub fn load_ip(&mut self) -> *const u8 {
    self.frame().ip()
  }

  /// Store an instruction pointer into the current frame
  pub fn store_ip(&mut self, ip: *const u8) {
    self.frame_mut().store_ip(ip);
  }

  /// Get a slice of the top count values on the stack
  pub fn frame_stack(&self) -> &[Value] {
    unsafe {
      let stack_start = (*self.frame).stack_start();
      let len = self.stack_top.offset_from(stack_start);

      #[cfg(debug_assertions)]
      {
        if len > 0 {
          assert_inbounds(&self.stack, stack_start);
        }
      }

      std::slice::from_raw_parts(stack_start, len as usize)
    }
  }

  /// Get a slice of the top count values on the stack
  ///
  /// ## Safety
  /// Not bounds checks are made against the lower bound
  /// a value of len greater than the len will read past
  /// the beginning of the stack
  pub unsafe fn stack_slice(&self, len: usize) -> &[Value] {
    let start = self.stack_top.sub(len);

    #[cfg(debug_assertions)]
    {
      if len > 0 {
        assert_inbounds(&self.stack, start);
      }
    }

    std::slice::from_raw_parts(start, len)
  }

  /// Retrieve the current error on this fiber
  pub fn error(&self) -> Option<Instance> {
    self.error
  }

  /// Set the current error on this fiber and ip it occurred at
  pub fn set_error(&mut self, error: Instance) {
    self.error = Some(error);
  }

  /// Push a frame onto the call stack
  pub fn push_frame(&mut self, fun: ObjRef<Fun>, captures: Captures, arg_count: usize) {
    unsafe {
      self.ensure_stack(fun.max_slots());
      let stack_start = self.stack_top.sub(arg_count + 1);

      #[cfg(debug_assertions)]
      assert_inbounds(&self.stack, stack_start);

      self.frames.push(CallFrame::new(fun, captures, stack_start));
      self.frame = self.frames.as_mut_ptr().add(self.frames.len() - 1);

      #[cfg(debug_assertions)]
      self.assert_frame_inbounds();
    }
  }

  /// Pop a frame off the call stack
  pub fn pop_frame(&mut self) -> FiberPopResult {
    // Check if frame is empty
    let len = self.frames.len();
    match len {
      0 => FiberPopResult::Empty,
      1 => {
        self.stack_top = self.frame().stack_start();
        self.frame = ptr::null_mut();
        self.frames.pop();

        FiberPopResult::Emptied
      },
      _ => {
        self.stack_top = self.frame().stack_start();

        unsafe {
          self.frame = self.frame.offset(-1);
        }

        #[cfg(debug_assertions)]
        self.assert_frame_inbounds();

        self.frames.pop();
        FiberPopResult::Ok(self.frame().fun())
      },
    }
  }

  /// Attempt to create a new fiber using the most recent call frame
  pub fn split<C: TraceRoot>(
    mut fiber: Ref<Fiber>,
    allocator: &mut RefMut<'_, Allocator>,
    context: &C,
    arg_count: usize,
  ) -> Ref<Fiber> {
    let frame = fiber.frames.pop().expect("Expected call frame");
    allocator.push_root(frame);

    let new_fiber = Fiber::new(
      allocator,
      context,
      Some(fiber),
      frame.fun(),
      frame.captures(),
      frame.fun().max_slots() + arg_count + 1,
    );

    let mut new_fiber = allocator.manage(new_fiber, context);
    allocator.pop_roots(1);

    let slots = fiber.frame_stack().len();
    assert_eq!(slots, arg_count + 1);

    // if we have any argument bulk copy them to the fiber
    if slots > 1 {
      unsafe {
        ptr::copy_nonoverlapping(
          fiber.frame().stack_start().offset(1),
          new_fiber.stack_top,
          arg_count,
        );
        new_fiber.stack_top = new_fiber.stack_top.add(arg_count);
      }
    }

    // Effectively pop the current fibers frame so they're 'moved'
    // to the new fiber
    unsafe {
      fiber.stack_top = fiber.frame().stack_start();
      fiber.frame = fiber.frame.sub(1);
    }

    new_fiber
  }

  /// Ensure the stack has enough space. If more space is required
  /// additional space is allocated. All pointers into the stack
  /// are then updated
  pub fn ensure_stack(&mut self, additional: usize) {
    // check is we already have enough space
    let len = unsafe { self.stack_top.offset_from(self.stack.as_ptr()) };
    if self.stack.capacity() >= len as usize + additional {
      return;
    }

    let stack_old = self.stack.as_mut_ptr();
    self.stack.reserve(additional);
    let stack_new = self.stack.as_mut_ptr();

    // If we relocated update pointers
    if stack_old != stack_new {
      unsafe {
        self.stack_top = stack_new.offset(self.stack_top.offset_from(stack_old));

        self.frames.iter_mut().for_each(|frame| {
          frame.store_stack_start(stack_new.offset(frame.stack_start().offset_from(stack_old)));
        });
      }
    }

    // eagerly fill stack will nil
    while self.stack.len() != self.stack.capacity() {
      self.stack.push(VALUE_NIL);
    }
    debug_assert_eq!(self.stack.len(), self.stack.capacity());
  }

  /// Unwind the stack searching for catch blocks to handle the unwind.
  /// If a handler is found returns the call frame that handles the exception
  /// if not found returns none
  ///
  /// # Safety
  /// Assumes the function try block slot offset points to a valid offset into the
  /// associated call frames slots
  pub unsafe fn stack_unwind(&mut self, bottom_frame: Option<usize>) -> UnwindResult {
    // grab the appropriate exception handler
    let exception_handler = match self.exception_handler() {
      Some(exception_handler) => {
        let bottom_frame = bottom_frame.unwrap_or(0);
        if exception_handler.call_frame_depth() >= bottom_frame {
          exception_handler
        } else {
          return UnwindResult::UnwindStopped;
        }
      },
      None => {
        return match bottom_frame {
          Some(_) => UnwindResult::UnwindStopped,
          None => UnwindResult::Unhandled,
        }
      },
    };

    debug_assert!(
      self.frames.len() >= exception_handler.call_frame_depth(),
      "Exception handler points to non existing stack frame"
    );

    // put the fiber in a state to search for handle
    self.pause_unwind(exception_handler.call_frame_depth());

    // set the current frame based on the exception handler offset
    let frame = &mut self.frames[exception_handler.call_frame_depth() - 1];
    let fun = frame.fun();
    let instructions = fun.chunk().instructions();

    let stack_top = frame.stack_start().add(exception_handler.slot_depth());

    // set the current ip frame and stack pointer
    frame.store_ip(&instructions[exception_handler.offset()] as *const u8);
    self.frame = frame as *mut CallFrame;
    self.stack_top = stack_top;

    UnwindResult::PotentiallyHandled(frame)
  }

  /// Signals to the fiber to finish unwinding. The fiber is
  /// moved back into the activated state. The unwound back trace
  /// is returned and then truncated off the fiber
  pub fn finish_unwind(&mut self) -> Vec<String> {
    // Now that we've found a handler we truncate the frames
    let handler = self
      .exception_handler()
      .expect("Expected handler to still exist");

    // Gather backtrace and set it on the error
    let backtrace = self.error_backtrace(&handler);
    self.frames.truncate(handler.call_frame_depth());
    self.backtrace_ips.clear();

    // Put the fiber back into an activated state
    self.activate();

    backtrace
  }

  // Create the string representation of the backtrace
  fn error_backtrace(&self, handler: &ExceptionHandler) -> Vec<String> {
    let backtrace_len = (self.frames.len() - handler.call_frame_depth()) + 1;

    self
      .frames
      .iter()
      .rev()
      .take(backtrace_len)
      .zip(self.backtrace_ips.iter())
      .map(|(frame, ip)| {
        let fun = frame.fun();
        let offset = unsafe { ip.offset_from(fun.chunk().instructions().as_ptr()) } as usize;

        frame_line(fun, offset.saturating_sub(1))
      })
      .collect::<Vec<String>>()
  }

  /// Print a error message with the associated stack track if found
  pub fn print_error(&self, log: &mut dyn Write, error: Instance) {
    writeln!(log, "Traceback (most recent call last):").expect("Unable to write to stderr");

    for frame in self.frames.iter().rev() {
      let fun = frame.fun();
      let location: String = match &*fun.name() {
        SCRIPT => SCRIPT.to_owned(),
        _ => format!("{}()", &*fun.name()),
      };

      let offset = unsafe { frame.ip().offset_from(fun.chunk().instructions().as_ptr()) } as usize;
      writeln!(
        log,
        "  {}:{} in {}",
        fun.module().path(),
        fun.chunk().get_line(offset.saturating_sub(1)),
        location
      )
      .expect("Unable to write to stderr");
    }

    let message = error[0].to_obj().to_str();
    writeln!(log, "{}: {}", &*error.class().name(), &*message).expect("Unable to write to stderr");
  }

  /// Get a value on the stack
  unsafe fn get_val(&self, offset: usize) -> Value {
    let location = self.stack_top.sub(offset);

    #[cfg(debug_assertions)]
    assert_inbounds(&self.stack, location);

    *location
  }

  /// Set a value on the stack
  unsafe fn set_val(&mut self, offset: usize, val: Value) {
    let location = self.stack_top.sub(offset);

    #[cfg(debug_assertions)]
    assert_inbounds(&self.stack, location);

    *location = val
  }

  /// An immutable reference to the current frame
  fn frame(&self) -> &CallFrame {
    debug_assert!(!self.frame.is_null());
    unsafe { &*self.frame }
  }

  /// A mutable reference to the current frame
  fn frame_mut(&mut self) -> &mut CallFrame {
    debug_assert!(!self.frame.is_null());
    unsafe { &mut *self.frame }
  }

  // assert the stack_top is currently pointing inbounds
  #[cfg(debug_assertions)]
  fn assert_stack_inbounds(&self) {
    assert_inbounds(&self.stack, self.stack_top);
  }

  // assert the frame is currently pointing inbounds
  #[cfg(debug_assertions)]
  fn assert_frame_inbounds(&self) {
    assert_inbounds(&self.frames, self.frame);
  }
}

/// Assert the provided pointer is inbounds this slice
#[cfg(debug_assertions)]
fn assert_inbounds<T>(slice: &[T], ptr: *mut T) {
  unsafe {
    let offset = ptr.offset_from(slice.as_ptr());
    assert!(offset >= 0, "Attempted to index prior to the slice. Pointer offset was {offset} with slice start {slice:p}, ptr {ptr:p}");
    assert!(
      (offset as usize) < slice.len(),
      "Attempted to index past the end of the slice, Pointer offset was {offset} with slice start {slice:p} and ptr {ptr:p}"
    )
  }
}

impl fmt::Display for Fiber {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<fiber {self:p}>")
  }
}

impl fmt::Debug for Fiber {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.fmt_heap(f, 2)
  }
}

impl Allocate<Ref<Self>> for Fiber {
  fn alloc(self) -> AllocResult<Ref<Self>> {
    Ref::alloc_result(self)
  }
}

impl DebugHeap for Fiber {
  fn fmt_heap(&self, f: &mut std::fmt::Formatter, depth: usize) -> std::fmt::Result {
    let stack_slice = unsafe {
      let start = self.stack.as_ptr();
      let len = self.stack_top.offset_from(start) as usize;
      std::slice::from_raw_parts(start, len)
    };

    f.debug_struct("Fiber")
      .field("stack", &DebugWrap(&stack_slice, depth))
      .field("frames", &DebugWrap(&&*self.frames, depth))
      .field("channels", &DebugWrap(&&*self.channels, depth))
      .field("stack_top", &format_args!("{:p}", self.stack_top))
      .field("current_frame", &format_args!("{:p}", self.frame))
      .field("state", &self.state)
      .field("current_error", &DebugWrap(&self.error, depth))
      .finish()
  }
}

impl Trace for Fiber {
  #[inline]
  fn trace(&self) {
    unsafe {
      let start = self.stack.as_ptr();
      let len = self.stack_top.offset_from(start) as usize;
      let slice = std::slice::from_raw_parts(start, len);

      slice.iter().for_each(|value| {
        value.trace();
      });
    }

    self.channels.iter().for_each(|channel| {
      channel.trace();
    });

    self.frames.iter().for_each(|call_frame| call_frame.trace());

    if let Some(error) = &self.error {
      error.trace();
    }

    if let Some(fiber) = &self.parent {
      fiber.trace();
    }
  }

  fn trace_debug(&self, log: &mut dyn std::io::Write) {
    unsafe {
      let start = self.stack.as_ptr();
      let len = self.stack_top.offset_from(start) as usize;
      let slice = std::slice::from_raw_parts(start, len);

      slice.iter().for_each(|value| {
        value.trace_debug(log);
      });
    }

    self.channels.iter().for_each(|channel| {
      channel.trace_debug(log);
    });

    self
      .frames
      .iter()
      .for_each(|call_frame| call_frame.trace_debug(log));

    if let Some(error) = &self.error {
      error.trace_debug(log);
    }

    if let Some(fiber) = &self.parent {
      fiber.trace_debug(log);
    }
  }
}

unsafe impl Send for Fiber {}
unsafe impl Sync for Fiber {}

#[cfg(test)]
mod test {
  use builder::TestFiberBuilder;
  use laythe_core::{
    object::FunBuilder,
    signature::Arity,
    support::{test_fun, test_fun_builder, test_module},
    Chunk, GcContext, GcHooks, NoContext,
  };

  use super::*;

  #[test]
  fn new() {
    let context = NoContext::default();

    TestFiberBuilder::default().build(&context);
  }

  #[test]
  fn push() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = TestFiberBuilder::default().max_slots(4).build(&context);

    unsafe {
      fiber.push(val!(10.5));
      fiber.push(val!(false));
      fiber.push(VALUE_NIL);
      fiber.push(val!(hooks.manage_str("test")));

      assert_eq!(fiber.peek(0), val!(hooks.manage_str("test")));
      assert_eq!(fiber.peek(1), VALUE_NIL);
      assert_eq!(fiber.peek(2), val!(false));
      assert_eq!(fiber.peek(3), val!(10.5));
    }
  }

  #[test]
  #[should_panic]
  fn push_out_of_bounds() {
    let context = NoContext::default();

    let mut fiber = TestFiberBuilder::default().max_slots(1).build(&context);

    unsafe {
      fiber.push(val!(10.5));
      fiber.push(val!(false));
    }
  }

  #[test]
  fn pop() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = TestFiberBuilder::default().max_slots(4).build(&context);

    unsafe {
      fiber.push(val!(10.5));
      fiber.push(val!(false));
      fiber.push(VALUE_NIL);
      fiber.push(val!(hooks.manage_str("test")));

      assert_eq!(fiber.pop(), val!(hooks.manage_str("test")));
      assert_eq!(fiber.pop(), VALUE_NIL);
      assert_eq!(fiber.pop(), val!(false));
      assert_eq!(fiber.pop(), val!(10.5));
    }
  }

  #[test]
  #[should_panic]
  fn pop_out_of_bounds() {
    let context = NoContext::default();

    let mut fiber = TestFiberBuilder::default().max_slots(4).build(&context);

    unsafe {
      fiber.pop();
      fiber.pop();
    }
  }

  #[test]
  fn drop() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = TestFiberBuilder::default().max_slots(4).build(&context);

    unsafe {
      fiber.push(val!(10.5));
      fiber.push(val!(false));
      fiber.push(VALUE_NIL);
      fiber.push(val!(hooks.manage_str("test")));

      fiber.drop();
      fiber.drop();
      assert_eq!(fiber.peek(0), val!(false));
      assert_eq!(fiber.peek(1), val!(10.5));
    }
  }

  #[test]
  #[should_panic]
  fn drop_out_of_bounds() {
    let context = NoContext::default();

    let mut fiber = TestFiberBuilder::default().max_slots(4).build(&context);

    unsafe {
      fiber.drop();
      fiber.drop();
    }
  }

  #[test]
  fn drop_n() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = TestFiberBuilder::default().max_slots(4).build(&context);

    unsafe {
      fiber.push(val!(10.5));
      fiber.push(val!(false));
      fiber.push(VALUE_NIL);
      fiber.push(val!(hooks.manage_str("test")));

      fiber.drop_n(2);
      assert_eq!(fiber.peek(0), val!(false));
      assert_eq!(fiber.peek(1), val!(10.5));
    }
  }

  #[test]
  #[should_panic]
  fn drop_n_out_of_bounds() {
    let context = NoContext::default();

    let mut fiber = TestFiberBuilder::default().max_slots(4).build(&context);

    unsafe {
      fiber.drop_n(2);
    }
  }

  #[test]
  fn peek() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = TestFiberBuilder::default().max_slots(4).build(&context);

    unsafe {
      fiber.push(val!(10.5));
      fiber.push(val!(false));
      fiber.push(VALUE_NIL);
      fiber.push(val!(hooks.manage_str("test")));

      assert_eq!(fiber.peek(3), val!(10.5));
      assert_eq!(fiber.peek(1), VALUE_NIL);
      fiber.drop();

      assert_eq!(fiber.peek(1), val!(false));
      assert_eq!(fiber.peek(0), VALUE_NIL);
    }
  }

  #[test]
  #[should_panic]
  fn peek_out_of_bounds() {
    let context = NoContext::default();

    let fiber = TestFiberBuilder::default().max_slots(1).build(&context);

    unsafe {
      fiber.peek(2);
    }
  }

  #[test]
  fn peek_set() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = TestFiberBuilder::default().max_slots(4).build(&context);

    unsafe {
      fiber.push(val!(10.5));
      fiber.push(val!(false));
      fiber.push(VALUE_NIL);
      fiber.push(val!(hooks.manage_str("test")));

      assert_eq!(fiber.peek(3), val!(10.5));
      fiber.peek_set(3, val!(true));
      assert_eq!(fiber.peek(3), val!(true));

      assert_eq!(fiber.peek(0), val!(hooks.manage_str("test")));
      fiber.peek_set(0, val!(-5.0));
      assert_eq!(fiber.peek(0), val!(-5.0));
    }
  }

  #[test]
  #[should_panic]
  fn peek_set_out_of_bounds() {
    let context = NoContext::default();

    let mut fiber = TestFiberBuilder::default().max_slots(0).build(&context);

    unsafe {
      fiber.peek_set(1, val!(true));
    }
  }

  #[test]
  fn push_exception_handler() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let module = test_module(&hooks, "test module");

    let mut builder = FunBuilder::new(hooks.manage_str("test"), module, Arity::default());
    builder.update_max_slots(3);

    let captures = Captures::new(&hooks, &[]);
    let fun = hooks.manage_obj(builder.build(Chunk::stub_with_instructions(&hooks, &[0, 0, 0])));

    let mut fiber = TestFiberBuilder::default()
      .max_slots(4)
      .instructions(vec![1, 2, 3])
      .build(&context);

    fiber.push_exception_handler(0, 1);

    unsafe {
      fiber.push(val!(fun));
      fiber.push(val!(10.0));
      fiber.push(val!(true));
    }

    fiber.push_frame(fun, captures, 2);
    fiber.push_exception_handler(2, 2);

    assert_eq!(fiber.exception_handlers[0].offset(), 0);
    assert_eq!(fiber.exception_handlers[0].slot_depth(), 1);
    assert_eq!(fiber.exception_handlers[0].call_frame_depth(), 1);

    assert_eq!(fiber.exception_handlers[1].offset(), 2);
    assert_eq!(fiber.exception_handlers[1].slot_depth(), 2);
    assert_eq!(fiber.exception_handlers[1].call_frame_depth(), 2);
  }

  #[test]
  #[should_panic]
  fn push_exception_handler_offset_out_of_bounds() {
    let context = NoContext::default();

    let mut fiber = TestFiberBuilder::default()
      .max_slots(3)
      .instructions(vec![1])
      .build(&context);

    fiber.push_exception_handler(2, 1);
  }

  #[test]
  #[should_panic]
  fn push_exception_handler_slot_depth_out_of_bounds() {
    let context = NoContext::default();

    let mut fiber = TestFiberBuilder::default()
      .max_slots(1)
      .instructions(vec![1, 2, 3])
      .build(&context);

    fiber.push_exception_handler(2, 3);
  }

  #[test]
  fn pop_exception_handler() {
    let context = NoContext::default();

    let mut fiber = TestFiberBuilder::default()
      .max_slots(4)
      .instructions(vec![1, 2, 3])
      .build(&context);

    fiber.push_exception_handler(2, 3);

    assert_eq!(fiber.exception_handlers.len(), 1);
    fiber.pop_exception_handler();
    assert_eq!(fiber.exception_handlers.len(), 0);
  }

  #[test]
  #[should_panic]
  fn pop_exception_handler_no_handler_to_pop() {
    let context = NoContext::default();

    let mut fiber = TestFiberBuilder::default()
      .max_slots(4)
      .instructions(vec![1, 2, 3])
      .build(&context);

    fiber.pop_exception_handler();
  }

  #[test]
  fn error_while_handling() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let module1 = test_module(&hooks, "first module");
    let module2 = test_module(&hooks, "second module");

    let mut builder1 = FunBuilder::new(hooks.manage_str("first"), module1, Arity::default());
    let mut builder2 = FunBuilder::new(hooks.manage_str("second"), module2, Arity::default());
    builder1.update_max_slots(4);
    builder2.update_max_slots(3);

    let chunk1 = Chunk::new(
      hooks.manage::<_, &[u8]>(&[20, 21, 22]),
      hooks.manage::<_, &[Value]>(&[]),
      hooks.manage::<_, &[u16]>(&[5, 6, 7]),
    );
    let chunk2 = Chunk::new(
      hooks.manage::<_, &[u8]>(&[40, 41, 42]),
      hooks.manage::<_, &[Value]>(&[]),
      hooks.manage::<_, &[u16]>(&[2, 3, 4]),
    );

    let fun1 = hooks.manage_obj(builder1.build(chunk1));
    let fun2 = hooks.manage_obj(builder2.build(chunk2));

    let captures = Captures::new(&hooks, &[]);

    let mut fiber = TestFiberBuilder::default()
      .max_slots(6)
      .instructions(vec![0, 0, 0])
      .build(&context);

    let base_fun = fiber.frames()[0].fun();

    unsafe {
      fiber.push(VALUE_NIL);
      fiber.push(val!(fun1));
      fiber.push(val!(10.0));
      fiber.push(val!(true));
      fiber.push(val!(fun2));
      fiber.push(val!(5.0));
    }

    fiber.push_exception_handler(2, 2);
    fiber.push_frame(fun1, captures, 2);
    fiber.store_ip(&fun1.chunk().instructions()[0]);

    fiber.push_exception_handler(1, 1);
    fiber.push_frame(fun2, captures, 1);

    fiber.activate();
    fiber.store_ip(&fun2.chunk().instructions()[2]);

    unsafe {
      match fiber.stack_unwind(None) {
        UnwindResult::PotentiallyHandled(frame) => {
          assert_eq!(frame.fun(), fun1);
          assert_eq!(frame.captures(), captures);
        },
        _ => panic!(),
      }
    }

    assert_eq!(fiber.frames().len(), 3);

    // simulate an error handler instruction going bad
    fiber.store_ip(&fun1.chunk().instructions()[2]);
    fiber.error_while_handling();

    unsafe {
      match fiber.stack_unwind(None) {
        UnwindResult::PotentiallyHandled(frame) => {
          assert_eq!(frame.fun(), base_fun);
        },
        _ => panic!(),
      }
    }
    let backtrace = fiber.finish_unwind();
    assert_eq!(backtrace.len(), 3);
    assert_eq!(backtrace[0], "example:3 in second()");
    assert_eq!(backtrace[1], "example:6 in first()");
    assert_eq!(backtrace[2], "example:0 in script");
  }

  #[test]
  fn is_complete() {
    let context = NoContext::default();

    let mut fiber = TestFiberBuilder::default().build(&context);

    assert!(!fiber.is_complete());

    fiber.activate();
    fiber.complete();

    assert!(fiber.is_complete());
  }

  #[test]
  fn is_pending() {
    let context = NoContext::default();

    let mut fiber = TestFiberBuilder::default().build(&context);

    assert!(fiber.is_pending());

    fiber.activate();

    assert!(!fiber.is_pending());

    fiber.sleep();

    assert!(fiber.is_pending());
  }

  #[test]
  fn complete_without_parent_or_channels() {
    let context = NoContext::default();

    let mut fiber = TestFiberBuilder::default().build(&context);

    fiber.activate();

    assert_eq!(fiber.complete(), None)
  }

  #[test]
  fn split() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let module = test_module(&hooks, "test module");

    let builder = FunBuilder::new(hooks.manage_str("test"), module, Arity::default());

    let fun = hooks.manage_obj(builder.build(Chunk::stub_with_instructions(&hooks, &[0, 0, 0])));
    let captures = Captures::new(&hooks, &[]);

    let mut fiber = TestFiberBuilder::default().max_slots(3).build(&context);

    fiber.activate();

    unsafe {
      fiber.push(val!(fun));
      fiber.push(val!(10.0));
      fiber.push(val!(true));
    }

    let arg_count = 2;
    fiber.push_frame(fun, captures, arg_count);

    let mut child = Fiber::split(fiber, &mut context.gc(), &context, arg_count);

    fiber.sleep();
    child.activate();

    assert_eq!(child.complete(), Some(fiber.waiter))
  }

  #[test]
  fn complete_with_parent_but_no_channels() {
    let context = NoContext::default();

    let parent = TestFiberBuilder::default().build(&context);

    let mut fiber = TestFiberBuilder::default().parent(parent).build(&context);

    fiber.activate();

    assert_eq!(fiber.complete(), Some(parent.waiter))
  }

  #[test]
  fn store_ip() {
    let context = NoContext::default();

    let mut fiber = TestFiberBuilder::default().max_slots(0).build(&context);

    let val: u8 = 0;
    let ip = &val as *const u8;

    fiber.store_ip(ip);
    assert_eq!(fiber.frame().ip(), ip)
  }

  #[test]
  fn load_ip() {
    let context = NoContext::default();

    let mut fiber = TestFiberBuilder::default()
      .max_slots(0)
      .instructions(vec![1, 2, 3])
      .build(&context);

    unsafe {
      assert_eq!(*fiber.load_ip(), 1);
    }
  }

  #[test]
  fn frame_stack() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = TestFiberBuilder::default().max_slots(4).build(&context);

    let fun = test_fun(&hooks, "next", "next module");
    let captures = Captures::new(&hooks, &[]);

    unsafe {
      fiber.push(val!(10.5));
      fiber.push(val!(false));
      fiber.push(VALUE_NIL);
      fiber.push(val!(fun));

      let frame_stack = fiber.frame_stack();

      assert_eq!(frame_stack.len(), 5);
      assert_eq!(frame_stack[0], val!(fiber.frame().fun()));
      assert_eq!(frame_stack[1], val!(10.5));
      assert_eq!(frame_stack[2], val!(false));
      assert_eq!(frame_stack[3], VALUE_NIL);
      assert_eq!(frame_stack[4], val!(fun));

      fiber.push_frame(fun, captures, 0);

      let frame_stack = fiber.frame_stack();

      assert_eq!(frame_stack.len(), 1);
      assert_eq!(frame_stack[0], val!(fun));
    }
  }

  #[test]
  #[should_panic]
  fn frame_stack_out_of_bounds() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = TestFiberBuilder::default().max_slots(4).build(&context);

    let fun = test_fun(&hooks, "next", "next module");
    let captures = Captures::new(&hooks, &[]);

    unsafe {
      fiber.push(val!(10.5));
      fiber.push(val!(false));
      fiber.push(VALUE_NIL);
      fiber.push(val!(fun));

      let frame_stack = fiber.frame_stack();

      assert_eq!(frame_stack.len(), 5);
      assert_eq!(frame_stack[0], val!(fiber.frame().fun()));
      assert_eq!(frame_stack[1], val!(10.5));
      assert_eq!(frame_stack[2], val!(false));
      assert_eq!(frame_stack[3], VALUE_NIL);
      assert_eq!(frame_stack[4], val!(fun));

      fiber.push_frame(fun, captures, 8);
      fiber.frame_stack();
    }
  }

  #[test]
  fn stack_slice() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = TestFiberBuilder::default().max_slots(4).build(&context);

    unsafe {
      fiber.push(val!(10.5));
      fiber.push(val!(false));
      fiber.push(VALUE_NIL);
      fiber.push(val!(hooks.manage_str("test")));

      let slice = fiber.stack_slice(3);

      assert_eq!(slice.len(), 3);
      assert_eq!(slice[0], val!(false));
      assert_eq!(slice[1], VALUE_NIL);
      assert_eq!(slice[2], val!(hooks.manage_str("test")));
    }
  }

  #[test]
  #[should_panic]
  fn stack_slice_out_of_bounds() {
    let context = NoContext::default();

    let fiber = TestFiberBuilder::default().max_slots(0).build(&context);

    unsafe {
      fiber.stack_slice(3);
    }
  }

  #[test]
  fn push_frame() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let fun = test_fun(&hooks, "next", "next module");
    let captures = Captures::new(&hooks, &[]);

    let mut fiber = TestFiberBuilder::default().max_slots(3).build(&context);

    unsafe {
      fiber.push(val!(fun));
      fiber.push(val!(10.0));
      fiber.push(val!(true));
    }

    fiber.push_frame(fun, captures, 2);

    let slice = fiber.frame_stack();

    assert_eq!(slice[0], val!(fun));
    assert_eq!(slice[1], val!(10.0));
    assert_eq!(slice[2], val!(true));

    assert_eq!(fiber.frame().fun(), fun);
    assert_eq!(fiber.frame().captures(), captures);
  }

  #[test]
  fn pop_frame() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let fun = test_fun(&hooks, "next", "next module");
    let captures = Captures::new(&hooks, &[]);

    let mut fiber = TestFiberBuilder::default().max_slots(4).build(&context);

    let og_fun = fiber.frame().fun();

    unsafe {
      fiber.push(VALUE_NIL);
      fiber.push(val!(fun));
      fiber.push(val!(10.0));
      fiber.push(val!(true));
    }

    fiber.push_frame(fun, captures, 2);

    match fiber.pop_frame() {
      FiberPopResult::Ok(popped_fun) => {
        assert_eq!(og_fun, popped_fun);

        let slice = fiber.frame_stack();

        assert_eq!(slice.len(), 2);
        assert_eq!(slice[0], val!(fiber.frame().fun()));
        assert_eq!(slice[1], VALUE_NIL);
      },
      _ => panic!(),
    };

    match fiber.pop_frame() {
      FiberPopResult::Emptied => {},
      _ => panic!(),
    }

    match fiber.pop_frame() {
      FiberPopResult::Empty => {},
      _ => panic!(),
    }
  }

  #[test]
  fn stack_unwind_unhandled() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let fun1 = test_fun(&hooks, "first", "first module");
    let fun2 = test_fun(&hooks, "second", "second module");

    let captures = Captures::new(&hooks, &[]);

    let mut fiber = TestFiberBuilder::default().max_slots(6).build(&context);

    unsafe {
      fiber.push(VALUE_NIL);
      fiber.push(val!(fun1));
      fiber.push(val!(10.0));
      fiber.push(val!(true));
      fiber.push(val!(fun2));
      fiber.push(val!(5.0));
    }

    fiber.push_frame(fun1, captures, 2);
    fiber.push_frame(fun2, captures, 1);

    unsafe {
      assert_eq!(fiber.stack_unwind(None), UnwindResult::Unhandled);
    }

    assert_eq!(fiber.frames().len(), 3);
    assert_eq!(fiber.frame().fun(), fun2);
    assert_eq!(fiber.frame().captures(), captures);
  }

  #[test]
  fn stack_unwind_potentially_handled() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let module1 = test_module(&hooks, "first module");
    let module2 = test_module(&hooks, "second module");

    let mut builder1 = FunBuilder::new(hooks.manage_str("first"), module1, Arity::default());
    let builder2 = FunBuilder::new(hooks.manage_str("second"), module2, Arity::default());
    builder1.update_max_slots(4);

    let chunk1 = Chunk::new(
      hooks.manage::<_, &[u8]>(&[0, 0, 0]),
      hooks.manage::<_, &[Value]>(&[]),
      hooks.manage::<_, &[u16]>(&[0, 0, 1]),
    );
    let chunk2 = Chunk::new(
      hooks.manage::<_, &[u8]>(&[0, 0, 0]),
      hooks.manage::<_, &[Value]>(&[]),
      hooks.manage::<_, &[u16]>(&[0, 1, 1]),
    );

    let fun1 = hooks.manage_obj(builder1.build(chunk1));
    let fun2 = hooks.manage_obj(builder2.build(chunk2));

    let captures = Captures::new(&hooks, &[]);

    let mut fiber = TestFiberBuilder::default().max_slots(6).build(&context);

    unsafe {
      fiber.push(VALUE_NIL);
      fiber.push(val!(fun1));
      fiber.push(val!(10.0));
      fiber.push(val!(true));
      fiber.push(val!(fun2));
      fiber.push(val!(5.0));
    }

    fiber.push_frame(fun1, captures, 2);
    fiber.push_exception_handler(2, 2);
    fiber.store_ip(&fun1.chunk().instructions()[1]);

    fiber.push_frame(fun2, captures, 1);

    fiber.activate();
    fiber.store_ip(&fun2.chunk().instructions()[2]);

    unsafe {
      match fiber.stack_unwind(None) {
        UnwindResult::PotentiallyHandled(frame) => {
          assert_eq!(frame.fun(), fun1);
          assert_eq!(frame.captures(), captures);
        },
        _ => panic!(),
      }
    }

    assert_eq!(fiber.frames().len(), 3);
    assert_eq!(fiber.frame().fun(), fun1);
    assert_eq!(fiber.frame().captures(), captures);

    let backtrace = fiber.finish_unwind();
    assert_eq!(backtrace.len(), 2);
    assert_eq!(backtrace[0], "example:1 in second()");
    assert_eq!(backtrace[1], "example:0 in first()");

    assert_eq!(fiber.frames().len(), 2);
    assert_eq!(fiber.frame().fun(), fun1);
    assert_eq!(fiber.frame().captures(), captures);
  }

  #[test]
  fn stack_unwind_not_bottom_frame() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fun1 = test_fun_builder(&hooks, "first", "first module");
    fun1.update_max_slots(4);
    let fun1 = hooks.manage_obj(fun1.build(Chunk::stub(&hooks)));

    let mut fun2 = test_fun_builder(&hooks, "second", "second module");
    fun2.update_max_slots(3);
    let fun2 = hooks.manage_obj(fun2.build(Chunk::stub(&hooks)));

    let captures = Captures::new(&hooks, &[]);

    let mut fiber = TestFiberBuilder::default().max_slots(6).build(&context);

    unsafe {
      fiber.push(VALUE_NIL);
      fiber.push(val!(fun1));
      fiber.push(val!(10.0));
      fiber.push(val!(true));
    }

    fiber.push_frame(fun1, captures, 2);

    unsafe {
      fiber.push(val!(fun2));
      fiber.push(val!(5.0));
      fiber.push(val!(8.0));
    }

    fiber.push_frame(fun2, captures, 1);

    unsafe {
      assert_eq!(fiber.stack_unwind(Some(2)), UnwindResult::UnwindStopped);
    }

    assert_eq!(fiber.frames().len(), 3);
    assert_eq!(fiber.frame().fun(), fun2);
    assert_eq!(fiber.frame().captures(), captures);
  }

  #[test]
  fn ensure_stack() {
    let context = NoContext::default();

    let mut fiber = TestFiberBuilder::default().max_slots(2).build(&context);

    unsafe {
      fiber.push(VALUE_NIL);
      fiber.push(val!(10.0));
    }

    fiber.ensure_stack(2);

    unsafe {
      fiber.push(val!(false));
      fiber.push(val!(-5.1));

      assert_eq!(fiber.peek(0), val!(-5.1));
      assert_eq!(fiber.peek(1), val!(false));
      assert_eq!(fiber.peek(2), val!(10.0));
      assert_eq!(fiber.peek(3), VALUE_NIL);
    }
  }
}
