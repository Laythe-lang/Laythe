mod call_frame;
mod exception_handler;

use self::{call_frame::CallFrame, exception_handler::ExceptionHandler};
use super::{Channel, Fun, ObjectKind};
use crate::{
  captures::Captures,
  constants::SCRIPT,
  hooks::GcHooks,
  managed::{DebugHeap, DebugWrap, GcObj, Instance, Object, Trace},
  val,
  value::{Value, VALUE_NIL},
};
use std::{fmt, io::Write, mem, ptr, usize};

const INITIAL_FRAME_SIZE: usize = 4;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum FiberState {
  Running,
  Pending,
  Blocked,
  Complete,
}

#[derive(Debug)]
pub enum FiberError {
  NoInstructions,
}

pub type FiberResult<T> = Result<T, FiberError>;

#[derive(Debug, PartialEq, Eq)]
pub enum UnwindResult<'a> {
  Handled(&'a mut CallFrame),
  Unhandled,
  UnwindStopped,
}

pub struct Fiber {
  /// A stack holding all local variable currently in use
  stack: Vec<Value>,

  /// A stack holding call frames currently in use
  frames: Vec<CallFrame>,

  /// A stack of exception handlers active on this fiber
  exception_handlers: Vec<ExceptionHandler>,

  /// A list of channels executed on this fiber
  channels: Vec<GcObj<Channel>>,

  /// The parent fiber to this fiber
  parent: Option<GcObj<Fiber>>,

  /// pointer to the top of the value stack
  stack_top: *mut Value,

  /// The current frame's closure
  frame: *mut CallFrame,

  /// The current state of this fiber
  state: FiberState,

  /// The current error if one is active
  error: Option<Instance>,
}

impl Fiber {
  /// Create a new fiber from the provided closure. The fiber uses
  /// this initial closure to determine how much stack space to initially
  /// reserve
  pub fn new(
    parent: Option<GcObj<Fiber>>,
    fun: GcObj<Fun>,
    captures: Captures,
  ) -> FiberResult<Self> {
    Fiber::new_inner(parent, fun, captures, fun.max_slots() + 1)
  }

  /// Inner initialization function that actually reserver and create
  /// each structure
  fn new_inner(
    parent: Option<GcObj<Fiber>>,
    fun: GcObj<Fun>,
    captures: Captures,
    stack_count: usize,
  ) -> FiberResult<Self> {
    // reserve resources
    let mut frames = Vec::<CallFrame>::with_capacity(INITIAL_FRAME_SIZE);
    let mut stack = vec![VALUE_NIL; stack_count];

    let instructions = fun.chunk().instructions();
    if instructions.is_empty() {
      return Err(FiberError::NoInstructions);
    }

    // push closure and frame onto fiber
    stack[0] = val!(fun);
    frames.push(CallFrame::new(fun, captures, stack.as_mut_ptr()));

    // get pointers to the call frame and stack top
    let current_frame = frames.as_mut_ptr();
    let stack_top = unsafe { stack.as_mut_ptr().offset(1) };

    Ok(Self {
      stack,
      frames,
      parent,
      channels: vec![],
      exception_handlers: vec![],
      state: FiberState::Pending,
      error: None,
      frame: current_frame,
      stack_top,
    })
  }

  /// Get the current slice call frame slice
  #[inline]
  pub fn frames(&self) -> &[CallFrame] {
    &self.frames
  }

  /// Get the current number of call frames on this fiber
  fn frame_count(&self) -> usize {
    self.frames.len()
  }

  /// Get the current frame's stack start
  #[inline]
  pub fn stack_start(&self) -> *mut Value {
    self.frame().stack_start()
  }

  /// Get the current frame's current function
  #[inline]
  pub fn fun(&self) -> GcObj<Fun> {
    self.frame().fun()
  }

  /// Get the current frame's current captures
  #[inline]
  pub fn captures(&self) -> Captures {
    self.frame().captures()
  }

  /// Is this fiber complete
  #[inline]
  pub fn is_complete(&self) -> bool {
    self.state == FiberState::Complete
  }

  /// Is this fiber pending
  #[inline]
  pub fn is_pending(&self) -> bool {
    self.state == FiberState::Pending
  }

  /// Activate the current fiber
  #[inline]
  pub fn activate(&mut self) {
    assert_eq!(self.state, FiberState::Pending);
    self.state = FiberState::Running;
  }

  /// Put the current fiber to sleep
  #[inline]
  pub fn sleep(&mut self) {
    assert_eq!(self.state, FiberState::Running);
    self.state = FiberState::Pending;
  }

  /// Put the current fiber to sleep
  #[inline]
  pub fn block(&mut self) {
    assert_eq!(self.state, FiberState::Running);
    self.state = FiberState::Blocked;
  }

  /// Put the current fiber to sleep
  #[inline]
  pub fn unblock(&mut self) {
    assert!(matches!(
      self.state,
      FiberState::Blocked | FiberState::Pending
    ));
    if let FiberState::Blocked = self.state {
      self.state = FiberState::Pending;
    }
  }

  /// Activate the current fiber
  #[inline]
  pub fn complete(mut fiber: GcObj<Fiber>) -> Option<GcObj<Fiber>> {
    assert_eq!(fiber.state, FiberState::Running);
    fiber.state = FiberState::Complete;

    // load from waiting fiber biases toward the parent fiber
    let waiter = fiber
      .parent
      .filter(|parent| parent.is_pending())
      .or_else(|| fiber.get_runnable());

    let channels = mem::take(&mut fiber.channels);
    for mut channel in channels {
      channel.remove_waiter(fiber);
    }

    waiter
  }

  /// Try to get a runnable fiber
  #[inline]
  pub fn get_runnable(&mut self) -> Option<GcObj<Fiber>> {
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

  /// A a channel to the list of used channels by
  /// this fiber
  pub fn add_used_channel(&mut self, channel: GcObj<Channel>) {
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
  #[inline]
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
  #[inline]
  pub unsafe fn pop(&mut self) -> Value {
    self.drop();
    self.get_val(0)
  }

  /// drop a value off the stack
  ///
  /// ## Safety
  /// A stack drop makes not bounds checks and it's possible to
  /// move the current stack top prior to the beginning of the stack
  #[inline]
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
  #[inline]
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
  #[inline]
  pub unsafe fn peek(&self, distance: usize) -> Value {
    self.get_val(distance + 1)
  }

  /// Set a value n slots from the stack head
  ///
  ///
  /// ## Safety
  /// Setting a value n slot down make no bounds checks if distance is
  /// greater than the current stack length
  #[inline]
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

  /// Push a new exception handler onto this fiber
  pub fn pop_exception_handler(&mut self) {
    assert!(
      self.exception_handlers.pop().is_some(),
      "Attempted to pop an empty vec of exception handlers"
    );
  }

  /// Do we currently have an active exception handler
  pub fn exception_handler(&self) -> Option<ExceptionHandler> {
    self.exception_handlers.last().copied()
  }

  /// Load the instruction pointer from the current frame
  #[inline]
  pub fn load_ip(&mut self) -> *const u8 {
    self.frame().ip()
  }

  /// Store an instruction pointer into the current frame
  #[inline]
  pub fn store_ip(&mut self, ip: *const u8) {
    self.frame_mut().store_ip(ip);
  }

  /// Get a slice of the top count values on the stack
  #[inline]
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
  #[inline]
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
  #[inline]
  pub fn error(&self) -> Option<Instance> {
    self.error
  }

  /// Set the current error on this fiber
  pub fn set_error(&mut self, error: Instance) {
    self.error = Some(error);
  }

  /// Push a frame onto the call stack
  pub fn push_frame(&mut self, fun: GcObj<Fun>, captures: Captures, arg_count: usize) {
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
  pub fn pop_frame(&mut self) -> Option<Option<GcObj<Fun>>> {
    if self.frames.is_empty() {
      return None;
    }

    self.stack_top = self.frame().stack_start();

    self.frames.pop();
    Some(match self.frames.last() {
      Some(frame) => {
        unsafe {
          self.frame = self.frame.offset(-1);
        }

        #[cfg(debug_assertions)]
        self.assert_frame_inbounds();

        Some(frame.fun())
      },
      None => {
        self.frame = ptr::null_mut();
        None
      },
    })
  }

  /// Attempt to create a new fiber using the most recent call frame
  pub fn split(
    mut _self: GcObj<Fiber>,
    hooks: &GcHooks,
    frame_count: usize,
    arg_count: usize,
  ) -> Option<GcObj<Fiber>> {
    if _self.frames().len() != frame_count + 1 {
      return None;
    }

    let frame = _self.frames.pop().expect("Expected call frame");
    hooks.push_root(frame);
    let mut fiber = hooks.manage_obj(
      Fiber::new_inner(
        Some(_self),
        frame.fun(),
        frame.captures(),
        frame.fun().max_slots() + arg_count + 1,
      )
      .expect("Assumed valid closure"),
    );
    hooks.pop_roots(1);

    let slots = _self.frame_stack().len();
    assert_eq!(slots, arg_count + 1);

    // if we have any argument bulk copy them to the fiber
    if slots > 1 {
      unsafe {
        ptr::copy_nonoverlapping(
          _self.frame().stack_start().offset(1),
          fiber.stack_top,
          arg_count,
        );
        fiber.stack_top = fiber.stack_top.add(arg_count);
      }
    }

    // Effectively pop the current fibers frame so they're 'moved'
    // to the new fiber
    unsafe {
      _self.stack_top = _self.frame().stack_start();
      _self.frame = _self.frame.sub(1);
    }

    Some(fiber)
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
    match self.exception_handler() {
      Some(exception_handler) => {
        let bottom_frame = bottom_frame.unwrap_or(0);
        if exception_handler.call_frame_depth() >= bottom_frame {
          self.frames.truncate(exception_handler.call_frame_depth());

          let frame = self
            .frames
            .last_mut()
            .expect("expected at least 1 frame to remain");

          let fun = frame.fun();
          let instructions = fun.chunk().instructions();

          let stack_top = frame.stack_start().add(exception_handler.slot_depth());

          // set the current ip frame and stack pointer
          frame.store_ip(&instructions[exception_handler.offset()] as *const u8);
          self.frame = frame as *mut CallFrame;
          self.stack_top = stack_top;

          UnwindResult::Handled(frame)
        } else {
          UnwindResult::UnwindStopped
        }
      },
      None => match bottom_frame {
        Some(_) => UnwindResult::UnwindStopped,
        None => UnwindResult::Unhandled,
      },
    }
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
        fun.chunk().get_line(offset),
        location
      )
      .expect("Unable to write to stderr");
    }

    let message = error[0].to_obj().to_str();
    writeln!(log, "{}: {}", &*error.class().name(), &*message).expect("Unable to write to stderr");
  }

  /// Get a value on the stack
  #[inline(always)]
  unsafe fn get_val(&self, offset: usize) -> Value {
    let location = self.stack_top.sub(offset);

    #[cfg(debug_assertions)]
    assert_inbounds(&self.stack, location);

    *location
  }

  /// Set a value on the stack
  #[inline(always)]
  unsafe fn set_val(&mut self, offset: usize, val: Value) {
    let location = self.stack_top.sub(offset);

    #[cfg(debug_assertions)]
    assert_inbounds(&self.stack, location);

    *location = val
  }

  /// An immutable reference to the current frame
  #[inline]
  fn frame(&self) -> &CallFrame {
    debug_assert!(!self.frame.is_null());
    unsafe { &*self.frame }
  }

  /// A mutable reference to the current frame
  #[inline]
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

impl Object for Fiber {
  fn kind(&self) -> ObjectKind {
    ObjectKind::Fiber
  }
}

impl DebugHeap for Fiber {
  fn fmt_heap(&self, f: &mut std::fmt::Formatter, depth: usize) -> std::fmt::Result {
    f.debug_struct("Fiber")
      .field("stack", &DebugWrap(&&*self.stack, depth))
      .field("frames", &DebugWrap(&&*self.frames, depth))
      .field("stack_top", &format_args!("{:p}", self.stack_top))
      .field("current_frame", &format_args!("{:p}", self.frame))
      .field("state", &self.state)
      .field("current_error", &DebugWrap(&self.error, depth))
      .finish()
  }
}

impl Trace for Fiber {
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

    if let Some(error) = self.error {
      error.trace();
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

    if let Some(error) = self.error {
      error.trace_debug(log);
    }
  }
}

unsafe impl Send for Fiber {}
unsafe impl Sync for Fiber {}

#[cfg(test)]
mod test {
  use super::*;
  use crate::{
    chunk::Chunk,
    hooks::{GcHooks, NoContext},
    object::FunBuilder,
    signature::Arity,
    support::{test_fun, test_fun_builder, test_module, FiberBuilder},
  };

  #[test]
  fn new() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let fiber = FiberBuilder::default().build(&hooks);
    assert!(fiber.is_ok())
  }

  #[test]
  fn push() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = FiberBuilder::default()
      .max_slots(4)
      .build(&hooks)
      .expect("Expected to build");

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
    let hooks = GcHooks::new(&context);

    let mut fiber = FiberBuilder::default()
      .max_slots(1)
      .build(&hooks)
      .expect("Expected to build");

    unsafe {
      fiber.push(val!(10.5));
      fiber.push(val!(false));
    }
  }

  #[test]
  fn pop() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = FiberBuilder::default()
      .max_slots(4)
      .build(&hooks)
      .expect("Expected to build");

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
    let hooks = GcHooks::new(&context);

    let mut fiber = FiberBuilder::default()
      .max_slots(4)
      .build(&hooks)
      .expect("Expected to build");

    unsafe {
      fiber.pop();
      fiber.pop();
    }
  }

  #[test]
  fn drop() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = FiberBuilder::default()
      .max_slots(4)
      .build(&hooks)
      .expect("Expected to build");

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
    let hooks = GcHooks::new(&context);

    let mut fiber = FiberBuilder::default()
      .max_slots(4)
      .build(&hooks)
      .expect("Expected to build");

    unsafe {
      fiber.drop();
      fiber.drop();
    }
  }

  #[test]
  fn drop_n() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = FiberBuilder::default()
      .max_slots(4)
      .build(&hooks)
      .expect("Expected to build");

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
    let hooks = GcHooks::new(&context);

    let mut fiber = FiberBuilder::default()
      .max_slots(4)
      .build(&hooks)
      .expect("Expected to build");

    unsafe {
      fiber.drop_n(2);
    }
  }

  #[test]
  fn peek() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = FiberBuilder::default()
      .max_slots(4)
      .build(&hooks)
      .expect("Expected to build");

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
    let hooks = GcHooks::new(&context);

    let fiber = FiberBuilder::default()
      .max_slots(1)
      .build(&hooks)
      .expect("Expected to build");

    unsafe {
      fiber.peek(2);
    }
  }

  #[test]
  fn peek_set() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = FiberBuilder::default()
      .max_slots(4)
      .build(&hooks)
      .expect("Expected to build");

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
    let hooks = GcHooks::new(&context);

    let mut fiber = FiberBuilder::default()
      .max_slots(0)
      .build(&hooks)
      .expect("Expected to build");

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

    let mut fiber = FiberBuilder::default()
      .max_slots(4)
      .instructions(vec![1, 2, 3])
      .build(&hooks)
      .expect("Expected to build");

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
    let hooks = GcHooks::new(&context);

    let mut fiber = FiberBuilder::default()
      .max_slots(3)
      .instructions(vec![1])
      .build(&hooks)
      .expect("Expected to build");

    fiber.push_exception_handler(2, 1);
  }

  #[test]
  #[should_panic]
  fn push_exception_handler_slot_depth_out_of_bounds() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = FiberBuilder::default()
      .max_slots(1)
      .instructions(vec![1, 2, 3])
      .build(&hooks)
      .expect("Expected to build");

    fiber.push_exception_handler(2, 3);
  }

  #[test]
  fn pop_exception_handler() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = FiberBuilder::default()
      .max_slots(4)
      .instructions(vec![1, 2, 3])
      .build(&hooks)
      .expect("Expected to build");

    fiber.push_exception_handler(2, 3);

    assert_eq!(fiber.exception_handlers.len(), 1);
    fiber.pop_exception_handler();
    assert_eq!(fiber.exception_handlers.len(), 0);
  }

  #[test]
  #[should_panic]
  fn pop_exception_handler_no_handler_to_pop() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = FiberBuilder::default()
      .max_slots(4)
      .instructions(vec![1, 2, 3])
      .build(&hooks)
      .expect("Expected to build");

    fiber.pop_exception_handler();
  }

  #[test]
  fn is_complete() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = FiberBuilder::default()
      .build(&hooks)
      .expect("Expected to build");

    assert_eq!(fiber.is_complete(), false);

    fiber.activate();
    Fiber::complete(fiber);

    assert_eq!(fiber.is_complete(), true);
  }

  #[test]
  fn is_pending() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = FiberBuilder::default()
      .build(&hooks)
      .expect("Expected to build");

    assert_eq!(fiber.is_pending(), true);

    fiber.activate();

    assert_eq!(fiber.is_pending(), false);

    fiber.sleep();

    assert_eq!(fiber.is_pending(), true);
  }

  #[test]
  fn complete_without_parent_or_channels() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = FiberBuilder::default()
      .build(&hooks)
      .expect("Expected to build");

    fiber.activate();

    assert_eq!(Fiber::complete(fiber), None)
  }

  #[test]
  fn split() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let module = test_module(&hooks, "test module");

    let builder = FunBuilder::new(hooks.manage_str("test"), module, Arity::default());

    let fun = hooks.manage_obj(builder.build(Chunk::stub_with_instructions(&hooks, &[0, 0, 0])));
    let captures = Captures::new(&hooks, &[]);

    let mut fiber = FiberBuilder::default()
      .max_slots(3)
      .build(&hooks)
      .expect("Expected to build");

    fiber.activate();

    unsafe {
      fiber.push(val!(fun));
      fiber.push(val!(10.0));
      fiber.push(val!(true));
    }

    let arg_count = 2;
    fiber.push_frame(fun, captures, arg_count);

    let mut child = Fiber::split(fiber, &hooks, 1, arg_count).expect("Expected split");

    fiber.sleep();
    child.activate();

    assert_eq!(Fiber::complete(child), Some(fiber))
  }

  #[test]
  fn complete_with_parent_but_no_channels() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let parent = FiberBuilder::default()
      .build(&hooks)
      .expect("Expected to build");

    let mut fiber = FiberBuilder::default()
      .parent(parent)
      .build(&hooks)
      .expect("Expected to build");

    fiber.activate();

    assert_eq!(Fiber::complete(fiber), Some(parent))
  }

  #[test]
  fn store_ip() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = FiberBuilder::default()
      .max_slots(0)
      .build(&hooks)
      .expect("Expected to build");

    let val: u8 = 0;
    let ip = &val as *const u8;

    fiber.store_ip(ip);
    assert_eq!(fiber.frame().ip(), ip)
  }

  #[test]
  fn load_ip() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = FiberBuilder::default()
      .max_slots(0)
      .instructions(vec![1, 2, 3])
      .build(&hooks)
      .expect("Expected to build");

    unsafe {
      assert_eq!(*fiber.load_ip(), 1);
    }
  }

  #[test]
  fn frame_stack() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = FiberBuilder::default()
      .max_slots(4)
      .build(&hooks)
      .expect("Expected to build");

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

    let mut fiber = FiberBuilder::default()
      .max_slots(4)
      .build(&hooks)
      .expect("Expected to build");

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

    let mut fiber = FiberBuilder::default()
      .max_slots(4)
      .build(&hooks)
      .expect("Expected to build");

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
    let hooks = GcHooks::new(&context);

    let fiber = FiberBuilder::default()
      .max_slots(0)
      .build(&hooks)
      .expect("Expected to build");

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

    let mut fiber = FiberBuilder::default()
      .max_slots(3)
      .build(&hooks)
      .expect("Expected to build");

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

    let mut fiber = FiberBuilder::default()
      .max_slots(4)
      .build(&hooks)
      .expect("Expected to build");

    unsafe {
      fiber.push(VALUE_NIL);
      fiber.push(val!(fun));
      fiber.push(val!(10.0));
      fiber.push(val!(true));
    }

    fiber.push_frame(fun, captures, 2);
    let popped_frame = fiber.pop_frame().unwrap().unwrap();

    assert_ne!(popped_frame, fun);

    let slice = fiber.frame_stack();

    assert_eq!(slice.len(), 2);
    assert_eq!(slice[0], val!(fiber.frame().fun()));
    assert_eq!(slice[1], VALUE_NIL);
  }

  #[test]
  fn stack_unwind() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let fun1 = test_fun(&hooks, "first", "first module");
    let fun2 = test_fun(&hooks, "second", "second module");

    let captures = Captures::new(&hooks, &[]);

    let mut fiber = FiberBuilder::default()
      .max_slots(6)
      .build(&hooks)
      .expect("Expected to build");

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

    let mut fiber = FiberBuilder::default()
      .max_slots(6)
      .build(&hooks)
      .expect("Expected to build");

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
    let hooks = GcHooks::new(&context);

    let mut fiber = FiberBuilder::default()
      .max_slots(2)
      .build(&hooks)
      .expect("Expected to build");

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
