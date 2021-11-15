use std::{fmt, io::Write, mem, ptr, usize};

use super::{Channel, Closure, Fun, Instance, ObjectKind};
use crate::{
  call_frame::CallFrame,
  constants::SCRIPT,
  hooks::GcHooks,
  managed::{DebugHeap, DebugWrap, GcObj, Manage, Object, Trace},
  val,
  value::{Value, VALUE_NIL},
};

const INITIAL_FRAME_SIZE: usize = 4;

#[derive(Debug, PartialEq, Clone, Copy)]
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

pub struct Fiber {
  /// A stack holding all local variable currently in use
  stack: Vec<Value>,

  /// A stack holding call frames currently in use
  frames: Vec<CallFrame>,

  /// A list of channels executed on this fiber
  channels: Vec<GcObj<Channel>>,

  /// pointer to the top of the value stack
  stack_top: *mut Value,

  /// The current frame's closure
  frame: *mut CallFrame,

  /// The current state of this fiber
  state: FiberState,

  /// The current error if one is active
  error: Option<GcObj<Instance>>,
}

impl Fiber {
  /// Create a new fiber from the provided closure. The fiber uses
  /// this initial closure to determine how much stack space to initially
  /// reserve
  pub fn new(closure: GcObj<Closure>) -> FiberResult<Self> {
    let fun = closure.fun();
    Fiber::new_inner(closure, fun.max_slots() + 1)
  }

  /// Create a new from the provided closure, used by split_fiber to use
  /// the top call frame to initialize a new fiber. This reserves stack space
  /// for the initial frame slots and arguments.
  fn split(closure: GcObj<Closure>, arg_count: usize) -> FiberResult<Self> {
    let fun = closure.fun();
    Fiber::new_inner(closure, fun.max_slots() + arg_count + 1)
  }

  /// Inner initialization function that actually reserver and create
  /// each structure
  fn new_inner(closure: GcObj<Closure>, stack_count: usize) -> FiberResult<Self> {
    // reserve resources
    let fun = closure.fun();
    let mut frames = Vec::<CallFrame>::with_capacity(INITIAL_FRAME_SIZE);
    let mut stack = vec![VALUE_NIL; stack_count];

    let instructions = fun.chunk().instructions();
    if instructions.is_empty() {
      return Err(FiberError::NoInstructions);
    }

    // push closure and frame onto fiber
    stack[0] = val!(closure);
    frames.push(CallFrame {
      closure,
      ip: instructions.as_ptr(),
      stack_start: stack.as_mut_ptr(),
    });

    // get pointers to the call frame and stack top
    let current_frame = frames.as_mut_ptr();
    let stack_top = unsafe { stack.as_mut_ptr().offset(1) };

    Ok(Self {
      stack,
      frames,
      channels: vec![],
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

  /// Get the current frame's stack start
  #[inline]
  pub fn stack_start(&self) -> *mut Value {
    self.frame().stack_start
  }

  /// Get the current frame's closure
  #[inline]
  pub fn closure(&self) -> GcObj<Closure> {
    self.frame().closure
  }

  #[inline]
  pub fn is_complete(&self) -> bool {
    self.state == FiberState::Complete
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

    let waiter = fiber.get_runnable();

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

  /// Load the instruction pointer from the current frame
  #[inline]
  pub fn load_ip(&mut self) -> *const u8 {
    self.frame().ip
  }

  /// Store an instruction pointer into the current frame
  #[inline]
  pub fn store_ip(&mut self, ip: *const u8) {
    self.frame_mut().ip = ip
  }

  /// Get a slice of the top count values on the stack
  #[inline]
  pub fn frame_stack(&self) -> &[Value] {
    unsafe {
      let stack_start = (*self.frame).stack_start;
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

    std::slice::from_raw_parts(start, len as usize)
  }

  /// Retrieve the current error on this fiber
  #[inline]
  pub fn error(&self) -> Option<GcObj<Instance>> {
    self.error
  }

  /// Set the current error on this fiber
  pub fn set_error(&mut self, error: GcObj<Instance>) {
    self.error = Some(error);
  }

  /// Push a frame onto the call stack
  pub fn push_frame(&mut self, closure: GcObj<Closure>, arg_count: usize) {
    unsafe {
      self.ensure_stack(closure.fun().max_slots());
      let stack_start = self.stack_top.sub(arg_count + 1);

      #[cfg(debug_assertions)]
      assert_inbounds(&self.stack, stack_start);

      self.frames.push(CallFrame {
        closure,
        ip: closure.fun().chunk().instructions().as_ptr(),
        stack_start,
      });
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

    self.stack_top = self.frame().stack_start;

    self.frames.pop();
    Some(match self.frames.last() {
      Some(frame) => {
        unsafe {
          self.frame = self.frame.offset(-1);
        }

        #[cfg(debug_assertions)]
        self.assert_frame_inbounds();

        Some(frame.closure.fun())
      }
      None => {
        self.frame = ptr::null_mut();
        None
      }
    })
  }

  /// Attempt to create a new fiber using the most recent call frame
  pub fn split_fiber(
    &mut self,
    hooks: &GcHooks,
    frame_count: usize,
    arg_count: usize,
  ) -> Option<GcObj<Fiber>> {
    if self.frames().len() != frame_count + 1 {
      return None;
    }

    let frame = self.frames.pop().expect("Expected call frame");
    hooks.push_root(frame.closure);
    let mut fiber =
      hooks.manage_obj(Fiber::split(frame.closure, arg_count).expect("Assumed valid closure"));
    hooks.pop_roots(1);

    let slots = self.frame_stack().len();
    assert_eq!(slots, arg_count + 1);

    // if we have any argument bulk copy them to the fiber
    if slots > 1 {
      unsafe {
        ptr::copy_nonoverlapping(
          self.frame().stack_start.offset(1),
          fiber.stack_top,
          arg_count,
        );
        fiber.stack_top = fiber.stack_top.add(arg_count);
      }
    }

    // Effectively pop the current fibers frame so they're 'moved'
    // to the new fiber
    unsafe {
      self.stack_top = self.frame().stack_start;
      self.frame = self.frame.sub(1);
    }

    Some(fiber)
  }

  /// Ensure the stack has enough space. If more space is required
  /// additional space is allocated. All pointers into the stack
  /// are then updated
  pub fn ensure_stack(&mut self, additional: usize) {
    // check is we already have enought space
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
          frame.stack_start = stack_new.offset(frame.stack_start.offset_from(stack_old));
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
  pub fn stack_unwind(&mut self) -> Option<&mut CallFrame> {
    let mut stack_top = self.frame().stack_start;
    let mut drop: usize = 0;
    let mut catch_offset: Option<u16> = None;

    for frame in self.frames.iter().rev() {
      let fun = frame.closure.fun();
      let instructions = fun.chunk().instructions();

      // see if the current functions has a catch block at
      // this offset
      let offset = unsafe { frame.ip.offset_from(instructions.as_ptr()) } as usize;
      if let Some(offset) = fun.has_catch_jump(offset as u16) {
        catch_offset = Some(offset);
        break;
      }

      drop += 1;
      stack_top = frame.stack_start;
    }

    match catch_offset {
      Some(catch_offset) => {
        // truncate the unwound frames
        self.frames.truncate(self.frames.len() - drop);

        let frame = self
          .frames
          .last_mut()
          .expect("expected at least 1 frame to remain");

        let fun = frame.closure.fun();
        let instructions = fun.chunk().instructions();

        // set the current ip frame and stack pointer
        frame.ip = &instructions[catch_offset as usize] as *const u8;
        self.frame = frame as *mut CallFrame;
        self.stack_top = stack_top;

        Some(frame)
      }
      None => None,
    }
  }

  /// Print a error message with the associated stack track if found
  pub fn print_error(&self, log: &mut dyn Write, error: GcObj<Instance>) {
    let message = error[0].to_obj().to_str();
    writeln!(log, "{}: {}", &*error.class().name(), &*message).expect("Unable to write to stderr");

    for frame in self.frames.iter().rev() {
      let fun = frame.closure.fun();
      let location: String = match &*fun.name() {
        SCRIPT => SCRIPT.to_owned(),
        _ => format!("{}()", &*fun.name()),
      };

      let offset = unsafe { frame.ip.offset_from(fun.chunk().instructions().as_ptr()) } as usize;
      writeln!(
        log,
        "  [line {}] in {}",
        fun.chunk().get_line(offset),
        location
      )
      .expect("Unable to write to stderr");
    }
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
    unsafe { &*self.frame }
  }

  /// A mutable reference to the current frame
  #[inline]
  fn frame_mut(&mut self) -> &mut CallFrame {
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

#[cfg(debug_assertions)]
fn assert_inbounds<T>(slice: &[T], ptr: *mut T) {
  unsafe {
    let offset = ptr.offset_from(slice.as_ptr());
    assert!(offset >= 0);
    assert!((offset as usize) < slice.len())
  }
}

impl fmt::Display for Fiber {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<fiber {:p}>", self)
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

impl Manage for Fiber {
  fn size(&self) -> usize {
    mem::size_of::<Self>()
      + mem::size_of::<CallFrame>() * self.frames.capacity()
      + mem::size_of::<Value>() * self.stack.capacity()
  }

  fn as_debug(&self) -> &dyn crate::managed::DebugHeap {
    todo!()
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

    self.frames.iter().for_each(|frame| {
      frame.closure.trace();
    });

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

    self.frames.iter().for_each(|frame| {
      frame.closure.trace_debug(log);
    });

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
    hooks::{GcHooks, NoContext},
    support::{test_fun, FiberBuilder},
  };

  #[test]
  fn new() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let fiber = FiberBuilder::<u8>::default().build(&hooks);
    assert!(fiber.is_ok())
  }

  #[test]
  fn push() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = FiberBuilder::<u8>::default()
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

    let mut fiber = FiberBuilder::<u8>::default()
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

    let mut fiber = FiberBuilder::<u8>::default()
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

    let mut fiber = FiberBuilder::<u8>::default()
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

    let mut fiber = FiberBuilder::<u8>::default()
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

    let mut fiber = FiberBuilder::<u8>::default()
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

    let mut fiber = FiberBuilder::<u8>::default()
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

    let mut fiber = FiberBuilder::<u8>::default()
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

    let mut fiber = FiberBuilder::<u8>::default()
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

    let fiber = FiberBuilder::<u8>::default()
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

    let mut fiber = FiberBuilder::<u8>::default()
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

    let mut fiber = FiberBuilder::<u8>::default()
      .max_slots(0)
      .build(&hooks)
      .expect("Expected to build");

    unsafe {
      fiber.peek_set(1, val!(true));
    }
  }

  #[test]
  fn store_ip() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = FiberBuilder::<u8>::default()
      .max_slots(0)
      .build(&hooks)
      .expect("Expected to build");

    let val: u8 = 0;
    let ip = &val as *const u8;

    fiber.store_ip(ip);
    assert_eq!(fiber.frame().ip, ip)
  }

  #[test]
  fn load_ip() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = FiberBuilder::<u8>::default()
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

    let mut fiber = FiberBuilder::<u8>::default()
      .max_slots(4)
      .build(&hooks)
      .expect("Expected to build");

    let fun = test_fun(&hooks, "next", "next module");
    let closure = hooks.manage_obj(Closure::without_captures(fun));

    unsafe {
      fiber.push(val!(10.5));
      fiber.push(val!(false));
      fiber.push(VALUE_NIL);
      fiber.push(val!(closure));

      let frame_stack = fiber.frame_stack();

      assert_eq!(frame_stack.len(), 5);
      assert_eq!(frame_stack[0], val!(fiber.frame().closure));
      assert_eq!(frame_stack[1], val!(10.5));
      assert_eq!(frame_stack[2], val!(false));
      assert_eq!(frame_stack[3], VALUE_NIL);
      assert_eq!(frame_stack[4], val!(closure));

      fiber.push_frame(closure, 0);

      let frame_stack = fiber.frame_stack();

      assert_eq!(frame_stack.len(), 1);
      assert_eq!(frame_stack[0], val!(closure));
    }
  }

  #[test]
  #[should_panic]
  fn frame_stack_out_of_bounds() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = FiberBuilder::<u8>::default()
      .max_slots(4)
      .build(&hooks)
      .expect("Expected to build");

    let fun = test_fun(&hooks, "next", "next module");
    let closure = hooks.manage_obj(Closure::without_captures(fun));

    unsafe {
      fiber.push(val!(10.5));
      fiber.push(val!(false));
      fiber.push(VALUE_NIL);
      fiber.push(val!(closure));

      let frame_stack = fiber.frame_stack();

      assert_eq!(frame_stack.len(), 5);
      assert_eq!(frame_stack[0], val!(fiber.frame().closure));
      assert_eq!(frame_stack[1], val!(10.5));
      assert_eq!(frame_stack[2], val!(false));
      assert_eq!(frame_stack[3], VALUE_NIL);
      assert_eq!(frame_stack[4], val!(closure));

      fiber.push_frame(closure, 8);
      fiber.frame_stack();
    }
  }

  #[test]
  fn stack_slice() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = FiberBuilder::<u8>::default()
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

    let fiber = FiberBuilder::<u8>::default()
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
    let closure = hooks.manage_obj(Closure::without_captures(fun));

    let mut fiber = FiberBuilder::<u8>::default()
      .max_slots(3)
      .build(&hooks)
      .expect("Expected to build");

    unsafe {
      fiber.push(val!(closure));
      fiber.push(val!(10.0));
      fiber.push(val!(true));
    }

    fiber.push_frame(closure, 2);

    let slice = fiber.frame_stack();

    assert_eq!(slice[0], val!(closure));
    assert_eq!(slice[1], val!(10.0));
    assert_eq!(slice[2], val!(true));

    assert_eq!(fiber.frame().closure, closure);
  }

  #[test]
  fn pop_frame() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let fun = test_fun(&hooks, "next", "next module");
    let closure = hooks.manage_obj(Closure::without_captures(fun));

    let mut fiber = FiberBuilder::<u8>::default()
      .max_slots(4)
      .build(&hooks)
      .expect("Expected to build");

    unsafe {
      fiber.push(VALUE_NIL);
      fiber.push(val!(closure));
      fiber.push(val!(10.0));
      fiber.push(val!(true));
    }

    fiber.push_frame(closure, 2);
    let popped_frame = fiber.pop_frame().unwrap().unwrap();

    assert_ne!(popped_frame, closure.fun());

    let slice = fiber.frame_stack();

    assert_eq!(slice.len(), 2);
    assert_eq!(slice[0], val!(fiber.frame().closure));
    assert_eq!(slice[1], VALUE_NIL);
  }

  #[test]
  fn ensure_stack() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut fiber = FiberBuilder::<u8>::default()
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
