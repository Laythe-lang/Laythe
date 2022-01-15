use crate::{
  captures::Captures,
  managed::{DebugHeap, DebugWrap, GcObj},
  object::Fun,
  value::Value,
};

/// A call frame in the space lox interpreter
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct CallFrame {
  /// The function defining this call frame
  fun: GcObj<Fun>,

  /// The captures for this call frame
  captures: Captures,

  /// The instruction pointer for this frame
  ip: *const u8,

  /// The stack offset for this frame
  stack_start: *mut Value,
}

impl CallFrame {
  /// Create a new call frame from the provided closure
  pub fn new(fun: GcObj<Fun>, captures: Captures, stack_start: *mut Value) -> Self {
    let ip = fun.chunk().instructions().as_ptr();

    CallFrame {
      fun,
      captures,
      ip,
      stack_start,
    }
  }

  pub fn fun(&self) -> GcObj<Fun> {
    self.fun
  }

  pub fn captures(&self) -> Captures {
    self.captures
  }

  pub fn ip(&self) -> *const u8 {
    self.ip
  }

  pub fn store_ip(&mut self, ip: *const u8) {
    self.ip = ip;
  }

  pub fn stack_start(&self) -> *mut Value {
    self.stack_start
  }

  pub fn store_stack_start(&mut self, stack_start: *mut Value) {
    self.stack_start = stack_start;
  }
}

impl DebugHeap for CallFrame {
  fn fmt_heap(&self, f: &mut std::fmt::Formatter, depth: usize) -> std::fmt::Result {
    f.debug_struct("CallFrame")
      .field("closure", &DebugWrap(&self.fun, depth))
      .field("ip", &format_args!("{:p}", self.ip))
      .field("stack_start", &format_args!("{:p}", self.stack_start))
      .finish()
  }
}

unsafe impl Send for CallFrame {}
unsafe impl Sync for CallFrame {}
