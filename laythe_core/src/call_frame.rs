use crate::{
  managed::{DebugHeap, DebugWrap, GcObj},
  object::Closure,
  value::Value,
};

/// A call frame in the space lox interpreter
#[derive(Clone, Copy, PartialEq)]
pub struct CallFrame {
  /// The function defining this call frame
  pub closure: GcObj<Closure>,

  /// The instruction pointer for this frame
  pub ip: *const u8,

  /// The stack offset for this frame
  pub stack_start: *mut Value,
}

impl CallFrame {
  /// Create a new call frame from the provided closure
  pub fn new(closure: GcObj<Closure>, stack_start: *mut Value) -> Self {
    let ip = closure.fun().chunk().instructions().as_ptr();

    CallFrame {
      closure,
      ip,
      stack_start,
    }
  }
}

impl DebugHeap for CallFrame {
  fn fmt_heap(&self, f: &mut std::fmt::Formatter, depth: usize) -> std::fmt::Result {
    f.debug_struct("CallFrame")
      .field("closure", &DebugWrap(&self.closure, depth))
      .field("ip", &format_args!("{:p}", self.ip))
      .field("stack_start", &format_args!("{:p}", self.stack_start))
      .finish()
  }
}
