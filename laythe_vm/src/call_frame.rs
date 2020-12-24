use laythe_core::{object::Closure, value::Value};
use laythe_env::managed::Gc;

/// A call frame in the space lox interpreter
#[derive(Clone, Copy, PartialEq)]
pub struct CallFrame {
  /// The function defining this call frame
  pub closure: Gc<Closure>,

  /// The instruction pointer for this frame
  pub ip: *const u8,

  /// The stack offset for this frame
  pub slots: *mut Value,
}

impl CallFrame {
  pub fn new(closure: Gc<Closure>) -> Self {
    CallFrame {
      closure,
      ip: std::ptr::null(),
      slots: std::ptr::null_mut(),
    }
  }
}
