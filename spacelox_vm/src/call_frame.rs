use spacelox_core::managed::Managed;
use spacelox_core::value::Closure;

/// A call frame in the space lox interpreter
#[derive(Clone, PartialEq)]
pub struct CallFrame {
  pub closure: Managed<Closure>,
  pub ip: usize,
  pub slots: usize,
}

impl<'a> CallFrame {
  pub fn new(closure: Managed<Closure>) -> Self {
    CallFrame {
      closure,
      ip: 0,
      slots: 0,
    }
  }
}
