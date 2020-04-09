use spacelox_core::managed::Trace;
use spacelox_core::memory::Gc;
use spacelox_core::native::{NativeFun, NativeMeta, NativeResult};
use spacelox_core::value::{ArityKind, Value};
use std::time::SystemTime;

#[derive(Clone, Debug)]
pub struct NativeClock {
  meta: Box<NativeMeta>,
  start: SystemTime,
}

const NATIVE_CLOCK_META: NativeMeta = NativeMeta::new("clock", ArityKind::Fixed(0));

impl Default for NativeClock {
  fn default() -> Self {
    Self::new()
  }
}

impl NativeClock {
  pub fn new() -> Self {
    Self {
      meta: Box::new(NATIVE_CLOCK_META),
      start: SystemTime::now(),
    }
  }
}

impl NativeFun for NativeClock {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, _gc: &Gc, _context: &dyn Trace, _args: &[Value]) -> NativeResult {
    match self.start.elapsed() {
      Ok(elapsed) => NativeResult::Success(Value::Number((elapsed.as_micros() as f64) / 1000000.0)),
      Err(e) => NativeResult::RuntimeError(format!("clock failed {}", e)),
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::support::test_native_dependencies;

  #[test]
  fn new() {
    let clock = NativeClock::new();

    assert_eq!(clock.meta.name, "clock");
    assert_eq!(clock.meta.arity, ArityKind::Fixed(0));
  }

  #[test]
  fn call() {
    let clock = NativeClock::new();
    let (gc, context) = test_native_dependencies();
    let values = &[];

    let result1 = clock.call(&gc, &*context, values);
    let res1 = match result1 {
      NativeResult::Success(res) => res,
      NativeResult::RuntimeError(_) => panic!(),
    };

    let result2 = clock.call(&gc, &*context, values);
    let res2 = match result2 {
      NativeResult::Success(res) => res,
      NativeResult::RuntimeError(_) => panic!(),
    };

    match (res1, res2) {
      (Value::Number(num1), Value::Number(num2)) => assert!(num1 <= num2),
      _ => panic!(),
    }
  }
}
