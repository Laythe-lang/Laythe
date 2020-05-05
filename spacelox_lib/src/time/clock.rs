use spacelox_core::{
  arity::ArityKind,
  hooks::Hooks,
  io::StdIo,
  managed::Trace,
  native::{NativeFun, NativeMeta},
  value::Value,
  CallResult,
};
use std::time::SystemTime;

#[derive(Clone, Debug, Trace)]
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

  fn call(&self, hooks: &Hooks, _args: &[Value]) -> CallResult {
    match self.start.elapsed() {
      Ok(elapsed) => Ok(Value::Number((elapsed.as_micros() as f64) / 1000000.0)),
      Err(e) => hooks.error(format!("clock failed {}", e)),
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::support::{test_native_dependencies, TestContext};

  #[test]
  fn new() {
    let clock = NativeClock::new();

    assert_eq!(clock.meta.name, "clock");
    assert_eq!(clock.meta.arity, ArityKind::Fixed(0));
  }

  #[test]
  fn call() {
    let clock = NativeClock::new();
    let gc = test_native_dependencies();
    let mut context = TestContext::new(&gc, &[]);
    let hooks = Hooks::new(&mut context);

    let values = &[];

    let result1 = clock.call(&hooks, values);
    let res1 = match result1 {
      Ok(res) => res,
      Err(_) => panic!(),
    };

    let result2 = clock.call(&hooks, values);
    let res2 = match result2 {
      Ok(res) => res,
      Err(_) => panic!(),
    };

    match (res1, res2) {
      (Value::Number(num1), Value::Number(num2)) => assert!(num1 <= num2),
      _ => panic!(),
    }
  }
}
