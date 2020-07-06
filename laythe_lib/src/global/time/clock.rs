use crate::support::export_and_insert;
use laythe_core::{
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{NativeFun, NativeMeta},
  signature::Arity,
  value::Value,
  CallResult, LyResult,
};
use laythe_env::{managed::Trace, stdio::Stdio};
use std::time::SystemTime;

const CLOCK_META: NativeMeta = NativeMeta::new("clock", Arity::Fixed(0), &[]);

pub fn declare_clock_funs(hooks: &GcHooks, module: &mut Module) -> LyResult<()> {
  export_and_insert(
    hooks,
    module,
    hooks.manage_str(CLOCK_META.name.to_string()),
    Value::from(hooks.manage(Box::new(Clock::new()) as Box<dyn NativeFun>)),
  )
}

#[derive(Clone, Debug, Trace)]
pub struct Clock {
  start: SystemTime,
}

impl Default for Clock {
  fn default() -> Self {
    Self::new()
  }
}

impl Clock {
  pub fn new() -> Self {
    Self {
      start: SystemTime::now(),
    }
  }
}

impl NativeFun for Clock {
  fn meta(&self) -> &NativeMeta {
    &CLOCK_META
  }

  fn call(&self, hooks: &mut Hooks, _args: &[Value]) -> CallResult {
    match self.start.elapsed() {
      Ok(elapsed) => Ok(Value::from((elapsed.as_micros() as f64) / 1000000.0)),
      Err(e) => hooks.error(format!("clock failed {}", e)),
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::support::{test_native_dependencies, MockedContext};

  #[test]
  fn new() {
    let clock = Clock::new();

    assert_eq!(clock.meta().name, "clock");
    assert_eq!(clock.meta().signature.arity, Arity::Fixed(0));
  }

  #[test]
  fn call() {
    let clock = Clock::new();
    let gc = test_native_dependencies();
    let mut context = MockedContext::new(&gc, &[]);
    let mut hooks = Hooks::new(&mut context);

    let values = &[];

    let result1 = clock.call(&mut hooks, values);
    let res1 = match result1 {
      Ok(res) => res,
      Err(_) => panic!(),
    };

    let result2 = clock.call(&mut hooks, values);
    let res2 = match result2 {
      Ok(res) => res,
      Err(_) => panic!(),
    };

    if res1.is_num() && res2.is_num() {
      assert!(res1.to_num() <= res2.to_num());
    } else {
      panic!();
    }
  }
}
