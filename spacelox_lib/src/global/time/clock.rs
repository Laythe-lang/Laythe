use crate::support::export_and_insert;
use spacelox_core::{
  signature::Arity,
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{NativeFun, NativeMeta},
  value::Value,
  CallResult, ModuleResult,
};
use spacelox_env::{managed::Trace, stdio::StdIo};
use std::time::SystemTime;

#[derive(Clone, Debug, Trace)]
pub struct Clock {
  meta: &'static NativeMeta,
  start: SystemTime,
}

const CLOCK_META: NativeMeta = NativeMeta::new("clock", Arity::Fixed(0), &[]);

pub fn declare_clock_funs(hooks: &GcHooks, self_module: &mut Module) -> ModuleResult<()> {
  export_and_insert(
    hooks,
    self_module,
    hooks.manage_str(CLOCK_META.name.to_string()),
    Value::from(hooks.manage(Box::new(Clock::new()) as Box<dyn NativeFun>)),
  )
}

impl Default for Clock {
  fn default() -> Self {
    Self::new()
  }
}

impl Clock {
  pub fn new() -> Self {
    Self {
      meta: &CLOCK_META,
      start: SystemTime::now(),
    }
  }
}

impl NativeFun for Clock {
  fn meta(&self) -> &NativeMeta {
    &self.meta
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
  use crate::support::{test_native_dependencies, TestContext};

  #[test]
  fn new() {
    let clock = Clock::new();

    assert_eq!(clock.meta.name, "clock");
    assert_eq!(clock.meta.signature.arity, Arity::Fixed(0));
  }

  #[test]
  fn call() {
    let clock = Clock::new();
    let gc = test_native_dependencies();
    let mut context = TestContext::new(&gc, &[]);
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
