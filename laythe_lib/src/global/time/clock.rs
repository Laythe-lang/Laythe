use crate::{
  native,
  support::{export_and_insert, to_dyn_native},
  StdResult,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  managed::Trace,
  module::Module,
  native::{MetaData, Native, NativeMeta, NativeMetaBuilder},
  signature::Arity,
  val,
  value::Value,
  Call,
};
use std::io::Write;

const CLOCK_META: NativeMetaBuilder = NativeMetaBuilder::fun("clock", Arity::Fixed(0));

pub fn declare_clock_funs(hooks: &GcHooks, module: &mut Module) -> StdResult<()> {
  export_and_insert(
    hooks,
    module,
    hooks.manage_str(CLOCK_META.name),
    val!(to_dyn_native(hooks, Clock::from(hooks))),
  )
}

native!(Clock, CLOCK_META);

impl Native for Clock {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, _args: &[Value]) -> Call {
    let io = hooks.as_io();
    let time = io.time();

    match time.elapsed() {
      Ok(elapsed) => Call::Ok(val!((elapsed.as_micros() as f64) / 1_000_000.0)),
      Err(e) => panic!(format!("TODO clock failed {}", e)),
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::support::MockedContext;

  #[test]
  fn new() {
    let mut context = MockedContext::default();
    let hooks = GcHooks::new(&mut context);

    let clock = Clock::from(&hooks);

    assert_eq!(clock.meta().name, "clock");
    assert_eq!(clock.meta().signature.arity, Arity::Fixed(0));
  }

  #[test]
  fn call() {
    let mut context = MockedContext::default();
    let mut hooks = Hooks::new(&mut context);
    let clock = Clock::from(&hooks);

    let values = &[];

    let result1 = clock.call(&mut hooks, None, values).unwrap();
    let result2 = clock.call(&mut hooks, None, values).unwrap();

    if result1.is_num() && result2.is_num() {
      assert!(result1.to_num() <= result2.to_num());
    } else {
      panic!();
    }
  }
}
