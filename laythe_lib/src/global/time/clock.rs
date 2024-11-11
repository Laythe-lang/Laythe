use crate::{native, support::export_and_insert, StdResult};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  managed::Trace,
  module::Module,
  object::{LyNative, Native, NativeMetaBuilder},
  signature::Arity,
  val,
  value::Value,
  Call, Ref,
};
use std::io::Write;

const CLOCK_META: NativeMetaBuilder = NativeMetaBuilder::fun("clock", Arity::Fixed(0));

pub fn declare_clock_funs(hooks: &GcHooks, module: Ref<Module>) -> StdResult<()> {
  export_and_insert(hooks, module, CLOCK_META.name, val!(Clock::native(hooks)))
}

native!(Clock, CLOCK_META);

impl LyNative for Clock {
  fn call(&self, hooks: &mut Hooks, _args: &[Value]) -> Call {
    let io = hooks.as_io();
    let time = io.time();

    match time.elapsed() {
      Ok(elapsed) => Call::Ok(val!((elapsed.as_micros() as f64) / 1_000_000.0)),
      Err(e) => panic!("clock failed {e}"),
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::support::MockedContext;

  #[test]
  fn call() {
    let mut context = MockedContext::default();
    let mut hooks = Hooks::new(&mut context);
    let clock = Clock::native(&hooks.as_gc());

    let values = &[];

    let result1 = clock.call(&mut hooks, values).unwrap();
    let result2 = clock.call(&mut hooks, values).unwrap();

    if result1.is_num() && result2.is_num() {
      assert!(result1.to_num() <= result2.to_num());
    } else {
      panic!();
    }
  }
}
