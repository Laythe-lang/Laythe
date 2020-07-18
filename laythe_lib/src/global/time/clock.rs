use crate::{native, support::export_and_insert};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{MetaData, Native, NativeMeta, NativeMetaBuilder},
  signature::Arity,
  val,
  value::Value,
  CallResult, LyResult,
};
use laythe_env::{managed::Trace, stdio::Stdio};

const CLOCK_META: NativeMetaBuilder = NativeMetaBuilder::fun("clock", Arity::Fixed(0));

pub fn declare_clock_funs(hooks: &GcHooks, module: &mut Module) -> LyResult<()> {
  export_and_insert(
    hooks,
    module,
    hooks.manage_str(CLOCK_META.name.to_string()),
    val!(hooks.manage(Box::new(Clock::from(hooks)) as Box<dyn Native>)),
  )
}

native!(Clock, CLOCK_META);

impl Native for Clock {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, _args: &[Value]) -> CallResult {
    let io = hooks.to_io();
    let time = io.time();

    match time.elapsed() {
      Ok(elapsed) => Ok(val!((elapsed.as_micros() as f64) / 1000000.0)),
      Err(e) => hooks.error(format!("clock failed {}", e)),
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

    let result1 = clock.call(&mut hooks, None, values);
    let res1 = match result1 {
      Ok(res) => res,
      Err(_) => panic!(),
    };

    let result2 = clock.call(&mut hooks, None, values);
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
