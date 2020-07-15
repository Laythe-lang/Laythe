use crate::support::export_and_insert;
use laythe_core::{
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{NativeFun, NativeMeta},
  signature::Arity,
  value::Value,
  CallResult, LyResult,
  val
};
use laythe_env::{managed::Trace, stdio::Stdio};

const CLOCK_META: NativeMeta = NativeMeta::new("clock", Arity::Fixed(0));

pub fn declare_clock_funs(hooks: &GcHooks, module: &mut Module) -> LyResult<()> {
  export_and_insert(
    hooks,
    module,
    hooks.manage_str(CLOCK_META.name.to_string()),
    val!(hooks.manage(Box::new(Clock()) as Box<dyn NativeFun>)),
  )
}

#[derive(Clone, Debug, Trace)]
pub struct Clock();

impl NativeFun for Clock {
  fn meta(&self) -> &NativeMeta {
    &CLOCK_META
  }

  fn call(&self, hooks: &mut Hooks, _args: &[Value]) -> CallResult {
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
    let clock = Clock();

    assert_eq!(clock.meta().name, "clock");
    assert_eq!(clock.meta().signature.arity, Arity::Fixed(0));
  }

  #[test]
  fn call() {
    let clock = Clock();
    let mut context = MockedContext::default();
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
