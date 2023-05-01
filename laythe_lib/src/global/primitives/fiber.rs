use super::class_inheritance;
use crate::{
  native,
  support::{export_and_insert, load_class_from_module},
  StdError, StdResult,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  managed::Trace,
  managed::{Gc, GcObj},
  module::Module,
  object::{LyNative, Native, NativeMetaBuilder},
  signature::Arity,
  val,
  value::Value,
  Call,
};
use std::io::Write;

pub const FIBER_CLASS_NAME: &str = "Fiber";
const FIBER_STR: NativeMetaBuilder = NativeMetaBuilder::method("str", Arity::Fixed(0));

pub fn declare_fiber_class(hooks: &GcHooks, module: Gc<Module>) -> StdResult<()> {
  let bool_class = class_inheritance(hooks, module, FIBER_CLASS_NAME)?;
  export_and_insert(module, bool_class.name(), val!(bool_class)).map_err(StdError::from)
}

pub fn define_fiber_class(hooks: &GcHooks, module: Gc<Module>) -> StdResult<()> {
  let mut bool_class = load_class_from_module(hooks, module, FIBER_CLASS_NAME)?;

  bool_class.add_method(
    hooks.manage_str(String::from(FIBER_STR.name)),
    val!(FiberStr::native(hooks)),
  );

  Ok(())
}

native!(FiberStr, FIBER_STR);

impl LyNative for FiberStr {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    let this = this.unwrap();
    let class = hooks.get_class(this);
    let fiber = this.to_obj().to_fiber();

    Call::Ok(val!(hooks.manage_str(format!(
      "<{} {:p}>",
      &*class.name(),
      &*fiber
    ))))
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod str {
    use laythe_core::support::FiberBuilder;

    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let fiber_str = FiberStr::native(&hooks);

      assert_eq!(fiber_str.meta().name, "str");
      assert_eq!(fiber_str.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::with_std(&[]).expect("std lib failure");
      let mut hooks = Hooks::new(&mut context);

      let fiber_str = FiberStr::native(&hooks.as_gc());

      let fiber = FiberBuilder::default()
        .instructions(vec![0])
        .build(&hooks.as_gc())
        .unwrap();

      let result = fiber_str.call(&mut hooks, Some(val!(fiber)), &[]).unwrap();
      assert!(result.to_obj().to_str().contains("<Fiber "));
    }
  }
}
