use super::class_inheritance;
use crate::{
  native,
  support::{export_and_insert, load_class_from_module},
  StdResult,
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
  export_and_insert(hooks, module, bool_class.name(), val!(bool_class))
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
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let this = args[0];
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
    fn call() {
      let mut context = MockedContext::with_std(&[]).expect("std lib failure");
      let mut hooks = Hooks::new(&mut context);

      let fiber_str = FiberStr::native(&hooks.as_gc());

      let fiber = FiberBuilder::default()
        .instructions(vec![0])
        .build(&hooks.as_gc())
        .unwrap();

      let result = fiber_str.call(&mut hooks, &[val!(fiber)]).unwrap();
      assert!(result.to_obj().to_str().contains("<Fiber "));
    }
  }
}
