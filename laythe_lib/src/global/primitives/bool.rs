use super::class_inheritance;
use crate::{
  native,
  support::{export_and_insert, load_class_from_module, to_dyn_native},
  StdError, StdResult,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  managed::Trace,
  module::Module,
  native::{MetaData, Native, NativeMeta, NativeMetaBuilder},
  signature::Arity,
  val,
  value::{Value, VALUE_TRUE},
  Call,
};
use std::io::Write;

pub const BOOL_CLASS_NAME: &str = "Bool";
const BOOL_STR: NativeMetaBuilder = NativeMetaBuilder::method("str", Arity::Fixed(0));

pub fn declare_bool_class(hooks: &GcHooks, module: &mut Module) -> StdResult<()> {
  let bool_class = class_inheritance(hooks, module, BOOL_CLASS_NAME)?;
  export_and_insert(hooks, module, bool_class.name(), val!(bool_class)).map_err(StdError::from)
}

pub fn define_bool_class(hooks: &GcHooks, module: &Module) -> StdResult<()> {
  let mut bool_class = load_class_from_module(hooks, module, BOOL_CLASS_NAME)?;

  bool_class.add_method(
    &hooks,
    hooks.manage_str(String::from(BOOL_STR.name)),
    val!(to_dyn_native(hooks, BoolStr::from(hooks))),
  );

  Ok(())
}

native!(BoolStr, BOOL_STR);

impl Native for BoolStr {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    if this.unwrap() == VALUE_TRUE {
      Call::Ok(val!(hooks.manage_str("true")))
    } else {
      Call::Ok(val!(hooks.manage_str("false")))
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod str {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let bool_str = BoolStr::from(&hooks);

      assert_eq!(bool_str.meta().name, "str");
      assert_eq!(bool_str.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let bool_str = BoolStr::from(&hooks);

      let b_true = val!(true);
      let b_false = val!(false);

      let result1 = bool_str.call(&mut hooks, Some(b_true), &[]);
      let result2 = bool_str.call(&mut hooks, Some(b_false), &[]);

      assert_eq!(result1.unwrap().to_str(), "true");
      assert_eq!(result2.unwrap().to_str(), "false");
    }
  }
}
