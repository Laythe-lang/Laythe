use crate::{
  native,
  support::{export_and_insert, load_class_from_module, to_dyn_native},
  StdResult,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  managed::Trace,
  module::Module,
  object::{MetaData, Native, NativeMeta, NativeMetaBuilder},
  signature::Arity,
  val,
  value::Value,
  Call,
};
use std::io::Write;

use super::class_inheritance;

pub const NIL_CLASS_NAME: &str = "Nil";
const NIL_STR: NativeMetaBuilder = NativeMetaBuilder::method("str", Arity::Fixed(0));

pub fn declare_nil_class(hooks: &GcHooks, module: &mut Module) -> StdResult<()> {
  let class = class_inheritance(hooks, module, NIL_CLASS_NAME)?;
  export_and_insert(hooks, module, class.name(), val!(class))
}

pub fn define_nil_class(hooks: &GcHooks, module: &Module) -> StdResult<()> {
  let mut class = load_class_from_module(hooks, module, NIL_CLASS_NAME)?;

  class.add_method(
    hooks,
    hooks.manage_str(NIL_STR.name),
    val!(to_dyn_native(hooks, NilStr::from(hooks))),
  );

  Ok(())
}

native!(NilStr, NIL_STR);

impl Native for NilStr {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, _args: &[Value]) -> Call {
    Call::Ok(val!(hooks.manage_str("nil")))
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod str {
    use super::*;
    use crate::support::MockedContext;
    use laythe_core::value::VALUE_NIL;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = Hooks::new(&mut context);

      let nil_str = NilStr::from(&hooks);

      assert_eq!(nil_str.meta.name, "str");
      assert_eq!(nil_str.meta.signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let nil_str = NilStr::from(&hooks);

      let result = nil_str.call(&mut hooks, Some(VALUE_NIL), &[]);
      match result {
        Call::Ok(r) => assert_eq!(*r.to_str(), "nil".to_string()),
        _ => assert!(false),
      }
    }
  }
}
