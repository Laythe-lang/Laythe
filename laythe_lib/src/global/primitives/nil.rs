use crate::{
  native,
  support::{export_and_insert, load_class_from_module},
  StdResult,
};
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

use super::class_inheritance;

pub const NIL_CLASS_NAME: &str = "Nil";
const NIL_STR: NativeMetaBuilder = NativeMetaBuilder::method("str", Arity::Fixed(0));

pub fn declare_nil_class(hooks: &GcHooks, module: Ref<Module>) -> StdResult<()> {
  let class = class_inheritance(hooks, module, NIL_CLASS_NAME)?;
  export_and_insert(hooks, module, class.name(), val!(class))
}

pub fn define_nil_class(hooks: &GcHooks, module: Ref<Module>) -> StdResult<()> {
  let mut class = load_class_from_module(hooks, module, NIL_CLASS_NAME)?;

  class.add_method(hooks.manage_str(NIL_STR.name), val!(NilStr::native(hooks)));

  Ok(())
}

native!(NilStr, NIL_STR);

impl LyNative for NilStr {
  fn call(&self, hooks: &mut Hooks, _args: &[Value]) -> Call {
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
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let nil_str = NilStr::native(&hooks.as_gc());

      let result = nil_str.call(&mut hooks, &[VALUE_NIL]);
      match result {
        Call::Ok(r) => assert_eq!(*r.to_obj().to_str(), "nil".to_string()),
        _ => panic!(),
      }
    }
  }
}
