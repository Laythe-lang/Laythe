use super::class_inheritance;
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
  value::{Value, VALUE_TRUE},
  Call, Ref,
};
use std::io::Write;

pub const BOOL_CLASS_NAME: &str = "Bool";
const BOOL_STR: NativeMetaBuilder = NativeMetaBuilder::method("str", Arity::Fixed(0));

pub fn declare_bool_class(hooks: &GcHooks, module: Ref<Module>) -> StdResult<()> {
  let bool_class = class_inheritance(hooks, module, BOOL_CLASS_NAME)?;
  export_and_insert(hooks, module, BOOL_CLASS_NAME, val!(bool_class))
}

pub fn define_bool_class(hooks: &GcHooks, module: Ref<Module>) -> StdResult<()> {
  let mut bool_class = load_class_from_module(hooks, module, BOOL_CLASS_NAME)?;

  bool_class.add_method(
    hooks.manage_str(String::from(BOOL_STR.name)),
    val!(BoolStr::native(hooks)),
  );

  Ok(())
}

native!(BoolStr, BOOL_STR);

impl LyNative for BoolStr {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    if args[0] == VALUE_TRUE {
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
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let bool_str = BoolStr::native(&hooks.as_gc());

      let b_true = val!(true);
      let b_false = val!(false);

      let result1 = bool_str.call(&mut hooks, &[b_true]);
      let result2 = bool_str.call(&mut hooks, &[b_false]);

      assert_eq!(result1.unwrap().to_obj().to_str(), "true");
      assert_eq!(result2.unwrap().to_obj().to_str(), "false");
    }
  }
}
