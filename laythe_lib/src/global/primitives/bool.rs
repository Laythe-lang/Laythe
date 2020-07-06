use crate::support::{
  default_class_inheritance, export_and_insert, load_class_from_module, to_dyn_method,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{NativeMeta, NativeMethod},
  package::Package,
  signature::Arity,
  value::{Value, VALUE_TRUE},
  CallResult, LyResult,
};
use laythe_env::{managed::Trace, stdio::Stdio};

pub const BOOL_CLASS_NAME: &'static str = "Bool";
const BOOL_STR: NativeMeta = NativeMeta::new("str", Arity::Fixed(0), &[]);

pub fn declare_bool_class(hooks: &GcHooks, module: &mut Module, package: &Package) -> LyResult<()> {
  let bool_class = default_class_inheritance(hooks, package, BOOL_CLASS_NAME)?;
  export_and_insert(hooks, module, bool_class.name, Value::from(bool_class))
}

pub fn define_bool_class(hooks: &GcHooks, module: &Module, _: &Package) -> LyResult<()> {
  let mut bool_class = load_class_from_module(hooks, module, BOOL_CLASS_NAME)?;

  bool_class.add_method(
    &hooks,
    hooks.manage_str(String::from(BOOL_STR.name)),
    Value::from(to_dyn_method(hooks, BoolStr())),
  );

  Ok(())
}

#[derive(Clone, Debug, Trace)]
struct BoolStr();

impl NativeMethod for BoolStr {
  fn meta(&self) -> &NativeMeta {
    &BOOL_STR
  }

  fn call(&self, hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    if this == VALUE_TRUE {
      Ok(Value::from(hooks.manage_str("true".to_string())))
    } else {
      Ok(Value::from(hooks.manage_str("false".to_string())))
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod str {
    use super::*;
    use crate::support::{test_native_dependencies, MockedContext};

    #[test]
    fn new() {
      let bool_str = BoolStr();

      assert_eq!(bool_str.meta().name, "str");
      assert_eq!(bool_str.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let bool_str = BoolStr();
      let gc = test_native_dependencies();
      let mut context = MockedContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let b_true = Value::from(true);
      let b_false = Value::from(false);

      let result1 = bool_str.call(&mut hooks, b_true, &[]);
      let result2 = bool_str.call(&mut hooks, b_false, &[]);

      match result1 {
        Ok(r) => assert_eq!(&*r.to_str(), "true"),
        Err(_) => assert!(false),
      }
      match result2 {
        Ok(r) => assert_eq!(&*r.to_str(), "false"),
        Err(_) => assert!(false),
      }
    }
  }
}
