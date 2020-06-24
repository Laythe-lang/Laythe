use crate::support::{export_and_insert, to_dyn_method};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{NativeMeta, NativeMethod},
  object::Class,
  package::Package,
  signature::Arity,
  value::{Value, VALUE_TRUE},
  CallResult, ModuleResult,
};
use laythe_env::{managed::Trace, stdio::StdIo};

pub const BOOL_CLASS_NAME: &'static str = "Bool";
const BOOL_STR: NativeMeta = NativeMeta::new("str", Arity::Fixed(0), &[]);

pub fn declare_bool_class(hooks: &GcHooks, self_module: &mut Module) -> ModuleResult<()> {
  let name = hooks.manage_str(String::from(BOOL_CLASS_NAME));
  let class = hooks.manage(Class::new(name));

  export_and_insert(hooks, self_module, name, Value::from(class))
}

pub fn define_bool_class(hooks: &GcHooks, self_module: &Module, _: &Package) {
  let name = hooks.manage_str(String::from(BOOL_CLASS_NAME));
  let mut class = self_module
    .import(hooks)
    .get_field(&name)
    .unwrap()
    .to_class();

  class.add_method(
    &hooks,
    hooks.manage_str(String::from(BOOL_STR.name)),
    Value::from(to_dyn_method(hooks, BoolStr())),
  );
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
    use crate::support::{test_native_dependencies, TestContext};

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
      let mut context = TestContext::new(&gc, &[]);
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