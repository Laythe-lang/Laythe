use crate::support::{export_and_insert, to_dyn_method};
use spacelox_core::{
  signature::{Arity, Parameter, ParameterKind},
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{NativeMeta, NativeMethod},
  object::Class,
  package::Package,
  value::Value,
  CallResult, ModuleResult,
};
use spacelox_env::{managed::Trace, stdio::StdIo};

pub const STRING_CLASS_NAME: &str = "String";
const STRING_STR: NativeMeta = NativeMeta::new("str", Arity::Fixed(0), &[]);
const STRING_HAS: NativeMeta = NativeMeta::new(
  "has",
  Arity::Fixed(1),
  &[Parameter::new("string", ParameterKind::String)],
);

pub fn declare_string_class(hooks: &GcHooks, self_module: &mut Module) -> ModuleResult<()> {
  let name = hooks.manage_str(String::from(STRING_CLASS_NAME));
  let class = hooks.manage(Class::new(name));

  export_and_insert(hooks, self_module, name, Value::from(class))
}

pub fn define_string_class(hooks: &GcHooks, self_module: &Module, _: &Package) {
  let name = hooks.manage_str(String::from(STRING_CLASS_NAME));
  let mut class = self_module
    .import(hooks)
    .get_field(&name)
    .unwrap()
    .to_class();

  class.add_method(
    hooks,
    hooks.manage_str(String::from(STRING_STR.name)),
    Value::from(to_dyn_method(hooks, StringStr())),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(STRING_HAS.name)),
    Value::from(to_dyn_method(hooks, StringHas())),
  );
}

#[derive(Clone, Debug, Trace)]
struct StringStr();

impl NativeMethod for StringStr {
  fn meta(&self) -> &NativeMeta {
    &STRING_STR
  }

  fn call(&self, _hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    Ok(this)
  }
}

#[derive(Clone, Debug, Trace)]
struct StringHas();

impl NativeMethod for StringHas {
  fn meta(&self) -> &NativeMeta {
    &STRING_HAS
  }

  fn call(&self, _vhooks: &mut Hooks, this: Value, args: &[Value]) -> CallResult {
    let str = this.to_str();
    Ok(Value::from(str.contains(&*args[0].to_str())))
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
      let string_str = StringStr();

      assert_eq!(string_str.meta().name, "str");
      assert_eq!(string_str.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let string_str = StringStr();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let this = Value::from(hooks.manage_str("test".to_string()));
      let result = string_str.call(&mut hooks, this, &[]);
      match result {
        Ok(r) => assert_eq!(*r.to_str(), "test".to_string()),
        Err(_) => assert!(false),
      }
    }
  }

  mod has {
    use super::*;
    use crate::support::{test_native_dependencies, TestContext};

    #[test]
    fn new() {
      let string_str = StringHas();

      assert_eq!(string_str.meta().name, "has");
      assert_eq!(string_str.meta().signature.arity, Arity::Fixed(1));
    }

    #[test]
    fn call() {
      let string_str = StringHas();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let this = Value::from(hooks.manage_str("some string".to_string()));
      let contained = Value::from(hooks.manage_str("ome".to_string()));
      let not_contained = Value::from(hooks.manage_str("other".to_string()));

      let result = string_str.call(&mut hooks, this, &[contained]);
      match result {
        Ok(r) => assert!(r.to_bool()),
        Err(_) => assert!(false),
      }

      let result = string_str.call(&mut hooks, this, &[not_contained]);
      match result {
        Ok(r) => assert!(!r.to_bool()),
        Err(_) => assert!(false),
      }
    }
  }
}
