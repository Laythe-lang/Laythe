use crate::support::{
  default_class_inheritance, export_and_insert, load_class_from_module, to_dyn_method,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{NativeMeta, NativeMethod},
  package::Package,
  signature::{Arity, Parameter, ParameterKind},
  value::Value,
  CallResult, LyResult,
};
use laythe_env::{managed::Trace, stdio::Stdio};

pub const STRING_CLASS_NAME: &str = "String";
const STRING_STR: NativeMeta = NativeMeta::new("str", Arity::Fixed(0), &[]);
const STRING_HAS: NativeMeta = NativeMeta::new(
  "has",
  Arity::Fixed(1),
  &[Parameter::new("string", ParameterKind::String)],
);

pub fn declare_string_class(
  hooks: &GcHooks,
  module: &mut Module,
  package: &Package,
) -> LyResult<()> {
  let class = default_class_inheritance(hooks, package, STRING_CLASS_NAME)?;
  export_and_insert(hooks, module, class.name, Value::from(class))
}

pub fn define_string_class(hooks: &GcHooks, module: &Module, _: &Package) -> LyResult<()> {
  let mut class = load_class_from_module(hooks, module, STRING_CLASS_NAME)?;

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

  Ok(())
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
    use crate::support::{test_native_dependencies, MockedContext};

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
      let mut context = MockedContext::new(&gc, &[]);
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
    use crate::support::{test_native_dependencies, MockedContext};

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
      let mut context = MockedContext::new(&gc, &[]);
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
