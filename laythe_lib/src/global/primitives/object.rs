use crate::support::{export_and_insert, load_class_from_module, to_dyn_method};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{NativeMeta, NativeMethod},
  object::Class,
  package::Package,
  signature::{Arity, Parameter, ParameterKind},
  value::Value,
  CallResult, LyResult,
};
use laythe_env::{managed::Trace, stdio::Stdio};

pub const OBJECT_CLASS_NAME: &'static str = "Object";

const OBJECT_EQUALS: NativeMeta = NativeMeta::new(
  "equals",
  Arity::Fixed(1),
  &[Parameter::new("other", ParameterKind::Any)],
);

pub fn declare_object_class(hooks: &GcHooks, self_module: &mut Module) -> LyResult<()> {
  let name = hooks.manage_str(String::from(OBJECT_CLASS_NAME));
  let class = hooks.manage(Class::bare(name));

  export_and_insert(hooks, self_module, name, Value::from(class))
}

pub fn define_object_class(hooks: &GcHooks, module: &Module, _: &Package) -> LyResult<()> {
  let mut class = load_class_from_module(hooks, module, OBJECT_CLASS_NAME)?;

  class.add_method(
    &hooks,
    hooks.manage_str(String::from(OBJECT_EQUALS.name)),
    Value::from(to_dyn_method(hooks, ObjectEquals())),
  );

  Ok(())
}

#[derive(Clone, Debug, Trace)]
struct ObjectEquals();

impl NativeMethod for ObjectEquals {
  fn meta(&self) -> &NativeMeta {
    &OBJECT_EQUALS
  }

  fn call(&self, _hooks: &mut Hooks, this: Value, args: &[Value]) -> CallResult {
    Ok(Value::from(this == args[0]))
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod equals {
    use super::*;
    use crate::support::{test_native_dependencies, MockedContext};

    #[test]
    fn new() {
      let object_equals = ObjectEquals();

      assert_eq!(object_equals.meta().name, "equals");
      assert_eq!(object_equals.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        object_equals.meta().signature.parameters[0].kind,
        ParameterKind::Any
      )
    }

    #[test]
    fn call() {
      let bool_str = ObjectEquals();
      let gc = test_native_dependencies();
      let mut context = MockedContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let ten_1 = Value::from(10.0);
      let b_false = Value::from(false);
      let ten_2 = Value::from(10.0);

      let result1 = bool_str.call(&mut hooks, ten_1, &[ten_2]);
      let result2 = bool_str.call(&mut hooks, ten_2, &[b_false]);

      match result1 {
        Ok(r) => assert_eq!(r.to_bool(), true),
        Err(_) => assert!(false),
      }
      match result2 {
        Ok(r) => assert_eq!(r.to_bool(), false),
        Err(_) => assert!(false),
      }
    }
  }
}
