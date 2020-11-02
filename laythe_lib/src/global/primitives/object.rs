use crate::{
  native,
  support::{export_and_insert, load_class_from_module, to_dyn_native},
};
use laythe_core::{
  constants::OBJECT,
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{MetaData, Native, NativeMeta, NativeMetaBuilder},
  object::Class,
  package::Package,
  signature::{Arity, ParameterBuilder, ParameterKind},
  val,
  value::Value,
  CallResult, LyResult,
};
use laythe_env::managed::Trace;
use std::io::Write;

pub const OBJECT_CLASS_NAME: &str = OBJECT;

const OBJECT_EQUALS: NativeMetaBuilder = NativeMetaBuilder::method("equals", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("other", ParameterKind::Any)]);

const OBJECT_CLASS: NativeMetaBuilder = NativeMetaBuilder::method("cls", Arity::Fixed(0));

const OBJECT_STR: NativeMetaBuilder = NativeMetaBuilder::method("str", Arity::Fixed(0));

pub fn declare_object_class(hooks: &GcHooks, self_module: &mut Module) -> LyResult<()> {
  let name = hooks.manage_str(OBJECT_CLASS_NAME);
  let class = hooks.manage(Class::bare(name));

  export_and_insert(hooks, self_module, name, val!(class))
}

pub fn define_object_class(hooks: &GcHooks, module: &Module, _: &Package) -> LyResult<()> {
  let mut class = load_class_from_module(hooks, module, OBJECT_CLASS_NAME)?;

  class.add_method(
    &hooks,
    hooks.manage_str(OBJECT_EQUALS.name),
    val!(to_dyn_native(hooks, ObjectEquals::from(hooks))),
  );

  class.add_method(
    &hooks,
    hooks.manage_str(OBJECT_CLASS.name),
    val!(to_dyn_native(hooks, ObjectClass::from(hooks))),
  );

  class.add_method(
    &hooks,
    hooks.manage_str(OBJECT_STR.name),
    val!(to_dyn_native(hooks, ObjectStr::from(hooks))),
  );

  Ok(())
}

native!(ObjectEquals, OBJECT_EQUALS);

impl Native for ObjectEquals {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> CallResult {
    Ok(val!(this.unwrap() == args[0]))
  }
}

native!(ObjectClass, OBJECT_CLASS);

impl Native for ObjectClass {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> CallResult {
    Ok(hooks.get_class(this.unwrap()))
  }
}

native!(ObjectStr, OBJECT_STR);

impl Native for ObjectStr {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> CallResult {
    Ok(val!(hooks.manage_str(&format!("{}", this.unwrap()))))
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod equals {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let object_equals = ObjectEquals::from(&hooks);

      assert_eq!(object_equals.meta().name, "equals");
      assert_eq!(object_equals.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        object_equals.meta().signature.parameters[0].kind,
        ParameterKind::Any
      )
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let bool_str = ObjectEquals::from(&hooks);

      let ten_1 = val!(10.0);
      let b_false = val!(false);
      let ten_2 = val!(10.0);

      let result1 = bool_str.call(&mut hooks, Some(ten_1), &[ten_2]);
      let result2 = bool_str.call(&mut hooks, Some(ten_2), &[b_false]);

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

  mod class {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let object_equals = ObjectStr::from(&hooks);

      assert_eq!(object_equals.meta().name, "str");
      assert_eq!(object_equals.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let bool_str = ObjectStr::from(&hooks);

      let ten = val!(10.0);

      let result = bool_str.call(&mut hooks, Some(ten), &[]);

      match result {
        Ok(r) => assert_eq!(r.to_str().as_str(), "10"),
        Err(_) => assert!(false),
      }
    }
  }

  mod str {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let object_equals = ObjectStr::from(&hooks);

      assert_eq!(object_equals.meta().name, "str");
      assert_eq!(object_equals.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let bool_str = ObjectStr::from(&hooks);

      let ten = val!(10.0);

      let result = bool_str.call(&mut hooks, Some(ten), &[]);

      match result {
        Ok(r) => assert_eq!(r.to_str().as_str(), "10"),
        Err(_) => assert!(false),
      }
    }
  }
}
