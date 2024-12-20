use crate::{
  create_error,
  support::{default_error_inheritance, export_and_insert, export_and_insert_native},
  StdResult,
};
use laythe_core::{
  hooks::{GcHooks, Hooks}, if_let_obj, managed::Trace, module::{Module, Package}, object::{LyNative, LyStr, Native, NativeMetaBuilder, ObjectKind}, signature::{Arity, ParameterBuilder, ParameterKind}, to_obj_kind, val, value::{Value, VALUE_NIL}, Call, LyError, ObjRef, Ref
};
use std::io::Write;

pub const ASSERT_ERROR_NAME: &str = "AssertError";

pub(crate) fn add_assert_funs(
  hooks: &GcHooks,
  module: Ref<Module>,
  package: Ref<Package>,
) -> StdResult<()> {
  let error = default_error_inheritance(hooks, package, ASSERT_ERROR_NAME)?;

  let error_val = val!(error);
  let str_name = hooks.manage_str("str");
  export_and_insert(hooks, module, ASSERT_ERROR_NAME, error_val)?;

  export_and_insert_native(hooks, module, Assert::native(hooks, str_name, error_val))?;
  export_and_insert_native(hooks, module, AssertEq::native(hooks, str_name, error_val))?;
  export_and_insert_native(hooks, module, AssertNe::native(hooks, str_name, error_val))
}

const ASSERT_META: NativeMetaBuilder = NativeMetaBuilder::fun("assert", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("value", ParameterKind::Bool)]);

const ASSERTEQ_META: NativeMetaBuilder = NativeMetaBuilder::fun("assertEq", Arity::Fixed(2))
  .with_params(&[
    ParameterBuilder::new("actual", ParameterKind::Object),
    ParameterBuilder::new("expected", ParameterKind::Object),
  ])
  .with_stack();

const ASSERTNE_META: NativeMetaBuilder = NativeMetaBuilder::fun("assertNe", Arity::Fixed(2))
  .with_params(&[
    ParameterBuilder::new("actual", ParameterKind::Object),
    ParameterBuilder::new("unexpected", ParameterKind::Object),
  ])
  .with_stack();

fn to_str(hooks: &mut Hooks, value: Value) -> LyStr {
  hooks
    .get_method(value, hooks.manage_str("str"))
    .map(|method| hooks.call_method(value, method, &[]))
    .map(|string| {
      if let Call::Ok(ok) = string {
        if_let_obj!(ObjectKind::String(string) = (ok) {
          return string;
        });
      }

      hooks.manage_str(format!("{string:?}"))
    })
    .expect("No str method")
}

#[derive(Debug)]
/// A native method to assert that for a boolean true value
pub struct Assert {
  /// reference to 'str'
  method_str: LyStr,
  error: Value,
}

impl Assert {
  /// Construct a new instance of the native assert function
  pub fn native(hooks: &GcHooks, method_str: LyStr, error: Value) -> ObjRef<Native> {
    debug_assert!(error.is_obj_kind(laythe_core::object::ObjectKind::Class));
    let native = Box::new(Self { method_str, error }) as Box<dyn LyNative>;

    hooks.manage_obj(Native::new(ASSERT_META.build(hooks), native))
  }
}

impl LyNative for Assert {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    if args[0].to_bool() {
      Call::Ok(VALUE_NIL)
    } else {
      create_error!(self.error, hooks, "Expected assertion to return true.")
    }
  }
}

impl Trace for Assert {
  fn trace(&self) {
    self.method_str.trace();
  }

  fn trace_debug(&self, stdout: &mut dyn Write) {
    self.method_str.trace_debug(stdout);
  }
}

#[derive(Debug)]
pub struct AssertEq {
  method_str: LyStr,
  error: Value,
}

impl AssertEq {
  /// Construct a new instance of the native assertEq function
  pub fn native(hooks: &GcHooks, method_str: LyStr, error: Value) -> ObjRef<Native> {
    debug_assert!(error.is_obj_kind(ObjectKind::Class));
    let native = Box::new(Self { method_str, error }) as Box<dyn LyNative>;

    hooks.manage_obj(Native::new(ASSERTEQ_META.build(hooks), native))
  }
}

impl LyNative for AssertEq {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    if args[0] == args[1] {
      return Call::Ok(VALUE_NIL);
    }

    let arg0 = to_str(hooks, args[0]);
    let arg1 = to_str(hooks, args[1]);

    create_error!(
      self.error,
      hooks,
      format!("Expected {arg0:?} to equal {arg1:?}.")
    )
  }
}

impl Trace for AssertEq {
  fn trace(&self) {
    self.method_str.trace();
    self.error.trace();
  }

  fn trace_debug(&self, stdout: &mut dyn Write) {
    self.method_str.trace_debug(stdout);
    self.error.trace_debug(stdout);
  }
}

#[derive(Debug)]
pub struct AssertNe {
  method_str: LyStr,
  error: Value,
}

impl AssertNe {
  /// Construct a new instance of the native assertNe function
  pub fn native(hooks: &GcHooks, method_str: LyStr, error: Value) -> ObjRef<Native> {
    debug_assert!(error.is_obj_kind(ObjectKind::Class));
    let native = Box::new(Self { method_str, error }) as Box<dyn LyNative>;

    hooks.manage_obj(Native::new(ASSERTNE_META.build(hooks), native))
  }
}

impl LyNative for AssertNe {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    if args[0] != args[1] {
      return Call::Ok(VALUE_NIL);
    }

    let arg0 = to_str(hooks, args[0]);
    let arg1 = to_str(hooks, args[1]);

    create_error!(
      self.error,
      hooks,
      format!("Expected {arg0} not to equal {arg1}.")
    )
  }
}

impl Trace for AssertNe {
  fn trace(&self) {
    self.method_str.trace();
    self.error.trace();
  }

  fn trace_debug(&self, stdout: &mut dyn Write) {
    self.method_str.trace_debug(stdout);
    self.error.trace_debug(stdout);
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use laythe_core::hooks::NoContext;

  #[cfg(test)]
  mod assert {
    use crate::support::test_error_class;

    use super::*;

    #[test]
    fn call() {
      let mut context = NoContext::default();
      let mut hooks = Hooks::new(&mut context);

      let error = val!(test_error_class(&hooks.as_gc()));
      let assert = Assert::native(&hooks.as_gc(), hooks.manage_str("str"), error);
      let values = &[val!(true)];

      let result = match assert.call(&mut hooks, values) {
        Call::Ok(res) => res,
        _ => panic!(),
      };

      assert_eq!(result, VALUE_NIL);
    }
  }

  #[cfg(test)]
  mod assert_eq {
    use crate::support::test_error_class;

    use super::*;

    #[test]
    fn call() {
      let mut context = NoContext::default();
      let mut hooks = Hooks::new(&mut context);

      let error = val!(test_error_class(&hooks.as_gc()));
      let assert_eq = AssertEq::native(&hooks.as_gc(), hooks.manage_str("str"), error);

      let values = &[val!(10.5), val!(10.5)];

      let result = match assert_eq.call(&mut hooks, values) {
        Call::Ok(res) => res,
        _ => panic!(),
      };

      assert_eq!(result, VALUE_NIL);
    }
  }

  #[cfg(test)]
  mod assert_ne {
    use crate::support::test_error_class;

    use super::*;

    #[test]
    fn call() {
      let mut context = NoContext::default();
      let mut hooks = Hooks::new(&mut context);

      let error = val!(test_error_class(&hooks.as_gc()));
      let assert_ne = AssertNe::native(&hooks.as_gc(), hooks.manage_str("str"), error);

      let values = &[val!(10.5), VALUE_NIL];

      let result = match assert_ne.call(&mut hooks, values) {
        Call::Ok(res) => res,
        _ => panic!(),
      };

      assert_eq!(result, VALUE_NIL);
    }
  }
}
