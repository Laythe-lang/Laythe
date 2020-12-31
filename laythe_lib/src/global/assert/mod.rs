use crate::{
  create_error,
  support::{default_error_inheritance, export_and_insert, load_class_from_module, to_dyn_native},
  InitResult,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{MetaData, Native, NativeMeta, NativeMetaBuilder},
  package::Package,
  signature::{Arity, ParameterBuilder, ParameterKind},
  val,
  value::{Value, VALUE_NIL},
  Call,
};
use laythe_env::managed::{Gc, Trace};
use smol_str::SmolStr;
use std::io::Write;

pub const ASSERT_ERROR_NAME: &str = "AssertError";

pub(crate) fn add_assert_funs(
  hooks: &GcHooks,
  module: &mut Module,
  package: &Package,
) -> InitResult<()> {
  let error = default_error_inheritance(hooks, package, ASSERT_ERROR_NAME)?;
  export_and_insert(hooks, module, error.name, val!(error))?;

  declare_assert_funs(hooks, module)
}

const ASSERT_META: NativeMetaBuilder = NativeMetaBuilder::fun("assert", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("value", ParameterKind::Bool)]);

const ASSERTEQ_META: NativeMetaBuilder = NativeMetaBuilder::fun("assertEq", Arity::Fixed(2))
  .with_params(&[
    ParameterBuilder::new("actual", ParameterKind::Any),
    ParameterBuilder::new("expected", ParameterKind::Any),
  ]);

const ASSERTNE_META: NativeMetaBuilder = NativeMetaBuilder::fun("assertNe", Arity::Fixed(2))
  .with_params(&[
    ParameterBuilder::new("actual", ParameterKind::Any),
    ParameterBuilder::new("unexpected", ParameterKind::Any),
  ]);

pub fn declare_assert_funs(hooks: &GcHooks, module: &mut Module) -> InitResult<()> {
  let str_name = hooks.manage_str("str");
  let assert_error = val!(load_class_from_module(hooks, module, ASSERT_ERROR_NAME)?);

  export_and_insert(
    hooks,
    module,
    hooks.manage_str(ASSERT_META.name),
    val!(to_dyn_native(
      hooks,
      Assert::new(ASSERT_META.to_meta(hooks), str_name, assert_error)
    )),
  )?;

  export_and_insert(
    hooks,
    module,
    hooks.manage_str(ASSERTEQ_META.name),
    val!(to_dyn_native(
      hooks,
      AssertEq::new(ASSERTEQ_META.to_meta(hooks), str_name, assert_error)
    )),
  )?;

  export_and_insert(
    hooks,
    module,
    hooks.manage_str(ASSERTNE_META.name),
    val!(to_dyn_native(
      hooks,
      AssertNe::new(ASSERTNE_META.to_meta(hooks), str_name, assert_error)
    )),
  )
}

fn to_str(hooks: &mut Hooks, value: Value) -> Gc<SmolStr> {
  hooks
    .get_method(value, hooks.manage_str("str"))
    .map(|method| hooks.call_method(value, method, &[]))
    .map(|string| {
      if let Call::Ok(ok) = string {
        if ok.is_str() {
          return ok.to_str();
        }
      }

      hooks.manage_str(format!("{:?}", string))
    })
    .expect("No str method")
}

#[derive(Debug)]
/// A native method to assert that for a boolean true value
pub struct Assert {
  /// reference to 'str'
  method_str: Gc<SmolStr>,
  meta: NativeMeta,
  error: Value,
}

impl Assert {
  /// Construct a new instance of the native assert function
  pub fn new(meta: NativeMeta, method_str: Gc<SmolStr>, error: Value) -> Self {
    Self {
      meta,
      method_str,
      error,
    }
  }
}

impl MetaData for Assert {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }
}

impl Native for Assert {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> Call {
    if args[0].to_bool() {
      Call::Ok(VALUE_NIL)
    } else {
      create_error!(
        self.error,
        hooks,
        "Assertion failed expected true received false"
      )
    }
  }
}

impl Trace for Assert {
  fn trace(&self) {
    self.meta.trace();
    self.method_str.trace();
  }

  fn trace_debug(&self, stdout: &mut dyn Write) {
    self.meta.trace_debug(stdout);
    self.method_str.trace_debug(stdout);
  }
}

#[derive(Debug)]
pub struct AssertEq {
  meta: NativeMeta,
  method_str: Gc<SmolStr>,
  error: Value,
}

impl AssertEq {
  /// Construct a new instance of the native assertEq function
  pub fn new(meta: NativeMeta, method_str: Gc<SmolStr>, error: Value) -> Self {
    Self {
      meta,
      method_str,
      error,
    }
  }
}

impl MetaData for AssertEq {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }
}

impl Native for AssertEq {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> Call {
    if args[0] == args[1] {
      return Call::Ok(VALUE_NIL);
    }

    let arg0 = to_str(hooks, args[0]);
    let arg1 = to_str(hooks, args[1]);

    create_error!(
      self.error,
      hooks,
      format!("Assertion failed {} and {} are not equal.", arg0, arg1)
    )
  }
}

impl Trace for AssertEq {
  fn trace(&self) {
    self.meta.trace();
    self.method_str.trace();
    self.error.trace();
  }

  fn trace_debug(&self, stdout: &mut dyn Write) {
    self.meta.trace_debug(stdout);
    self.method_str.trace_debug(stdout);
    self.error.trace_debug(stdout);
  }
}

#[derive(Debug)]
pub struct AssertNe {
  meta: NativeMeta,
  method_str: Gc<SmolStr>,
  error: Value,
}

impl AssertNe {
  /// Construct a new instance of the native assertNe function
  pub fn new(meta: NativeMeta, method_str: Gc<SmolStr>, error: Value) -> Self {
    Self {
      meta,
      method_str,
      error,
    }
  }
}

impl MetaData for AssertNe {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }
}

impl Native for AssertNe {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> Call {
    if args[0] != args[1] {
      return Call::Ok(VALUE_NIL);
    }

    let arg0 = to_str(hooks, args[0]);
    let arg1 = to_str(hooks, args[1]);

    create_error!(
      self.error,
      hooks,
      format!("Assertion failed {} and {} are equal.", arg0, arg1)
    )
  }
}

impl Trace for AssertNe {
  fn trace(&self) {
    self.meta.trace();
    self.method_str.trace();
    self.error.trace();
  }

  fn trace_debug(&self, stdout: &mut dyn Write) {
    self.meta.trace_debug(stdout);
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
    fn new() {
      let mut context = NoContext::default();
      let hooks = Hooks::new(&mut context);

      let error = val!(test_error_class(&hooks.as_gc()));
      let assert = Assert::new(
        ASSERT_META.to_meta(&hooks.as_gc()),
        hooks.manage_str("str".to_string()),
        error,
      );

      assert_eq!(&*assert.meta().name, "assert");
      assert_eq!(assert.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        assert.meta().signature.parameters[0].kind,
        ParameterKind::Bool
      );
    }

    #[test]
    fn call() {
      let mut context = NoContext::default();
      let mut hooks = Hooks::new(&mut context);

      let error = val!(test_error_class(&hooks.as_gc()));
      let assert = Assert::new(
        ASSERT_META.to_meta(&hooks.as_gc()),
        hooks.manage_str("str".to_string()),
        error,
      );
      let values = &[val!(true)];

      let result = match assert.call(&mut hooks, None, values) {
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
    fn new() {
      let mut context = NoContext::default();
      let hooks = GcHooks::new(&mut context);

      let error = val!(test_error_class(&hooks));
      let assert_eq = AssertEq::new(
        ASSERTEQ_META.to_meta(&hooks),
        hooks.manage_str("str".to_string()),
        error,
      );

      assert_eq!(&*assert_eq.meta().name, "assertEq");
      assert_eq!(assert_eq.meta().signature.arity, Arity::Fixed(2));
      assert_eq!(
        assert_eq.meta().signature.parameters[0].kind,
        ParameterKind::Any
      );
      assert_eq!(
        assert_eq.meta().signature.parameters[1].kind,
        ParameterKind::Any
      );
    }

    #[test]
    fn call() {
      let mut context = NoContext::default();
      let mut hooks = Hooks::new(&mut context);

      let error = val!(test_error_class(&hooks.as_gc()));
      let assert_eq = AssertEq::new(
        ASSERTEQ_META.to_meta(&hooks.as_gc()),
        hooks.manage_str("str".to_string()),
        error,
      );

      let values = &[val!(10.5), val!(10.5)];

      let result = match assert_eq.call(&mut hooks, None, values) {
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
    fn new() {
      let mut context = NoContext::default();
      let hooks = GcHooks::new(&mut context);

      let error = val!(test_error_class(&hooks));
      let assert_eq = AssertNe::new(
        ASSERTNE_META.to_meta(&hooks),
        hooks.manage_str("str".to_string()),
        error,
      );

      assert_eq!(&*assert_eq.meta().name, "assertNe");
      assert_eq!(assert_eq.meta().signature.arity, Arity::Fixed(2));
      assert_eq!(
        assert_eq.meta().signature.parameters[0].kind,
        ParameterKind::Any
      );
      assert_eq!(
        assert_eq.meta().signature.parameters[1].kind,
        ParameterKind::Any
      );
    }

    #[test]
    fn call() {
      let mut context = NoContext::default();
      let mut hooks = Hooks::new(&mut context);

      let error = val!(test_error_class(&hooks.as_gc()));
      let assert_ne = AssertNe::new(
        ASSERTNE_META.to_meta(&hooks.as_gc()),
        hooks.manage_str("str".to_string()),
        error,
      );

      let values = &[val!(10.5), VALUE_NIL];

      let result = match assert_ne.call(&mut hooks, None, values) {
        Call::Ok(res) => res,
        _ => panic!(),
      };

      assert_eq!(result, VALUE_NIL);
    }
  }
}
