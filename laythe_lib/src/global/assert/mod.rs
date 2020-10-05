use crate::support::export_and_insert;
use laythe_core::{
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{MetaData, Native, NativeMeta, NativeMetaBuilder},
  package::Package,
  signature::{Arity, ParameterBuilder, ParameterKind},
  val,
  value::{Value, VALUE_NIL},
  CallResult, LyResult,
};
use laythe_env::managed::{Managed, Trace};
use smol_str::SmolStr;
use std::io::Write;

pub(crate) fn add_assert_funs(
  hooks: &GcHooks,
  mut module: Managed<Module>,
  _package: Managed<Package>,
) -> LyResult<()> {
  declare_assert_funs(hooks, &mut module)
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

pub fn declare_assert_funs(hooks: &GcHooks, self_module: &mut Module) -> LyResult<()> {
  let str_name = hooks.manage_str("str");

  export_and_insert(
    hooks,
    self_module,
    hooks.manage_str(ASSERT_META.name),
    val!(
      hooks.manage(Box::new(Assert::new(ASSERT_META.to_meta(hooks), str_name)) as Box<dyn Native>)
    ),
  )?;

  export_and_insert(
    hooks,
    self_module,
    hooks.manage_str(ASSERTEQ_META.name),
    val!(hooks
      .manage(Box::new(AssertEq::new(ASSERTEQ_META.to_meta(hooks), str_name)) as Box<dyn Native>)),
  )?;

  export_and_insert(
    hooks,
    self_module,
    hooks.manage_str(ASSERTNE_META.name),
    val!(hooks
      .manage(Box::new(AssertNe::new(ASSERTNE_META.to_meta(hooks), str_name)) as Box<dyn Native>)),
  )
}

fn to_str(hooks: &mut Hooks, value: Value) -> Managed<SmolStr> {
  hooks
    .get_method(value, hooks.manage_str("str"))
    .map(|method| hooks.call_method(value, method, &[]))
    .map(|string| {
      if let Ok(ok) = string {
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
  method_str: Managed<SmolStr>,
  meta: NativeMeta,
}

impl Assert {
  /// Construct a new instance of the native assert function
  pub fn new(meta: NativeMeta, method_str: Managed<SmolStr>) -> Self {
    Self { meta, method_str }
  }
}

impl MetaData for Assert {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }
}

impl Native for Assert {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> CallResult {
    if args[0].to_bool() {
      Ok(VALUE_NIL)
    } else {
      hooks.error(String::from(
        "Assertion failed expected true received false",
      ))
    }
  }
}

impl Trace for Assert {
  fn trace(&self) -> bool {
    self.meta.trace();
    self.method_str.trace()
  }

  fn trace_debug(&self, stdout: &mut dyn Write) -> bool {
    self.meta.trace_debug(stdout);
    self.method_str.trace_debug(stdout)
  }
}

#[derive(Debug, Trace)]
pub struct AssertEq {
  meta: NativeMeta,
  method_str: Managed<SmolStr>,
}

impl AssertEq {
  /// Construct a new instance of the native assertEq function
  pub fn new(meta: NativeMeta, method_str: Managed<SmolStr>) -> Self {
    Self { meta, method_str }
  }
}

impl MetaData for AssertEq {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }
}

impl Native for AssertEq {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> CallResult {
    if args[0] == args[1] {
      return Ok(VALUE_NIL);
    }

    let arg0 = to_str(hooks, args[0]);
    let arg1 = to_str(hooks, args[1]);

    hooks.error(format!(
      "Assertion failed {} and {} are not equal.",
      arg0, arg1
    ))
  }
}

#[derive(Debug, Trace)]
pub struct AssertNe {
  meta: NativeMeta,
  method_str: Managed<SmolStr>,
}

impl AssertNe {
  /// Construct a new instance of the native assertNe function
  pub fn new(meta: NativeMeta, method_str: Managed<SmolStr>) -> Self {
    Self { meta, method_str }
  }
}

impl MetaData for AssertNe {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }
}

impl Native for AssertNe {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> CallResult {
    if args[0] != args[1] {
      return Ok(VALUE_NIL);
    }

    let arg0 = to_str(hooks, args[0]);
    let arg1 = to_str(hooks, args[1]);

    hooks.error(format!("Assertion failed {} and {} are equal.", arg0, arg1,))
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use laythe_core::hooks::support::TestContext;

  #[cfg(test)]
  mod assert {
    use super::*;

    #[test]
    fn new() {
      let mut context = TestContext::default();
      let hooks = Hooks::new(&mut context);
      let assert = Assert::new(
        ASSERT_META.to_meta(&hooks.as_gc()),
        hooks.manage_str("str".to_string()),
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
      let mut context = TestContext::default();
      let mut hooks = Hooks::new(&mut context);

      let assert = Assert::new(
        ASSERT_META.to_meta(&hooks.as_gc()),
        hooks.manage_str("str".to_string()),
      );
      let values = &[val!(true)];

      let result = match assert.call(&mut hooks, None, values) {
        Ok(res) => res,
        Err(_) => panic!(),
      };

      assert_eq!(result, VALUE_NIL);
    }
  }

  #[cfg(test)]
  mod assert_eq {
    use super::*;

    #[test]
    fn new() {
      let mut context = TestContext::default();
      let hooks = Hooks::new(&mut context);

      let assert_eq = AssertEq::new(
        ASSERTEQ_META.to_meta(&hooks.as_gc()),
        hooks.manage_str("str".to_string()),
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
      let mut context = TestContext::default();
      let mut hooks = Hooks::new(&mut context);

      let assert_eq = AssertEq::new(
        ASSERTEQ_META.to_meta(&hooks.as_gc()),
        hooks.manage_str("str".to_string()),
      );

      let values = &[val!(10.5), val!(10.5)];

      let result = match assert_eq.call(&mut hooks, None, values) {
        Ok(res) => res,
        Err(_) => panic!(),
      };

      assert_eq!(result, VALUE_NIL);
    }
  }

  #[cfg(test)]
  mod assert_ne {
    use super::*;

    #[test]
    fn new() {
      let mut context = TestContext::default();
      let hooks = Hooks::new(&mut context);

      let assert_eq = AssertNe::new(
        ASSERTNE_META.to_meta(&hooks.as_gc()),
        hooks.manage_str("str".to_string()),
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
      let mut context = TestContext::default();
      let mut hooks = Hooks::new(&mut context);

      let assert_ne = AssertNe::new(
        ASSERTNE_META.to_meta(&hooks.as_gc()),
        hooks.manage_str("str".to_string()),
      );

      let values = &[val!(10.5), VALUE_NIL];

      let result = match assert_ne.call(&mut hooks, None, values) {
        Ok(res) => res,
        Err(_) => panic!(),
      };

      assert_eq!(result, VALUE_NIL);
    }
  }
}
