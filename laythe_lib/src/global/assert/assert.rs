use crate::support::export_and_insert;
use laythe_core::{
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{NativeFun, NativeMeta},
  signature::{Arity, Parameter, ParameterKind},
  value::{Value, VALUE_NIL},
  CallResult, LyResult,
};
use laythe_env::{
  managed::{Managed, Trace},
  stdio::Stdio,
};

const ASSERT_META: NativeMeta = NativeMeta::new(
  "assert",
  Arity::Fixed(1),
  &[Parameter::new("value", ParameterKind::Bool)],
);
const ASSERTEQ_META: NativeMeta = NativeMeta::new(
  "assertEq",
  Arity::Fixed(2),
  &[
    Parameter::new("actual", ParameterKind::Any),
    Parameter::new("expected", ParameterKind::Any),
  ],
);
const ASSERTNE_META: NativeMeta = NativeMeta::new(
  "assertNe",
  Arity::Fixed(2),
  &[
    Parameter::new("actual", ParameterKind::Any),
    Parameter::new("unexpected", ParameterKind::Any),
  ],
);

pub fn declare_assert_funs(hooks: &GcHooks, self_module: &mut Module) -> LyResult<()> {
  let str_name = hooks.manage_str("str".to_string());

  export_and_insert(
    hooks,
    self_module,
    hooks.manage_str(ASSERT_META.name.to_string()),
    Value::from(hooks.manage(Box::new(Assert::new(str_name)) as Box<dyn NativeFun>)),
  )?;

  export_and_insert(
    hooks,
    self_module,
    hooks.manage_str(ASSERTEQ_META.name.to_string()),
    Value::from(hooks.manage(Box::new(AssertEq::new(str_name)) as Box<dyn NativeFun>)),
  )?;

  export_and_insert(
    hooks,
    self_module,
    hooks.manage_str(ASSERTNE_META.name.to_string()),
    Value::from(hooks.manage(Box::new(AssertNe::new(str_name)) as Box<dyn NativeFun>)),
  )
}

fn to_str(hooks: &mut Hooks, value: Value) -> Managed<String> {
  let result = hooks.call_method_by_name(value, hooks.manage_str("str".to_string()), &[]);

  if let Ok(ok) = result {
    if ok.is_str() {
      return ok.to_str();
    }
  }

  hooks.manage_str(format!("{:?}", result))
}

#[derive(Clone, Debug, Trace)]
/// A native method to assert that for a boolean true value
pub struct Assert {
  /// reference to 'str'
  method_str: Managed<String>,
}

impl Assert {
  /// Construct a new instance of the native assert function
  pub fn new(method_str: Managed<String>) -> Self {
    Self { method_str }
  }
}

impl NativeFun for Assert {
  fn meta(&self) -> &NativeMeta {
    &ASSERT_META
  }

  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> CallResult {
    if args[0].to_bool() {
      Ok(VALUE_NIL)
    } else {
      hooks.error(String::from(
        "Assertion failed expected true received false",
      ))
    }
  }
}

#[derive(Clone, Debug, Trace)]
pub struct AssertEq {
  method_str: Managed<String>,
}

impl AssertEq {
  /// Construct a new instance of the native assertEq function
  pub fn new(method_str: Managed<String>) -> Self {
    Self { method_str }
  }
}

impl NativeFun for AssertEq {
  fn meta(&self) -> &NativeMeta {
    &ASSERTEQ_META
  }

  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> CallResult {
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

#[derive(Clone, Debug, Trace)]
pub struct AssertNe {
  method_str: Managed<String>,
}

impl AssertNe {
  /// Construct a new instance of the native assertNe function
  pub fn new(method_str: Managed<String>) -> Self {
    Self { method_str }
  }
}

impl NativeFun for AssertNe {
  fn meta(&self) -> &NativeMeta {
    &ASSERTNE_META
  }

  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> CallResult {
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
  use crate::support::{test_native_dependencies, MockedContext};
  use laythe_env::memory::NO_GC;

  #[cfg(test)]
  mod assert {
    use super::*;

    #[test]
    fn new() {
      let gc = test_native_dependencies();
      let assert = Assert::new(gc.manage_str("str".to_string(), &NO_GC));

      assert_eq!(assert.meta().name, "assert");
      assert_eq!(assert.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        assert.meta().signature.parameters[0].kind,
        ParameterKind::Bool
      );
    }

    #[test]
    fn call() {
      let gc = test_native_dependencies();
      let mut context = MockedContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let assert = Assert::new(hooks.manage_str("str".to_string()));
      let values = &[Value::from(true)];

      let result = match assert.call(&mut hooks, values) {
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
      let gc = test_native_dependencies();
      let assert_eq = AssertEq::new(gc.manage_str("str".to_string(), &NO_GC));

      assert_eq!(assert_eq.meta().name, "assertEq");
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
      let gc = test_native_dependencies();
      let mut context = MockedContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);
      let assert_eq = AssertEq::new(hooks.manage_str("str".to_string()));

      let values = &[Value::from(10.5), Value::from(10.5)];

      let result = match assert_eq.call(&mut hooks, values) {
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
      let gc = test_native_dependencies();
      let assert_eq = AssertNe::new(gc.manage_str("str".to_string(), &NO_GC));

      assert_eq!(assert_eq.meta().name, "assertNe");
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
      let gc = test_native_dependencies();
      let mut context = MockedContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);
      let assert_eq = AssertNe::new(hooks.manage_str("str".to_string()));

      let values = &[Value::from(10.5), VALUE_NIL];

      let result = match assert_eq.call(&mut hooks, values) {
        Ok(res) => res,
        Err(_) => panic!(),
      };

      assert_eq!(result, VALUE_NIL);
    }
  }
}
