use spacelox_core::{
  arity::ArityKind,
  hooks::Hooks,
  io::StdIo,
  managed::{Managed, Trace},
  native::{NativeFun, NativeMeta},
  value::Value,
  CallResult,
};

const ASSERT_META: NativeMeta = NativeMeta::new("assert", ArityKind::Fixed(1));

#[derive(Clone, Debug, Trace)]
/// A native method to assert that for a boolean true value
pub struct Assert {
  /// the assert meta data
  meta: &'static NativeMeta,

  /// reference to 'str'
  method_str: Managed<String>,
}

impl Assert {
  /// Construct a new instance of the native assert function
  pub fn new(method_str: Managed<String>) -> Self {
    Self {
      meta: &ASSERT_META,
      method_str,
    }
  }
}

impl NativeFun for Assert {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> CallResult {
    match args[0] {
      Value::Bool(b) => match b {
        true => Ok(Value::Nil),
        false => hooks.error(String::from(
          "Assertion failed expected true received false",
        )),
      },
      _ => {
        let result = hooks.call_method_by_name(args[0], self.method_str, &[]);

        if let Ok(ok1) = result {
          if let Value::String(str1) = ok1 {
            return hooks.error(format!("Assertion failed expected true received {}.", str1));
          }
        }

        return hooks.error(format!(
          "Assertion failed expected true received {:?}.",
          args[0]
        ));
      }
    }
  }
}

const ASSERTEQ_META: NativeMeta = NativeMeta::new("assertEq", ArityKind::Fixed(2));

#[derive(Clone, Debug, Trace)]
pub struct AssertEq {
  meta: &'static NativeMeta,
  method_str: Managed<String>,
}

impl AssertEq {
  /// Construct a new instance of the native assertEq function
  pub fn new(method_str: Managed<String>) -> Self {
    Self {
      meta: &ASSERTEQ_META,
      method_str,
    }
  }
}

impl NativeFun for AssertEq {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> CallResult {
    if args[0] == args[1] {
      return Ok(Value::Nil);
    }

    let result1 = hooks.call_method_by_name(args[0], self.method_str, &[]);
    let result2 = hooks.call_method_by_name(args[1], self.method_str, &[]);

    if let (Ok(ok1), Ok(ok2)) = (result1, result2) {
      if let (Value::String(str1), Value::String(str2)) = (ok1, ok2) {
        return hooks.error(format!(
          "Assertion failed {} and {} are not equal.",
          str1, str2
        ));
      }
    }

    return hooks.error(format!(
      "Assertion failed {:?} and {:?} are not equal.",
      args[0], args[1]
    ));
  }
}

const ASSERTNE_META: NativeMeta = NativeMeta::new("assertNe", ArityKind::Fixed(2));

#[derive(Clone, Debug, Trace)]
pub struct AssertNe {
  meta: &'static NativeMeta,
  method_str: Managed<String>,
}

impl AssertNe {
  /// Construct a new instance of the native assertNe function
  pub fn new(method_str: Managed<String>) -> Self {
    Self {
      meta: &ASSERTNE_META,
      method_str,
    }
  }
}

impl NativeFun for AssertNe {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> CallResult {
    if args[0] != args[1] {
      return Ok(Value::Nil);
    }

    let result1 = hooks.call_method_by_name(args[0], self.method_str, &[]);
    let result2 = hooks.call_method_by_name(args[1], self.method_str, &[]);

    if let (Ok(ok1), Ok(ok2)) = (result1, result2) {
      if let (Value::String(str1), Value::String(str2)) = (ok1, ok2) {
        return hooks.error(format!("Assertion failed {} and {} are equal.", str1, str2));
      }
    }

    return hooks.error(format!(
      "Assertion failed {:?} and {:?} are equal.",
      args[0], args[1]
    ));
  }
}

mod test {
  #[cfg(test)]
  use super::*;
  #[cfg(test)]
  use crate::support::test_native_dependencies;

  #[cfg(test)]
  mod assert {
    use super::*;
    use crate::support::{test_native_dependencies, TestContext};
    use spacelox_core::memory::NO_GC;

    #[test]
    fn new() {
      let gc = test_native_dependencies();
      let assert = Assert::new(gc.manage_str(String::from("str"), &NO_GC));

      assert_eq!(assert.meta.name, "assert");
      assert_eq!(assert.meta.arity, ArityKind::Fixed(1));
    }

    #[test]
    fn call() {
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let assert = Assert::new(hooks.manage_str(String::from("str")));
      let values = &[Value::Bool(true)];

      let result = match assert.call(&mut hooks, values) {
        Ok(res) => res,
        Err(_) => panic!(),
      };

      assert_eq!(result, Value::Nil);
    }
  }

  #[cfg(test)]
  mod assert_eq {
    use super::*;
    use crate::support::TestContext;
    use spacelox_core::memory::NO_GC;

    #[test]
    fn new() {
      let gc = test_native_dependencies();
      let assert_eq = AssertEq::new(gc.manage_str(String::from("str"), &NO_GC));

      assert_eq!(assert_eq.meta.name, "assertEq");
      assert_eq!(assert_eq.meta.arity, ArityKind::Fixed(2));
    }

    #[test]
    fn call() {
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);
      let assert_eq = AssertEq::new(hooks.manage_str(String::from("str")));

      let values = &[Value::Number(10.5), Value::Number(10.5)];

      let result = match assert_eq.call(&mut hooks, values) {
        Ok(res) => res,
        Err(_) => panic!(),
      };

      assert_eq!(result, Value::Nil);
    }
  }

  #[cfg(test)]
  mod assert_ne {
    use super::*;
    use crate::support::TestContext;
    use spacelox_core::memory::NO_GC;

    #[test]
    fn new() {
      let gc = test_native_dependencies();
      let assert_eq = AssertNe::new(gc.manage_str(String::from("str"), &NO_GC));

      assert_eq!(assert_eq.meta.name, "assertNe");
      assert_eq!(assert_eq.meta.arity, ArityKind::Fixed(2));
    }

    #[test]
    fn call() {
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);
      let assert_eq = AssertNe::new(hooks.manage_str(String::from("str")));

      let values = &[Value::Number(10.5), Value::Nil];

      let result = match assert_eq.call(&mut hooks, values) {
        Ok(res) => res,
        Err(_) => panic!(),
      };

      assert_eq!(result, Value::Nil);
    }
  }
}
