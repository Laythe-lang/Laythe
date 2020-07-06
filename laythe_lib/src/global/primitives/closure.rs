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

pub const CLOSURE_CLASS_NAME: &'static str = "Fun";

const CLOSURE_NAME: NativeMeta = NativeMeta::new("name", Arity::Fixed(0), &[]);
const CLOSURE_SIZE: NativeMeta = NativeMeta::new("size", Arity::Fixed(0), &[]);
const CLOSURE_CALL: NativeMeta = NativeMeta::new(
  "call",
  Arity::Variadic(0),
  &[Parameter::new("args", ParameterKind::Any)],
);

pub fn declare_closure_class(
  hooks: &GcHooks,
  module: &mut Module,
  package: &Package,
) -> LyResult<()> {
  let class = default_class_inheritance(hooks, package, CLOSURE_CLASS_NAME)?;
  export_and_insert(hooks, module, class.name, Value::from(class))
}

pub fn define_closure_class(hooks: &GcHooks, module: &Module, _: &Package) -> LyResult<()> {
  let mut class = load_class_from_module(hooks, module, CLOSURE_CLASS_NAME)?;

  class.add_method(
    hooks,
    hooks.manage_str(String::from(CLOSURE_NAME.name)),
    Value::from(to_dyn_method(hooks, ClosureName())),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(CLOSURE_SIZE.name)),
    Value::from(to_dyn_method(hooks, ClosureSize())),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(CLOSURE_CALL.name)),
    Value::from(to_dyn_method(hooks, ClosureCall())),
  );

  Ok(())
}

#[derive(Clone, Debug, Trace)]
struct ClosureName();

impl NativeMethod for ClosureName {
  fn meta(&self) -> &NativeMeta {
    &CLOSURE_NAME
  }

  fn call(&self, _hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    Ok(Value::from(this.to_closure().fun.name))
  }
}

#[derive(Clone, Debug, Trace)]
struct ClosureSize();

impl NativeMethod for ClosureSize {
  fn meta(&self) -> &NativeMeta {
    &CLOSURE_SIZE
  }

  fn call(&self, _hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    let req = match this.to_closure().fun.arity {
      Arity::Default(req, _) => req,
      Arity::Fixed(req) => req,
      Arity::Variadic(req) => req,
    };

    Ok(Value::from(req as f64))
  }
}

#[derive(Clone, Debug, Trace)]
struct ClosureCall();

impl NativeMethod for ClosureCall {
  fn meta(&self) -> &NativeMeta {
    &CLOSURE_CALL
  }

  fn call(&self, hooks: &mut Hooks, this: Value, args: &[Value]) -> CallResult {
    hooks.call(this, args)
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod name {
    use super::*;
    use crate::support::{fun_from_hooks, test_native_dependencies, MockedContext};
    use laythe_core::object::Closure;

    #[test]
    fn new() {
      let closure_name = ClosureName();

      assert_eq!(closure_name.meta().name, "name");
      assert_eq!(closure_name.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let closure_name = ClosureName();
      let gc = test_native_dependencies();
      let mut context = MockedContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let fun = fun_from_hooks(&hooks.to_gc(), "example".to_string(), "module");
      let closure = hooks.manage(Closure::new(fun));

      let result1 = closure_name.call(&mut hooks, Value::from(closure), &[]);

      match result1 {
        Ok(r) => assert_eq!(&*r.to_str(), "example"),
        Err(_) => assert!(false),
      }
    }
  }

  mod size {
    use super::*;
    use crate::support::{fun_from_hooks, test_native_dependencies, MockedContext};
    use laythe_core::object::Closure;

    #[test]
    fn new() {
      let closure_name = ClosureSize();

      assert_eq!(closure_name.meta().name, "size");
      assert_eq!(closure_name.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let closure_name = ClosureSize();
      let gc = test_native_dependencies();
      let mut context = MockedContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let mut fun = fun_from_hooks(&hooks.to_gc(), "example".to_string(), "module");
      fun.arity = Arity::Fixed(4);

      let closure = hooks.manage(Closure::new(fun));

      let result = closure_name.call(&mut hooks, Value::from(closure), &[]);
      match result {
        Ok(r) => assert_eq!(r.to_num(), 4.0),
        Err(_) => assert!(false),
      }

      fun.arity = Arity::Default(2, 2);
      let result = closure_name.call(&mut hooks, Value::from(closure), &[]);
      match result {
        Ok(r) => assert_eq!(r.to_num(), 2.0),
        Err(_) => assert!(false),
      }

      fun.arity = Arity::Variadic(5);
      let result = closure_name.call(&mut hooks, Value::from(closure), &[]);
      match result {
        Ok(r) => assert_eq!(r.to_num(), 5.0),
        Err(_) => assert!(false),
      }
    }
  }

  mod call {
    use super::*;
    use crate::support::{fun_from_hooks, test_native_dependencies, MockedContext};
    use laythe_core::object::Closure;

    #[test]
    fn new() {
      let closure_call = ClosureCall();

      assert_eq!(closure_call.meta().name, "call");
      assert_eq!(closure_call.meta().signature.arity, Arity::Variadic(0));
      assert_eq!(
        closure_call.meta().signature.parameters[0].kind,
        ParameterKind::Any
      );
    }

    #[test]
    fn call() {
      let closure_call = ClosureCall();
      let gc = test_native_dependencies();
      let mut context = MockedContext::new(&gc, &[Value::from(4.3)]);
      let mut hooks = Hooks::new(&mut context);

      let mut fun = fun_from_hooks(&hooks.to_gc(), "example".to_string(), "module");
      fun.arity = Arity::Fixed(1);

      let closure = hooks.manage(Closure::new(fun));

      let args = &[Value::from(hooks.manage_str("input".to_string()))];
      let result1 = closure_call.call(&mut hooks, Value::from(closure), args);

      match result1 {
        Ok(r) => assert_eq!(r.to_num(), 4.3),
        Err(_) => assert!(false),
      }
    }
  }
}
