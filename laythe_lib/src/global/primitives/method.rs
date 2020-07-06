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
use laythe_env::{
  managed::{Managed, Trace},
  stdio::Stdio,
};

pub const METHOD_CLASS_NAME: &'static str = "Method";

const METHOD_NAME: NativeMeta = NativeMeta::new("name", Arity::Fixed(0), &[]);
const METHOD_CALL: NativeMeta = NativeMeta::new(
  "call",
  Arity::Variadic(0),
  &[Parameter::new("args", ParameterKind::Any)],
);

pub fn declare_method_class(
  hooks: &GcHooks,
  module: &mut Module,
  package: &Package,
) -> LyResult<()> {
  let method_class = default_class_inheritance(hooks, package, METHOD_CLASS_NAME)?;
  export_and_insert(hooks, module, method_class.name, Value::from(method_class))
}

pub fn define_method_class(hooks: &GcHooks, module: &Module, _: &Package) -> LyResult<()> {
  let mut class = load_class_from_module(hooks, module, METHOD_CLASS_NAME)?;

  class.add_method(
    hooks,
    hooks.manage_str(String::from(METHOD_NAME.name)),
    Value::from(to_dyn_method(
      hooks,
      MethodName::new(hooks.manage_str(String::from(METHOD_NAME.name))),
    )),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(METHOD_CALL.name)),
    Value::from(to_dyn_method(hooks, MethodCall())),
  );

  Ok(())
}

#[derive(Clone, Debug, Trace)]
struct MethodName {
  method_name: Managed<String>,
}

impl MethodName {
  fn new(method_name: Managed<String>) -> Self {
    Self { method_name }
  }
}

impl NativeMethod for MethodName {
  fn meta(&self) -> &NativeMeta {
    &METHOD_NAME
  }

  fn call(&self, hooks: &mut Hooks, this: Value, args: &[Value]) -> CallResult {
    hooks.call_method_by_name(this.to_method().method, self.method_name, args)
  }
}

#[derive(Clone, Debug, Trace)]
struct MethodCall();

impl NativeMethod for MethodCall {
  fn meta(&self) -> &NativeMeta {
    &METHOD_CALL
  }

  fn call(&self, hooks: &mut Hooks, this: Value, args: &[Value]) -> CallResult {
    let method = this.to_method();
    let callable = method.method;

    if callable.is_closure() {
      hooks.call(this, args)
    } else if callable.is_native_method() {
      hooks.call_method(method.receiver, method.method, args)
    } else {
      panic!("TODO")
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod name {
    use super::*;
    use crate::support::{fun_from_hooks, test_native_dependencies, MockedContext};
    use laythe_core::object::{Class, Closure, Instance, Method};
    use laythe_env::memory::NO_GC;

    #[test]
    fn new() {
      let gc = test_native_dependencies();
      let method_name = MethodName::new(gc.manage_str("name".to_string(), &NO_GC));

      assert_eq!(method_name.meta().name, "name");
      assert_eq!(method_name.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let gc = test_native_dependencies();
      let mut context = MockedContext::new(
        &gc,
        &[Value::from(gc.manage_str("example".to_string(), &NO_GC))],
      );
      let mut hooks = Hooks::new(&mut context);
      let method_name = MethodName::new(hooks.manage_str("name".to_string()));

      let fun = fun_from_hooks(&hooks.to_gc(), "example".to_string(), "module");
      let class = hooks.manage(Class::bare(hooks.manage_str("exampleClass".to_string())));
      let closure = hooks.manage(Closure::new(fun));
      let instance = hooks.manage(Instance::new(class));
      let method = hooks.manage(Method::new(Value::from(instance), Value::from(closure)));

      let result1 = method_name.call(&mut hooks, Value::from(method), &[]);

      match result1 {
        Ok(r) => assert_eq!(&*r.to_str(), "example"),
        Err(_) => assert!(false),
      }
    }
  }

  mod call {
    use super::*;
    use crate::support::{fun_from_hooks, test_native_dependencies, MockedContext};
    use laythe_core::object::{Class, Closure, Instance, Method};

    #[test]
    fn new() {
      let closure_call = MethodCall();

      assert_eq!(closure_call.meta().name, "call");
      assert_eq!(closure_call.meta().signature.arity, Arity::Variadic(0));
      assert_eq!(
        closure_call.meta().signature.parameters[0].kind,
        ParameterKind::Any
      );
    }

    #[test]
    fn call() {
      let method_call = MethodCall();
      let gc = test_native_dependencies();
      let mut context = MockedContext::new(&gc, &[Value::from(14.3)]);
      let mut hooks = Hooks::new(&mut context);

      let fun = fun_from_hooks(&hooks.to_gc(), "example".to_string(), "module");
      let class = hooks.manage(Class::bare(hooks.manage_str("exampleClass".to_string())));
      let closure = hooks.manage(Closure::new(fun));
      let instance = hooks.manage(Instance::new(class));
      let method = hooks.manage(Method::new(Value::from(instance), Value::from(closure)));

      let result1 = method_call.call(&mut hooks, Value::from(method), &[]);

      match result1 {
        Ok(r) => assert_eq!(r.to_num(), 14.3),
        Err(_) => assert!(false),
      }
    }
  }
}
