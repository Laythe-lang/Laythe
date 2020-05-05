use spacelox_core::{
  arity::ArityKind,
  hooks::Hooks,
  io::StdIo,
  managed::Trace,
  module::Module,
  native::{NativeMeta, NativeMethod},
  package::Package,
  value::{Class, Value},
  CallResult, ModuleResult,
};

pub const METHOD_CLASS_NAME: &'static str = "Method";

const METHOD_NAME: NativeMeta = NativeMeta::new("name", ArityKind::Fixed(0));
const METHOD_CALL: NativeMeta = NativeMeta::new("call", ArityKind::Variadic(0));

pub fn declare_method_class(hooks: &Hooks, self_module: &mut Module) -> ModuleResult<()> {
  let name = hooks.manage_str(String::from(METHOD_CLASS_NAME));
  let class = hooks.manage(Class::new(name));

  self_module.add_export(hooks, name, Value::Class(class))
}

pub fn define_method_class(hooks: &Hooks, self_module: &Module, _: &Package) {
  let name = hooks.manage_str(String::from(METHOD_CLASS_NAME));
  let mut class = self_module.get_symbol(hooks, name).unwrap().to_class();

  class.add_method(
    hooks,
    hooks.manage_str(String::from(METHOD_NAME.name)),
    Value::NativeMethod(hooks.manage(Box::new(MethodName::new()))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(METHOD_CALL.name)),
    Value::NativeMethod(hooks.manage(Box::new(MethodCall::new()))),
  );
}

#[derive(Clone, Debug, Trace)]
struct MethodName {
  meta: Box<NativeMeta>,
}

impl MethodName {
  fn new() -> Self {
    Self {
      meta: Box::new(METHOD_NAME),
    }
  }
}

impl NativeMethod for MethodName {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, _hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    Ok(Value::String(this.to_method().method.to_closure().fun.name))
  }
}

#[derive(Clone, Debug, Trace)]
struct MethodCall {
  meta: Box<NativeMeta>,
}

impl MethodCall {
  fn new() -> Self {
    Self {
      meta: Box::new(METHOD_CALL),
    }
  }
}

impl NativeMethod for MethodCall {
  fn meta(&self) -> &NativeMeta {
    &self.meta
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
    use crate::support::{test_native_dependencies, TestContext};
    use spacelox_core::value::{Closure, Fun, Instance, Method};

    #[test]
    fn new() {
      let method_name = MethodName::new();

      assert_eq!(method_name.meta.name, "name");
      assert_eq!(method_name.meta.arity, ArityKind::Fixed(0));
    }

    #[test]
    fn call() {
      let method_name = MethodName::new();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let fun = hooks.manage(Fun::new(hooks.manage_str(String::from("example"))));
      let class = hooks.manage(Class::new(hooks.manage_str(String::from("exampleClass"))));
      let closure = hooks.manage(Closure::new(fun));
      let instance = hooks.manage(Instance::new(class));
      let method = hooks.manage(Method::new(
        Value::Instance(instance),
        Value::Closure(closure),
      ));

      let result1 = method_name.call(&mut hooks, Value::Method(method), &[]);

      match result1 {
        Ok(r) => assert_eq!(&*r.to_str(), "example"),
        Err(_) => assert!(false),
      }
    }
  }

  mod call {
    use super::*;
    use crate::support::{test_native_dependencies, TestContext};
    use spacelox_core::value::{Closure, Fun, Instance, Method};

    #[test]
    fn new() {
      let closure_call = MethodCall::new();

      assert_eq!(closure_call.meta.name, "call");
      assert_eq!(closure_call.meta.arity, ArityKind::Variadic(0));
    }

    #[test]
    fn call() {
      let method_call = MethodCall::new();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[Value::Number(14.3)]);
      let mut hooks = Hooks::new(&mut context);

      let fun = hooks.manage(Fun::new(hooks.manage_str(String::from("example"))));
      let class = hooks.manage(Class::new(hooks.manage_str(String::from("exampleClass"))));
      let closure = hooks.manage(Closure::new(fun));
      let instance = hooks.manage(Instance::new(class));
      let method = hooks.manage(Method::new(
        Value::Instance(instance),
        Value::Closure(closure),
      ));

      let result1 = method_call.call(&mut hooks, Value::Method(method), &[]);

      match result1 {
        Ok(r) => assert_eq!(r.to_num(), 14.3),
        Err(_) => assert!(false),
      }
    }
  }
}
