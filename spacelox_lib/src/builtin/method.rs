use spacelox_core::{
  arity::ArityKind,
  hooks::Hooks,
  io::StdIo,
  managed::{Managed, Trace},
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
    Value::NativeMethod(hooks.manage(Box::new(MethodName::new(
      hooks.manage_str(String::from(METHOD_NAME.name)),
    )))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(METHOD_CALL.name)),
    Value::NativeMethod(hooks.manage(Box::new(MethodCall::new()))),
  );
}

#[derive(Clone, Debug, Trace)]
struct MethodName {
  meta: &'static NativeMeta,
  method_name: Managed<String>,
}

impl MethodName {
  fn new(method_name: Managed<String>) -> Self {
    Self {
      meta: &METHOD_NAME,
      method_name,
    }
  }
}

impl NativeMethod for MethodName {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, hooks: &mut Hooks, this: Value, args: &[Value]) -> CallResult {
    hooks.call_method_by_name(this.to_method().method, self.method_name, args)
  }
}

#[derive(Clone, Debug, Trace)]
struct MethodCall {
  meta: &'static NativeMeta,
}

impl MethodCall {
  fn new() -> Self {
    Self { meta: &METHOD_CALL }
  }
}

impl NativeMethod for MethodCall {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, hooks: &mut Hooks, this: Value, args: &[Value]) -> CallResult {
    let method = this.to_method();
    match method.method {
      Value::Closure(_) => hooks.call(this, args),
      Value::NativeMethod(_) => hooks.call_method(method.receiver, method.method, args),
      _ => panic!("TODO"),
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod name {
    use super::*;
    use crate::support::{test_native_dependencies, TestContext};
    use spacelox_core::{
      memory::NO_GC,
      value::{Closure, Fun, Instance, Method},
    };

    #[test]
    fn new() {
      let gc = test_native_dependencies();
      let method_name = MethodName::new(gc.manage_str(String::from("name"), &NO_GC));

      assert_eq!(method_name.meta.name, "name");
      assert_eq!(method_name.meta.arity, ArityKind::Fixed(0));
    }

    #[test]
    fn call() {
      let gc = test_native_dependencies();
      let mut context = TestContext::new(
        &gc,
        &[Value::String(
          gc.manage_str(String::from("example"), &NO_GC),
        )],
      );
      let mut hooks = Hooks::new(&mut context);
      let method_name = MethodName::new(hooks.manage_str(String::from("name")));

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
