use crate::support::to_dyn_method;
use spacelox_core::{
  arity::ArityKind,
  hooks::Hooks,
  io::StdIo,
  managed::Trace,
  module::Module,
  native::{NativeMeta, NativeMethod},
  object::Class,
  package::Package,
  value::Value,
  CallResult, ModuleResult,
};

pub const NATIVE_METHOD_CLASS_NAME: &'static str = "Native Method";

const NATIVE_METHOD_NAME: NativeMeta = NativeMeta::new("name", ArityKind::Fixed(0));
const NATIVE_METHOD_CALL: NativeMeta = NativeMeta::new("call", ArityKind::Variadic(0));

pub fn declare_native_method_class(hooks: &Hooks, self_module: &mut Module) -> ModuleResult<()> {
  let name = hooks.manage_str(String::from(NATIVE_METHOD_CLASS_NAME));
  let class = hooks.manage(Class::new(name));

  self_module.export_symbol(hooks, name, Value::from(class))
}

pub fn define_native_method_class(hooks: &Hooks, self_module: &Module, _: &Package) {
  let name = hooks.manage_str(String::from(NATIVE_METHOD_CLASS_NAME));
  let mut class = self_module.import().get(&name).unwrap().to_class();

  class.add_method(
    hooks,
    hooks.manage_str(String::from(NATIVE_METHOD_NAME.name)),
    Value::from(to_dyn_method(hooks, NativeMethodName::new())),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(NATIVE_METHOD_CALL.name)),
    Value::from(to_dyn_method(hooks, NativeMethodCall::new())),
  );
}

#[derive(Clone, Debug, Trace)]
struct NativeMethodName {
  meta: &'static NativeMeta,
}

impl NativeMethodName {
  fn new() -> Self {
    Self {
      meta: &NATIVE_METHOD_NAME,
    }
  }
}

impl NativeMethod for NativeMethodName {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    Ok(Value::from(hooks.manage_str(String::from(
      this.to_native_method().meta().name,
    ))))
  }
}

#[derive(Clone, Debug, Trace)]
struct NativeMethodCall {
  meta: &'static NativeMeta,
}

impl NativeMethodCall {
  fn new() -> Self {
    Self {
      meta: &NATIVE_METHOD_CALL,
    }
  }
}

impl NativeMethod for NativeMethodCall {
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
    use spacelox_core::managed::Managed;

    #[test]
    fn new() {
      let native_method_name = NativeMethodName::new();

      assert_eq!(native_method_name.meta.name, "name");
      assert_eq!(native_method_name.meta.arity, ArityKind::Fixed(0));
    }

    #[test]
    fn call() {
      let native_method_name = NativeMethodName::new();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let managed: Managed<Box<dyn NativeMethod>> = hooks.manage(Box::new(NativeMethodName::new()));
      let result = native_method_name.call(&mut hooks, Value::from(managed), &[]);
      match result {
        Ok(r) => assert_eq!(*r.to_str(), "name".to_string()),
        Err(_) => assert!(false),
      }
    }
  }

  mod call {
    use super::*;
    use crate::{
      assert::assert::Assert,
      support::{test_native_dependencies, TestContext},
    };
    use spacelox_core::{managed::Managed, native::NativeFun, value::VALUE_NIL};

    #[test]
    fn new() {
      let native_fun_call = NativeMethodCall::new();

      assert_eq!(native_fun_call.meta.name, "call");
      assert_eq!(native_fun_call.meta.arity, ArityKind::Variadic(0));
    }

    #[test]
    fn call() {
      let native_fun_call = NativeMethodCall::new();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[VALUE_NIL]);
      let mut hooks = Hooks::new(&mut context);

      let managed: Managed<Box<dyn NativeFun>> =
        hooks.manage(Box::new(Assert::new(hooks.manage_str("str".to_string()))));
      let result = native_fun_call.call(&mut hooks, Value::from(managed), &[Value::from(true)]);
      match result {
        Ok(r) => assert!(r.is_nil()),
        Err(_) => assert!(false),
      }
    }
  }
}
