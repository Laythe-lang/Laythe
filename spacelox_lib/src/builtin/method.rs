use spacelox_core::{
  CallResult,
  managed::Managed,
  native::{NativeMeta, NativeMethod},
  arity::ArityKind,
  hooks::Hooks,
  value::{Class, Value},
};

pub const METHOD_CLASS_NAME: &'static str = "Method";

const METHOD_NAME: NativeMeta = NativeMeta::new("name", ArityKind::Fixed(0));
const METHOD_CALL: NativeMeta = NativeMeta::new("call", ArityKind::Variadic(0));

pub fn create_method_class(hooks: &Hooks) -> Managed<Class> {
  let name = hooks.manage_str(String::from(METHOD_CLASS_NAME));
  let mut class = hooks.manage(Class::new(name));

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

  class
}

#[derive(Clone, Debug)]
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
    Ok(Value::String(this.to_method().method.to_fun().name))
  }
}

#[derive(Clone, Debug)]
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
