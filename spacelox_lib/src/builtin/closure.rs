use spacelox_core::{
  CallResult,
  managed::Managed,
  native::{NativeMeta, NativeMethod},
  arity::ArityKind,
  hooks::Hooks,
  value::{Class, Value},
};

pub const CLOSURE_CLASS_NAME: &'static str = "Fun";

const CLOSURE_NAME: NativeMeta = NativeMeta::new("name", ArityKind::Fixed(0));
const CLOSURE_CALL: NativeMeta = NativeMeta::new("call", ArityKind::Variadic(0));

pub fn create_closure_class(hooks: &Hooks) -> Managed<Class> {
  let name = hooks.manage_str(String::from(CLOSURE_CLASS_NAME));
  let mut class = hooks.manage(Class::new(name));

  class.add_method(
    hooks,
    hooks.manage_str(String::from(CLOSURE_NAME.name)),
    Value::NativeMethod(hooks.manage(Box::new(ClosureName::new()))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(CLOSURE_CALL.name)),
    Value::NativeMethod(hooks.manage(Box::new(ClosureCall::new()))),
  );

  class
}

#[derive(Clone, Debug)]
struct ClosureName {
  meta: Box<NativeMeta>,
}

impl ClosureName {
  fn new() -> Self {
    Self {
      meta: Box::new(CLOSURE_NAME),
    }
  }
}

impl NativeMethod for ClosureName {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, _hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    Ok(Value::String(this.to_closure().fun.name))
  }
}

#[derive(Clone, Debug)]
struct ClosureCall {
  meta: Box<NativeMeta>,
}

impl ClosureCall {
  fn new() -> Self {
    Self {
      meta: Box::new(CLOSURE_CALL),
    }
  }
}

impl NativeMethod for ClosureCall {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, hooks: &mut Hooks, this: Value, args: &[Value]) -> CallResult {
    hooks.call(this, args)
  }
}
