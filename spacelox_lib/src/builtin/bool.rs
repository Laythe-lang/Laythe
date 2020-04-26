use spacelox_core::{
  CallResult,
  arity::ArityKind,
  hooks::Hooks,
  managed::Managed,
  native::{NativeMeta, NativeMethod},
  value::{Class, Value},
};

pub const BOOL_CLASS_NAME: &'static str = "Bool";
const BOOL_STR: NativeMeta = NativeMeta::new("str", ArityKind::Fixed(0));

pub fn create_bool_class(hooks: &Hooks) -> Managed<Class> {
  let name = hooks.manage_str(String::from(BOOL_CLASS_NAME));
  let mut class = hooks.manage(Class::new(name));

  class.add_method(
    hooks,
    hooks.manage_str(String::from(BOOL_STR.name)),
    Value::NativeMethod(hooks.manage(Box::new(BoolStr::new()))),
  );

  class
}

#[derive(Clone, Debug)]
struct BoolStr {
  meta: Box<NativeMeta>,
}

impl BoolStr {
  fn new() -> Self {
    Self {
      meta: Box::new(BOOL_STR),
    }
  }
}

impl NativeMethod for BoolStr {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    Ok(Value::String(hooks.manage_str(this.to_string())))
  }
}
