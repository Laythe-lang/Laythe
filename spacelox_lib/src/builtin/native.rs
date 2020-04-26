use spacelox_core::{
  CallResult,
  hooks::Hooks,
  managed::Managed,
  native::{NativeMeta, NativeMethod},
  arity::ArityKind,
  value::{Class, Value},
};

pub const NATIVE_CLASS_NAME: &'static str = "Native";

const NATIVE_NAME: NativeMeta = NativeMeta::new("name", ArityKind::Fixed(0));

pub fn create_native_class(hooks: &Hooks) -> Managed<Class> {
  let name = hooks.manage_str(String::from(NATIVE_CLASS_NAME));
  let mut class = hooks.manage(Class::new(name));

  class.add_method(
    hooks,
    hooks.manage_str(String::from(NATIVE_NAME.name)),
    Value::NativeMethod(hooks.manage(Box::new(NativeName::new()))),
  );

  class
}

#[derive(Clone, Debug)]
struct NativeName {
  meta: Box<NativeMeta>,
}

impl NativeName {
  fn new() -> Self {
    Self {
      meta: Box::new(NATIVE_NAME),
    }
  }
}

impl NativeMethod for NativeName {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    Ok(Value::String(
      hooks.manage_str(String::from(this.to_native_method().meta().name)),
    ))
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod name {
    use super::*;
    use crate::support::{TestContext, test_native_dependencies};

    #[test]
    fn new() {
      let native_name = NativeName::new();

      assert_eq!(native_name.meta.name, "name");
      assert_eq!(native_name.meta.arity, ArityKind::Fixed(0));
    }

    #[test]
    fn call() {
      let native_name = NativeName::new();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let managed: Managed<Box<dyn NativeMethod>> = hooks.manage(Box::new(NativeName::new()));
      let result = native_name.call(&mut hooks, Value::NativeMethod(managed), &[]);
      match result {
        Ok(r) => assert_eq!(*r.to_str(), String::from("name")),
        Err(_) => assert!(false),
      }
    }
  }
}