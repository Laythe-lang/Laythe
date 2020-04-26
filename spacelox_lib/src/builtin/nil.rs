use spacelox_core::{
  CallResult,
  native::{NativeMeta, NativeMethod},
  managed::Managed,
  arity::ArityKind,
  hooks::Hooks,
  value::{Class, Value},
};

pub const NIL_CLASS_NAME: &'static str = "Nil";
const NIL_STR: NativeMeta = NativeMeta::new("str", ArityKind::Fixed(0));

pub fn create_nil_class(hooks: &Hooks) -> Managed<Class> {
  let name = hooks.manage_str(String::from(NIL_CLASS_NAME));
  let mut class = hooks.manage(Class::new(name));

  class.add_method(
    hooks,
    hooks.manage_str(String::from(NIL_STR.name)),
    Value::NativeMethod(hooks.manage(Box::new(NilStr::new()))),
  );

  class
}

#[derive(Clone, Debug)]
struct NilStr {
  meta: Box<NativeMeta>,
}

impl NilStr {
  fn new() -> Self {
    Self {
      meta: Box::new(NIL_STR),
    }
  }
}

impl NativeMethod for NilStr {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    Ok(Value::String(hooks.manage_str(this.to_string())))
  }
}



#[cfg(test)]
mod test {
  use super::*;

  mod str {
    use super::*;
    use crate::support::{TestContext, test_native_dependencies};

    #[test]
    fn new() {
      let nil_str = NilStr::new();

      assert_eq!(nil_str.meta.name, "str");
      assert_eq!(nil_str.meta.arity, ArityKind::Fixed(0));
    }

    #[test]
    fn call() {
      let nil_str = NilStr::new();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let result = nil_str.call(&mut hooks, Value::Nil, &[]);
      match result {
        Ok(r) => assert_eq!(*r.to_str(), String::from("nil")),
        Err(_) => assert!(false),
      }
    }
  }
}