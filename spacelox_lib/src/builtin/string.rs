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

pub const STRING_CLASS_NAME: &'static str = "String";
const STRING_STR: NativeMeta = NativeMeta::new("str", ArityKind::Fixed(0));

pub fn declare_string_class(hooks: &Hooks, self_module: &mut Module) -> ModuleResult<()> {
  let name = hooks.manage_str(String::from(STRING_CLASS_NAME));
  let class = hooks.manage(Class::new(name));

  self_module.add_export(hooks, name, Value::Class(class))
}

pub fn define_string_class(hooks: &Hooks, self_module: &Module, _: &Package) {
  let name = hooks.manage_str(String::from(STRING_CLASS_NAME));
  let mut class = self_module.get_symbol(hooks, name).unwrap().to_class();

  class.add_method(
    hooks,
    hooks.manage_str(String::from(STRING_STR.name)),
    Value::NativeMethod(hooks.manage(Box::new(StringStr::new()))),
  );
}

#[derive(Clone, Debug, Trace)]
struct StringStr {
  meta: &'static NativeMeta,
}

impl StringStr {
  fn new() -> Self {
    Self { meta: &STRING_STR }
  }
}

impl NativeMethod for StringStr {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, _hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    Ok(Value::String(this.to_str()))
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod str {
    use super::*;
    use crate::support::{test_native_dependencies, TestContext};

    #[test]
    fn new() {
      let string_str = StringStr::new();

      assert_eq!(string_str.meta.name, "str");
      assert_eq!(string_str.meta.arity, ArityKind::Fixed(0));
    }

    #[test]
    fn call() {
      let string_str = StringStr::new();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let this = Value::String(hooks.manage_str(String::from("test")));
      let result = string_str.call(&mut hooks, this, &[]);
      match result {
        Ok(r) => assert_eq!(*r.to_str(), String::from("test")),
        Err(_) => assert!(false),
      }
    }
  }
}
