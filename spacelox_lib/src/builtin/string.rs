use crate::support::{export_and_insert, to_dyn_method};
use spacelox_core::{
  arity::ArityKind,
  hooks::Hooks,
  module::Module,
  native::{NativeMeta, NativeMethod},
  object::Class,
  package::Package,
  value::Value,
  CallResult, ModuleResult,
};
use spacelox_env::{managed::Trace, stdio::StdIo};

pub const STRING_CLASS_NAME: &'static str = "String";
const STRING_STR: NativeMeta = NativeMeta::new("str", ArityKind::Fixed(0));

pub fn declare_string_class(hooks: &Hooks, self_module: &mut Module) -> ModuleResult<()> {
  let name = hooks.manage_str(String::from(STRING_CLASS_NAME));
  let class = hooks.manage(Class::new(name));

  export_and_insert(hooks, self_module, name, Value::from(class))
}

pub fn define_string_class(hooks: &Hooks, self_module: &Module, _: &Package) {
  let name = Value::from(hooks.manage_str(String::from(STRING_CLASS_NAME)));
  let mut class = self_module.import().get(&name).unwrap().to_class();

  class.add_method(
    hooks,
    hooks.manage_str(String::from(STRING_STR.name)),
    Value::from(to_dyn_method(hooks, StringStr::new())),
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
    Ok(Value::from(this.to_str()))
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

      let this = Value::from(hooks.manage_str("test".to_string()));
      let result = string_str.call(&mut hooks, this, &[]);
      match result {
        Ok(r) => assert_eq!(*r.to_str(), "test".to_string()),
        Err(_) => assert!(false),
      }
    }
  }
}
