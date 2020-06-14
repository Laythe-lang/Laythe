use crate::support::{export_and_insert, to_dyn_method};
use spacelox_core::{
  arity::ArityKind,
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{NativeMeta, NativeMethod},
  object::Class,
  package::Package,
  value::Value,
  CallResult, ModuleResult,
};
use spacelox_env::{managed::Trace, stdio::StdIo};

pub const NUMBER_CLASS_NAME: &'static str = "Number";
const NUMBER_STR: NativeMeta = NativeMeta::new("str", ArityKind::Fixed(0), &[]);

pub fn declare_number_class(hooks: &GcHooks, self_module: &mut Module) -> ModuleResult<()> {
  let name = hooks.manage_str(String::from(NUMBER_CLASS_NAME));
  let class = hooks.manage(Class::new(name));

  export_and_insert(hooks, self_module, name, Value::from(class))
}

pub fn define_number_class(hooks: &GcHooks, self_module: &Module, _: &Package) {
  let name = hooks.manage_str(String::from(NUMBER_CLASS_NAME));
  let mut class = self_module
    .import(hooks)
    .get_field(&name)
    .unwrap()
    .to_class();

  class.add_method(
    hooks,
    hooks.manage_str(String::from(NUMBER_STR.name)),
    Value::from(to_dyn_method(hooks, NumberStr::new())),
  );
}

#[derive(Clone, Debug, Trace)]
struct NumberStr {
  meta: &'static NativeMeta,
}

impl NumberStr {
  fn new() -> Self {
    Self { meta: &NUMBER_STR }
  }
}

impl NativeMethod for NumberStr {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, hook: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    Ok(Value::from(hook.manage_str(this.to_num().to_string())))
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
      let number_str = NumberStr::new();

      assert_eq!(number_str.meta.name, "str");
      assert_eq!(number_str.meta.arity, ArityKind::Fixed(0));
    }

    #[test]
    fn call() {
      let number_str = NumberStr::new();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let result = number_str.call(&mut hooks, Value::from(10.0), &[]);
      match result {
        Ok(r) => assert_eq!(*r.to_str(), "10".to_string()),
        Err(_) => assert!(false),
      }
    }
  }
}
