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

pub const NUMBER_CLASS_NAME: &'static str = "Number";
const NUMBER_STR: NativeMeta = NativeMeta::new("str", ArityKind::Fixed(0));

pub fn declare_number_class(hooks: &Hooks, self_module: &mut Module) -> ModuleResult<()> {
  let name = hooks.manage_str(String::from(NUMBER_CLASS_NAME));
  let class = hooks.manage(Class::new(name));

  self_module.add_export(hooks, name, Value::Class(class))
}

pub fn define_number_class(hooks: &Hooks, self_module: &Module, _: &Package) {
  let name = hooks.manage_str(String::from(NUMBER_CLASS_NAME));
  let mut class = self_module.get_symbol(hooks, name).unwrap().to_class();

  class.add_method(
    hooks,
    hooks.manage_str(String::from(NUMBER_STR.name)),
    Value::NativeMethod(hooks.manage(Box::new(NumberStr::new()))),
  );
}

#[derive(Clone, Debug, Trace)]
struct NumberStr {
  meta: Box<NativeMeta>,
}

impl NumberStr {
  fn new() -> Self {
    Self {
      meta: Box::new(NUMBER_STR),
    }
  }
}

impl NativeMethod for NumberStr {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, hook: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    Ok(Value::String(hook.manage_str(this.to_string())))
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

      let result = number_str.call(&mut hooks, Value::Number(10.0), &[]);
      match result {
        Ok(r) => assert_eq!(*r.to_str(), String::from("10")),
        Err(_) => assert!(false),
      }
    }
  }
}
