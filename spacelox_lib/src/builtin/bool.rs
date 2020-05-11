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

pub const BOOL_CLASS_NAME: &'static str = "Bool";
const BOOL_STR: NativeMeta = NativeMeta::new("str", ArityKind::Fixed(0));

pub fn declare_bool_class(hooks: &Hooks, self_module: &mut Module) -> ModuleResult<()> {
  let name = hooks.manage_str(String::from(BOOL_CLASS_NAME));
  let class = hooks.manage(Class::new(name));

  self_module.add_export(hooks, name, Value::Class(class))
}

pub fn define_bool_class(hooks: &Hooks, self_module: &Module, _: &Package) {
  let name = hooks.manage_str(String::from(BOOL_CLASS_NAME));
  let mut class = self_module.get_symbol(hooks, name).unwrap().to_class();

  class.add_method(
    hooks,
    hooks.manage_str(String::from(BOOL_STR.name)),
    Value::NativeMethod(hooks.manage(Box::new(BoolStr::new()))),
  );
}

#[derive(Clone, Debug, Trace)]
struct BoolStr {
  meta: &'static NativeMeta,
}

impl BoolStr {
  fn new() -> Self {
    Self { meta: &BOOL_STR }
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

#[cfg(test)]
mod test {
  use super::*;

  mod str {
    use super::*;
    use crate::support::{test_native_dependencies, TestContext};

    #[test]
    fn new() {
      let bool_str = BoolStr::new();

      assert_eq!(bool_str.meta.name, "str");
      assert_eq!(bool_str.meta.arity, ArityKind::Fixed(0));
    }

    #[test]
    fn call() {
      let bool_str = BoolStr::new();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let b_true = Value::Bool(true);
      let b_false = Value::Bool(false);

      let result1 = bool_str.call(&mut hooks, b_true, &[]);
      let result2 = bool_str.call(&mut hooks, b_false, &[]);

      match result1 {
        Ok(r) => assert_eq!(&*r.to_str(), "true"),
        Err(_) => assert!(false),
      }
      match result2 {
        Ok(r) => assert_eq!(&*r.to_str(), "false"),
        Err(_) => assert!(false),
      }
    }
  }
}
