use crate::support::to_dyn_method;
use spacelox_core::{
  arity::ArityKind,
  hooks::Hooks,
  io::StdIo,
  managed::Trace,
  module::Module,
  native::{NativeMeta, NativeMethod},
  object::Class,
  package::Package,
  value::Value,
  CallResult, ModuleResult,
};

pub const NIL_CLASS_NAME: &'static str = "Nil";
const NIL_STR: NativeMeta = NativeMeta::new("str", ArityKind::Fixed(0));

pub fn declare_nil_class(hooks: &Hooks, self_module: &mut Module) -> ModuleResult<()> {
  let name = hooks.manage_str(String::from(NIL_CLASS_NAME));
  let class = hooks.manage(Class::new(name));

  self_module.export_symbol(hooks, name, Value::from(class))
}

pub fn define_nil_class(hooks: &Hooks, self_module: &Module, _: &Package) {
  let name = hooks.manage_str(String::from(NIL_CLASS_NAME));
  let mut class = self_module.import().get(&name).unwrap().to_class();

  class.add_method(
    hooks,
    hooks.manage_str(String::from(NIL_STR.name)),
    Value::from(to_dyn_method(hooks, NilStr::new())),
  );
}

#[derive(Clone, Debug, Trace)]
struct NilStr {
  meta: &'static NativeMeta,
}

impl NilStr {
  fn new() -> Self {
    Self { meta: &NIL_STR }
  }
}

impl NativeMethod for NilStr {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, hooks: &mut Hooks, _this: Value, _args: &[Value]) -> CallResult {
    Ok(Value::from(hooks.manage_str("nil".to_string())))
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod str {
    use super::*;
    use crate::support::{test_native_dependencies, TestContext};
    use spacelox_core::value::VALUE_NIL;

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

      let result = nil_str.call(&mut hooks, VALUE_NIL, &[]);
      match result {
        Ok(r) => assert_eq!(*r.to_str(), String::from("nil")),
        Err(_) => assert!(false),
      }
    }
  }
}
