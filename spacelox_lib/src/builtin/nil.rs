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

pub const NIL_CLASS_NAME: &'static str = "Nil";
const NIL_STR: NativeMeta = NativeMeta::new("str", ArityKind::Fixed(0));

pub fn declare_nil_class(hooks: &Hooks, self_module: &mut Module) -> ModuleResult<()> {
  let name = hooks.manage_str(String::from(NIL_CLASS_NAME));
  let class = hooks.manage(Class::new(name));

  self_module.add_export(hooks, name, Value::Class(class))
}

pub fn define_nil_class(hooks: &Hooks, self_module: &Module, _: &Package) {
  let name = hooks.manage_str(String::from(NIL_CLASS_NAME));
  let mut class = self_module.get_symbol(hooks, name).unwrap().to_class();

  class.add_method(
    hooks,
    hooks.manage_str(String::from(NIL_STR.name)),
    Value::NativeMethod(hooks.manage(Box::new(NilStr::new()))),
  );
}

#[derive(Clone, Debug, Trace)]
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
    use crate::support::{test_native_dependencies, TestContext};

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
