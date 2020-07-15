use crate::support::{
  default_class_inheritance, export_and_insert, load_class_from_module, to_dyn_method,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{NativeMeta, NativeMethod},
  package::Package,
  signature::Arity,
  value::Value,
  CallResult, LyResult,
  val
};
use laythe_env::{managed::Trace, stdio::Stdio};

pub const NIL_CLASS_NAME: &'static str = "Nil";
const NIL_STR: NativeMeta = NativeMeta::new("str", Arity::Fixed(0));

pub fn declare_nil_class(hooks: &GcHooks, module: &mut Module, package: &Package) -> LyResult<()> {
  let class = default_class_inheritance(hooks, package, NIL_CLASS_NAME)?;
  export_and_insert(hooks, module, class.name, val!(class))
}

pub fn define_nil_class(hooks: &GcHooks, module: &Module, _: &Package) -> LyResult<()> {
  let mut class = load_class_from_module(hooks, module, NIL_CLASS_NAME)?;

  class.add_method(
    hooks,
    hooks.manage_str(String::from(NIL_STR.name)),
    val!(to_dyn_method(hooks, NilStr::new())),
  );

  Ok(())
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
    Ok(val!(hooks.manage_str("nil".to_string())))
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod str {
    use super::*;
    use crate::support::MockedContext;
    use laythe_core::value::VALUE_NIL;

    #[test]
    fn new() {
      let nil_str = NilStr::new();

      assert_eq!(nil_str.meta.name, "str");
      assert_eq!(nil_str.meta.signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let nil_str = NilStr::new();
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let result = nil_str.call(&mut hooks, VALUE_NIL, &[]);
      match result {
        Ok(r) => assert_eq!(*r.to_str(), "nil".to_string()),
        Err(_) => assert!(false),
      }
    }
  }
}
