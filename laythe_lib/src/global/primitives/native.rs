use crate::{
  native,
  support::{export_and_insert, load_class_from_module},
  StdResult,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  managed::Trace,
  module::Module,
  object::{LyNative, Native, NativeMetaBuilder},
  signature::{Arity, ParameterBuilder, ParameterKind},
  val,
  value::Value,
  Call, Ref,
};
use std::io::Write;

use super::class_inheritance;

pub const NATIVE_CLASS_NAME: &str = "Native";

const NATIVE_NAME: NativeMetaBuilder = NativeMetaBuilder::method("name", Arity::Fixed(0));

const NATIVE_CALL: NativeMetaBuilder = NativeMetaBuilder::method("call", Arity::Variadic(0))
  .with_params(&[ParameterBuilder::new("args", ParameterKind::Object)])
  .with_stack();

pub fn declare_native_class(hooks: &GcHooks, module: Ref<Module>) -> StdResult<()> {
  let class = class_inheritance(hooks, module, NATIVE_CLASS_NAME)?;
  export_and_insert(hooks, module, class.name(), val!(class))
}

pub fn define_native_class(hooks: &GcHooks, module: Ref<Module>) -> StdResult<()> {
  let mut class = load_class_from_module(hooks, module, NATIVE_CLASS_NAME)?;

  class.add_method(
    hooks.manage_str(NATIVE_NAME.name),
    val!(NativeName::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(NATIVE_CALL.name),
    val!(NativeCall::native(hooks)),
  );

  Ok(())
}

native!(NativeName, NATIVE_NAME);

impl LyNative for NativeName {
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
    Call::Ok(val!(args[0].to_obj().to_native().name()))
  }
}

native!(NativeCall, NATIVE_CALL);

impl LyNative for NativeCall {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    hooks.call(args[0], &args[1..])
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::{global::support::TestNative, support::MockedContext};

  mod name {
    use super::*;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let native_name = NativeName::native(&hooks.as_gc());

      let managed = TestNative::native(&hooks.as_gc());
      let result = native_name.call(&mut hooks, &[val!(managed)]);
      match result {
        Call::Ok(r) => assert_eq!(*r.to_obj().to_str(), "test".to_string()),
        _ => panic!(),
      }
    }
  }

  mod call {
    use super::*;
    use crate::{global::support::TestNative, support::MockedContext};
    use laythe_core::value::VALUE_NIL;

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[VALUE_NIL]);
      let mut hooks = Hooks::new(&mut context);
      let native_call = NativeCall::native(&hooks.as_gc());

      let managed = TestNative::native(&hooks.as_gc());
      let result = native_call.call(&mut hooks, &[val!(managed)]);
      match result {
        Call::Ok(r) => assert!(r.is_nil()),
        _ => panic!(),
      }
    }
  }
}
