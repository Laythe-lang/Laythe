use crate::{
  native,
  support::{export_and_insert, load_class_from_module},
  StdResult,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  managed::Trace,
  managed::{Gc, GcObj},
  module::Module,
  object::{LyNative, Native, NativeMetaBuilder},
  signature::{Arity, ParameterBuilder, ParameterKind},
  val,
  value::Value,
  Call,
};
use std::io::Write;

use super::class_inheritance;

pub const NATIVE_CLASS_NAME: &str = "Native";

const NATIVE_NAME: NativeMetaBuilder = NativeMetaBuilder::method("name", Arity::Fixed(0));

const NATIVE_CALL: NativeMetaBuilder = NativeMetaBuilder::method("call", Arity::Variadic(0))
  .with_params(&[ParameterBuilder::new("args", ParameterKind::Any)])
  .with_stack();

pub fn declare_native_class(hooks: &GcHooks, module: Gc<Module>) -> StdResult<()> {
  let class = class_inheritance(hooks, module, NATIVE_CLASS_NAME)?;
  export_and_insert(module, class.name(), val!(class))
}

pub fn define_native_class(hooks: &GcHooks, module: Gc<Module>) -> StdResult<()> {
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
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    Call::Ok(val!(this.unwrap().to_obj().to_native().meta().name))
  }
}

native!(NativeCall, NATIVE_CALL);

impl LyNative for NativeCall {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    hooks.call(this.unwrap(), args)
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::{global::support::TestNative, support::MockedContext};

  mod name {
    use super::*;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let native_name = NativeName::native(&hooks);

      assert_eq!(native_name.meta().name, "name");
      assert_eq!(native_name.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let native_name = NativeName::native(&hooks.as_gc());

      let managed = TestNative::native(&hooks.as_gc());
      let result = native_name.call(&mut hooks, Some(val!(managed)), &[]);
      match result {
        Call::Ok(r) => assert_eq!(*r.to_obj().to_str(), "test".to_string()),
        _ => assert!(false),
      }
    }
  }

  mod call {
    use super::*;
    use crate::{global::support::TestNative, support::MockedContext};
    use laythe_core::value::VALUE_NIL;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let native_call = NativeCall::native(&hooks);

      assert_eq!(native_call.meta().name, "call");
      assert_eq!(native_call.meta().signature.arity, Arity::Variadic(0));
      assert_eq!(
        native_call.meta().signature.parameters[0].kind,
        ParameterKind::Any
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[VALUE_NIL]);
      let mut hooks = Hooks::new(&mut context);
      let native_call = NativeCall::native(&hooks.as_gc());

      let managed = TestNative::native(&hooks.as_gc());
      let result = native_call.call(&mut hooks, Some(val!(managed)), &[]);
      match result {
        Call::Ok(r) => assert!(r.is_nil()),
        _ => assert!(false),
      }
    }
  }
}
