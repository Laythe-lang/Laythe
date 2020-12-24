use crate::{
  native,
  support::{default_class_inheritance, export_and_insert, load_class_from_module, to_dyn_native},
  InitResult,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{MetaData, Native, NativeMeta, NativeMetaBuilder},
  package::Package,
  signature::{Arity, ParameterBuilder, ParameterKind},
  val,
  value::Value,
  Call,
};
use laythe_env::managed::Trace;
use std::io::Write;

pub const NATIVE_CLASS_NAME: &str = "Native";

const NATIVE_NAME: NativeMetaBuilder = NativeMetaBuilder::method("name", Arity::Fixed(0));

const NATIVE_CALL: NativeMetaBuilder = NativeMetaBuilder::method("call", Arity::Variadic(0))
  .with_params(&[ParameterBuilder::new("args", ParameterKind::Any)])
  .with_stack();

pub fn declare_native_class(
  hooks: &GcHooks,
  module: &mut Module,
  package: &Package,
) -> InitResult<()> {
  let class = default_class_inheritance(hooks, package, NATIVE_CLASS_NAME)?;
  export_and_insert(hooks, module, class.name, val!(class))
}

pub fn define_native_class(hooks: &GcHooks, module: &Module, _: &Package) -> InitResult<()> {
  let mut class = load_class_from_module(hooks, module, NATIVE_CLASS_NAME)?;

  class.add_method(
    hooks,
    hooks.manage_str(NATIVE_NAME.name),
    val!(to_dyn_native(hooks, NativeName::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(NATIVE_CALL.name),
    val!(to_dyn_native(hooks, NativeCall::from(hooks))),
  );

  Ok(())
}

native!(NativeName, NATIVE_NAME);

impl Native for NativeName {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    Call::Ok(val!(this.unwrap().to_native().meta().name))
  }
}

native!(NativeCall, NATIVE_CALL);

impl Native for NativeCall {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    hooks.call(this.unwrap(), args)
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::{global::support::TestNative, support::MockedContext};
  use laythe_env::managed::Gc;

  mod name {
    use super::*;
    use laythe_core::native::Native;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = Hooks::new(&mut context);

      let native_name = NativeName::from(&hooks);

      assert_eq!(native_name.meta().name, "name");
      assert_eq!(native_name.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let native_name = NativeName::from(&hooks);

      let managed: Gc<Box<dyn Native>> = hooks.manage(Box::new(TestNative::from(&hooks)));
      let result = native_name.call(&mut hooks, Some(val!(managed)), &[]);
      match result {
        Call::Ok(r) => assert_eq!(*r.to_str(), "test".to_string()),
        _ => assert!(false),
      }
    }
  }

  mod call {
    use super::*;
    use crate::{global::support::TestNative, support::MockedContext};
    use laythe_core::{native::Native, value::VALUE_NIL};

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = Hooks::new(&mut context);

      let native_call = NativeCall::from(&hooks);

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
      let native_call = NativeCall::from(&hooks);

      let managed: Gc<Box<dyn Native>> = hooks.manage(Box::new(TestNative::from(&hooks)));
      let result = native_call.call(&mut hooks, Some(val!(managed)), &[]);
      match result {
        Call::Ok(r) => assert!(r.is_nil()),
        _ => assert!(false),
      }
    }
  }
}
