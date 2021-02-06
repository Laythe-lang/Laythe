use crate::{
  native,
  support::{export_and_insert, load_class_from_module, to_dyn_native},
  StdResult,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  managed::{GcStr, Trace},
  module::Module,
  object::{MetaData, Native, NativeMeta, NativeMetaBuilder},
  signature::{Arity, ParameterBuilder, ParameterKind},
  val,
  value::Value,
  Call,
};
use std::io::Write;

use super::class_inheritance;

pub const METHOD_CLASS_NAME: &str = "Method";

const METHOD_NAME: NativeMetaBuilder = NativeMetaBuilder::method("name", Arity::Fixed(0));

const METHOD_CALL: NativeMetaBuilder = NativeMetaBuilder::method("call", Arity::Variadic(0))
  .with_params(&[ParameterBuilder::new("args", ParameterKind::Any)])
  .with_stack();

pub fn declare_method_class(hooks: &GcHooks, module: &mut Module) -> StdResult<()> {
  let method_class = class_inheritance(hooks, module, METHOD_CLASS_NAME)?;
  export_and_insert(hooks, module, method_class.name(), val!(method_class))
}

pub fn define_method_class(hooks: &GcHooks, module: &Module) -> StdResult<()> {
  let mut class = load_class_from_module(hooks, module, METHOD_CLASS_NAME)?;

  class.add_method(
    hooks,
    hooks.manage_str(METHOD_NAME.name),
    val!(to_dyn_native(
      hooks,
      MethodName::new(
        METHOD_NAME.to_meta(&hooks),
        hooks.manage_str(METHOD_NAME.name)
      ),
    )),
  );

  class.add_method(
    hooks,
    hooks.manage_str(METHOD_CALL.name),
    val!(to_dyn_native(hooks, MethodCall::from(hooks))),
  );

  Ok(())
}

#[derive(Debug)]
struct MethodName {
  meta: NativeMeta,
  method_name: GcStr,
}

impl MethodName {
  fn new(meta: NativeMeta, method_name: GcStr) -> Self {
    Self { meta, method_name }
  }
}

impl MetaData for MethodName {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }
}

impl Native for MethodName {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let method = this.unwrap().to_method().method();

    hooks
      .get_method(this.unwrap().to_method().method(), self.method_name)
      .and_then(|method_name| hooks.call_method(method, method_name, args))
  }
}

impl Trace for MethodName {
  fn trace(&self) {
    self.meta.trace();
    self.method_name.trace();
  }

  fn trace_debug(&self, stdout: &mut dyn Write) {
    self.meta.trace_debug(stdout);
    self.method_name.trace_debug(stdout);
  }
}

native!(MethodCall, METHOD_CALL);

impl Native for MethodCall {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let method = this.unwrap().to_method();
    hooks.call_method(method.receiver(), method.method(), args)
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod name {
    use super::*;
    use crate::support::{fun_from_hooks, MockedContext};
    use laythe_core::{
      memory::NO_GC,
      object::{Class, Closure, Instance, Method},
    };

    #[test]
    fn new() {
      let context = MockedContext::default();
      let hooks = GcHooks::new(&context);

      let method_name = MethodName::new(
        METHOD_NAME.to_meta(&hooks),
        hooks.manage_str("name".to_string()),
      );

      assert_eq!(method_name.meta().name, "name");
      assert_eq!(method_name.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::with_std(&[]).unwrap();
      let responses = &[val!(context.gc.borrow_mut().manage_str("example", &NO_GC))];
      context.responses.extend_from_slice(responses);

      let mut hooks = Hooks::new(&mut context);
      let method_name = MethodName::new(
        METHOD_NAME.to_meta(&hooks.as_gc()),
        hooks.manage_str("name".to_string()),
      );

      let fun = fun_from_hooks(&hooks.as_gc(), "example", "module");
      let class = hooks.manage(Class::bare(hooks.manage_str("exampleClass".to_string())));
      let closure = hooks.manage(Closure::without_upvalues(fun));
      let instance = hooks.manage(Instance::new(class));
      let method = hooks.manage(Method::new(val!(instance), val!(closure)));

      let result1 = method_name.call(&mut hooks, Some(val!(method)), &[]);

      match result1 {
        Call::Ok(r) => assert_eq!(&*r.to_str(), "example"),
        _ => assert!(false),
      }
    }
  }

  mod call {
    use super::*;
    use crate::support::{fun_from_hooks, MockedContext};
    use laythe_core::object::{Class, Closure, Instance, Method};

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = Hooks::new(&mut context);

      let closure_call = MethodCall::from(&hooks);

      assert_eq!(closure_call.meta().name, "call");
      assert_eq!(closure_call.meta().signature.arity, Arity::Variadic(0));
      assert_eq!(
        closure_call.meta().signature.parameters[0].kind,
        ParameterKind::Any
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(14.3)]);
      let mut hooks = Hooks::new(&mut context);
      let method_call = MethodCall::from(&hooks);

      let fun = fun_from_hooks(&hooks.as_gc(), "example", "module");
      let class = hooks.manage(Class::bare(hooks.manage_str("exampleClass".to_string())));
      let closure = hooks.manage(Closure::without_upvalues(fun));
      let instance = hooks.manage(Instance::new(class));
      let method = hooks.manage(Method::new(val!(instance), val!(closure)));

      let result1 = method_call.call(&mut hooks, Some(val!(method)), &[]);

      match result1 {
        Call::Ok(r) => assert_eq!(r.to_num(), 14.3),
        _ => assert!(false),
      }
    }
  }
}
