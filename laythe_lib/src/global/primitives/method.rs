use crate::{
  native,
  support::{export_and_insert, load_class_from_module},
  StdResult,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  managed::{Gc, GcObj, GcStr, Trace},
  module::Module,
  object::{LyNative, Native, NativeMetaBuilder},
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
  .with_params(&[ParameterBuilder::new("args", ParameterKind::Object)])
  .with_stack();

pub fn declare_method_class(hooks: &GcHooks, module: Gc<Module>) -> StdResult<()> {
  let method_class = class_inheritance(hooks, module, METHOD_CLASS_NAME)?;
  export_and_insert(hooks, module, method_class.name(), val!(method_class))
}

pub fn define_method_class(hooks: &GcHooks, module: Gc<Module>) -> StdResult<()> {
  let mut class = load_class_from_module(hooks, module, METHOD_CLASS_NAME)?;

  class.add_method(
    hooks.manage_str(METHOD_NAME.name),
    val!(MethodName::native(
      hooks,
      hooks.manage_str(METHOD_NAME.name)
    )),
  );

  class.add_method(
    hooks.manage_str(METHOD_CALL.name),
    val!(MethodCall::native(hooks)),
  );

  Ok(())
}

#[derive(Debug)]
struct MethodName {
  method_name: GcStr,
}

impl MethodName {
  fn native(hooks: &GcHooks, method_name: GcStr) -> GcObj<Native> {
    let native = Box::new(Self { method_name }) as Box<dyn LyNative>;

    hooks.manage_obj(Native::new(METHOD_NAME.build(hooks), native))
  }
}

impl LyNative for MethodName {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let method = args[0].to_obj().to_method().method();

    hooks
      .get_method(method, self.method_name)
      .and_then(|method_name| hooks.call_method(method, method_name, &[]))
  }
}

impl Trace for MethodName {
  fn trace(&self) {
    self.method_name.trace();
  }

  fn trace_debug(&self, stdout: &mut dyn Write) {
    self.method_name.trace_debug(stdout);
  }
}

native!(MethodCall, METHOD_CALL);

impl LyNative for MethodCall {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let method = args[0].to_obj().to_method();
    hooks.call_method(method.receiver(), method.method(), &args[1..])
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod name {
    use super::*;
    use crate::support::{test_fun, MockedContext};
    use laythe_core::{
      captures::Captures, managed::NO_GC, object::{Class, Closure, Method}
    };

    #[test]
    fn call() {
      let mut context = MockedContext::with_std(&[]).unwrap();
      let responses = &[val!(context.gc.borrow_mut().manage_str("example", &NO_GC))];
      context.responses.extend_from_slice(responses);

      let mut hooks = Hooks::new(&mut context);
      let method_name = MethodName::native(&hooks.as_gc(), hooks.manage_str("name".to_string()));

      let fun = test_fun(&hooks.as_gc(), "example", "module");
      let class = hooks.manage_obj(Class::bare(hooks.manage_str("exampleClass".to_string())));
      let captures = Captures::new(&hooks.as_gc(), &[]);
      let closure = hooks.manage_obj(Closure::new(fun, captures));
      let instance = hooks.manage_obj(class);
      let method = hooks.manage_obj(Method::new(val!(instance), val!(closure)));

      let result1 = method_name.call(&mut hooks, &[val!(method)]);

      match result1 {
        Call::Ok(r) => assert_eq!(&*r.to_obj().to_str(), "example"),
        _ => assert!(false),
      }
    }
  }

  mod call {
    use super::*;
    use crate::support::MockedContext;
    use laythe_core::object::{Class, Method};

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(14.3)]);
      let mut hooks = Hooks::new(&mut context);
      let method_call = MethodCall::native(&hooks.as_gc());

      let class = hooks.manage_obj(Class::bare(hooks.manage_str("exampleClass".to_string())));
      let native = val!(MethodName::native(
        &hooks.as_gc(),
        hooks.manage_str(METHOD_NAME.name)
      ));
      let instance = hooks.manage_obj(class);
      let method = hooks.manage_obj(Method::new(val!(instance), val!(native)));

      let result1 = method_call.call(&mut hooks, &[val!(method)]);

      match result1 {
        Call::Ok(r) => assert_eq!(r.to_num(), 14.3),
        _ => assert!(false),
      }
    }
  }
}
