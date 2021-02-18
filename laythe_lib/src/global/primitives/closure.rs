use crate::{
  native,
  support::{export_and_insert, load_class_from_module},
  StdResult,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  managed::GcObj,
  managed::Trace,
  module::Module,
  object::{LyNative, Native, NativeMetaBuilder},
  signature::{Arity, ParameterBuilder, ParameterKind},
  val,
  value::Value,
  Call,
};
use std::io::Write;

use super::class_inheritance;

pub const CLOSURE_CLASS_NAME: &str = "Fun";

const CLOSURE_NAME: NativeMetaBuilder = NativeMetaBuilder::method("name", Arity::Fixed(0));
const CLOSURE_LEN: NativeMetaBuilder = NativeMetaBuilder::method("len", Arity::Fixed(0));

const CLOSURE_CALL: NativeMetaBuilder = NativeMetaBuilder::method("call", Arity::Variadic(0))
  .with_params(&[ParameterBuilder::new("args", ParameterKind::Any)])
  .with_stack();

pub fn declare_closure_class(hooks: &GcHooks, module: &mut Module) -> StdResult<()> {
  let class = class_inheritance(hooks, module, CLOSURE_CLASS_NAME)?;
  export_and_insert(hooks, module, class.name(), val!(class))
}

pub fn define_closure_class(hooks: &GcHooks, module: &Module) -> StdResult<()> {
  let mut class = load_class_from_module(hooks, module, CLOSURE_CLASS_NAME)?;

  class.add_method(
    hooks,
    hooks.manage_str(CLOSURE_NAME.name),
    val!(ClosureName::native(hooks)),
  );

  class.add_method(
    hooks,
    hooks.manage_str(CLOSURE_LEN.name),
    val!(ClosureLen::native(hooks)),
  );

  class.add_method(
    hooks,
    hooks.manage_str(CLOSURE_CALL.name),
    val!(ClosureCall::native(hooks)),
  );

  Ok(())
}

native!(ClosureName, CLOSURE_NAME);

impl LyNative for ClosureName {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    Call::Ok(val!(this.unwrap().to_obj().to_closure().fun().name()))
  }
}

native!(ClosureLen, CLOSURE_LEN);

impl LyNative for ClosureLen {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    let req = match this.unwrap().to_obj().to_closure().fun().arity() {
      Arity::Default(req, _) => *req,
      Arity::Fixed(req) => *req,
      Arity::Variadic(req) => *req,
    };

    Call::Ok(val!(req as f64))
  }
}

native!(ClosureCall, CLOSURE_CALL);

impl LyNative for ClosureCall {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    hooks.call(this.unwrap(), args)
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::support::{fun_from_hooks, MockedContext};
  use laythe_core::object::Closure;

  mod name {
    use super::*;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let closure_name = ClosureName::native(&hooks);

      assert_eq!(closure_name.meta().name, "name");
      assert_eq!(closure_name.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let closure_name = ClosureName::native(&hooks.as_gc());

      let fun = fun_from_hooks(&hooks.as_gc(), "example", "module");
      let closure = hooks.manage_obj(Closure::without_upvalues(fun));

      let result1 = closure_name.call(&mut hooks, Some(val!(closure)), &[]);

      match result1 {
        Call::Ok(r) => assert_eq!(&*r.to_obj().to_str(), "example"),
        _ => assert!(false),
      }
    }
  }

  mod size {
    use super::*;
    use crate::support::{fun_builder_from_hooks, MockedContext};
    use laythe_core::object::Closure;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let closure_name = ClosureLen::native(&hooks);

      assert_eq!(closure_name.meta().name, "len");
      assert_eq!(closure_name.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let closure_name = ClosureLen::native(&hooks.as_gc());

      let mut builder = fun_builder_from_hooks(&hooks.as_gc(), "example", "module");
      builder.set_arity(Arity::Fixed(4));
      let closure = hooks.manage_obj(Closure::without_upvalues(hooks.manage_obj(builder.build())));

      let result = closure_name.call(&mut hooks, Some(val!(closure)), &[]);
      assert_eq!(result.unwrap().to_num(), 4.0);

      let mut builder = fun_builder_from_hooks(&hooks.as_gc(), "example", "module");
      builder.set_arity(Arity::Default(2, 2));
      let closure = hooks.manage_obj(Closure::without_upvalues(hooks.manage_obj(builder.build())));

      let result = closure_name.call(&mut hooks, Some(val!(closure)), &[]);
      assert_eq!(result.unwrap().to_num(), 2.0);

      let mut builder = fun_builder_from_hooks(&hooks.as_gc(), "example", "module");
      builder.set_arity(Arity::Variadic(5));
      let closure = hooks.manage_obj(Closure::without_upvalues(hooks.manage_obj(builder.build())));

      let result = closure_name.call(&mut hooks, Some(val!(closure)), &[]);
      assert_eq!(result.unwrap().to_num(), 5.0);
    }
  }

  mod call {
    use super::*;
    use crate::support::{fun_builder_from_hooks, MockedContext};
    use laythe_core::object::Closure;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let closure_call = ClosureCall::native(&hooks);

      assert_eq!(closure_call.meta().name, "call");
      assert_eq!(closure_call.meta().signature.arity, Arity::Variadic(0));
      assert_eq!(
        closure_call.meta().signature.parameters[0].kind,
        ParameterKind::Any
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(4.3)]);
      let mut hooks = Hooks::new(&mut context);
      let closure_call = ClosureCall::native(&hooks.as_gc());

      let mut builder = fun_builder_from_hooks(&hooks.as_gc(), "example", "module");
      builder.set_arity(Arity::Fixed(1));

      let closure = hooks.manage_obj(Closure::without_upvalues(hooks.manage_obj(builder.build())));

      let args = &[val!(hooks.manage_str("input".to_string()))];
      let result1 = closure_call.call(&mut hooks, Some(val!(closure)), args);

      assert_eq!(result1.unwrap().to_num(), 4.3);
    }
  }
}
