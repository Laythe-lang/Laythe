use crate::{
  native,
  support::{default_class_inheritance, export_and_insert, load_class_from_module, to_dyn_native},
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{MetaData, Native, NativeMeta, NativeMetaBuilder},
  package::Package,
  signature::{Arity, ParameterBuilder, ParameterKind},
  val,
  value::Value,
  CallResult, LyResult,
};
use laythe_env::managed::Trace;
use std::io::Write;

pub const CLOSURE_CLASS_NAME: &str = "Fun";

const CLOSURE_NAME: NativeMetaBuilder = NativeMetaBuilder::method("name", Arity::Fixed(0));
const CLOSURE_LEN: NativeMetaBuilder = NativeMetaBuilder::method("len", Arity::Fixed(0));

const CLOSURE_CALL: NativeMetaBuilder = NativeMetaBuilder::method("call", Arity::Variadic(0))
  .with_params(&[ParameterBuilder::new("args", ParameterKind::Any)])
  .with_stack();

pub fn declare_closure_class(
  hooks: &GcHooks,
  module: &mut Module,
  package: &Package,
) -> LyResult<()> {
  let class = default_class_inheritance(hooks, package, CLOSURE_CLASS_NAME)?;
  export_and_insert(hooks, module, class.name, val!(class))
}

pub fn define_closure_class(hooks: &GcHooks, module: &Module, _: &Package) -> LyResult<()> {
  let mut class = load_class_from_module(hooks, module, CLOSURE_CLASS_NAME)?;

  class.add_method(
    hooks,
    hooks.manage_str(CLOSURE_NAME.name),
    val!(to_dyn_native(hooks, ClosureName::from(hooks),)),
  );

  class.add_method(
    hooks,
    hooks.manage_str(CLOSURE_LEN.name),
    val!(to_dyn_native(hooks, ClosureLen::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(CLOSURE_CALL.name),
    val!(to_dyn_native(hooks, ClosureCall::from(hooks))),
  );

  Ok(())
}

native!(ClosureName, CLOSURE_NAME);

impl Native for ClosureName {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> CallResult {
    Ok(val!(this.unwrap().to_closure().fun.name))
  }
}

native!(ClosureLen, CLOSURE_LEN);

impl Native for ClosureLen {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> CallResult {
    let req = match this.unwrap().to_closure().fun.arity {
      Arity::Default(req, _) => req,
      Arity::Fixed(req) => req,
      Arity::Variadic(req) => req,
    };

    Ok(val!(req as f64))
  }
}

native!(ClosureCall, CLOSURE_CALL);

impl Native for ClosureCall {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> CallResult {
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

      let closure_name = ClosureName::from(&hooks);

      assert_eq!(closure_name.meta().name, "name");
      assert_eq!(closure_name.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let closure_name = ClosureName::from(&hooks);

      let fun = fun_from_hooks(&hooks.as_gc(), "example", "module");
      let closure = hooks.manage(Closure::new(fun));

      let result1 = closure_name.call(&mut hooks, Some(val!(closure)), &[]);

      match result1 {
        Ok(r) => assert_eq!(&*r.to_str(), "example"),
        Err(_) => assert!(false),
      }
    }
  }

  mod size {
    use super::*;
    use crate::support::{fun_from_hooks, MockedContext};
    use laythe_core::object::Closure;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let closure_name = ClosureLen::from(&hooks);

      assert_eq!(closure_name.meta().name, "len");
      assert_eq!(closure_name.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let closure_name = ClosureLen::from(&hooks);

      let mut fun = fun_from_hooks(&hooks.as_gc(), "example", "module");
      fun.arity = Arity::Fixed(4);

      let closure = hooks.manage(Closure::new(fun));

      let result = closure_name.call(&mut hooks, Some(val!(closure)), &[]);
      match result {
        Ok(r) => assert_eq!(r.to_num(), 4.0),
        Err(_) => assert!(false),
      }

      fun.arity = Arity::Default(2, 2);
      let result = closure_name.call(&mut hooks, Some(val!(closure)), &[]);
      match result {
        Ok(r) => assert_eq!(r.to_num(), 2.0),
        Err(_) => assert!(false),
      }

      fun.arity = Arity::Variadic(5);
      let result = closure_name.call(&mut hooks, Some(val!(closure)), &[]);
      match result {
        Ok(r) => assert_eq!(r.to_num(), 5.0),
        Err(_) => assert!(false),
      }
    }
  }

  mod call {
    use super::*;
    use crate::support::{fun_from_hooks, MockedContext};
    use laythe_core::object::Closure;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let closure_call = ClosureCall::from(&hooks);

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
      let closure_call = ClosureCall::from(&hooks.as_gc());

      let mut fun = fun_from_hooks(&hooks.as_gc(), "example", "module");
      fun.arity = Arity::Fixed(1);

      let closure = hooks.manage(Closure::new(fun));

      let args = &[val!(hooks.manage_str("input".to_string()))];
      let result1 = closure_call.call(&mut hooks, Some(val!(closure)), args);

      match result1 {
        Ok(r) => assert_eq!(r.to_num(), 4.3),
        Err(_) => assert!(false),
      }
    }
  }
}
