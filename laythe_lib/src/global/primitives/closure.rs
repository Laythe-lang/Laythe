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

pub const CLOSURE_CLASS_NAME: &str = "Closure";

const CLOSURE_NAME: NativeMetaBuilder = NativeMetaBuilder::method("name", Arity::Fixed(0));
const CLOSURE_LEN: NativeMetaBuilder = NativeMetaBuilder::method("len", Arity::Fixed(0));

const CLOSURE_CALL: NativeMetaBuilder = NativeMetaBuilder::method("call", Arity::Variadic(0))
  .with_params(&[ParameterBuilder::new("args", ParameterKind::Object)])
  .with_stack();

pub fn declare_closure_class(hooks: &GcHooks, module: Gc<Module>) -> StdResult<()> {
  let class = class_inheritance(hooks, module, CLOSURE_CLASS_NAME)?;
  export_and_insert(hooks, module, CLOSURE_CLASS_NAME, val!(class))
}

pub fn define_closure_class(hooks: &GcHooks, module: Gc<Module>) -> StdResult<()> {
  let mut class = load_class_from_module(hooks, module, CLOSURE_CLASS_NAME)?;

  class.add_method(
    hooks.manage_str(CLOSURE_NAME.name),
    val!(ClosureName::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(CLOSURE_LEN.name),
    val!(ClosureLen::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(CLOSURE_CALL.name),
    val!(ClosureCall::native(hooks)),
  );

  Ok(())
}

native!(ClosureName, CLOSURE_NAME);

impl LyNative for ClosureName {
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
    Call::Ok(val!(args[0].to_obj().to_closure().fun().name()))
  }
}

native!(ClosureLen, CLOSURE_LEN);

impl LyNative for ClosureLen {
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
    let req = args[0].to_obj().to_closure().fun().parameter_count();

    Call::Ok(val!(req as f64))
  }
}

native!(ClosureCall, CLOSURE_CALL);

impl LyNative for ClosureCall {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    hooks.call(args[0], &args[1..])
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::support::{test_fun, MockedContext};
  use laythe_core::object::Closure;

  mod name {
    use laythe_core::captures::Captures;

    use super::*;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let closure_name = ClosureName::native(&hooks.as_gc());

      let fun = test_fun(&hooks.as_gc(), "example", "module");
      let captures = Captures::new(&hooks.as_gc(), &[]);
      let closure = hooks.manage_obj(Closure::new(fun, captures));

      let result1 = closure_name.call(&mut hooks, &[val!(closure)]);

      match result1 {
        Call::Ok(r) => assert_eq!(&*r.to_obj().to_str(), "example"),
        _ => panic!(),
      }
    }
  }

  mod size {
    use super::*;
    use crate::support::{test_fun_builder, MockedContext};
    use laythe_core::{captures::Captures, chunk::Chunk, object::Closure};

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let closure_name = ClosureLen::native(&hooks.as_gc());

      let captures = Captures::new(&hooks.as_gc(), &[]);

      let builder = test_fun_builder(&hooks.as_gc(), "example", "module", Arity::Fixed(4));
      let closure = hooks.manage_obj(Closure::new(
        hooks.manage_obj(builder.build(Chunk::stub(&hooks.as_gc()))),
        captures,
      ));

      let result = closure_name.call(&mut hooks, &[val!(closure)]);
      assert_eq!(result.unwrap().to_num(), 4.0);

      let builder = test_fun_builder(&hooks.as_gc(), "example", "module", Arity::Default(2, 2));
      let closure = hooks.manage_obj(Closure::new(
        hooks.manage_obj(builder.build(Chunk::stub(&hooks.as_gc()))),
        captures,
      ));

      let result = closure_name.call(&mut hooks, &[val!(closure)]);
      assert_eq!(result.unwrap().to_num(), 2.0);

      let builder = test_fun_builder(&hooks.as_gc(), "example", "module", Arity::Variadic(5));
      let closure = hooks.manage_obj(Closure::new(
        hooks.manage_obj(builder.build(Chunk::stub(&hooks.as_gc()))),
        captures,
      ));

      let result = closure_name.call(&mut hooks, &[val!(closure)]);
      assert_eq!(result.unwrap().to_num(), 5.0);
    }
  }

  mod call {
    use super::*;
    use crate::support::{test_fun_builder, MockedContext};
    use laythe_core::{captures::Captures, chunk::Chunk, object::Closure};

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(4.3)]);
      let mut hooks = Hooks::new(&mut context);
      let closure_call = ClosureCall::native(&hooks.as_gc());

      let builder = test_fun_builder(&hooks.as_gc(), "example", "module", Arity::Fixed(1));
      let captures = Captures::new(&hooks.as_gc(), &[]);

      let closure = hooks.manage_obj(Closure::new(
        hooks.manage_obj(builder.build(Chunk::stub(&hooks.as_gc()))),
        captures,
      ));

      let args = &[val!(closure), val!(hooks.manage_str("input"))];
      let result1 = closure_call.call(&mut hooks, args);

      assert_eq!(result1.unwrap().to_num(), 4.3);
    }
  }
}
