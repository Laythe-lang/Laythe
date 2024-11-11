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

pub const FUN_CLASS_NAME: &str = "Fun";

const FUN_NAME: NativeMetaBuilder = NativeMetaBuilder::method("name", Arity::Fixed(0));
const FUN_LEN: NativeMetaBuilder = NativeMetaBuilder::method("len", Arity::Fixed(0));

const FUN_CALL: NativeMetaBuilder = NativeMetaBuilder::method("call", Arity::Variadic(0))
  .with_params(&[ParameterBuilder::new("args", ParameterKind::Object)])
  .with_stack();

pub fn declare_fun_class(hooks: &GcHooks, module: Ref<Module>) -> StdResult<()> {
  let class = class_inheritance(hooks, module, FUN_CLASS_NAME)?;
  export_and_insert(hooks, module, class.name(), val!(class))
}

pub fn define_fun_class(hooks: &GcHooks, module: Ref<Module>) -> StdResult<()> {
  let mut class = load_class_from_module(hooks, module, FUN_CLASS_NAME)?;

  class.add_method(
    hooks.manage_str(FUN_NAME.name),
    val!(FunName::native(hooks)),
  );

  class.add_method(hooks.manage_str(FUN_LEN.name), val!(FunLen::native(hooks)));

  class.add_method(
    hooks.manage_str(FUN_CALL.name),
    val!(FunCall::native(hooks)),
  );

  Ok(())
}

native!(FunName, FUN_NAME);

impl LyNative for FunName {
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
    Call::Ok(val!(args[0].to_obj().to_fun().name()))
  }
}

native!(FunLen, FUN_LEN);

impl LyNative for FunLen {
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
    let req = args[0].to_obj().to_fun().parameter_count();

    Call::Ok(val!(req as f64))
  }
}

native!(FunCall, FUN_CALL);

impl LyNative for FunCall {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    hooks.call(args[0], &args[1..])
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::support::{test_fun, MockedContext};

  mod name {
    use super::*;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let closure_name = FunName::native(&hooks.as_gc());

      let fun = test_fun(&hooks.as_gc(), "example", "module");
      let result1 = closure_name.call(&mut hooks, &[val!(fun)]);

      match result1 {
        Call::Ok(r) => assert_eq!(&*r.to_obj().to_str(), "example"),
        _ => panic!(),
      }
    }
  }

  mod len {
    use laythe_core::Chunk;

    use super::*;
    use crate::support::{test_fun_builder, MockedContext};

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let fun_name = FunLen::native(&hooks.as_gc());

      let builder = test_fun_builder(&hooks.as_gc(), "example", "module", Arity::Fixed(4));
      let fun = hooks.manage_obj(builder.build(Chunk::stub(&hooks.as_gc())));

      let result = fun_name.call(&mut hooks, &[val!(fun)]);
      assert_eq!(result.unwrap().to_num(), 4.0);

      let builder =
        test_fun_builder(&hooks.as_gc(), "example", "module", Arity::Default(2, 2));
      let fun = hooks.manage_obj(builder.build(Chunk::stub(&hooks.as_gc())));

      let result = fun_name.call(&mut hooks, &[val!(fun)]);
      assert_eq!(result.unwrap().to_num(), 2.0);

      let builder = test_fun_builder(&hooks.as_gc(), "example", "module", Arity::Variadic(5));
      let fun = hooks.manage_obj(builder.build(Chunk::stub(&hooks.as_gc())));

      let result = fun_name.call(&mut hooks, &[val!(fun)]);
      assert_eq!(result.unwrap().to_num(), 5.0);
    }
  }

  mod call {
    use laythe_core::Chunk;

    use super::*;
    use crate::support::{test_fun_builder, MockedContext};

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(4.3)]);
      let mut hooks = Hooks::new(&mut context);
      let fun_call = FunCall::native(&hooks.as_gc());

      let builder = test_fun_builder(&hooks.as_gc(), "example", "module", Arity::Fixed(1));

      let fun = hooks.manage_obj(builder.build(Chunk::stub(&hooks.as_gc())));

      let args = &[val!(fun), val!(hooks.manage_str("input"))];
      let result1 = fun_call.call(&mut hooks, args);

      assert_eq!(result1.unwrap().to_num(), 4.3);
    }
  }
}
