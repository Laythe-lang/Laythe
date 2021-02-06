use crate::{
  native,
  support::{export_and_insert, to_dyn_native},
  StdResult,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  managed::{Gc, Trace},
  module::Module,
  object::{List, MetaData, Native, NativeMeta, NativeMetaBuilder},
  signature::Arity,
  val,
  value::Value,
  Call,
};
use std::io::Write;

const ARGS_META: NativeMetaBuilder = NativeMetaBuilder::fun("args", Arity::Fixed(0));
const CWD_META: NativeMetaBuilder = NativeMetaBuilder::fun("cwd", Arity::Fixed(0));

pub fn declare_env_module(hooks: &GcHooks, self_module: &mut Module) -> StdResult<()> {
  export_and_insert(
    hooks,
    self_module,
    hooks.manage_str(ARGS_META.name),
    val!(to_dyn_native(hooks, Args::from(hooks))),
  )?;

  export_and_insert(
    hooks,
    self_module,
    hooks.manage_str(CWD_META.name),
    val!(to_dyn_native(hooks, Cwd::from(hooks))),
  )
}

pub fn define_env_module(_: &GcHooks, _: &mut Module) -> StdResult<()> {
  Ok(())
}

native!(Args, ARGS_META);

impl Native for Args {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, _args: &[Value]) -> Call {
    let io = hooks.as_io();
    let mut list: Gc<List<Value>> = hooks.manage(List::new());
    hooks.push_root(list);

    for arg in io.env().args() {
      let arg = val!(hooks.manage_str(arg));
      hooks.grow(&mut list, |list| list.push(arg));
    }

    hooks.pop_roots(1);
    Call::Ok(val!(list))
  }
}

native!(Cwd, CWD_META);

impl Native for Cwd {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, _args: &[Value]) -> Call {
    let io = hooks.as_io();
    match io.env().current_dir() {
      Ok(path) => match path.to_str() {
        Some(path) => Call::Ok(val!(hooks.manage_str(path))),
        None => panic!("TODO: Unable to create string from current working directory"),
      },
      Err(err) => panic!(err.to_string()),
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod args {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = Hooks::new(&mut context);

      let stdout_write = Args::from(&hooks);

      assert_eq!(stdout_write.meta().name, "args");
      assert_eq!(stdout_write.meta().signature.arity, Arity::Fixed(0));
    }

    // TODO call
  }

  mod cwd {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = Hooks::new(&mut context);

      let stdout_write = Cwd::from(&hooks);

      assert_eq!(stdout_write.meta().name, "cwd");
      assert_eq!(stdout_write.meta().signature.arity, Arity::Fixed(0));
    }

    // TODO call
  }
}
