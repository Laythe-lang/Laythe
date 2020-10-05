use crate::{
  native,
  support::{export_and_insert, to_dyn_native},
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{MetaData, Native, NativeMeta, NativeMetaBuilder},
  object::List,
  signature::Arity,
  val,
  value::Value,
  CallResult, LyResult,
};
use laythe_env::{managed::Trace};
use std::io::Write;

const ARGS_META: NativeMetaBuilder = NativeMetaBuilder::fun("args", Arity::Fixed(0));
const CWD_META: NativeMetaBuilder = NativeMetaBuilder::fun("cwd", Arity::Fixed(0));

pub fn declare_env_module(hooks: &GcHooks, self_module: &mut Module) -> LyResult<()> {
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

pub fn define_env_module(_: &GcHooks, _: &mut Module) -> LyResult<()> {
  Ok(())
}

native!(Args, ARGS_META);

impl Native for Args {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, _args: &[Value]) -> CallResult {
    let io = hooks.as_io();
    let vec: Vec<Value> = io
      .env()
      .args()
      .iter()
      .map(|arg| val!(hooks.manage_str(arg)))
      .collect();
    let list = hooks.manage(List::from(vec));

    Ok(val!(list))
  }
}

native!(Cwd, CWD_META);

impl Native for Cwd {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, _args: &[Value]) -> CallResult {
    let io = hooks.as_io();
    match io.env().current_dir() {
      Ok(path) => match path.to_str() {
        Some(path) => Ok(val!(hooks.manage_str(path))),
        None => hooks.error("Unable to create string from current working directory"),
      },
      Err(err) => hooks.error(err.to_string()),
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
