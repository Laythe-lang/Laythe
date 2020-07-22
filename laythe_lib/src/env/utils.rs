use crate::{native, support::{export_and_insert, to_dyn_native}};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{MetaData, Native, NativeMeta, NativeMetaBuilder},
  signature::{Arity},
  val,
  value::Value,
  object::List,
  CallResult, LyResult,
};
use laythe_env::{managed::Trace, stdio::Stdio};

const ARGS_META: NativeMetaBuilder = NativeMetaBuilder::fun("args", Arity::Fixed(0));

pub fn declare_env_module(hooks: &GcHooks, self_module: &mut Module) -> LyResult<()> {
  export_and_insert(
    hooks,
    self_module,
    hooks.manage_str(ARGS_META.name),
    val!(to_dyn_native(hooks, Args::from(hooks))),
  )
}

pub fn define_env_module(_: &GcHooks, _: &mut Module) -> LyResult<()> {
  Ok(())
}

native!(Args, ARGS_META);

impl Native for Args {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, _args: &[Value]) -> CallResult {
    let io = hooks.to_io();
    let vec: Vec<Value> = io.env().args().iter().map(|arg| val!(hooks.manage_str(arg))).collect();
    let list = hooks.manage(List::from(vec));

    Ok(val!(list))
  }
}
