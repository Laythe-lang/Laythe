use crate::{native, support::export_and_insert_native, StdResult};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  list,
  managed::Trace,
  module::Module,
  object::{List, LyNative, Native, NativeMetaBuilder},
  signature::Arity,
  val,
  value::Value,
  Call, Ref,
};
use std::io::Write;

const ARGS_META: NativeMetaBuilder = NativeMetaBuilder::fun("args", Arity::Fixed(0));
const CWD_META: NativeMetaBuilder = NativeMetaBuilder::fun("cwd", Arity::Fixed(0));

pub fn declare_env_module(hooks: &GcHooks, self_module: Ref<Module>) -> StdResult<()> {
  export_and_insert_native(hooks, self_module, Args::native(hooks))?;
  export_and_insert_native(hooks, self_module, Cwd::native(hooks))
}

pub fn define_env_module(_: &GcHooks, _: &mut Module) -> StdResult<()> {
  Ok(())
}

native!(Args, ARGS_META);

impl LyNative for Args {
  fn call(&self, hooks: &mut Hooks, _args: &[Value]) -> Call {
    let io = hooks.as_io();
    let mut list = List::new(hooks.manage_obj(list!()));
    hooks.push_root(list);

    for arg in io.env().args() {
      let arg = val!(hooks.manage_str(arg));
      list.push(arg, &hooks.as_gc());
    }

    hooks.pop_roots(1);
    Call::Ok(val!(list))
  }
}

native!(Cwd, CWD_META);

impl LyNative for Cwd {
  fn call(&self, hooks: &mut Hooks, _args: &[Value]) -> Call {
    let io = hooks.as_io();
    match io.env().current_dir() {
      Ok(path) => match path.to_str() {
        Some(path) => Call::Ok(val!(hooks.manage_str(path))),
        None => panic!("TODO: Unable to create string from current working directory"),
      },
      Err(err) => panic!("{}", err),
    }
  }
}

#[cfg(test)]
mod test {
  mod args {
    // TODO: call
  }

  mod cwd {
    // TODO: call
  }
}
