use laythe_core::{
  hooks::GcHooks,
  module::{Module, Package},
  val,
};
use laythe_core::{utils::IdEmitter, value::Value};
use laythe_env::managed::Gc;
use std::path::PathBuf;

use crate::{
  support::{default_error_inheritance, export_and_insert},
  StdResult,
};

use super::IO_MODULE_PATH;

pub const IO_ERROR: &str = "IoError";

pub fn io_module(hooks: &GcHooks, std: &Package, emitter: &mut IdEmitter) -> StdResult<Gc<Module>> {
  let mut module = hooks.manage(Module::from_path(
    &hooks,
    PathBuf::from(IO_MODULE_PATH),
    emitter.emit(),
  )?);

  declare_io_errors(hooks, &mut module, std)?;
  define_io_errors(hooks, &module, std)?;

  Ok(module)
}

pub fn declare_io_errors(hooks: &GcHooks, module: &mut Module, package: &Package) -> StdResult<()> {
  let io_error = default_error_inheritance(hooks, package, IO_ERROR)?;

  export_and_insert(hooks, module, io_error.name(), val!(io_error))
}

pub fn define_io_errors(_hooks: &GcHooks, _module: &Module, _: &Package) -> StdResult<()> {
  Ok(())
}
