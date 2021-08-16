use laythe_core::{
  hooks::GcHooks,
  module::{Module, Package},
  val,
};
use laythe_core::{managed::Gc, utils::IdEmitter, value::Value};
use std::path::PathBuf;

use crate::{
  global::MODULE_CLASS_NAME,
  support::{default_error_inheritance, export_and_insert, load_class_from_package},
  StdResult, STD,
};

use super::IO_MODULE_PATH;

pub const IO_ERROR: &str = "IoError";

pub fn io_module(
  hooks: &GcHooks,
  std: Gc<Package>,
  emitter: &mut IdEmitter,
) -> StdResult<Gc<Module>> {
  let module_class = load_class_from_package(hooks, std, STD, MODULE_CLASS_NAME)?;

  let module = hooks.manage(Module::from_path(
    hooks,
    PathBuf::from(IO_MODULE_PATH),
    module_class,
    emitter.emit(),
  )?);

  declare_io_errors(hooks, module, std)?;
  define_io_errors(hooks, module, std)?;

  Ok(module)
}

pub fn declare_io_errors(
  hooks: &GcHooks,
  module: Gc<Module>,
  package: Gc<Package>,
) -> StdResult<()> {
  let io_error = default_error_inheritance(hooks, package, IO_ERROR)?;

  export_and_insert(hooks, module, io_error.name(), val!(io_error))
}

pub fn define_io_errors(_hooks: &GcHooks, _module: Gc<Module>, _package: Gc<Package>) -> StdResult<()> {
  Ok(())
}
