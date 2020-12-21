use laythe_core::value::Value;
use laythe_core::{hooks::GcHooks, module::Module, package::Package, val};
use laythe_env::managed::Managed;
use std::path::PathBuf;

use crate::{
  support::{default_error_inheritance, export_and_insert},
  InitResult,
};

pub const ERROR_PATH: &str = "std/io/global.ly";
pub const IO_ERROR: &str = "IoError";

pub fn errors_module(hooks: &GcHooks, std: &Package) -> InitResult<Managed<Module>> {
  let mut module = hooks.manage(Module::from_path(
    &hooks,
    hooks.manage(PathBuf::from(ERROR_PATH)),
  )?);

  declare_io_errors(hooks, &mut module, std)?;
  define_io_errors(hooks, &mut module, std)?;

  Ok(module)
}

pub fn declare_io_errors(
  hooks: &GcHooks,
  module: &mut Module,
  package: &Package,
) -> InitResult<()> {
  let io_error = default_error_inheritance(hooks, package, IO_ERROR)?;

  export_and_insert(hooks, module, io_error.name, val!(io_error))
}

pub fn define_io_errors(_hooks: &GcHooks, _module: &Module, _: &Package) -> InitResult<()> {
  Ok(())
}
