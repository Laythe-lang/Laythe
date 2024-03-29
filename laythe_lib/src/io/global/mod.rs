use laythe_core::{
  hooks::GcHooks,
  module::{Module, Package},
  object::Class,
  val,
};
use laythe_core::{managed::Gc, utils::IdEmitter, value::Value};

use crate::{
  global::MODULE_CLASS_NAME,
  support::{default_error_inheritance, export_and_insert, load_class_from_package},
  StdResult, STD,
};

use super::IO_MODULE_NAME;

pub const IO_ERROR: &str = "IoError";

pub fn io_module(
  hooks: &GcHooks,
  std: Gc<Package>,
  emitter: &mut IdEmitter,
) -> StdResult<Gc<Module>> {
  let module_class = load_class_from_package(hooks, std, STD, MODULE_CLASS_NAME)?;
  let io_module_class =
    Class::with_inheritance(hooks, hooks.manage_str(IO_MODULE_NAME), module_class);

  let module_path = hooks.manage_str(format!("native/{}", IO_MODULE_NAME));
  let module = hooks.manage(Module::new(io_module_class, module_path, emitter.emit()));

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

  export_and_insert(module, io_error.name(), val!(io_error))
}

pub fn define_io_errors(
  _hooks: &GcHooks,
  _module: Gc<Module>,
  _package: Gc<Package>,
) -> StdResult<()> {
  Ok(())
}
