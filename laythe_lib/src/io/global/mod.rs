use laythe_core::{
  hooks::GcHooks,
  module::{Module, Package},
  object::Class,
  val, Ref,
};
use laythe_core::{utils::IdEmitter};

use crate::{
  global::MODULE_CLASS_NAME,
  support::{default_error_inheritance, export_and_insert, load_class_from_package},
  StdResult, STD,
};

use super::IO_MODULE_NAME;

pub const IO_ERROR: &str = "IoError";

pub fn io_module(
  hooks: &GcHooks,
  std: Ref<Package>,
  emitter: &mut IdEmitter,
) -> StdResult<Ref<Module>> {
  let module_class = load_class_from_package(hooks, std, STD, MODULE_CLASS_NAME)?;
  let io_module_class =
    Class::with_inheritance(hooks, hooks.manage_str(IO_MODULE_NAME), module_class);

  let module = hooks.manage(Module::new(
    hooks,
    io_module_class,
    &format!("native/{IO_MODULE_NAME}"),
    emitter.emit(),
  ));

  declare_io_errors(hooks, module, std)?;
  define_io_errors(hooks, module, std)?;

  Ok(module)
}

pub fn declare_io_errors(
  hooks: &GcHooks,
  module: Ref<Module>,
  package: Ref<Package>,
) -> StdResult<()> {
  let io_error = default_error_inheritance(hooks, package, IO_ERROR)?;

  export_and_insert(hooks, module, io_error.name(), val!(io_error))
}

pub fn define_io_errors(
  _hooks: &GcHooks,
  _module: Ref<Module>,
  _package: Ref<Package>,
) -> StdResult<()> {
  Ok(())
}
