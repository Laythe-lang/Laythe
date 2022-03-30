mod file;

use crate::{global::MODULE_CLASS_NAME, support::load_class_from_package, StdResult, STD};
use file::{declare_file, define_file};
use laythe_core::{
  hooks::GcHooks,
  managed::Gc,
  module::{Module, Package},
  object::Class,
  utils::IdEmitter,
};

const FS_MODULE_NAME: &str = "fs";

pub fn fs_module(
  hooks: &GcHooks,
  std: Gc<Package>,
  emitter: &mut IdEmitter,
) -> StdResult<Gc<Module>> {
  let module_class = load_class_from_package(hooks, std, STD, MODULE_CLASS_NAME)?;
  let fs_module_class =
    Class::with_inheritance(hooks, hooks.manage_str(FS_MODULE_NAME), module_class);

  let module = hooks.manage(Module::new(fs_module_class, emitter.emit()));

  declare_file(hooks, module, std)?;
  define_file(hooks, module, std)?;

  Ok(module)
}
