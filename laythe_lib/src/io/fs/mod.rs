mod utils;

use crate::{global::MODULE_CLASS_NAME, support::load_class_from_package, StdResult, STD};
use laythe_core::{
  hooks::GcHooks,
  module::{Module, Package},
  object::Class,
  utils::IdEmitter, Ref,
};
use utils::{declare_fs_module, define_fs_module};

use super::IO_MODULE_NAME;

const FS_MODULE_NAME: &str = "fs";

pub fn fs_module(
  hooks: &GcHooks,
  std: Ref<Package>,
  emitter: &mut IdEmitter,
) -> StdResult<Ref<Module>> {
  let module_class = load_class_from_package(hooks, std, STD, MODULE_CLASS_NAME)?;
  let fs_module_class =
    Class::with_inheritance(hooks, hooks.manage_str(FS_MODULE_NAME), module_class);

  let module = hooks.manage(Module::new(
    hooks,
    fs_module_class,
    &format!("native/{}/{}", IO_MODULE_NAME, FS_MODULE_NAME),
    emitter.emit(),
  ));

  declare_fs_module(hooks, module, std)?;
  define_fs_module(hooks, module, std)?;

  Ok(module)
}
