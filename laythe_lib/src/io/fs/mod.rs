mod file;

use file::{declare_file, define_file};
use laythe_core::{
  hooks::GcHooks,
  managed::Gc,
  module::{Module, Package},
  utils::IdEmitter,
};
use std::path::PathBuf;

use crate::{global::MODULE_CLASS_NAME, support::load_class_from_package, StdResult, STD};

const FS_PATH: &str = "std/io/fs";

pub fn fs_module(
  hooks: &GcHooks,
  std: Gc<Package>,
  emitter: &mut IdEmitter,
) -> StdResult<Gc<Module>> {
  let module_class = load_class_from_package(hooks, std, STD, MODULE_CLASS_NAME)?;

  let module = hooks.manage(Module::from_path(
    hooks,
    PathBuf::from(FS_PATH),
    module_class,
    emitter.emit(),
  )?);

  declare_file(hooks, module, std)?;
  define_file(hooks, module, std)?;

  Ok(module)
}
