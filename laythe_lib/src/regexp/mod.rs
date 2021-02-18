mod class;

use laythe_core::{
  hooks::GcHooks,
  managed::Gc,
  module::{Module, Package},
  utils::IdEmitter,
};
use std::path::PathBuf;

use crate::{global::MODULE_CLASS_NAME, support::load_class_from_package, StdResult, STD};

use self::class::{declare_regexp_class, define_regexp_class};

const REGEXP_PATH: &str = "std/regexp";

pub fn regexp_module(
  hooks: &GcHooks,
  std: &Package,
  emitter: &mut IdEmitter,
) -> StdResult<Gc<Module>> {
  let module_class = load_class_from_package(hooks, std, STD, MODULE_CLASS_NAME)?;

  let mut module = hooks.manage(Module::from_path(
    &hooks,
    PathBuf::from(REGEXP_PATH),
    module_class,
    emitter.emit(),
  )?);

  declare_regexp_class(hooks, &mut module, std)?;
  define_regexp_class(hooks, &module, std)?;

  Ok(module)
}
