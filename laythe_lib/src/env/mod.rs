mod utils;

use laythe_core::{
  hooks::GcHooks,
  managed::Gc,
  module::{Module, Package},
  utils::IdEmitter,
};
use std::path::PathBuf;
use utils::{declare_env_module, define_env_module};

use crate::{global::MODULE_CLASS_NAME, support::load_class_from_package, StdResult, STD};

const ENV_PATH: &str = "std/env";

pub fn env_module(
  hooks: &GcHooks,
  std: &Package,
  emitter: &mut IdEmitter,
) -> StdResult<Gc<Module>> {
  let module_class = load_class_from_package(hooks, std, STD, MODULE_CLASS_NAME)?;

  let mut module = hooks.manage(Module::from_path(
    &hooks,
    PathBuf::from(ENV_PATH),
    module_class,
    emitter.emit(),
  )?);

  declare_env_module(hooks, &mut module)?;
  define_env_module(hooks, &mut module)?;

  Ok(module)
}
