mod utils;

use laythe_core::{
  hooks::GcHooks,
  module::{Module, Package},
  utils::IdEmitter,
};
use laythe_env::managed::Gc;
use std::path::PathBuf;
use utils::{declare_env_module, define_env_module};

use crate::StdResult;

const ENV_PATH: &str = "std/env";

pub fn env_module(
  hooks: &GcHooks,
  _std: &Package,
  emitter: &mut IdEmitter,
) -> StdResult<Gc<Module>> {
  let mut module = hooks.manage(Module::from_path(
    &hooks,
    PathBuf::from(ENV_PATH),
    emitter.emit(),
  )?);

  declare_env_module(hooks, &mut module)?;
  define_env_module(hooks, &mut module)?;

  Ok(module)
}
