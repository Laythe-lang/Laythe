mod utils;

use laythe_core::{hooks::GcHooks, module::Module, package::Package, utils::IdEmitter};
use laythe_env::managed::Gc;
use std::path::PathBuf;
use utils::{declare_env_module, define_env_module};

use crate::InitResult;

const ENV_PATH: &str = "std/env.ly";

pub fn env_module(hooks: &GcHooks, _std: &Package, emitter: &mut IdEmitter) -> InitResult<Gc<Module>> {
  let mut module = hooks.manage(Module::from_path(
    &hooks,
    PathBuf::from(ENV_PATH),
    emitter.emit(),
  )?);

  declare_env_module(hooks, &mut module)?;
  define_env_module(hooks, &mut module)?;

  Ok(module)
}
