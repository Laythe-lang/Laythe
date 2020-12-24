mod utils;

use laythe_core::{hooks::GcHooks, module::Module, package::Package};
use laythe_env::managed::Gc;
use std::path::PathBuf;
use utils::{declare_env_module, define_env_module};

use crate::InitResult;

const ENV_PATH: &str = "std/env.ly";

pub fn env_module(hooks: &GcHooks, _std: &Package) -> InitResult<Gc<Module>> {
  let mut module = hooks.manage(Module::from_path(
    &hooks,
    hooks.manage(PathBuf::from(ENV_PATH)),
  )?);

  declare_env_module(hooks, &mut module)?;
  define_env_module(hooks, &mut module)?;

  Ok(module)
}
