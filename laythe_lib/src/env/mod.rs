mod utils;

use laythe_core::{hooks::GcHooks, module::Module, package::Package, LyResult};
use laythe_env::managed::Managed;
use std::path::PathBuf;
use utils::{declare_env_module, define_env_module};

const ENV_PATH: &str = "std/env.ly";

pub fn env_module(hooks: &GcHooks, mut _std: Managed<Package>) -> LyResult<Managed<Module>> {
  let mut module = hooks.manage(Module::from_path(
    &hooks,
    hooks.manage(PathBuf::from(ENV_PATH)),
  )?);

  declare_env_module(hooks, &mut module)?;
  define_env_module(hooks, &mut module)?;

  Ok(module)
}
