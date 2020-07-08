mod utils;

use laythe_core::{hooks::GcHooks, module::Module, package::Package, LyResult};
use laythe_env::managed::Managed;
use std::path::PathBuf;
use utils::{declare_math_module, define_math_module};

const MATH_PATH: &str = "std/math.ly";

pub fn math_module(hooks: &GcHooks, mut _std: Managed<Package>) -> LyResult<Managed<Module>> {
  let mut module = hooks.manage(Module::from_path(
    &hooks,
    hooks.manage(PathBuf::from(MATH_PATH)),
  )?);

  declare_math_module(hooks, &mut module)?;
  define_math_module(hooks, &mut module)?;

  Ok(module)
}
