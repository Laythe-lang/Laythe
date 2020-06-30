mod utils;

use laythe_core::{hooks::GcHooks, module::Module, package::Package, LyResult};
use laythe_env::managed::Managed;
use std::path::PathBuf;
use utils::{declare_math_module, define_math_module};

const MATH_PATH: &str = "std/math.ly";

pub fn add_math(hooks: &GcHooks, mut std: Managed<Package>) -> LyResult<()> {
  let module = match Module::from_path(&hooks, hooks.manage(PathBuf::from(MATH_PATH))) {
    Some(module) => module,
    None => {
      return Err(hooks.make_error("Could not create math module, path malformed.".to_string()));
    }
  };

  let mut module = hooks.manage(module);
  std.add_module(hooks, module)?;

  declare_math_module(hooks, &mut module)?;
  define_math_module(hooks, &mut module)?;

  Ok(())
}
