mod utils;

use spacelox_core::{hooks::GcHooks, module::Module, package::Package, ModuleResult};
use spacelox_env::managed::Managed;
use std::path::PathBuf;
use utils::{declare_math_module, define_math_module};

const MATH_PATH: &str = "std/math.lox";

pub fn create_math(hooks: &GcHooks, _std: Managed<Package>) -> ModuleResult<Managed<Module>> {
  let module = match Module::from_path(&hooks, hooks.manage(PathBuf::from(MATH_PATH))) {
    Some(module) => module,
    None => {
      return Err(hooks.make_error("Could not create math module, path malformed.".to_string()));
    }
  };

  let mut math = hooks.manage(module);

  declare_math_module(hooks, &mut math)?;
  define_math_module(hooks, &mut math)?;

  Ok(math)
}
