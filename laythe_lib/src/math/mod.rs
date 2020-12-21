mod utils;

use laythe_core::{hooks::GcHooks, module::Module, package::Package};
use std::path::PathBuf;
use utils::{declare_math_module, define_math_module};

use crate::InitResult;

const MATH_PATH: &str = "std/math.ly";

pub fn add_math_module(hooks: &GcHooks, std: &mut Package) -> InitResult<()> {
  let mut module = hooks.manage(Module::from_path(
    &hooks,
    hooks.manage(PathBuf::from(MATH_PATH)),
  )?);

  std.add_module(hooks, module)?;

  declare_math_module(hooks, &mut module)?;
  define_math_module(hooks, &mut module)
}
