mod utils;

use laythe_core::{
  hooks::GcHooks,
  module::{Module, Package},
  utils::IdEmitter,
};
use std::path::PathBuf;
use utils::{declare_math_module, define_math_module};

use crate::StdResult;

const MATH_PATH: &str = "std/math";

pub fn add_math_module(
  hooks: &GcHooks,
  std: &mut Package,
  emitter: &mut IdEmitter,
) -> StdResult<()> {
  let mut module = hooks.manage(Module::from_path(
    &hooks,
    PathBuf::from(MATH_PATH),
    emitter.emit(),
  )?);

  let mut root = std.root_module();
  root.insert_module(hooks, module)?;

  declare_math_module(hooks, &mut module)?;
  define_math_module(hooks, &mut module)
}
