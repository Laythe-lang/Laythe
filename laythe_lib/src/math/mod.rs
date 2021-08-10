mod utils;

use laythe_core::{
  hooks::GcHooks,
  module::{Module, Package},
  utils::IdEmitter,
};
use std::path::PathBuf;
use utils::{declare_math_module, define_math_module};

use crate::{global::MODULE_CLASS_NAME, support::load_class_from_package, StdResult, STD};

const MATH_PATH: &str = "std/math";

pub fn add_math_module(
  hooks: &GcHooks,
  std: &mut Package,
  emitter: &mut IdEmitter,
) -> StdResult<()> {
  let module_class = load_class_from_package(hooks, std, STD, MODULE_CLASS_NAME)?;

  let mut module = hooks.manage(Module::from_path(
    hooks,
    PathBuf::from(MATH_PATH),
    module_class,
    emitter.emit(),
  )?);

  let mut root = std.root_module();
  root.insert_module(hooks, module)?;

  declare_math_module(hooks, &mut module)?;
  define_math_module(hooks, &mut module)
}
