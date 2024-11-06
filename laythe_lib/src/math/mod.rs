mod utils;

use crate::{global::MODULE_CLASS_NAME, support::load_class_from_package, StdResult, STD};
use laythe_core::{
  hooks::GcHooks,
  managed::Gc,
  module::{Module, Package},
  utils::IdEmitter, object::Class,
};
use utils::{declare_math_module, define_math_module};

const MATH_MODULE_NAME: &str = "math";

pub fn add_math_module(
  hooks: &GcHooks,
  std: Gc<Package>,
  emitter: &mut IdEmitter,
) -> StdResult<()> {
  let module_class = load_class_from_package(hooks, std, STD, MODULE_CLASS_NAME)?;
  let math_module_class =
    Class::with_inheritance(hooks, hooks.manage_str(MATH_MODULE_NAME), module_class);

  let module = hooks.manage(Module::new(hooks, math_module_class, &format!("native/{}", MATH_MODULE_NAME), emitter.emit()));

  let mut root = std.root_module();
  root.insert_module(module)?;

  declare_math_module(hooks, module)?;
  define_math_module(hooks, module)
}
