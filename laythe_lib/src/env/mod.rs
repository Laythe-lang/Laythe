mod utils;
use crate::{global::MODULE_CLASS_NAME, support::load_class_from_package, StdResult, STD};
use laythe_core::{
  hooks::GcHooks,
  managed::Gc,
  module::{Module, Package},
  object::Class,
  utils::IdEmitter,
};
use utils::{declare_env_module, define_env_module};

const ENV_MODULE_NAME: &str = "env";

pub fn env_module(
  hooks: &GcHooks,
  std: Gc<Package>,
  emitter: &mut IdEmitter,
) -> StdResult<Gc<Module>> {
  let module_class = load_class_from_package(hooks, std, STD, MODULE_CLASS_NAME)?;
  let env_module_class =
    Class::with_inheritance(hooks, hooks.manage_str(ENV_MODULE_NAME), module_class);

  let module_path = hooks.manage_str(format!("native/{}", ENV_MODULE_NAME));
  let mut module = hooks.manage(Module::new(env_module_class, module_path, emitter.emit()));

  declare_env_module(hooks, module)?;
  define_env_module(hooks, &mut module)?;

  Ok(module)
}
