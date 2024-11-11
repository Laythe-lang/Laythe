mod class;

use self::class::{declare_regexp_class, define_regexp_class};
use crate::{global::MODULE_CLASS_NAME, support::load_class_from_package, StdResult, STD};
use laythe_core::{
  hooks::GcHooks,
  module::{Module, Package},
  object::Class,
  utils::IdEmitter, Ref,
};

const REGEXP_MODULE_NAME: &str = "regexp";

pub fn regexp_module(
  hooks: &GcHooks,
  std: Ref<Package>,
  emitter: &mut IdEmitter,
) -> StdResult<Ref<Module>> {
  let module_class = load_class_from_package(hooks, std, STD, MODULE_CLASS_NAME)?;
  let regexp_module_class =
    Class::with_inheritance(hooks, hooks.manage_str(REGEXP_MODULE_NAME), module_class);

  let module = hooks.manage(Module::new(
    hooks,
    regexp_module_class,
    &format!("native/{}", REGEXP_MODULE_NAME),
    emitter.emit(),
  ));

  declare_regexp_class(hooks, module, std)?;
  define_regexp_class(hooks, module, std)?;

  Ok(module)
}
