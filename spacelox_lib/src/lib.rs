#![deny(clippy::all)]
use assert::create_assert_funs;
use builtin::{
  bool::BOOL_CLASS_NAME, class::CLASS_CLASS_NAME, closure::CLOSURE_CLASS_NAME,
  create_builtin_classes, list::LIST_CLASS_NAME, map::MAP_CLASS_NAME, method::METHOD_CLASS_NAME,
  native_fun::NATIVE_FUN_CLASS_NAME, native_method::NATIVE_METHOD_CLASS_NAME, nil::NIL_CLASS_NAME,
  number::NUMBER_CLASS_NAME, string::STRING_CLASS_NAME,
};
use spacelox_core::{
  hooks::Hooks, module::Module, object::BuiltInClasses, package::Package, ModuleResult,
  PackageResult,
};
use spacelox_env::managed::Managed;
use time::create_clock_funs;
pub mod assert;
pub mod builtin;
mod support;
pub mod time;

pub const STD: &'static str = "std";
pub const GLOBAL: &'static str = "global";

pub fn create_std_lib(hooks: &Hooks) -> PackageResult<Managed<Package>> {
  let mut std = hooks.manage(Package::new(hooks.manage_str(STD.to_string())));

  let global = create_global(hooks, std)?;
  std.add_module(hooks, global)?;

  Ok(std)
}

fn create_global(hooks: &Hooks, std: Managed<Package>) -> ModuleResult<Managed<Module>> {
  let global = hooks.manage(Module::new(hooks.manage_str(GLOBAL.to_string())));

  create_builtin_classes(hooks, global, std)?;
  create_assert_funs(hooks, global, std)?;
  create_clock_funs(hooks, global, std)?;

  Ok(global)
}

pub fn builtin_from_std_lib(hooks: &Hooks, module: &Module) -> Option<BuiltInClasses> {
  Some(BuiltInClasses {
    nil: module
      .get_symbol(hooks.manage_str(NIL_CLASS_NAME.to_string()))?
      .to_class(),
    bool: module
      .get_symbol(hooks.manage_str(BOOL_CLASS_NAME.to_string()))?
      .to_class(),
    class: module
      .get_symbol(hooks.manage_str(CLASS_CLASS_NAME.to_string()))?
      .to_class(),
    number: module
      .get_symbol(hooks.manage_str(NUMBER_CLASS_NAME.to_string()))?
      .to_class(),
    string: module
      .get_symbol(hooks.manage_str(STRING_CLASS_NAME.to_string()))?
      .to_class(),
    list: module
      .get_symbol(hooks.manage_str(LIST_CLASS_NAME.to_string()))?
      .to_class(),
    map: module
      .get_symbol(hooks.manage_str(MAP_CLASS_NAME.to_string()))?
      .to_class(),
    closure: module
      .get_symbol(hooks.manage_str(CLOSURE_CLASS_NAME.to_string()))?
      .to_class(),
    method: module
      .get_symbol(hooks.manage_str(METHOD_CLASS_NAME.to_string()))?
      .to_class(),
    native_fun: module
      .get_symbol(hooks.manage_str(NATIVE_FUN_CLASS_NAME.to_string()))?
      .to_class(),
    native_method: module
      .get_symbol(hooks.manage_str(NATIVE_METHOD_CLASS_NAME.to_string()))?
      .to_class(),
  })
}

pub fn assert_function_same_interface(_builtin: &BuiltInClasses) -> bool {
  true
}
