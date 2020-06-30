mod assert;
mod dependencies;
mod primitives;
mod time;

#[cfg(test)]
mod support;

use crate::GLOBAL_PATH;
use assert::create_assert_funs;
use dependencies::create_dependency_classes;
use dependencies::module::MODULE_CLASS_NAME;
use laythe_core::{
  hooks::GcHooks,
  module::Module,
  object::{BuiltIn, BuiltInDependencies, BuiltinPrimitives},
  package::Package,
  LyResult,
};
use laythe_env::managed::Managed;
use primitives::{
  bool::BOOL_CLASS_NAME, class::CLASS_CLASS_NAME, closure::CLOSURE_CLASS_NAME,
  create_primitive_classes, iter::ITER_CLASS_NAME, list::LIST_CLASS_NAME, map::MAP_CLASS_NAME,
  method::METHOD_CLASS_NAME, native_fun::NATIVE_FUN_CLASS_NAME,
  native_method::NATIVE_METHOD_CLASS_NAME, nil::NIL_CLASS_NAME, number::NUMBER_CLASS_NAME,
  string::STRING_CLASS_NAME,
};
use std::path::PathBuf;
use time::create_clock_funs;

pub fn add_global(hooks: &GcHooks, mut std: Managed<Package>) -> LyResult<()> {
  let module = match Module::from_path(&hooks, hooks.manage(PathBuf::from(GLOBAL_PATH))) {
    Some(module) => module,
    None => {
      return Err(hooks.make_error("Could not create global module, path malformed.".to_string()));
    }
  };

  let module = hooks.manage(module);
  std.add_module(hooks, module)?;

  create_primitive_classes(hooks, module, std)?;
  create_dependency_classes(hooks, module, std)?;
  create_assert_funs(hooks, module, std)?;
  create_clock_funs(hooks, module, std)?;

  Ok(())
}

pub fn builtin_from_global_module(hooks: &GcHooks, module: &Module) -> Option<BuiltIn> {
  Some(BuiltIn {
    primitives: BuiltinPrimitives {
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
      iter: module
        .get_symbol(hooks.manage_str(ITER_CLASS_NAME.to_string()))?
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
    },
    dependencies: BuiltInDependencies {
      module: module
        .get_symbol(hooks.manage_str(MODULE_CLASS_NAME.to_string()))?
        .to_class(),
    },
  })
}

pub fn assert_function_same_interface(_builtin: &BuiltinPrimitives) -> bool {
  true
}
