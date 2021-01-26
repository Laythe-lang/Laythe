mod assert;
mod dependencies;
mod misc;
mod primitives;
mod time;

#[cfg(test)]
mod support;

use crate::{InitResult, GLOBAL_PATH};
use assert::add_assert_funs;
use dependencies::add_dependency_classes;
use laythe_core::{hooks::GcHooks, module::Module, package::Package, utils::IdEmitter};
use misc::add_misc_funs;
use primitives::add_primitive_classes;
use std::path::PathBuf;
use time::add_clock_funs;

pub use dependencies::module::MODULE_CLASS_NAME;
pub use primitives::{
  bool::BOOL_CLASS_NAME, class::CLASS_CLASS_NAME, closure::CLOSURE_CLASS_NAME,
  iter::ITER_CLASS_NAME, list::LIST_CLASS_NAME, map::MAP_CLASS_NAME, method::METHOD_CLASS_NAME,
  native::NATIVE_CLASS_NAME, nil::NIL_CLASS_NAME, number::NUMBER_CLASS_NAME,
  object::OBJECT_CLASS_NAME, string::STRING_CLASS_NAME,
};

pub fn add_global_module(
  hooks: &GcHooks,
  std: &mut Package,
  emitter: &mut IdEmitter,
) -> InitResult<()> {
  let mut module = hooks.manage(Module::from_path(
    &hooks,
    PathBuf::from(GLOBAL_PATH),
    emitter.emit(),
  )?);

  std.add_module(hooks, module)?;

  add_primitive_classes(hooks, &mut module, std)?;
  add_dependency_classes(hooks, &mut module, std)?;
  add_assert_funs(hooks, &mut module, std)?;
  add_clock_funs(hooks, &mut module, std)?;
  add_misc_funs(hooks, &mut module, std)
}

pub use primitives::error::{
  EXPORT_ERROR_NAME, IMPORT_ERROR_NAME, INDEX_ERROR_NAME, METHOD_NOT_FOUND_ERROR_NAME,
  PROPERTY_ERROR_NAME, RUNTIME_ERROR_NAME, SYNTAX_ERROR_NAME, TYPE_ERROR_NAME, VALUE_ERROR_NAME,
};
