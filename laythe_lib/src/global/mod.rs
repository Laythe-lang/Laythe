mod assert;
mod misc;
mod primitives;
mod time;

#[cfg(test)]
mod support;

use crate::{StdResult, STD};
use assert::add_assert_funs;
use laythe_core::{hooks::GcHooks, managed::Gc, module::Package, utils::IdEmitter};
use misc::add_misc_funs;
use time::add_clock_funs;

pub use primitives::{
  bool::BOOL_CLASS_NAME, class::CLASS_CLASS_NAME, closure::CLOSURE_CLASS_NAME,
  iter::ITER_CLASS_NAME, list::LIST_CLASS_NAME, map::MAP_CLASS_NAME, method::METHOD_CLASS_NAME,
  module::MODULE_CLASS_NAME, native::NATIVE_CLASS_NAME, nil::NIL_CLASS_NAME,
  number::NUMBER_CLASS_NAME, object::OBJECT_CLASS_NAME, string::STRING_CLASS_NAME,
};

pub fn create_std_core(hooks: &GcHooks, emitter: &mut IdEmitter) -> StdResult<Gc<Package>> {
  let mut global_module = create_primitives(hooks, emitter)?;
  let std = hooks.manage(Package::new(
    hooks.manage_str(STD.to_string()),
    global_module,
  ));

  add_assert_funs(hooks, &mut global_module, &std)?;
  add_clock_funs(hooks, &mut global_module)?;
  add_misc_funs(hooks, &mut global_module)?;

  Ok(std)
}

pub use primitives::error::{
  EXPORT_ERROR_NAME, IMPORT_ERROR_NAME, INDEX_ERROR_NAME, METHOD_NOT_FOUND_ERROR_NAME,
  PROPERTY_ERROR_NAME, RUNTIME_ERROR_NAME, SYNTAX_ERROR_NAME, TYPE_ERROR_NAME, VALUE_ERROR_NAME,
};

use self::primitives::create_primitives;
