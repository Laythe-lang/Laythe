mod class;

use laythe_core::{hooks::GcHooks, module::Module, package::Package, utils::IdEmitter};
use laythe_env::managed::Gc;
use std::path::PathBuf;

use crate::InitResult;

use self::class::{declare_regexp_class, define_regexp_class};

const REGEXP_PATH: &str = "std/regexp.ly";

pub fn regexp_module(
  hooks: &GcHooks,
  std: &Package,
  emitter: &mut IdEmitter,
) -> InitResult<Gc<Module>> {
  let mut module = hooks.manage(Module::from_path(
    &hooks,
    PathBuf::from(REGEXP_PATH),
    emitter.emit(),
  )?);

  declare_regexp_class(hooks, &mut module, std)?;
  define_regexp_class(hooks, &module, std)?;

  Ok(module)
}
