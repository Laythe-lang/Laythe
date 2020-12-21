mod class;

use laythe_core::{hooks::GcHooks, module::Module, package::Package};
use laythe_env::managed::Managed;
use std::path::PathBuf;

use crate::InitResult;

use self::class::{declare_regexp_class, define_regexp_class};

const REGEXP_PATH: &str = "std/regexp.ly";

pub fn regexp_module(hooks: &GcHooks, std: &Package) -> InitResult<Managed<Module>> {
  let mut module = hooks.manage(Module::from_path(
    &hooks,
    hooks.manage(PathBuf::from(REGEXP_PATH)),
  )?);

  declare_regexp_class(hooks, &mut module, std)?;
  define_regexp_class(hooks, &module, std)?;

  Ok(module)
}
