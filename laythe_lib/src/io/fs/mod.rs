mod file;

use file::{declare_file, define_file};
use laythe_core::{hooks::GcHooks, module::Module, package::Package};
use laythe_env::managed::Gc;
use std::path::PathBuf;

use crate::InitResult;

const FS_PATH: &str = "std/io/fs.ly";

pub fn fs_module(hooks: &GcHooks, std: &Package) -> InitResult<Gc<Module>> {
  let mut module = hooks.manage(Module::from_path(
    &hooks,
    PathBuf::from(FS_PATH),
  )?);

  declare_file(hooks, &mut module, &*std)?;
  define_file(hooks, &module, &*std)?;

  Ok(module)
}
