mod file;

use file::{declare_file, define_file};
use laythe_core::{hooks::GcHooks, module::Module, package::Package, LyResult};
use laythe_env::managed::Managed;
use std::path::PathBuf;

const FS_PATH: &str = "std/io/fs.ly";

pub fn fs_module(hooks: &GcHooks, std: Managed<Package>) -> LyResult<Managed<Module>> {
  let mut module = hooks.manage(Module::from_path(
    &hooks,
    hooks.manage(PathBuf::from(FS_PATH)),
  )?);

  declare_file(hooks, &mut module, &*std)?;

  define_file(hooks, &module, &*std)?;

  Ok(module)
}
