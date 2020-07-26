mod fs;
mod stdio;

use fs::fs_module;
use laythe_core::{hooks::GcHooks, package::Package, LyResult};
use laythe_env::managed::Managed;
use stdio::stdio_module;

const IO_PACKAGE_NAME: &str = "io";

pub fn io_package(hooks: &GcHooks, std: Managed<Package>) -> LyResult<Managed<Package>> {
  let mut package = hooks.manage(Package::new(hooks.manage_str(IO_PACKAGE_NAME)));

  let stdio = stdio_module(hooks, std)?;
  let fs = fs_module(hooks, std)?;

  package.add_module(hooks, stdio)?;
  package.add_module(hooks, fs)?;

  Ok(package)
}
