mod stdio;

use laythe_core::{hooks::GcHooks, package::Package, LyResult};
use laythe_env::managed::Managed;
use stdio::stdio_module;

const IO_PACKAGE_NAME: &str = "io";

pub fn io_package(hooks: &GcHooks, std: Managed<Package>) -> LyResult<Managed<Package>> {
  let mut package = hooks.manage(Package::new(hooks.manage_str(IO_PACKAGE_NAME.to_string())));

  let stdio = stdio_module(hooks, std)?;

  package.add_module(hooks, stdio)?;

  Ok(package)
}
