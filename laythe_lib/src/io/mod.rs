mod fs;
mod global;
mod stdio;

use fs::fs_module;
use laythe_core::{hooks::GcHooks, package::Package, utils::IdEmitter};
use stdio::stdio_module;

use crate::InitResult;

use self::global::errors_module;

const IO_PACKAGE_NAME: &str = "io";

pub fn add_io_package(
  hooks: &GcHooks,
  std: &mut Package,
  emitter: &mut IdEmitter,
) -> InitResult<()> {
  let mut package = hooks.manage(Package::new(hooks.manage_str(IO_PACKAGE_NAME)));
  std.add_package(hooks, package)?;

  let errors = errors_module(hooks, std, emitter)?;
  package.add_module(hooks, errors)?;

  let stdio = stdio_module(hooks, std, emitter)?;
  let fs = fs_module(hooks, std, emitter)?;

  package.add_module(hooks, stdio)?;
  package.add_module(hooks, fs)
}
