mod clock;

use clock::declare_clock_funs;
use spacelox_core::{hooks::GcHooks, module::Module, package::Package, ModuleResult};
use spacelox_env::managed::Managed;

pub(crate) fn create_clock_funs(
  hooks: &GcHooks,
  mut module: Managed<Module>,
  _package: Managed<Package>,
) -> ModuleResult<()> {
  declare_clock_funs(hooks, &mut module)
}
