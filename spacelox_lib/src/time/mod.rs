pub mod clock;

use clock::declare_clock_funs;
use spacelox_core::{hooks::Hooks, module::Module, package::Package, ModuleResult};
use spacelox_env::managed::Managed;

pub(crate) fn create_clock_funs(
  hooks: &Hooks,
  mut module: Managed<Module>,
  _package: Managed<Package>,
) -> ModuleResult<()> {
  declare_clock_funs(hooks, &mut module)
}
