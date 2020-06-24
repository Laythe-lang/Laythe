mod assert;

use assert::declare_assert_funs;
use laythe_core::{hooks::GcHooks, module::Module, package::Package, ModuleResult};
use laythe_env::managed::Managed;

pub(crate) fn create_assert_funs(
  hooks: &GcHooks,
  mut module: Managed<Module>,
  _package: Managed<Package>,
) -> ModuleResult<()> {
  declare_assert_funs(hooks, &mut module)
}
