mod clock;

use clock::declare_clock_funs;
use laythe_core::{hooks::GcHooks, module::Module, package::Package, LyResult};
use laythe_env::managed::Managed;

pub(crate) fn add_clock_funs(
  hooks: &GcHooks,
  mut module: Managed<Module>,
  _package: Managed<Package>,
) -> LyResult<()> {
  declare_clock_funs(hooks, &mut module)
}
