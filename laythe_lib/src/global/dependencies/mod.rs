pub mod module;

use laythe_core::{hooks::GcHooks, module::Module, package::Package, LyResult};
use laythe_env::managed::Managed;
use module::{declare_module_class, define_module_class};

pub(crate) fn add_dependency_classes(
  hooks: &GcHooks,
  mut module: Managed<Module>,
  package: Managed<Package>,
) -> LyResult<()> {
  declare_module_class(hooks, &mut module)?;

  define_module_class(hooks, &module, &package);

  Ok(())
}
