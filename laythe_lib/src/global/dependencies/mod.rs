pub mod module;

use module::{declare_module_class, define_module_class};
use laythe_core::{hooks::GcHooks, module::Module, package::Package, ModuleResult};
use laythe_env::managed::Managed;

pub(crate) fn create_dependency_classes(
  hooks: &GcHooks,
  mut module: Managed<Module>,
  package: Managed<Package>,
) -> ModuleResult<()> {
  declare_module_class(hooks, &mut module)?;

  define_module_class(hooks, &module, &package);

  Ok(())
}
