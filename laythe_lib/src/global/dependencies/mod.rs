pub mod module;

use laythe_core::{hooks::GcHooks, module::Module, package::Package};
use module::{declare_module_class, define_module_class};

use crate::InitResult;

pub(crate) fn add_dependency_classes(
  hooks: &GcHooks,
  module: &mut Module,
  package: &Package,
) -> InitResult<()> {
  declare_module_class(hooks, module)?;

  define_module_class(hooks, module, &package);

  Ok(())
}
