use crate::{support::export_and_insert, InitResult};
use laythe_core::{
  hooks::GcHooks, module::Module, object::Class, package::Package, val, value::Value,
};

pub const MODULE_CLASS_NAME: &str = "Module";

pub fn declare_module_class(hooks: &GcHooks, self_module: &mut Module) -> InitResult<()> {
  let name = hooks.manage_str(MODULE_CLASS_NAME);
  let class = hooks.manage(Class::bare(name));

  export_and_insert(hooks, self_module, name, val!(class))
}

pub fn define_module_class(_hooks: &GcHooks, _self_module: &Module, _: &Package) {}
