use crate::support::export_and_insert;
use laythe_core::{
  hooks::GcHooks, module::Module, object::Class, package::Package, value::Value, LyResult,
};

pub const MODULE_CLASS_NAME: &str = "Module";

pub fn declare_module_class(hooks: &GcHooks, self_module: &mut Module) -> LyResult<()> {
  let name = hooks.manage_str(String::from(MODULE_CLASS_NAME));
  let class = hooks.manage(Class::bare(name));

  export_and_insert(hooks, self_module, name, Value::from(class))
}

pub fn define_module_class(_hooks: &GcHooks, _self_module: &Module, _: &Package) {}
