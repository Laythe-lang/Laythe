use crate::support::export_and_insert;
use spacelox_core::{
  hooks::GcHooks, module::Module, object::Class, package::Package, value::Value, ModuleResult,
};

pub const CLASS_CLASS_NAME: &'static str = "Class";

pub fn declare_class_class(hooks: &GcHooks, self_module: &mut Module) -> ModuleResult<()> {
  let name = hooks.manage_str(String::from(CLASS_CLASS_NAME));
  let class = hooks.manage(Class::new(name));

  export_and_insert(hooks, self_module, name, Value::from(class))
}

pub fn define_class_class(_hooks: &GcHooks, _self_module: &Module, _: &Package) {}
