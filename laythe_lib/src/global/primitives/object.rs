use crate::support::export_and_insert;
use laythe_core::{
  hooks::GcHooks, module::Module, object::Class, package::Package, value::Value, ModuleResult,
};

pub const OBJECT_CLASS_NAME: &'static str = "Object";

pub fn declare_object_class(hooks: &GcHooks, self_module: &mut Module) -> ModuleResult<()> {
  let name = hooks.manage_str(String::from(OBJECT_CLASS_NAME));
  let class = hooks.manage(Class::bare(name));

  export_and_insert(hooks, self_module, name, Value::from(class))
}

pub fn define_object_class(_hooks: &GcHooks, _self_module: &Module, _: &Package) {}
