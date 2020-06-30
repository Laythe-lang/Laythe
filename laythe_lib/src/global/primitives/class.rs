use crate::support::export_and_insert;
use laythe_core::{
  hooks::GcHooks, module::Module, object::Class, package::Package, value::Value, LyResult,
};

pub const CLASS_CLASS_NAME: &'static str = "Class";

pub fn declare_class_class(hooks: &GcHooks, module: &mut Module) -> LyResult<()> {
  let name = hooks.manage_str(String::from(CLASS_CLASS_NAME));
  let class = hooks.manage(Class::bare(name));

  export_and_insert(hooks, module, name, Value::from(class))
}

pub fn define_class_class(_hooks: &GcHooks, _self_module: &Module, _: &Package) {}
