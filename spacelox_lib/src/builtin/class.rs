use spacelox_core::{
  hooks::Hooks,
  module::Module,
  package::Package,
  value::Value,
  object::Class,
  ModuleResult,
};

pub const STRING_CLASS_NAME: &'static str = "Class";

pub fn declare_class_class(hooks: &Hooks, self_module: &mut Module) -> ModuleResult<()> {
  let name = hooks.manage_str(String::from(STRING_CLASS_NAME));
  let class = hooks.manage(Class::new(name));

  self_module.add_export(hooks, name, Value::from(class))
}

pub fn define_class_class(_hooks: &Hooks, _self_module: &Module, _: &Package) {
}
