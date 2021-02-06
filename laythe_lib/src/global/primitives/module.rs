use laythe_core::{hooks::GcHooks, managed::Gc, object::Class};

pub const MODULE_CLASS_NAME: &str = "Module";

pub fn create_module_class(hooks: &GcHooks, object: Gc<Class>) -> Gc<Class> {
  let name = hooks.manage_str(MODULE_CLASS_NAME);
  Class::with_inheritance(hooks, name, object)
}
