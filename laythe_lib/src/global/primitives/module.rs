use laythe_core::{hooks::GcHooks, object::Class};
use laythe_env::managed::Gc;

pub const MODULE_CLASS_NAME: &str = "Module";

pub fn create_module_class(hooks: &GcHooks, object: Gc<Class>) -> Gc<Class> {
  let name = hooks.manage_str(MODULE_CLASS_NAME);
  Class::with_inheritance(hooks, name, object)
}
