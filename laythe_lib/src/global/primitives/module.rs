use laythe_core::{ObjRef, hooks::GcHooks, object::Class};

pub const MODULE_CLASS_NAME: &str = "Module";

pub fn create_module_class(hooks: &GcHooks, object: ObjRef<Class>) -> ObjRef<Class> {
  let name = hooks.manage_str(MODULE_CLASS_NAME);
  Class::with_inheritance(hooks, name, object)
}
