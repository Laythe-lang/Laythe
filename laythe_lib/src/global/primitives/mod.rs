pub mod bool;
pub mod class;
pub mod closure;
pub mod error;
pub mod iter;
pub mod list;
pub mod map;
pub mod method;
pub mod native;
pub mod nil;
pub mod number;
pub mod object;
pub mod string;

use self::{
  bool::{declare_bool_class, define_bool_class},
  error::{declare_error_class, declare_global_errors, define_error_class, define_global_errors},
};
use crate::InitResult;
use class::{declare_class_class, define_class_class, CLASS_CLASS_NAME};
use closure::{declare_closure_class, define_closure_class};
use iter::{declare_iter_class, define_iter_class};
use laythe_core::{hooks::GcHooks, module::Module, object::Class, package::Package};
use list::{declare_list_class, define_list_class};
use map::{declare_map_class, define_map_class};
use method::{declare_method_class, define_method_class};
use native::{declare_native_class, define_native_class};
use nil::{declare_nil_class, define_nil_class};
use number::{declare_number_class, define_number_class};
use object::{declare_object_class, define_object_class, OBJECT_CLASS_NAME};
use string::{declare_string_class, define_string_class};

pub(crate) fn add_primitive_classes(
  hooks: &GcHooks,
  module: &mut Module,
  package: &Package,
) -> InitResult<()> {
  bootstrap_classes(hooks, module, package)?;

  declare_global_errors(hooks, module, &package)?;
  declare_bool_class(hooks, module, &package)?;
  declare_closure_class(hooks, module, &package)?;
  declare_iter_class(hooks, module, &package)?;
  declare_list_class(hooks, module, &package)?;
  declare_map_class(hooks, module, &package)?;
  declare_method_class(hooks, module, &package)?;
  declare_native_class(hooks, module, &package)?;
  declare_nil_class(hooks, module, &package)?;
  declare_number_class(hooks, module, &package)?;
  declare_string_class(hooks, module, &package)?;

  define_global_errors(hooks, &module, &package)?;
  define_bool_class(hooks, &module, &package)?;
  define_closure_class(hooks, &module, &package)?;
  define_iter_class(hooks, &module, &package)?;
  define_list_class(hooks, &module, &package)?;
  define_map_class(hooks, &module, &package)?;
  define_method_class(hooks, &module, &package)?;
  define_native_class(hooks, &module, &package)?;
  define_nil_class(hooks, &module, &package)?;
  define_number_class(hooks, &module, &package)?;
  define_string_class(hooks, &module, &package)?;

  Ok(())
}

fn bootstrap_classes(hooks: &GcHooks, module: &mut Module, package: &Package) -> InitResult<()> {
  // create object class by itself
  declare_object_class(hooks, module)?;
  define_object_class(hooks, &module, &package)?;

  let mut object_class = match module.get_symbol(hooks.manage_str(OBJECT_CLASS_NAME)) {
    Some(class) => class.to_class(),
    None => {
      return Err(hooks.manage_str("Could not find Laythe class object in std_lib construction."))
    }
  };

  // declare class
  declare_class_class(hooks, module)?;
  let mut class_class = match module.get_symbol(hooks.manage_str(CLASS_CLASS_NAME.to_string())) {
    Some(class) => class.to_class(),
    None => {
      return Err(hooks.manage_str("Could not find Laythe class class in std_lib construction."))
    }
  };

  // inherit from object
  class_class.inherit(hooks, object_class);
  define_class_class(hooks, &module, &package)?;

  // class metaClass is itself
  let class_copy = class_class;
  class_class.set_meta(class_copy);

  // create object's meta class
  let object_meta_class = Class::with_inheritance(
    hooks,
    hooks.manage_str(format!("{} metaClass", object_class.name())),
    class_class,
  );

  // object meta class is object metaClass and it's is class
  object_class.set_meta(object_meta_class);

  declare_error_class(hooks, module, &package)?;
  define_error_class(hooks, &module, &package)?;

  Ok(())
}
