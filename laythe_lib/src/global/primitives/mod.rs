pub mod bool;
pub mod channel;
pub mod class;
pub mod closure;
pub mod error;
pub mod fun;
pub mod iter;
pub mod list;
pub mod map;
pub mod method;
pub mod module;
pub mod native;
pub mod nil;
pub mod number;
pub mod object;
pub mod string;
pub mod tuple;

use self::{
  bool::{declare_bool_class, define_bool_class},
  channel::{declare_channel_class, define_channel_class},
  error::{create_error_class, declare_global_errors, define_global_errors, ERROR_CLASS_NAME},
  fun::{declare_fun_class, define_fun_class},
  module::create_module_class,
  tuple::{declare_tuple_class, define_tuple_class},
};
use crate::{support::export_and_insert, StdError, StdResult, STD};
use class::create_class_class;
use closure::{declare_closure_class, define_closure_class};
use iter::{declare_iter_class, define_iter_class};
use laythe_core::{
  hooks::GcHooks, if_let_obj, module::Module, object::{Class, ObjectKind}, to_obj_kind, utils::IdEmitter, val, value::Value, ObjRef, Ref
};
use list::{declare_list_class, define_list_class};
use map::{declare_map_class, define_map_class};
use method::{declare_method_class, define_method_class};
use native::{declare_native_class, define_native_class};
use nil::{declare_nil_class, define_nil_class};
use number::{declare_number_class, define_number_class};
use object::create_object_class;
use string::{declare_string_class, define_string_class};

use super::OBJECT_CLASS_NAME;

fn error_inheritance(
  hooks: &GcHooks,
  module: Ref<Module>,
  class_name: &str,
) -> StdResult<ObjRef<Class>> {
  let name = hooks.manage_str(class_name);
  let error_name = hooks.manage_str(ERROR_CLASS_NAME);
  let error_class = match module.get_exported_symbol_by_name(error_name) {
    Some(class) => class,
    None => return Err(StdError::SymbolNotFound),
  };

  if_let_obj!(ObjectKind::Class(class) = (error_class) {
    Ok(Class::with_inheritance(
      hooks,
      name,
      class,
    ))
  } else {
    Err(StdError::SymbolNotClass)
  })
}

fn class_inheritance(
  hooks: &GcHooks,
  module: Ref<Module>,
  class_name: &str,
) -> StdResult<ObjRef<Class>> {
  let name = hooks.manage_str(class_name);
  let object_name = hooks.manage_str(OBJECT_CLASS_NAME);
  let object_class = match module.get_exported_symbol_by_name(object_name) {
    Some(class) => class,
    None => return Err(StdError::SymbolNotFound),
  };

  if_let_obj!(ObjectKind::Class(class) = (object_class) {
    Ok(Class::with_inheritance(
      hooks,
      name,
      class,
    ))
  } else {
    Err(StdError::SymbolNotClass)
  })
}

pub(crate) fn create_primitives(hooks: &GcHooks, emitter: &mut IdEmitter) -> StdResult<Ref<Module>> {
  let module = bootstrap_classes(hooks, emitter)?;

  declare_bool_class(hooks, module)?;
  declare_channel_class(hooks, module)?;
  declare_closure_class(hooks, module)?;
  declare_fun_class(hooks, module)?;
  declare_global_errors(hooks, module)?;
  declare_iter_class(hooks, module)?;
  declare_list_class(hooks, module)?;
  declare_map_class(hooks, module)?;
  declare_method_class(hooks, module)?;
  declare_native_class(hooks, module)?;
  declare_nil_class(hooks, module)?;
  declare_number_class(hooks, module)?;
  declare_string_class(hooks, module)?;
  declare_tuple_class(hooks, module)?;

  define_bool_class(hooks, module)?;
  define_channel_class(hooks, module)?;
  define_closure_class(hooks, module)?;
  define_fun_class(hooks, module)?;
  define_global_errors(hooks, &module)?;
  define_iter_class(hooks, module)?;
  define_list_class(hooks, module)?;
  define_map_class(hooks, module)?;
  define_method_class(hooks, module)?;
  define_native_class(hooks, module)?;
  define_nil_class(hooks, module)?;
  define_number_class(hooks, module)?;
  define_string_class(hooks, module)?;
  define_tuple_class(hooks, module)?;

  Ok(module)
}

fn bootstrap_classes(hooks: &GcHooks, emitter: &mut IdEmitter) -> StdResult<Ref<Module>> {
  // create object class by itself
  let mut object_class = create_object_class(hooks);

  // create class class inheriting from object
  let mut class_class = create_class_class(hooks, object_class);

  // class metaClass is itself
  let class_copy = class_class;
  class_class.set_meta(class_copy);

  // create object's meta class
  let mut object_meta_class = hooks.manage_obj(Class::bare(
    hooks.manage_str(format!("{} metaClass", &*object_class.name())),
  ));

  // object meta class inherits from class class
  // as well as it's meta class
  object_meta_class.inherit(hooks, class_class);
  object_meta_class.set_meta(class_class);

  // object meta class is object metaClass and it's is class
  object_class.set_meta(object_meta_class);

  let error_class = create_error_class(hooks, object_class);

  let module_class = create_module_class(hooks, object_class);
  let std_module_class = Class::with_inheritance(hooks, hooks.manage_str(STD), module_class);

  let module = hooks.manage(Module::new(
    hooks,
    std_module_class,
    "native",
    emitter.emit(),
  ));

  export_and_insert(hooks, module, object_class.name(), val!(object_class))?;
  export_and_insert(hooks, module, class_class.name(), val!(class_class))?;
  export_and_insert(hooks, module, error_class.name(), val!(error_class))?;
  export_and_insert(hooks, module, module_class.name(), val!(module_class))?;

  Ok(module)
}
