pub mod bool;
pub mod class;
pub mod closure;
pub mod iter;
pub mod list;
pub mod map;
pub mod method;
pub mod native_fun;
pub mod native_method;
pub mod nil;
pub mod number;
pub mod string;

use crate::builtin::bool::{declare_bool_class, define_bool_class};
use class::{declare_class_class, define_class_class};
use closure::{declare_closure_class, define_closure_class};
use iter::{declare_iter_class, define_iter_class};
use list::{declare_list_class, define_list_class};
use map::{declare_map_class, define_map_class};
use method::{declare_method_class, define_method_class};
use native_fun::{declare_native_fun_class, define_native_fun_class};
use native_method::{declare_native_method_class, define_native_method_class};
use nil::{declare_nil_class, define_nil_class};
use number::{declare_number_class, define_number_class};
use spacelox_core::{
  hooks::Hooks, module::Module, object::BuiltInClasses, package::Package, SlError,
};
use string::{declare_string_class, define_string_class};

pub fn make_builtin_classes(hooks: &Hooks) -> Result<BuiltInClasses, SlError> {
  let mut module = Module::new(hooks.manage_str(String::from("builtin")));
  let package = Package::new(hooks.manage_str(String::from("std")));

  declare_bool_class(hooks, &mut module)?;
  declare_closure_class(hooks, &mut module)?;
  declare_class_class(hooks, &mut module)?;
  declare_iter_class(hooks, &mut module)?;
  declare_list_class(hooks, &mut module)?;
  declare_map_class(hooks, &mut module)?;
  declare_method_class(hooks, &mut module)?;
  declare_native_fun_class(hooks, &mut module)?;
  declare_native_method_class(hooks, &mut module)?;
  declare_nil_class(hooks, &mut module)?;
  declare_number_class(hooks, &mut module)?;
  declare_string_class(hooks, &mut module)?;

  define_bool_class(hooks, &module, &package);
  define_class_class(hooks, &module, &package);
  define_closure_class(hooks, &module, &package);
  define_iter_class(hooks, &module, &package);
  define_list_class(hooks, &module, &package);
  define_map_class(hooks, &module, &package);
  define_method_class(hooks, &module, &package);
  define_native_fun_class(hooks, &module, &package);
  define_native_method_class(hooks, &module, &package);
  define_nil_class(hooks, &module, &package);
  define_number_class(hooks, &module, &package);
  define_string_class(hooks, &module, &package);

  let exports = module.import();

  let builtin = BuiltInClasses {
    bool: exports
      .get(&hooks.manage_str(String::from("Bool")))
      .unwrap()
      .to_class(),
    class: exports
      .get(&hooks.manage_str(String::from("Class")))
      .unwrap()
      .to_class(),
    nil: exports
      .get(&hooks.manage_str(String::from("Nil")))
      .unwrap()
      .to_class(),
    number: exports
      .get(&hooks.manage_str(String::from("Number")))
      .unwrap()
      .to_class(),
    string: exports
      .get(&hooks.manage_str(String::from("String")))
      .unwrap()
      .to_class(),
    list: exports
      .get(&hooks.manage_str(String::from("List")))
      .unwrap()
      .to_class(),
    map: exports
      .get(&hooks.manage_str(String::from("Map")))
      .unwrap()
      .to_class(),
    closure: exports
      .get(&hooks.manage_str(String::from("Fun")))
      .unwrap()
      .to_class(),
    method: exports
      .get(&hooks.manage_str(String::from("Method")))
      .unwrap()
      .to_class(),
    native_fun: exports
      .get(&hooks.manage_str(String::from("Native Fun")))
      .unwrap()
      .to_class(),
    native_method: exports
      .get(&hooks.manage_str(String::from("Native Method")))
      .unwrap()
      .to_class(),
  };

  debug_assert!(assert_function_same_interface(&builtin));

  Ok(builtin)
}

fn assert_function_same_interface(_builtin: &BuiltInClasses) -> bool {
  true
}
