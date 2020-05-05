pub mod bool;
pub mod closure;
pub mod iter;
pub mod list;
pub mod map;
pub mod method;
pub mod native;
pub mod nil;
pub mod number;
pub mod string;

use crate::builtin::bool::{declare_bool_class, define_bool_class};
use crate::builtin::closure::{declare_closure_class, define_closure_class};
use crate::builtin::list::{declare_list_class, define_list_class};
use crate::builtin::map::{declare_map_class, define_map_class};
use crate::builtin::method::{declare_method_class, define_method_class};
use crate::builtin::native::{declare_native_class, define_native_class};
use crate::builtin::nil::{declare_nil_class, define_nil_class};
use crate::builtin::number::{declare_number_class, define_number_class};
use crate::builtin::string::{declare_string_class, define_string_class};
use iter::{declare_iter_class, define_iter_class};
use spacelox_core::hooks::Hooks;
use spacelox_core::{module::Module, package::Package, value::BuiltInClasses, SpaceloxError};

pub fn make_builtin_classes(hooks: &Hooks) -> Result<BuiltInClasses, SpaceloxError> {
  let mut module = Module::new(hooks.manage_str(String::from("builtin")));
  let package = Package::new(hooks.manage_str(String::from("std")));

  declare_bool_class(hooks, &mut module)?;
  declare_closure_class(hooks, &mut module)?;
  declare_list_class(hooks, &mut module)?;
  declare_map_class(hooks, &mut module)?;
  declare_method_class(hooks, &mut module)?;
  declare_native_class(hooks, &mut module)?;
  declare_nil_class(hooks, &mut module)?;
  declare_number_class(hooks, &mut module)?;
  declare_string_class(hooks, &mut module)?;
  declare_iter_class(hooks, &mut module)?;

  define_bool_class(hooks, &module, &package);
  define_closure_class(hooks, &module, &package);
  define_list_class(hooks, &module, &package);
  define_map_class(hooks, &module, &package);
  define_method_class(hooks, &module, &package);
  define_native_class(hooks, &module, &package);
  define_nil_class(hooks, &module, &package);
  define_number_class(hooks, &module, &package);
  define_string_class(hooks, &module, &package);
  define_iter_class(hooks, &module, &package);

  let exports = module.get_all_symbols();

  Ok(BuiltInClasses {
    bool: exports
      .get(&hooks.manage_str(String::from("Bool")))
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
    native: exports
      .get(&hooks.manage_str(String::from("Native")))
      .unwrap()
      .to_class(),
  })
}
