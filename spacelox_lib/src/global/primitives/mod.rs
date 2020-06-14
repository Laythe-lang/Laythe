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

use self::bool::{declare_bool_class, define_bool_class};
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
use spacelox_core::{hooks::GcHooks, module::Module, package::Package, ModuleResult};
use spacelox_env::managed::Managed;
use string::{declare_string_class, define_string_class};

pub(crate) fn create_primitive_classes(
  hooks: &GcHooks,
  mut module: Managed<Module>,
  package: Managed<Package>,
) -> ModuleResult<()> {
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

  Ok(())
}
