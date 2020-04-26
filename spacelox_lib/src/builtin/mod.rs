pub mod bool;
pub mod closure;
pub mod method;
pub mod list;
pub mod map;
pub mod native;
pub mod nil;
pub mod number;
pub mod string;

use crate::builtin::bool::create_bool_class;
use crate::builtin::closure::create_closure_class;
use crate::builtin::method::create_method_class;
use crate::builtin::list::create_list_class;
use crate::builtin::map::create_map_class;
use crate::builtin::native::create_native_class;
use crate::builtin::nil::create_nil_class;
use crate::builtin::number::create_number_class;
use crate::builtin::string::create_string_class;
use spacelox_core::hooks::Hooks;
use spacelox_core::value::BuiltInClasses;

pub fn make_builtin_classes(hooks: &Hooks) -> BuiltInClasses {
  BuiltInClasses {
    bool: create_bool_class(hooks),
    nil: create_nil_class(hooks),
    number: create_number_class(hooks),
    string: create_string_class(hooks),
    list: create_list_class(hooks),
    map: create_map_class(hooks),
    closure: create_closure_class(hooks),
    method: create_method_class(hooks),
    native: create_native_class(hooks),
  }
}
