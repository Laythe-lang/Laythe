pub mod bool;
pub mod fun;
pub mod list;
pub mod map;
pub mod native;
pub mod nil;
pub mod number;
pub mod string;

use crate::builtin::bool::create_bool_class;
use crate::builtin::fun::create_fun_class;
use crate::builtin::list::create_list_class;
use crate::builtin::map::create_map_class;
use crate::builtin::native::create_native_class;
use crate::builtin::nil::create_nil_class;
use crate::builtin::number::create_number_class;
use crate::builtin::string::create_string_class;
use spacelox_core::value::BuiltInClasses;
use spacelox_core::{managed::Trace, memory::Gc};

pub fn make_builtin_classes<C: Trace>(gc: &Gc, context: &C) -> BuiltInClasses {
  BuiltInClasses {
    bool: create_bool_class(gc, context),
    nil: create_nil_class(gc, context),
    number: create_number_class(gc, context),
    string: create_string_class(gc, context),
    list: create_list_class(gc, context),
    map: create_map_class(gc, context),
    fun: create_fun_class(gc, context),
    native: create_native_class(gc, context),
  }
}
