pub mod assert;

use crate::assert::assert::{Assert, AssertEq, AssertNe};
use spacelox_core::{hooks::Hooks, native::NativeFun};

pub fn assert_funs(hooks: &Hooks) -> Vec<Box<dyn NativeFun>> {
  let mut native_funs: Vec<Box<dyn NativeFun>> = Vec::new();
  let str_method_name = hooks.manage_str(String::from("str"));

  native_funs.push(Box::new(Assert::new(str_method_name)));
  native_funs.push(Box::new(AssertEq::new(str_method_name)));
  native_funs.push(Box::new(AssertNe::new(str_method_name)));

  native_funs
}
