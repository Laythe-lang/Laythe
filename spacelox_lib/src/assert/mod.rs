pub mod assert;

use crate::assert::assert::{NativeAssert, NativeAssertEq, NativeAssertNe};
use spacelox_core::native::NativeFun;

pub fn assert_funs() -> Vec<Box<dyn NativeFun>> {
  let mut native_funs: Vec<Box<dyn NativeFun>> = Vec::new();

  native_funs.push(Box::new(NativeAssert::new()));
  native_funs.push(Box::new(NativeAssertEq::new()));
  native_funs.push(Box::new(NativeAssertNe::new()));

  native_funs
}
