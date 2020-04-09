pub mod clock;

use crate::time::clock::NativeClock;
use spacelox_core::native::NativeFun;

pub fn clock_funs() -> Vec<Box<dyn NativeFun>> {
  let mut native_funs: Vec<Box<dyn NativeFun>> = Vec::new();

  native_funs.push(Box::new(NativeClock::new()));

  native_funs
}
