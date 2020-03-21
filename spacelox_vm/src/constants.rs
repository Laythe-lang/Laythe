use crate::memory::{Gc, NO_GC};
use spacelox_core::managed::Managed;
use spacelox_interner::IStr;

pub const FRAME_MAX: usize = std::u8::MAX as usize;
pub const DEFAULT_STACK_MAX: usize = FRAME_MAX * 32;

pub struct SpecialStrings {
  pub init: Managed<IStr>,
}

pub fn define_special_string(gc: &mut Gc) -> SpecialStrings {
  SpecialStrings {
    init: gc.manage(IStr::new("init"), &NO_GC),
  }
}
