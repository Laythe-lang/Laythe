pub const FRAME_MAX: usize = std::u8::MAX as usize;
pub const DEFAULT_STACK_MAX: usize = FRAME_MAX * 32;

pub struct SpecialStrings {
  pub init: String,
}

pub fn define_special_string() -> SpecialStrings {
  SpecialStrings {
    init: "init".to_string(),
  }
}
