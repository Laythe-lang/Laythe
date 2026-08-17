use laythe_core::value::{VALUE_UNDEFINED, Value};

pub const MAX_FRAME_SIZE: usize = 255;
pub const REPL_MODULE: &str = "repl.ly";
pub const UNDEFINED_ARRAY: [Value; u8::MAX as usize] = [VALUE_UNDEFINED; u8::MAX as usize];
