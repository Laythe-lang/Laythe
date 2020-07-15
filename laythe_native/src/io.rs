use crate::{env::IoEnvNative, fs::IoFsNative, stdio::IoStdioNative, time::IoTimeNative};
use laythe_env::io::Io;
use std::rc::Rc;

pub fn io_native() -> Io {
  Io::new(
    Rc::new(IoStdioNative()),
    Rc::new(IoFsNative()),
    Rc::new(IoEnvNative()),
    Rc::new(IoTimeNative::default()),
  )
}
