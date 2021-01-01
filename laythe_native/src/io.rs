use crate::{env::IoEnvNative, fs::IoFsNative, stdio::IoStdioNative, time::IoTimeNative};
use laythe_env::io::Io;
use std::sync::Arc;

pub fn io_native() -> Io {
  Io::new(
    Arc::new(IoStdioNative()),
    Arc::new(IoFsNative()),
    Arc::new(IoEnvNative()),
    Arc::new(IoTimeNative::default()),
  )
}
