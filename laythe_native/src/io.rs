use crate::{env::NativeEnvio, fs::NativeFsio, stdio::NativeStdio};
use laythe_env::{env::Env, fs::Fs, io::IoImpl, stdio::Stdio};

#[derive(Debug, Default)]
pub struct NativeIo();

impl IoImpl for NativeIo {
  fn stdio(&self) -> Stdio {
    Stdio::new(Box::new(NativeStdio::default()))
  }

  fn fsio(&self) -> Fs {
    Fs::new(Box::new(NativeFsio()))
  }

  fn envio(&self) -> Env {
    Env::new(Box::new(NativeEnvio()))
  }

  fn clone_box(&self) -> Box<dyn IoImpl> {
    Box::new(NativeIo())
  }
}
