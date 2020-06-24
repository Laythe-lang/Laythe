use std::env;
use std::{io::Result, path::PathBuf};

pub trait EnvIo {
  fn current_dir(&self) -> Result<PathBuf>;
  fn args(&self) -> Vec<String>;
}

#[derive(Clone)]
pub struct NativeEnvIo();

impl Default for NativeEnvIo {
  fn default() -> Self {
    Self()
  }
}

impl EnvIo for NativeEnvIo {
  fn current_dir(&self) -> Result<PathBuf> {
    env::current_dir()
  }

  fn args(&self) -> Vec<String> {
    env::args().collect()
  }
}
