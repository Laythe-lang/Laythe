use laythe_env::env::EnvImpl;
use std::{env, io, path::PathBuf};

#[derive(Clone)]
pub struct NativeEnvio();

impl Default for NativeEnvio {
  fn default() -> Self {
    Self()
  }
}

impl EnvImpl for NativeEnvio {
  fn current_dir(&self) -> io::Result<PathBuf> {
    env::current_dir()
  }

  fn args(&self) -> Vec<String> {
    env::args().collect()
  }
}
