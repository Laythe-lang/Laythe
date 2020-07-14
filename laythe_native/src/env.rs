use laythe_env::{
  env::{Env, EnvImpl},
  io::IoImpl,
};
use std::{env, io, path::PathBuf};

#[derive(Debug)]
pub struct IoEnvNative();

impl IoImpl<Env> for IoEnvNative {
  fn make(&self) -> Env {
    Env::new(Box::new(EnvNative()))
  }
}

#[derive(Clone)]
pub struct EnvNative();

impl Default for EnvNative {
  fn default() -> Self {
    Self()
  }
}

impl EnvImpl for EnvNative {
  fn current_dir(&self) -> io::Result<PathBuf> {
    env::current_dir()
  }

  fn args(&self) -> Vec<String> {
    env::args().collect()
  }
}
