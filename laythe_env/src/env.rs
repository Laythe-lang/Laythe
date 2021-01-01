use crate::io::IoImpl;
use std::{io, path::PathBuf};

/// A wrapper around environmental facilities provided to Laythe
pub struct Env {
  env: Box<dyn EnvImpl>,
}

impl Default for Env {
  fn default() -> Self {
    Self {
      env: Box::new(EnvMock()),
    }
  }
}

impl Env {
  /// Create a new wrapper around the provided environmental facilities
  pub fn new(env: Box<dyn EnvImpl>) -> Self {
    Self { env }
  }

  /// Get the current working directory
  pub fn current_dir(&self) -> io::Result<PathBuf> {
    self.env.current_dir()
  }

  /// Get the arguments pass to this script
  pub fn args(&self) -> Vec<String> {
    self.env.args()
  }
}

pub trait EnvImpl: Send + Sync {
  fn current_dir(&self) -> io::Result<PathBuf>;
  fn args(&self) -> Vec<String>;
}

#[derive(Debug)]
pub struct IoEnvMock();

impl IoImpl<Env> for IoEnvMock {
  fn make(&self) -> Env {
    Env::new(Box::new(EnvMock()))
  }
}

pub struct EnvMock();

impl EnvImpl for EnvMock {
  fn current_dir(&self) -> io::Result<PathBuf> {
    Ok(PathBuf::new())
  }

  fn args(&self) -> Vec<String> {
    vec![]
  }
}
