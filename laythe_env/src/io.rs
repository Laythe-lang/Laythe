use crate::{
  env::{Env, EnvMock},
  fs::{Fs, FsMock},
  stdio::Stdio,
};
use std::fmt;

#[derive(Debug)]
/// A struct wrapping the externally provided io to Laythe
pub struct Io {
  io: Box<dyn IoImpl>,
}

impl Default for Io {
  fn default() -> Self {
    Self::new(Box::new(MockIo()))
  }
}

impl Io {
  /// Create a new io wrapper uses the provided io impl
  pub fn new(io: Box<dyn IoImpl>) -> Self {
    Self { io }
  }

  /// Generate a wrapper to stdio facilities
  pub fn stdio(&self) -> Stdio {
    self.io.stdio()
  }

  /// Generate a wrapper to file system facilities
  pub fn fsio(&self) -> Fs {
    self.io.fsio()
  }

  /// Generate a wrapper to environment facilities
  pub fn envio(&self) -> Env {
    self.io.envio()
  }
}

impl Clone for Io {
  fn clone(&self) -> Self {
    Io::new(self.io.clone_box())
  }
}

/// A trait defining what facilities need to be provided
/// to Laythe in terms of io
pub trait IoImpl: fmt::Debug {
  /// homebrew clone as trait objects can't be cloned. Trait objects
  /// are unsized and clone requires Sized trait
  fn clone_box(&self) -> Box<dyn IoImpl>;

  /// Create a handle to stdio facilities
  fn stdio(&self) -> Stdio;

  /// Create a handle to file system facilities
  fn fsio(&self) -> Fs;

  /// Create a handle to environment facilities
  fn envio(&self) -> Env;
}

#[derive(Debug)]
/// A mock implementation of the io systems
pub struct MockIo();

impl IoImpl for MockIo {
  fn stdio(&self) -> Stdio {
    Stdio::default()
  }
  fn fsio(&self) -> Fs {
    Fs::new(Box::new(FsMock()))
  }
  fn envio(&self) -> Env {
    Env::new(Box::new(EnvMock()))
  }
  fn clone_box(&self) -> Box<dyn IoImpl> {
    Box::new(MockIo())
  }
}

pub mod support {
  use super::*;
  use crate::stdio::support::{StdioTest, StdioTestContainer};

  #[derive(Debug, Clone)]
  /// A mock implementation of the io systems
  pub struct IoTest {
    pub stdio: StdioTest,
  }

  impl IoTest {
    pub fn new(stdio_container: &mut StdioTestContainer) -> Self {
      Self {
        stdio: stdio_container.make_stdio(),
      }
    }
  }

  impl IoImpl for IoTest {
    fn stdio(&self) -> Stdio {
      Stdio::new(Box::new(self.stdio.clone()))
    }

    fn fsio(&self) -> Fs {
      Fs::new(Box::new(FsMock()))
    }

    fn envio(&self) -> Env {
      Env::new(Box::new(EnvMock()))
    }

    fn clone_box(&self) -> Box<dyn IoImpl> {
      Box::new(self.clone())
    }
  }
}
