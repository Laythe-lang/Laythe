use crate::{
  env::{Env, IoEnvMock},
  fs::{Fs, IoFsMock},
  stdio::{IoStdioMock, Stdio},
  time::{IoTimeMock, Time},
};
use std::{fmt, rc::Rc};

// type IoImpl<T> = Rc<dyn Fn() -> T>;

#[derive(Debug)]
/// A struct wrapping the externally provided io to Laythe
pub struct Io {
  stdio_impl: Rc<dyn IoImpl<Stdio>>,
  fs_impl: Rc<dyn IoImpl<Fs>>,
  env_impl: Rc<dyn IoImpl<Env>>,
  time_impl: Rc<dyn IoImpl<Time>>,
}

impl Default for Io {
  fn default() -> Self {
    Self {
      stdio_impl: Rc::new(IoStdioMock()),
      fs_impl: Rc::new(IoFsMock()),
      env_impl: Rc::new(IoEnvMock()),
      time_impl: Rc::new(IoTimeMock()),
    }
  }
}

impl Io {
  /// Create a new io wrapper uses the provided io impl
  pub fn new(
    stdio_impl: Rc<dyn IoImpl<Stdio>>,
    fs_impl: Rc<dyn IoImpl<Fs>>,
    env_impl: Rc<dyn IoImpl<Env>>,
    time_impl: Rc<dyn IoImpl<Time>>,
  ) -> Self {
    Self {
      stdio_impl,
      fs_impl,
      env_impl,
      time_impl,
    }
  }

  /// Replace this stdio implementation
  pub fn with_stdio(self, stdio_impl: Rc<dyn IoImpl<Stdio>>) -> Self {
    Self {
      stdio_impl,
      fs_impl: self.fs_impl,
      env_impl: self.env_impl,
      time_impl: self.time_impl,
    }
  }

  /// Replace this fs implementation
  pub fn with_fs(self, fs_impl: Rc<dyn IoImpl<Fs>>) -> Self {
    Self {
      stdio_impl: self.stdio_impl,
      fs_impl: fs_impl,
      env_impl: self.env_impl,
      time_impl: self.time_impl,
    }
  }

  /// Replace this env implementation
  pub fn with_env(self, env_impl: Rc<dyn IoImpl<Env>>) -> Self {
    Self {
      stdio_impl: self.stdio_impl,
      fs_impl: self.fs_impl,
      env_impl,
      time_impl: self.time_impl,
    }
  }

  /// Replace this env implementation
  pub fn with_time(self, time_impl: Rc<dyn IoImpl<Time>>) -> Self {
    Self {
      stdio_impl: self.stdio_impl,
      fs_impl: self.fs_impl,
      env_impl: self.env_impl,
      time_impl,
    }
  }

  /// Generate a wrapper to stdio facilities
  pub fn stdio(&self) -> Stdio {
    self.stdio_impl.make()
  }

  /// Generate a wrapper to file system facilities
  pub fn fs(&self) -> Fs {
    self.fs_impl.make()
  }

  /// Generate a wrapper to environment facilities
  pub fn env(&self) -> Env {
    self.env_impl.make()
  }

  /// Generate a wrapper to time facilities
  pub fn time(&self) -> Time {
    self.time_impl.make()
  }
}

impl Clone for Io {
  fn clone(&self) -> Self {
    Io::new(
      Rc::clone(&self.stdio_impl),
      Rc::clone(&self.fs_impl),
      Rc::clone(&self.env_impl),
      Rc::clone(&self.time_impl),
    )
  }
}

pub trait IoImpl<T>: fmt::Debug {
  fn make(&self) -> T;
}
