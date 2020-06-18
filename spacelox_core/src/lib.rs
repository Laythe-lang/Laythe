#![deny(clippy::all)]
pub mod chunk;
pub mod constants;
pub mod dynamic_map;
pub mod hooks;
pub mod iterator;
pub mod module;
pub mod native;
pub mod object;
pub mod package;
pub mod signature;
pub mod token;
pub mod utils;
pub mod value;

pub type CallResult = Result<value::Value, SlError>;
pub type ModuleResult<T> = Result<T, SlError>;
pub type PackageResult<T> = Result<T, SlError>;
pub type SlHashSet<K> = HashSet<K, FnvBuildHasher>;

use fnv::FnvBuildHasher;
use hashbrown::HashSet;
use spacelox_env::managed::Managed;
use std::fmt;

#[derive(Clone, PartialEq, Debug)]
pub struct SlError {
  pub message: Managed<String>,
  inner: Option<Box<SlError>>,
}

impl SlError {
  pub fn new(message: Managed<String>) -> Self {
    Self {
      message,
      inner: None,
    }
  }
}

impl fmt::Display for SlError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.message)
  }
}
