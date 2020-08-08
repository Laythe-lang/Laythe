#![deny(clippy::all)]
pub mod chunk;
pub mod constants;
pub mod dynamic_map;
pub mod hooks;
pub mod iterator;
pub mod lin_map;
pub mod module;
pub mod native;
pub mod object;
pub mod package;
pub mod signature;
pub mod token;
pub mod utils;
pub mod value;

pub type LyResult<T> = Result<T, Box<LyError>>;
pub type CallResult = LyResult<value::Value>;
pub type LyHashSet<K> = HashSet<K, FnvBuildHasher>;

use fnv::FnvBuildHasher;
use hashbrown::HashSet;
use laythe_env::managed::Managed;
use smol_str::SmolStr;
use std::fmt;

#[derive(Clone, PartialEq, Debug)]
pub struct LyError {
  pub message: Managed<SmolStr>,
  pub exit: bool,
  inner: Option<Box<LyError>>,
}

impl LyError {
  pub fn new(message: Managed<SmolStr>) -> Self {
    Self {
      message,
      exit: false,
      inner: None,
    }
  }

  pub fn exit(message: Managed<SmolStr>) -> Self {
    Self {
      message,
      exit: true,
      inner: None,
    }
  }
}

impl fmt::Display for LyError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.message)
  }
}
