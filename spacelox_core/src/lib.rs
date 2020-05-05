#![deny(clippy::all)]
pub mod arity;
pub mod chunk;
pub mod constants;
pub mod dynamic_map;
pub mod hooks;
pub mod io;
pub mod iterator;
pub mod managed;
pub mod memory;
pub mod module;
pub mod native;
pub mod package;
pub mod token;
pub mod utils;
pub mod value;

pub type CallResult = Result<value::Value, SpaceloxError>;
pub type ModuleResult<T> = Result<T, SpaceloxError>;
pub type PackageResult<T> = Result<T, SpaceloxError>;
pub type SlHashMap<K, V> = HashMap<K, V, FnvBuildHasher>;
pub type SymbolResult = Result<SlHashMap<Managed<String>, Value>, SpaceloxError>;

use crate::managed::Managed;
use fnv::FnvBuildHasher;
use hashbrown::HashMap;
use std::fmt;
use value::Value;

#[derive(Clone, Debug)]
pub struct SpaceloxError {
  pub message: Managed<String>,
  inner: Option<Box<SpaceloxError>>,
}

impl SpaceloxError {
  pub fn new(message: Managed<String>) -> Self {
    Self {
      message,
      inner: None,
    }
  }
}

impl fmt::Display for SpaceloxError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.message)
  }
}
