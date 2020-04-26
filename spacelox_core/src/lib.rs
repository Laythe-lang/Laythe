#![deny(clippy::all)]
pub mod arity;
pub mod chunk;
pub mod constants;
pub mod dynamic_map;
pub mod hooks;
pub mod io;
pub mod managed;
pub mod memory;
pub mod native;
pub mod token;
pub mod utils;
pub mod value;

pub type CallResult = Result<value::Value, SpaceloxError>;

use crate::managed::Managed;
use std::fmt;

#[derive(Clone)]
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