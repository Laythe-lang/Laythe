pub mod env;
pub mod fs;
pub mod impls;
pub mod io;
pub mod managed;
pub mod memory;
pub mod stdio;

pub enum Error {
  InvalidPath,
}

pub struct SlIoError {
  pub kind: Error,
  pub message: String,
}

impl SlIoError {
  pub fn new(kind: Error, message: String) -> Self {
    Self { kind, message }
  }
}
