pub mod env;
pub mod fs;
pub mod impls;
pub mod io;
pub mod managed;
pub mod memory;
pub mod stdio;

pub struct LyIoError {
  pub message: String,
}

impl LyIoError {
  pub fn new(message: String) -> Self {
    Self { message }
  }
}
