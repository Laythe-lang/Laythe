use spacelox_core::{module::Module, object::SlHashMap};
use spacelox_env::{io::Io, managed::Managed};

pub struct DepLoader<I: Io> {
  io: I,

  cache: SlHashMap<Managed<String>, Managed<Module>>,
}

impl<I: Io> DepLoader<I> {
  pub fn new(io: I) -> Self {
    Self {
      io,
      cache: SlHashMap::default(),
    }
  }
}

impl<I: Io + Default> Default for DepLoader<I> {
  fn default() -> Self {
    Self {
      io: I::default(),
      cache: SlHashMap::default(),
    }
  }
}
