use crate::{
  env::{EnvIo, NativeEnvIo},
  fs::{FsIo, NativeFsIo},
  stdio::{NativeStdIo, StdIo},
};
use std::fmt;

pub trait Io: fmt::Debug + Default + Copy {
  type StdIo: StdIo + Clone;
  type FsIo: FsIo + Clone;
  type EnvIo: EnvIo + Clone;

  fn stdio(&self) -> Self::StdIo;
  fn fsio(&self) -> Self::FsIo;
  fn envio(&self) -> Self::EnvIo;
}

#[derive(Debug, Default, Clone, Copy)]
pub struct NativeIo();

impl Io for NativeIo {
  type StdIo = NativeStdIo;
  type FsIo = NativeFsIo;
  type EnvIo = NativeEnvIo;

  fn stdio(&self) -> Self::StdIo {
    NativeStdIo()
  }

  fn fsio(&self) -> Self::FsIo {
    NativeFsIo()
  }

  fn envio(&self) -> Self::EnvIo {
    NativeEnvIo()
  }
}
