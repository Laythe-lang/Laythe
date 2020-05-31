use crate::{
  fs::{FsIo, NativeFsIo},
  stdio::{NativeStdIo, StdIo},
};
use std::fmt;

pub trait Io: fmt::Debug + Default + Copy {
  type StdIo: StdIo + Clone;
  type FsIo: FsIo + Clone;

  fn stdio(&self) -> Self::StdIo;
  fn fsio(&self) -> Self::FsIo;
}

#[derive(Debug, Default, Clone, Copy)]
pub struct NativeIo();

impl Io for NativeIo {
  type StdIo = NativeStdIo;
  type FsIo = NativeFsIo;
  fn stdio(&self) -> Self::StdIo {
    NativeStdIo()
  }
  fn fsio(&self) -> Self::FsIo {
    NativeFsIo()
  }
}
