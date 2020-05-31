use std::{io::Result, path::Path};

pub trait FsIo {
  fn read_file<P: AsRef<Path>>(&self, path: P) -> String;
  fn read_directory<P: AsRef<Path>>(&self, path: P) -> Result<SlDirEntry>;
}

pub struct SlDirEntry {}

#[derive(Clone)]
pub struct NativeFsIo();

impl FsIo for NativeFsIo {
  fn read_file<P: AsRef<Path>>(&self, _path: P) -> String {
    todo!()
  }
  fn read_directory<P: AsRef<Path>>(&self, _path: P) -> Result<SlDirEntry> {
    todo!()
  }
}
