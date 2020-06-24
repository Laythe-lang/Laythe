use crate::{Error, SlIoError};
use std::{
  fs::{canonicalize, read_to_string},
  io,
  path::{Path, PathBuf},
};

pub trait FsIo {
  fn read_file(&self, path: &Path) -> Result<String, SlIoError>;
  fn read_directory(&self, path: &Path) -> io::Result<SlDirEntry>;
  fn canonicalize(&self, path: &Path) -> Result<PathBuf, SlIoError>;
  fn relative_path(&self, base: &PathBuf, import: &Path) -> Result<PathBuf, SlIoError>;
}

pub struct SlDirEntry {}

#[derive(Clone)]
pub struct NativeFsIo();

impl Default for NativeFsIo {
  fn default() -> Self {
    Self()
  }
}

impl FsIo for NativeFsIo {
  fn read_file(&self, path: &Path) -> Result<String, SlIoError> {
    match read_to_string(path) {
      Ok(file_contents) => Ok(file_contents),
      Err(_) => panic!(),
    }
  }

  fn canonicalize(&self, path: &Path) -> Result<PathBuf, SlIoError> {
    canonicalize(path).map_err(|_| SlIoError::new(Error::InvalidPath, format!("failed {:?}", path)))
  }

  fn read_directory(&self, _path: &Path) -> io::Result<SlDirEntry> {
    todo!()
  }

  fn relative_path(&self, base: &PathBuf, import: &Path) -> Result<PathBuf, SlIoError> {
    match import.strip_prefix(base) {
      Ok(relative) => Ok(PathBuf::from(relative)),
      Err(_) => Err(SlIoError::new(
        Error::InvalidPath,
        "Import was not a child of the base path".to_string(),
      )),
    }
  }
}
