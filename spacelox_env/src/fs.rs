use crate::{Error, SlIoError};
use std::fs::canonicalize;
use std::{
  io,
  path::{Path, PathBuf},
};

pub trait FsIo {
  fn read_file<P: AsRef<Path>>(&self, path: P) -> String;
  fn read_directory<P: AsRef<Path>>(&self, path: P) -> io::Result<SlDirEntry>;
  fn normalized_import<P: AsRef<Path>>(&self, base: P, import: P) -> Result<PathBuf, SlIoError>;
}

pub struct SlDirEntry {}

#[derive(Clone)]
pub struct NativeFsIo();

impl FsIo for NativeFsIo {
  fn read_file<P: AsRef<Path>>(&self, _path: P) -> String {
    todo!()
  }

  fn read_directory<P: AsRef<Path>>(&self, _path: P) -> io::Result<SlDirEntry> {
    todo!()
  }

  fn normalized_import<P: AsRef<Path>>(&self, base: P, import: P) -> Result<PathBuf, SlIoError> {
    let base = match canonicalize(base.as_ref().clone()) {
      Ok(base) => base,
      Err(_) => {
        return Err(SlIoError::new(
          Error::InvalidPath,
          format!("Base path '{}' is invalid", base.as_ref().display()),
        ))
      }
    };

    let import = match canonicalize(import.as_ref().clone()) {
      Ok(import) => import,
      Err(_) => {
        return Err(SlIoError::new(
          Error::InvalidPath,
          format!("Import path '{}' is invalid", import.as_ref().display()),
        ))
      }
    };

    match base.strip_prefix(import) {
      Ok(relative) => Ok(PathBuf::from(relative)),
      Err(_) => Err(SlIoError::new(
        Error::InvalidPath,
        "Import was not a child of the base path".to_string(),
      )),
    }
  }
}
