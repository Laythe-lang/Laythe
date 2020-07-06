use laythe_env::{
  fs::{FsImpl, SlDirEntry},
  LyIoError,
};
use std::{
  fs::{canonicalize, read_to_string},
  io,
  path::{Path, PathBuf},
};

#[derive(Clone)]
pub struct NativeFsio();

impl Default for NativeFsio {
  fn default() -> Self {
    Self()
  }
}

impl FsImpl for NativeFsio {
  fn read_to_string(&self, path: &Path) -> io::Result<String> {
    read_to_string(path)
  }

  fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
    canonicalize(path)
  }

  fn read_directory(&self, _path: &Path) -> io::Result<SlDirEntry> {
    todo!()
  }

  fn relative_path(&self, base: &PathBuf, import: &Path) -> Result<PathBuf, LyIoError> {
    import
      .strip_prefix(base)
      .map(|prefix| prefix.to_path_buf())
      .map_err(|err| LyIoError::new(err.to_string()))
  }
}
