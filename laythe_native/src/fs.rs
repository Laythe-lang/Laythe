use laythe_env::{
  fs::{Fs, FsImpl, SlDirEntry},
  io::IoImpl,
};
use std::{
  fs::{canonicalize, read_to_string},
  io,
  path::{Path, PathBuf},
};

#[derive(Debug)]
pub struct IoFsNative();

impl IoImpl<Fs> for IoFsNative {
  fn make(&self) -> Fs {
    Fs::new(Box::new(FsNative()))
  }
}

#[derive(Clone, Default)]
pub struct FsNative();

impl FsImpl for FsNative {
  fn read_to_string(&self, path: &Path) -> io::Result<String> {
    read_to_string(path)
  }

  fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
    canonicalize(path)
  }

  fn read_directory(&self, _path: &Path) -> io::Result<SlDirEntry> {
    todo!()
  }

  fn relative_path(&self, base: &Path, import: &Path) -> io::Result<PathBuf> {
    import
      .strip_prefix(base)
      .map(|prefix| prefix.to_path_buf())
      .map_err(|err| io::Error::new(io::ErrorKind::InvalidInput, err.to_string()))
  }
}
