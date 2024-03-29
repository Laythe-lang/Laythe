use laythe_env::{
  fs::{Fs, FsImpl, LyDirEntry},
  io::IoImpl,
};
use std::{
  ffi::OsString,
  fs::{canonicalize, read_dir, read_to_string, remove_file, DirEntry, File},
  io::{self, Write},
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
  fn write_file(&self, path: &Path, contents: &str) -> io::Result<()> {
    let mut file = File::create(path)?;
    file.write_all(contents.as_bytes())?;
    Ok(())
  }

  fn remove_file(&self, path: &Path) -> io::Result<()> {
    remove_file(path)
  }

  fn read_file(&self, path: &Path) -> io::Result<String> {
    read_to_string(path)
  }

  fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
    canonicalize(path)
  }

  fn read_directory(&self, path: &Path) -> io::Result<Vec<Box<dyn LyDirEntry>>> {
    Ok(
      read_dir(path)?
        .filter(|entry| entry.is_ok())
        .map(|entry| Box::new(NativeDirEntry(entry.unwrap())) as Box<dyn LyDirEntry>)
        .collect(),
    )
  }

  fn relative_path(&self, base: &Path, import: &Path) -> io::Result<PathBuf> {
    import
      .strip_prefix(base)
      .map(|prefix| prefix.to_path_buf())
      .map_err(|err| io::Error::new(io::ErrorKind::InvalidInput, err.to_string()))
  }
}

struct NativeDirEntry(DirEntry);

impl LyDirEntry for NativeDirEntry {
  fn path(&self) -> PathBuf {
    self.0.path()
  }

  fn file_name(&self) -> OsString {
    self.0.file_name()
  }

  fn file_type(&self) -> io::Result<std::fs::FileType> {
    self.0.file_type()
  }
}
