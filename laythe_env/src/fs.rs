use crate::io::IoImpl;
use std::{
  io,
  path::{Path, PathBuf}, ffi::OsString, fs::FileType,
};

/// A wrapper around file system facilities provided to Laythe
pub struct Fs {
  fs: Box<dyn FsImpl>,
}

impl Default for Fs {
  fn default() -> Self {
    Self {
      fs: Box::new(FsMock()),
    }
  }
}

impl Fs {
  /// Create a new file system wrapper
  pub fn new(fs: Box<dyn FsImpl>) -> Self {
    Self { fs }
  }

  /// Read a file into String
  pub fn read_file(&self, path: &Path) -> io::Result<String> {
    self.fs.read_file(path)
  }

  /// Read a directory for files and sub directories
  pub fn read_directory(&self, path: &Path) -> io::Result<Vec<Box<dyn LyDirEntry>>> {
    self.fs.read_directory(path)
  }

  /// Canonicalize a provided filepath
  pub fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
    self.fs.canonicalize(path)
  }

  /// Get a relative path from a base
  pub fn relative_path(&self, base: &Path, import: &Path) -> io::Result<PathBuf> {
    self.fs.relative_path(base, import)
  }
}

pub trait LyDirEntry {
  fn path(&self) -> PathBuf;
  fn file_name(&self) -> OsString;
  fn file_type(&self) -> io::Result<FileType>;
}

pub trait FsImpl: Send + Sync {
  fn read_file(&self, path: &Path) -> io::Result<String>;
  fn read_directory(&self, path: &Path) -> io::Result<Vec<Box<dyn LyDirEntry>>>;
  fn canonicalize(&self, path: &Path) -> io::Result<PathBuf>;
  fn relative_path(&self, base: &Path, import: &Path) -> io::Result<PathBuf>;
}

#[derive(Debug)]
pub struct IoFsMock();

impl IoImpl<Fs> for IoFsMock {
  fn make(&self) -> Fs {
    Fs::new(Box::new(FsMock()))
  }
}

pub struct FsMock();

impl FsImpl for FsMock {
  fn read_file(&self, _path: &Path) -> io::Result<String> {
    Ok("let x = 10;".to_string())
  }
  fn read_directory(&self, _path: &Path) -> io::Result<Vec<Box<dyn LyDirEntry>>> {
    Ok(vec![])
  }
  fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
    Ok(path.to_path_buf())
  }
  fn relative_path(&self, _base: &Path, import: &Path) -> io::Result<PathBuf> {
    Ok(import.to_path_buf())
  }
}
