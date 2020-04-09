use std::fmt;
use std::io::{stdin, stdout, Result, Write};

pub trait Io: fmt::Debug + Default + Copy {
  type StdIo: StdIo + Clone;

  fn stdio(&self) -> Self::StdIo;
}

pub trait StdIo {
  fn print(&self, message: &str);
  fn println(&self, message: &str);
  fn eprint(&self, message: &str);
  fn eprintln(&self, message: &str);
  fn flush(&self) -> Result<()>;
  fn read_line(&self, buffer: &mut String) -> Result<usize>;
}

#[derive(Debug, Default, Clone, Copy)]
pub struct NativeIo();

impl NativeIo {
  pub fn new() -> Self {
    Self()
  }
}

impl Io for NativeIo {
  type StdIo = NativeStdIo;

  fn stdio(&self) -> Self::StdIo {
    NativeStdIo::new()
  }
}

#[derive(Debug, Clone)]
pub struct NativeStdIo();

impl NativeStdIo {
  pub fn new() -> Self {
    Self()
  }
}

impl StdIo for NativeStdIo {
  fn print(&self, message: &str) {
    print!("{}", message);
  }
  fn println(&self, message: &str) {
    println!("{}", message);
  }
  fn eprint(&self, message: &str) {
    eprint!("{}", message);
  }
  fn eprintln(&self, message: &str) {
    eprintln!("{}", message);
  }
  fn flush(&self) -> Result<()> {
    stdout().flush()
  }
  fn read_line(&self, buffer: &mut String) -> Result<usize> {
    stdin().read_line(buffer)
  }
}
