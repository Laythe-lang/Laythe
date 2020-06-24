use std::io::{stdin, stdout, Result, Write};

pub trait StdIo {
  fn print(&self, message: &str);
  fn println(&self, message: &str);
  fn eprint(&self, message: &str);
  fn eprintln(&self, message: &str);
  fn flush(&self) -> Result<()>;
  fn read_line(&self, buffer: &mut String) -> Result<usize>;
}

#[derive(Debug, Clone)]
pub struct NativeStdIo();

impl Default for NativeStdIo {
  fn default() -> Self {
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
