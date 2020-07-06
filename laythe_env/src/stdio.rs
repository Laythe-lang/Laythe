use io::{Read, Write};
use std::io;

/// A wrapper the provided facilities around standard input output and err
pub struct Stdio {
  stdio: Box<dyn StdioImpl>,
}

impl Default for Stdio {
  fn default() -> Self {
    Self {
      stdio: Box::new(StdioMock::default()),
    }
  }
}

impl Stdio {
  /// Create a new wrapper from the provided stdio facilities
  pub fn new(stdio: Box<dyn StdioImpl>) -> Self {
    Self { stdio }
  }

  /// Get a Write to stdout
  pub fn stdout(&mut self) -> &mut dyn Write {
    self.stdio.stdout()
  }

  /// Get a Write to stderr
  pub fn stderr(&mut self) -> &mut dyn Write {
    self.stdio.stderr()
  }

  /// Get a Read to stdin
  pub fn stdin(&mut self) -> &mut dyn Read {
    self.stdio.stdin()
  }

  /// Read a line from standard in
  pub fn read_line(&self, buffer: &mut String) -> io::Result<usize> {
    self.stdio.read_line(buffer)
  }
}

pub trait StdioImpl {
  fn stdout(&mut self) -> &mut dyn Write;
  fn stderr(&mut self) -> &mut dyn Write;
  fn stdin(&mut self) -> &mut dyn Read;

  fn read_line(&self, buffer: &mut String) -> io::Result<usize>;
}

pub struct StdioMock {
  write: MockWrite,
  read: MockRead,
}

impl Default for StdioMock {
  fn default() -> Self {
    Self {
      write: MockWrite(),
      read: MockRead(),
    }
  }
}

impl StdioImpl for StdioMock {
  fn stdout(&mut self) -> &mut dyn Write {
    &mut self.write
  }
  fn stderr(&mut self) -> &mut dyn Write {
    &mut self.write
  }
  fn stdin(&mut self) -> &mut dyn Read {
    &mut self.read
  }
  fn read_line(&self, buffer: &mut String) -> io::Result<usize> {
    const LINE: &str = "let x = 10;";
    buffer.push_str(LINE);

    Ok(LINE.len())
  }
}

pub struct MockWrite();

impl Write for MockWrite {
  fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
    Ok(buf.len())
  }
  fn flush(&mut self) -> io::Result<()> {
    Ok(())
  }
}

pub struct MockRead();

impl Read for MockRead {
  fn read(&mut self, _buf: &mut [u8]) -> io::Result<usize> {
    Ok(0)
  }
}
