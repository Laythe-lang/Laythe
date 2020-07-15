use crate::io::IoImpl;
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

#[derive(Debug)]
pub struct IoStdioMock();

impl IoImpl<Stdio> for IoStdioMock {
  fn make(&self) -> Stdio {
    Stdio::new(Box::new(StdioMock::default()))
  }
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

pub mod support {
  use super::*;
  use std::io::{Cursor, Write};
  use std::{rc::Rc, str};

  #[derive(Debug)]
  pub struct IoStdioTest {
    stdio_container: Rc<StdioTestContainer>,
  }

  impl Default for IoStdioTest {
    fn default() -> Self {
      Self {
        stdio_container: Rc::new(StdioTestContainer::default()),
      }
    }
  }

  impl IoStdioTest {
    pub fn new(stdio_container: &Rc<StdioTestContainer>) -> Self {
      Self {
        stdio_container: Rc::clone(stdio_container),
      }
    }
  }

  impl IoImpl<Stdio> for IoStdioTest {
    fn make(&self) -> Stdio {
      Stdio::new(Box::new(self.stdio_container.make_stdio()))
    }
  }

  #[derive(Debug)]
  pub struct StdioTestContainer {
    pub stdout: Box<Vec<u8>>,
    pub stderr: Box<Vec<u8>>,
    pub stdin: Box<Cursor<Vec<u8>>>,
    pub lines: Box<Vec<String>>,
    pub line_index: Box<usize>,
  }

  impl Default for StdioTestContainer {
    fn default() -> Self {
      Self {
        stdout: Box::new(vec![]),
        stderr: Box::new(vec![]),
        stdin: Box::new(Cursor::new(vec![])),
        lines: Box::new(vec![]),
        line_index: Box::new(0),
      }
    }
  }

  impl StdioTestContainer {
    pub fn with_lines(lines: Vec<String>) -> Self {
      Self {
        stdout: Box::new(vec![]),
        stderr: Box::new(vec![]),
        stdin: Box::new(Cursor::new(vec![])),
        lines: Box::new(lines),
        line_index: Box::new(0),
      }
    }

    pub fn with_stdin(buf: &[u8]) -> Self {
      Self {
        stdout: Box::new(vec![]),
        stderr: Box::new(vec![]),
        stdin: Box::new(Cursor::new(Vec::from(buf))),
        lines: Box::new(vec![]),
        line_index: Box::new(0),
      }
    }

    /// Note this should NEVER be used in the actual vm. This deliberately
    /// perverts the rust safety checks
    pub fn make_stdio(&self) -> StdioTest {
      StdioTest {
        stdout: &*self.stdout as *const Vec<u8> as *mut Vec<u8>,
        stdin: &*self.stdin as *const Cursor<Vec<u8>> as *mut Cursor<Vec<u8>>,
        stderr: &*self.stderr as *const Vec<u8> as *mut Vec<u8>,
        lines: &*self.lines as *const Vec<String> as *mut Vec<String>,
        line_index: &*self.line_index as *const usize as *mut usize,
      }
    }

    pub fn log_stdio(&self) {
      eprintln!(
        "{}",
        str::from_utf8(&*self.stdout).expect("Could not unwrap stdout")
      );
      eprintln!(
        "{}",
        str::from_utf8(&*self.stderr).expect("Could not unwrap stderr")
      );
    }
  }

  #[derive(Debug, Clone)]
  pub struct StdioTest {
    pub stdout: *mut Vec<u8>,
    pub stderr: *mut Vec<u8>,
    pub stdin: *mut Cursor<Vec<u8>>,
    pub lines: *mut Vec<String>,
    line_index: *mut usize,
  }

  impl StdioImpl for StdioTest {
    fn stdout(&mut self) -> &mut dyn Write {
      unsafe { &mut *self.stdout }
    }
    fn stderr(&mut self) -> &mut dyn Write {
      unsafe { &mut *self.stderr }
    }
    fn stdin(&mut self) -> &mut dyn std::io::Read {
      unsafe { &mut *self.stdin }
    }
    fn read_line(&self, buffer: &mut String) -> std::io::Result<usize> {
      unsafe {
        let line = match (&*self.lines).get(*self.line_index) {
          Some(line) => line.clone(),
          None => panic!("Not enough test lines"),
        };

        buffer.push_str(&line);

        *self.line_index = *self.line_index + 1;
        Ok(line.len())
      }
    }
  }
}
