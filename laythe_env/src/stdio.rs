use crate::io::IoImpl;
use std::io::{self, Read, Write};
use termcolor::WriteColor;

/// A wrapper the provided facilities around standard input output and err
pub struct Stdio {
  stdio: Box<dyn StdioImpl>,
}

#[allow(clippy::derivable_impls)]
impl Default for Stdio {
  fn default() -> Self {
    Self {
      stdio: Box::<StdioMock>::default(),
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

  /// Get a WriteColor to stderr
  pub fn stderr_color(&mut self) -> &mut dyn WriteColor {
    self.stdio.stderr_color()
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
  fn stderr_color(&mut self) -> &mut dyn WriteColor;
  fn stdin(&mut self) -> &mut dyn Read;

  fn read_line(&self, buffer: &mut String) -> io::Result<usize>;
}

#[derive(Debug)]
pub struct IoStdioMock();

impl IoImpl<Stdio> for IoStdioMock {
  fn make(&self) -> Stdio {
    Stdio::new(Box::<StdioMock>::default())
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
  fn stderr_color(&mut self) -> &mut dyn WriteColor {
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

impl WriteColor for MockWrite {
  fn supports_color(&self) -> bool {
    false
  }

  fn set_color(&mut self, _: &termcolor::ColorSpec) -> io::Result<()> {
    Ok(())
  }

  fn reset(&mut self) -> io::Result<()> {
    Ok(())
  }
}

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
  use std::{
    io::{Cursor, Write},
    ops::Deref,
    str,
    sync::Arc,
  };
  use termcolor::WriteColor;

  #[derive(Default, Debug)]
  pub struct TestWriter(Vec<u8>);

  impl WriteColor for TestWriter {
    fn supports_color(&self) -> bool {
      false
    }

    fn set_color(&mut self, _: &termcolor::ColorSpec) -> io::Result<()> {
      Ok(())
    }

    fn reset(&mut self) -> io::Result<()> {
      Ok(())
    }
  }

  impl Write for TestWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
      self.0.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
      self.0.flush()
    }
  }

  impl Deref for TestWriter {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
      self.0.deref()
    }
  }

  #[derive(Debug)]
  pub struct IoStdioTest {
    stdio_container: Arc<StdioTestContainer>,
  }

  impl Default for IoStdioTest {
    fn default() -> Self {
      Self {
        stdio_container: Arc::new(StdioTestContainer::default()),
      }
    }
  }

  impl IoStdioTest {
    pub fn new(stdio_container: &Arc<StdioTestContainer>) -> Self {
      Self {
        stdio_container: Arc::clone(stdio_container),
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
    pub stdout: TestWriter,
    pub stderr: TestWriter,
    pub stdin: Box<Cursor<Vec<u8>>>,
    pub lines: Vec<String>,
    pub line_index: Box<usize>,
  }

  impl Default for StdioTestContainer {
    fn default() -> Self {
      Self {
        stdout: TestWriter::default(),
        stderr: TestWriter::default(),
        stdin: Box::new(Cursor::new(vec![])),
        lines: vec![],
        line_index: Box::new(0),
      }
    }
  }

  impl StdioTestContainer {
    pub fn with_lines(lines: Vec<String>) -> Self {
      Self {
        stdout: TestWriter::default(),
        stderr: TestWriter::default(),
        stdin: Box::new(Cursor::new(vec![])),
        lines,
        line_index: Box::new(0),
      }
    }

    pub fn with_stdin(buf: &[u8]) -> Self {
      Self {
        stdout: TestWriter::default(),
        stderr: TestWriter::default(),
        stdin: Box::new(Cursor::new(Vec::from(buf))),
        lines: vec![],
        line_index: Box::new(0),
      }
    }

    /// Note this should NEVER be used in the actual vm. This deliberately
    /// perverts the rust safety checks
    pub fn make_stdio(&self) -> StdioTest {
      StdioTest {
        stdout: &self.stdout as *const TestWriter as *mut TestWriter,
        stdin: &*self.stdin as *const Cursor<Vec<u8>> as *mut Cursor<Vec<u8>>,
        stderr: &self.stderr as *const TestWriter as *mut TestWriter,
        lines: &self.lines as *const Vec<String> as *mut Vec<String>,
        line_index: &*self.line_index as *const usize as *mut usize,
      }
    }

    pub fn log_stdio(&self) {
      eprintln!(
        "{}",
        str::from_utf8(&self.stdout.0).expect("Could not unwrap stdout")
      );
      eprintln!(
        "{}",
        str::from_utf8(&self.stderr.0).expect("Could not unwrap stderr")
      );
    }
  }

  /// Note this implementation is definitely not actually thread safe and shouldn't
  /// be used outside the context of a single thread test.
  #[derive(Debug, Clone)]
  pub struct StdioTest {
    pub stdout: *mut TestWriter,
    pub stderr: *mut TestWriter,
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
    fn stderr_color(&mut self) -> &mut dyn WriteColor {
      unsafe { &mut *self.stderr }
    }
    fn stdin(&mut self) -> &mut dyn std::io::Read {
      unsafe { &mut *self.stdin }
    }
    fn read_line(&self, buffer: &mut String) -> std::io::Result<usize> {
      unsafe {
        let line = match (*self.lines).get(*self.line_index) {
          Some(line) => line.clone(),
          None => panic!("Not enough test lines"),
        };

        buffer.push_str(&line);

        *self.line_index += 1;
        Ok(line.len())
      }
    }
  }
}
