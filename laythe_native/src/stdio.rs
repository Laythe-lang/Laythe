use io::Stdin;
use laythe_env::{
  io::IoImpl,
  stdio::{Stdio, StdioImpl},
};
use std::io::{self, stdin, Write};
use termcolor::{StandardStream, WriteColor};

#[derive(Debug)]
pub struct IoStdioNative();

impl IoImpl<Stdio> for IoStdioNative {
  fn make(&self) -> Stdio {
    Stdio::new(Box::<StdioNative>::default())
  }
}

pub struct StdioNative {
  stdout: StandardStream,
  stderr: StandardStream,
  stdin: Stdin,
}

impl Default for StdioNative {
  fn default() -> Self {
    Self {
      stdout: StandardStream::stdout(termcolor::ColorChoice::Always),
      stderr: StandardStream::stderr(termcolor::ColorChoice::Always),
      stdin: stdin(),
    }
  }
}

impl StdioImpl for StdioNative {
  fn stdout(&mut self) -> &mut dyn Write {
    &mut self.stdout
  }

  fn stderr(&mut self) -> &mut dyn Write {
    &mut self.stderr
  }

  fn stderr_color(&mut self) -> &mut dyn WriteColor {
    &mut self.stderr
  }

  fn stdin(&mut self) -> &mut dyn std::io::Read {
    &mut self.stdin
  }

  fn read_line(&self, buffer: &mut String) -> io::Result<usize> {
    stdin().read_line(buffer)
  }
}
