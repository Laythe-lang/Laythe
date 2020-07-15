use io::{Stderr, Stdin, Stdout};
use laythe_env::{
  io::IoImpl,
  stdio::{Stdio, StdioImpl},
};
use std::io::{self, stderr, stdin, stdout};

#[derive(Debug)]
pub struct IoStdioNative();

impl IoImpl<Stdio> for IoStdioNative {
  fn make(&self) -> Stdio {
    Stdio::new(Box::new(StdioNative::default()))
  }
}

#[derive(Debug)]
pub struct StdioNative {
  stdout: Stdout,
  stderr: Stderr,
  stdin: Stdin,
}

impl Default for StdioNative {
  fn default() -> Self {
    Self {
      stdout: stdout(),
      stderr: stderr(),
      stdin: stdin(),
    }
  }
}

impl StdioImpl for StdioNative {
  fn stdout(&mut self) -> &mut dyn std::io::Write {
    &mut self.stdout
  }

  fn stderr(&mut self) -> &mut dyn std::io::Write {
    &mut self.stderr
  }

  fn stdin(&mut self) -> &mut dyn std::io::Read {
    &mut self.stdin
  }

  fn read_line(&self, buffer: &mut String) -> io::Result<usize> {
    stdin().read_line(buffer)
  }
}
