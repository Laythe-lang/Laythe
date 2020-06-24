use spacelox_env::{env::NativeEnvIo, fs::NativeFsIo, io::Io, stdio::StdIo};
use spacelox_vm::vm::{ExecuteResult, Vm};
use std::fs::File;
use std::io::prelude::*;
use std::{
  cell::RefCell,
  path::{Path, PathBuf},
};
use std::{io::Result, rc::Rc};

pub fn fixture_path_inner(fixture_path: &str, test_file_path: &str) -> Option<PathBuf> {
  let test_path = Path::new(test_file_path);

  test_path
    .parent()
    .and_then(|path| path.parent())
    .and_then(|path| path.parent())
    .and_then(|path| Some(path.join("fixture").join(fixture_path)))
}

pub fn assert_files_exit<I: Io + 'static>(
  paths: &[&str],
  test_file_path: &str,
  io: I,
  result: ExecuteResult,
) -> Result<()> {
  for path in paths {
    let mut vm = Vm::new(io.clone());

    let test_path = fixture_path_inner(path, test_file_path).expect("No parent directory");
    let debug_path = test_path.to_str().map(|s| s.to_string());

    let mut file = match File::open(test_path.clone()) {
      Ok(file) => file,
      Err(err) => {
        println!("Could not find {}", test_path.to_str().unwrap());
        return Err(err);
      }
    };
    let mut source = String::new();
    file.read_to_string(&mut source)?;

    assert_eq!(
      vm.run(test_path, &source),
      result,
      "Failing file {:?}",
      debug_path
    );
  }

  Ok(())
}

pub fn assert_file_exit_and_stdio(
  path: &str,
  file_path: &str,
  stdout: Option<Vec<&str>>,
  errout: Option<Vec<&str>>,
  result: ExecuteResult,
) -> Result<()> {
  let io = MockedConsoleIo::new(MockedStdIo::default());

  assert_files_exit(&[path], file_path, io.clone(), result)?;

  if let Some(stdout) = stdout {
    io.stdio
      .stdout
      .borrow()
      .iter()
      .zip(stdout.iter())
      .for_each(|(actual, expected)| {
        assert_eq!(actual, expected);
      });

    assert_eq!(
      io.stdio.stdout.borrow().len(),
      stdout.len(),
      "Different standard out lines were collected than expected"
    );
  }

  if let Some(errout) = errout {
    io.stdio
      .errout
      .borrow()
      .iter()
      .zip(errout.iter())
      .for_each(|(actual, expected)| {
        assert_eq!(actual, expected);
      });

    assert_eq!(
      io.stdio.errout.borrow().len(),
      errout.len(),
      "Different error out lines were collected than expected"
    );
  }

  Ok(())
}
#[derive(Debug, Clone, Default)]
pub struct MockedConsoleIo {
  pub stdio: MockedStdIo,
}

impl MockedConsoleIo {
  pub fn new(stdio: MockedStdIo) -> Self {
    Self { stdio }
  }
}

impl Io for MockedConsoleIo {
  type StdIo = MockedStdIo;
  type FsIo = NativeFsIo;
  type EnvIo = NativeEnvIo;

  fn stdio(&self) -> Self::StdIo {
    self.stdio.clone()
  }

  fn fsio(&self) -> Self::FsIo {
    NativeFsIo()
  }

  fn envio(&self) -> Self::EnvIo {
    NativeEnvIo()
  }
}

#[derive(Clone, Debug)]
pub struct MockedStdIo {
  pub stdout: Rc<RefCell<Vec<String>>>,
  pub errout: Rc<RefCell<Vec<String>>>,
  pub read_lines: Rc<RefCell<Vec<String>>>,
}

impl Default for MockedStdIo {
  fn default() -> Self {
    Self {
      stdout: Rc::new(RefCell::new(vec![])),
      errout: Rc::new(RefCell::new(vec![])),
      read_lines: Rc::new(RefCell::new(vec![])),
    }
  }
}

impl StdIo for MockedStdIo {
  fn print(&self, message: &str) {
    print!("{}", message);

    match self.stdout.borrow_mut().last_mut() {
      Some(line) => line.push_str(message),
      None => self.stdout.borrow_mut().push(message.to_string()),
    }
  }
  fn println(&self, message: &str) {
    println!("{}", message);

    self.stdout.borrow_mut().push(message.to_string());
  }
  fn eprint(&self, message: &str) {
    eprint!("{}", message);

    match self.errout.borrow_mut().last_mut() {
      Some(line) => line.push_str(message),
      None => self.errout.borrow_mut().push(message.to_string()),
    }
  }
  fn eprintln(&self, message: &str) {
    eprintln!("{}", message);

    self.errout.borrow_mut().push(message.to_string());
  }
  fn flush(&self) -> Result<()> {
    Ok(())
  }
  fn read_line(&self, _buffer: &mut String) -> Result<usize> {
    Ok(0)
  }
}
