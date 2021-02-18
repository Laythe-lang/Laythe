use laythe_env::{
  io::Io,
  stdio::support::{IoStdioTest, StdioTestContainer, TestWriter},
};
use laythe_native::{env::IoEnvNative, fs::IoFsNative, time::IoTimeNative};
use laythe_vm::vm::{ExecuteResult, Vm};
use std::fs::File;
use std::io::prelude::*;
use std::io::{self, Cursor};
use std::path::{Path, PathBuf};
use std::str;
use std::{fmt, sync::Arc};

pub fn fixture_path_inner(fixture_path: &str, test_file_path: &str) -> Option<PathBuf> {
  let test_path = Path::new(test_file_path);

  test_path
    .parent()
    .and_then(|path| path.parent())
    .and_then(|path| path.parent())
    .and_then(|path| Some(path.join("fixture").join(fixture_path)))
}

#[allow(dead_code)]
pub fn assert_files_exit(
  paths: &[&str],
  test_file_path: &str,
  result: ExecuteResult,
) -> io::Result<()> {
  for path in paths {
    let mut stdio_container = Arc::new(StdioTestContainer::default());
    let stdio = Arc::new(IoStdioTest::new(&mut stdio_container));
    let time = Arc::new(IoTimeNative::default());
    let fs = Arc::new(IoFsNative());
    let env = Arc::new(IoEnvNative());

    {
      let io = Io::default()
        .with_stdio(stdio)
        .with_time(time)
        .with_fs(fs)
        .with_env(env);

      if let Err(err) = assert_files_exit_inner(path, test_file_path, io, result.clone()) {
        eprintln!(
          "{}",
          str::from_utf8(&*stdio_container.stdout).expect("Could not unwrap stdout")
        );
        eprintln!(
          "{}",
          str::from_utf8(&*stdio_container.stderr).expect("Could not unwrap stderr")
        );
        return Err(err);
      }
    }
  }

  Ok(())
}

// this seems like a compiler bug? this is used in language.rs
#[allow(dead_code)]
pub fn assert_file_exit_and_stdio(
  path: &str,
  file_path: &str,
  stdin: Option<String>,
  lines: Option<Vec<String>>,
  stdout: Option<Vec<&str>>,
  stderr: Option<Vec<&str>>,
  result: ExecuteResult,
) -> io::Result<()> {
  let stdio_container = Arc::new(StdioTestContainer {
    stdout: TestWriter::default(),
    stderr: TestWriter::default(),
    stdin: Box::new(Cursor::new(Vec::from(
      stdin.unwrap_or("".to_string()).as_bytes(),
    ))),
    lines: lines.unwrap_or(vec![]),
    line_index: Box::new(0),
  });
  let stdio = Arc::new(IoStdioTest::new(&stdio_container));

  {
    let io = Io::default().with_stdio(stdio);

    if let Err(err) = assert_files_exit_inner(path, file_path, io, result) {
      stdio_container.log_stdio();
      return Err(err);
    }
  }

  // assert stdout matches if provided
  if let Some(stdout) = stdout {
    let stdout_result = str::from_utf8(&stdio_container.stdout).map(|s| s.to_string());

    assert!(stdout_result.is_ok());
    let stdout_string = stdout_result.unwrap();
    let stdout_string = stdout_string.trim_end();

    let stdout_lines: Vec<&str> = stdout_string.split("\n").collect();

    stdout_lines
      .iter()
      .zip(stdout.iter())
      .for_each(|(actual, expected)| {
        assert_eq!(actual, expected);
      });

    if let Err(err) = ly_assert_eq(&stdout_lines.len(), &stdout.len(), None) {
      stdio_container.log_stdio();
      panic!("{}", err)
    }
  }

  // assert stderr matches if provided
  if let Some(stderr) = stderr {
    let stderr_result = str::from_utf8(&stdio_container.stderr).map(|s| s.to_string());

    assert!(stderr_result.is_ok());
    let stderr_string = stderr_result.unwrap();
    let stderr_string = stderr_string.trim_end();

    let stderr_lines: Vec<&str> = stderr_string.split("\n").collect();

    stderr_lines
      .iter()
      .zip(stderr.iter())
      .for_each(|(actual, expected)| {
        assert_eq!(actual, expected);
      });

    if let Err(err) = ly_assert_eq(&stderr_lines.len(), &stderr.len(), None) {
      stdio_container.log_stdio();
      panic!("{}", err)
    }
  }

  Ok(())
}

fn assert_files_exit_inner(
  path: &str,
  test_file_path: &str,
  io: Io,
  result: ExecuteResult,
) -> io::Result<()> {
  let mut vm = Vm::new(io.clone());

  let test_path = fixture_path_inner(path, test_file_path).expect("No parent directory");
  let debug_path = test_path.to_str().map(|s| s.to_string());

  let mut file = match File::open(test_path.clone()) {
    Ok(file) => file,
    Err(err) => {
      println!("Could not find {}", test_path.to_str().unwrap());
      return Err(err);
    },
  };
  let mut source = String::new();
  file.read_to_string(&mut source)?;

  ly_assert_eq(
    &vm.run(test_path, &source),
    &result,
    Some(format!("Failing file {:?}", debug_path)),
  )?;

  Ok(())
}

/// Assert equal returning a result so debug information has a chance to be captured and displayed
fn ly_assert_eq<T: PartialEq + fmt::Debug>(
  expected: &T,
  received: &T,
  message: Option<String>,
) -> io::Result<()> {
  if expected == received {
    return Ok(());
  }

  // should consider mapping io errors to something else
  Err(io::Error::new(
    io::ErrorKind::Other,
    message.unwrap_or(format!("Expected {:?} Received {:?}", expected, received)),
  ))
}
