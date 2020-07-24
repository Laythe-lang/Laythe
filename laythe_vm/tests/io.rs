use laythe_vm::vm::ExecuteResult;
use support::assert_file_exit_and_stdio;

mod support;

fn test_file_with_stdout(
  path: &str,
  stdout: Vec<&str>,
  result: ExecuteResult,
) -> Result<(), std::io::Error> {
  assert_file_exit_and_stdio(path, FILE_PATH, None, None, Some(stdout), None, result)
}

fn test_file_with_stderr(
  path: &str,
  stderr: Vec<&str>,
  result: ExecuteResult,
) -> Result<(), std::io::Error> {
  assert_file_exit_and_stdio(path, FILE_PATH, None, None, None, Some(stderr), result)
}

fn test_file_with_stdin(
  path: &str,
  stdin: String,
  result: ExecuteResult,
) -> Result<(), std::io::Error> {
  assert_file_exit_and_stdio(path, FILE_PATH, Some(stdin), None, None, None, result)
}

fn test_file_with_stdin_lines(
  path: &str,
  lines: Vec<String>,
  result: ExecuteResult,
) -> Result<(), std::io::Error> {
  assert_file_exit_and_stdio(path, FILE_PATH, None, Some(lines), None, None, result)
}

const FILE_PATH: &str = file!();

#[test]
fn stdio() -> Result<(), std::io::Error> {
  test_file_with_stdout(
    "std_lib/io/stdio/stdout/write.ly",
    vec!["expected 1 expected 2"],
    ExecuteResult::Ok,
  )?;

  test_file_with_stdout(
    "std_lib/io/stdio/stdout/writeln.ly",
    vec!["expected 1", "expected 2"],
    ExecuteResult::Ok,
  )?;

  test_file_with_stderr(
    "std_lib/io/stdio/stderr/write.ly",
    vec!["expected 1 expected 2"],
    ExecuteResult::Ok,
  )?;

  test_file_with_stderr(
    "std_lib/io/stdio/stderr/writeln.ly",
    vec!["expected 1", "expected 2"],
    ExecuteResult::Ok,
  )?;

  test_file_with_stdin(
    "std_lib/io/stdio/stdin/read.ly",
    "expected".to_string(),
    ExecuteResult::Ok,
  )?;

  test_file_with_stdin_lines(
    "std_lib/io/stdio/stdin/readline.ly",
    vec!["expected 1".to_string(), "expected 2".to_string()],
    ExecuteResult::Ok,
  )
}
