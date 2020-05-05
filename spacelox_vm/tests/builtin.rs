use spacelox_vm::vm::ExecuteResult;
use support::test_files_inner;

mod support;

fn test_files(paths: &[&str], result: ExecuteResult) -> Result<(), std::io::Error> {
  test_files_inner(paths, FILE_PATH, result)
}

const FILE_PATH: &str = file!();

#[test]
fn bool() -> Result<(), std::io::Error> {
  test_files(&vec!["std_lib/builtin/bool/str.lox"], ExecuteResult::Ok)?;

  test_files(&vec![], ExecuteResult::CompileError)?;

  test_files(
    &vec!["std_lib/builtin/bool/str_wrong_args.lox"],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn nil() -> Result<(), std::io::Error> {
  test_files(&vec!["std_lib/builtin/nil/str.lox"], ExecuteResult::Ok)?;

  test_files(&vec![], ExecuteResult::CompileError)?;

  test_files(
    &vec!["std_lib/builtin/nil/str_wrong_args.lox"],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn closure() -> Result<(), std::io::Error> {
  test_files(&vec!["std_lib/builtin/closure/name.lox", "std_lib/builtin/closure/call.lox"], ExecuteResult::Ok)?;

  test_files(&vec![], ExecuteResult::CompileError)?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}
