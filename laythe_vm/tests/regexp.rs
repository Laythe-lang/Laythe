use laythe_vm::vm::ExecuteResult;
use support::assert_files_exit;

mod support;

fn test_files(paths: &[&str], result: ExecuteResult) -> Result<(), std::io::Error> {
  assert_files_exit(paths, FILE_PATH, result)
}

const FILE_PATH: &str = file!();

#[test]
fn utils() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/regexp/class/captures.lay",
      "std_lib/regexp/class/match.lay",
      "std_lib/regexp/class/test.lay",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(&vec![], ExecuteResult::CompileError)?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}
