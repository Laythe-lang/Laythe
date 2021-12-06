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
      "std_lib/math/utils/abs.lay",
      "std_lib/math/utils/cos.lay",
      "std_lib/math/utils/ln.lay",
      "std_lib/math/utils/max.lay",
      "std_lib/math/utils/min.lay",
      "std_lib/math/utils/rand.lay",
      "std_lib/math/utils/rem.lay",
      "std_lib/math/utils/sin.lay",
    ],
    ExecuteResult::Ok(0),
  )?;

  test_files(&vec![], ExecuteResult::CompileError)?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}
