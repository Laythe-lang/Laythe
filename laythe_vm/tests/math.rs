use laythe_env::io::NativeIo;
use laythe_vm::vm::ExecuteResult;
use support::assert_files_exit;

mod support;

fn test_files(paths: &[&str], result: ExecuteResult) -> Result<(), std::io::Error> {
  assert_files_exit(paths, FILE_PATH, NativeIo(), result)
}

const FILE_PATH: &str = file!();

#[test]
fn utils() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/math/utils/abs.ly",
      "std_lib/math/utils/cos.ly",
      "std_lib/math/utils/ln.ly",
      "std_lib/math/utils/sin.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(&vec![], ExecuteResult::CompileError)?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}
