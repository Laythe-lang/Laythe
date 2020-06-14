use spacelox_vm::vm::ExecuteResult;
use support::test_files_inner;

mod support;

fn test_files(paths: &[&str], result: ExecuteResult) -> Result<(), std::io::Error> {
  test_files_inner(paths, FILE_PATH, result)
}

const FILE_PATH: &str = file!();

#[test]
fn utils() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/math/utils/abs.lox",
      "std_lib/math/utils/cos.lox",
      "std_lib/math/utils/ln.lox",
      "std_lib/math/utils/sin.lox" 
    ],
    ExecuteResult::Ok,
  )?;

  test_files(&vec![], ExecuteResult::CompileError)?;

  test_files(
    &vec![
      "std_lib/math/utils/cos_wrong_args.lox",
      "std_lib/math/utils/cos_wrong_type.lox",
      "std_lib/math/utils/sin_wrong_args.lox",
      "std_lib/math/utils/sin_wrong_args.lox",
    ],
    ExecuteResult::RuntimeError,
  )
}
