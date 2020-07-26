use laythe_vm::vm::ExecuteResult;
use support::assert_files_exit;

mod support;

fn test_files(paths: &[&str], result: ExecuteResult) -> Result<(), std::io::Error> {
  assert_files_exit(paths, FILE_PATH, result)
}

const FILE_PATH: &str = file!();

#[test]
fn env() -> Result<(), std::io::Error> {
  test_files(
    &["std_lib/env/cwd.ly", "std_lib/env/args.ly"],
    ExecuteResult::Ok,
  )
}
