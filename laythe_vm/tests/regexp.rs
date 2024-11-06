use laythe_vm::vm::VmExit;
use support::assert_files_exit;

mod support;

fn test_files(paths: &[&str], result: VmExit) -> Result<(), std::io::Error> {
  assert_files_exit(paths, FILE_PATH, result)
}

const FILE_PATH: &str = file!();

#[test]
fn utils() -> Result<(), std::io::Error> {
  test_files(
    &["std_lib/regexp/class/captures.lay",
      "std_lib/regexp/class/match.lay",
      "std_lib/regexp/class/test.lay"],
    VmExit::Ok,
  )?;

  test_files(&[], VmExit::CompileError)?;

  test_files(&[], VmExit::RuntimeError)
}
