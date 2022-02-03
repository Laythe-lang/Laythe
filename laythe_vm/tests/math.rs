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
    VmExit::Ok,
  )?;

  test_files(&vec![], VmExit::CompileError)?;

  test_files(&vec![], VmExit::RuntimeError)
}
