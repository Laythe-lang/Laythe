use laythe_vm::vm::VmExit;
use support::assert_files_exit;

mod support;

fn test_files(paths: &[&str], result: VmExit) -> Result<(), std::io::Error> {
  assert_files_exit(paths, FILE_PATH, result)
}

const FILE_PATH: &str = file!();

#[test]
fn env() -> Result<(), std::io::Error> {
  test_files(&["std_lib/env/cwd.lay", "std_lib/env/args.lay"], VmExit::Ok)
}
