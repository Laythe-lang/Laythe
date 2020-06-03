use spacelox_vm::vm::{default_native_vm, ExecuteResult};
use std::fs::File;
use std::io::prelude::*;
use std::path::{Path, PathBuf};

pub fn fixture_path_inner(fixture_path: &str, test_file_path: &str) -> Option<PathBuf> {
  let test_path = Path::new(test_file_path);

  test_path
    .parent()
    .and_then(|path| path.parent())
    .and_then(|path| path.parent())
    .and_then(|path| Some(path.join("fixture").join(fixture_path)))
}

pub fn test_files_inner(
  paths: &[&str],
  test_file_path: &str,
  result: ExecuteResult,
) -> Result<(), std::io::Error> {
  for path in paths {
    let mut vm = default_native_vm();

    let assert = fixture_path_inner(path, test_file_path).expect("No parent directory");
    let debug_path = assert.to_str().map(|s| s.to_string());

    let mut file = match File::open(assert.clone()) {
      Ok(file) => file,
      Err(err) => {
        println!("Could not find {}", assert.to_str().unwrap());
        return Err(err);
      }
    };
    let mut source = String::new();
    file.read_to_string(&mut source)?;
    let module_name = PathBuf::from("./Test.lox");

    assert_eq!(
      vm.run(module_name, &source),
      result,
      "Failing file {:?}",
      debug_path
    );
  }

  Ok(())
}
