use laythe_env::io::NativeIo;
use laythe_vm::vm::ExecuteResult;
use support::assert_files_exit;

mod support;

fn test_files(paths: &[&str], result: ExecuteResult) -> Result<(), std::io::Error> {
  assert_files_exit(paths, FILE_PATH, NativeIo(), result)
}

const FILE_PATH: &str = file!();

#[test]
fn bool() -> Result<(), std::io::Error> {
  test_files(&vec!["std_lib/builtin/bool/str.ly"], ExecuteResult::Ok)?;

  test_files(&vec![], ExecuteResult::CompileError)?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn class() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/builtin/class/superClass.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn closure() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/builtin/closure/name.ly",
      "std_lib/builtin/closure/call.ly",
      "std_lib/builtin/closure/size.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![
      "std_lib/builtin/closure/name_wrong_args.ly",
      "std_lib/builtin/closure/call_wrong_args.ly",
      "std_lib/builtin/closure/size_wrong_args.ly",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn iter() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/builtin/iter/each.ly",
      "std_lib/builtin/iter/filter.ly",
      "std_lib/builtin/iter/into.ly",
      "std_lib/builtin/iter/iter.ly",
      "std_lib/builtin/iter/map.ly",
      "std_lib/builtin/iter/next.ly",
      "std_lib/builtin/iter/reduce.ly",
      "std_lib/builtin/iter/str.ly",
      "std_lib/builtin/iter/zip.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn list() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/builtin/list/clear.ly",
      "std_lib/builtin/list/collect.ly",
      "std_lib/builtin/list/has.ly",
      "std_lib/builtin/list/insert.ly",
      "std_lib/builtin/list/index.ly",
      "std_lib/builtin/list/iter.ly",
      "std_lib/builtin/list/pop.ly",
      "std_lib/builtin/list/push.ly",
      "std_lib/builtin/list/remove.ly",
      "std_lib/builtin/list/size.ly",
      "std_lib/builtin/list/str.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![
      "std_lib/builtin/list/insert_out_of_bounds.ly",
      "std_lib/builtin/list/remove_out_of_bounds.ly",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn map() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/builtin/map/get.ly",
      "std_lib/builtin/map/has.ly",
      "std_lib/builtin/map/insert.ly",
      "std_lib/builtin/map/iter.ly",
      "std_lib/builtin/map/remove.ly",
      "std_lib/builtin/map/set.ly",
      "std_lib/builtin/map/size.ly",
      "std_lib/builtin/map/str.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec!["std_lib/builtin/map/remove_missing_key.ly"],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn method() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/builtin/method/name.ly",
      "std_lib/builtin/method/call.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn nil() -> Result<(), std::io::Error> {
  test_files(&vec!["std_lib/builtin/nil/str.ly"], ExecuteResult::Ok)?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn number() -> Result<(), std::io::Error> {
  test_files(&vec!["std_lib/builtin/number/times.ly"], ExecuteResult::Ok)?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn str() -> Result<(), std::io::Error> {
  test_files(
    &vec!["std_lib/builtin/str/str.ly", "std_lib/builtin/str/has.ly"],
    ExecuteResult::Ok,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}
