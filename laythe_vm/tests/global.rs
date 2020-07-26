use laythe_vm::vm::ExecuteResult;
use support::{assert_file_exit_and_stdio, assert_files_exit};

mod support;

fn test_files(paths: &[&str], result: ExecuteResult) -> Result<(), std::io::Error> {
  assert_files_exit(paths, FILE_PATH, result)
}

fn test_file_with_stdio(
  path: &str,
  stdout: Option<Vec<&str>>,
  stderr: Option<Vec<&str>>,
  result: ExecuteResult,
) -> Result<(), std::io::Error> {
  assert_file_exit_and_stdio(path, FILE_PATH, None, None, stdout, stderr, result)
}

const FILE_PATH: &str = file!();

#[test]
fn bool() -> Result<(), std::io::Error> {
  test_files(&vec!["std_lib/global/bool/str.ly"], ExecuteResult::Ok)?;

  test_files(&vec![], ExecuteResult::CompileError)?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn class() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/global/class/superClass.ly",
      "std_lib/global/class/str.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn closure() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/global/closure/name.ly",
      "std_lib/global/closure/call.ly",
      "std_lib/global/closure/size.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![
      "std_lib/global/closure/name_wrong_args.ly",
      "std_lib/global/closure/call_wrong_args.ly",
      "std_lib/global/closure/size_wrong_args.ly",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn iter() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/global/iter/each.ly",
      "std_lib/global/iter/filter.ly",
      "std_lib/global/iter/into.ly",
      "std_lib/global/iter/iter.ly",
      "std_lib/global/iter/map.ly",
      "std_lib/global/iter/next.ly",
      "std_lib/global/iter/reduce.ly",
      "std_lib/global/iter/size.ly",
      "std_lib/global/iter/str.ly",
      "std_lib/global/iter/zip.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn list() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/global/list/clear.ly",
      "std_lib/global/list/collect.ly",
      "std_lib/global/list/has.ly",
      "std_lib/global/list/insert.ly",
      "std_lib/global/list/index.ly",
      "std_lib/global/list/iter.ly",
      "std_lib/global/list/pop.ly",
      "std_lib/global/list/push.ly",
      "std_lib/global/list/remove.ly",
      "std_lib/global/list/size.ly",
      "std_lib/global/list/str.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![
      "std_lib/global/list/insert_out_of_bounds.ly",
      "std_lib/global/list/remove_out_of_bounds.ly",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn map() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/global/map/get.ly",
      "std_lib/global/map/has.ly",
      "std_lib/global/map/insert.ly",
      "std_lib/global/map/iter.ly",
      "std_lib/global/map/remove.ly",
      "std_lib/global/map/set.ly",
      "std_lib/global/map/size.ly",
      "std_lib/global/map/str.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec!["std_lib/global/map/remove_missing_key.ly"],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn method() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/global/method/name.ly",
      "std_lib/global/method/call.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn nil() -> Result<(), std::io::Error> {
  test_files(&vec!["std_lib/global/nil/str.ly"], ExecuteResult::Ok)?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn number() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/global/number/times.ly",
      "std_lib/global/number/str.ly",
      "std_lib/global/number/parse.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn print() -> Result<(), std::io::Error> {
  test_file_with_stdio(
    "std_lib/global/print/basic.ly",
    Some(vec!["10", "true", "['cat']", "{}"]),
    None,
    ExecuteResult::Ok,
  )?;

  test_file_with_stdio(
    "std_lib/global/print/multi.ly",
    Some(vec!["10 false true ['dog'] { 'cat': nil }"]),
    None,
    ExecuteResult::Ok,
  )?;

  test_file_with_stdio(
    "std_lib/global/print/with_newline_char.ly",
    Some(vec!["hi!", "bye!"]),
    None,
    ExecuteResult::Ok,
  )
}

#[test]
fn str() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/global/str/str.ly",
      "std_lib/global/str/has.ly",
      "std_lib/global/str/iter.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}
