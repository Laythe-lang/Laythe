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
  test_files(&vec!["std_lib/global/bool/str.lay"], ExecuteResult::Ok)?;

  test_files(&vec![], ExecuteResult::CompileError)?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn class() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/global/class/superClass.lay",
      "std_lib/global/class/str.lay",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn closure() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/global/closure/name.lay",
      "std_lib/global/closure/call.lay",
      "std_lib/global/closure/len.lay",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![
      "std_lib/global/closure/name_wrong_args.lay",
      "std_lib/global/closure/call_wrong_args.lay",
      "std_lib/global/closure/size_wrong_args.lay",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn iter() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/global/iter/all.lay",
      "std_lib/global/iter/any.lay",
      "std_lib/global/iter/each.lay",
      "std_lib/global/iter/filter.lay",
      "std_lib/global/iter/into.lay",
      "std_lib/global/iter/iter.lay",
      "std_lib/global/iter/len.lay",
      "std_lib/global/iter/map.lay",
      "std_lib/global/iter/next.lay",
      "std_lib/global/iter/reduce.lay",
      "std_lib/global/iter/str.lay",
      "std_lib/global/iter/zip.lay",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn list() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/global/list/clear.lay",
      "std_lib/global/list/collect.lay",
      "std_lib/global/list/has.lay",
      "std_lib/global/list/insert.lay",
      "std_lib/global/list/index.lay",
      "std_lib/global/list/iter.lay",
      "std_lib/global/list/pop.lay",
      "std_lib/global/list/push.lay",
      "std_lib/global/list/remove.lay",
      "std_lib/global/list/len.lay",
      "std_lib/global/list/str.lay",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![
      "std_lib/global/list/insert_out_of_bounds.lay",
      "std_lib/global/list/remove_out_of_bounds.lay",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn map() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/global/map/get.lay",
      "std_lib/global/map/has.lay",
      "std_lib/global/map/insert.lay",
      "std_lib/global/map/iter.lay",
      "std_lib/global/map/remove.lay",
      "std_lib/global/map/set.lay",
      "std_lib/global/map/len.lay",
      "std_lib/global/map/str.lay",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec!["std_lib/global/map/remove_missing_key.lay"],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn method() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/global/method/name.lay",
      "std_lib/global/method/call.lay",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn nil() -> Result<(), std::io::Error> {
  test_files(&vec!["std_lib/global/nil/str.lay"], ExecuteResult::Ok)?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn number() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/global/number/times.lay",
      "std_lib/global/number/str.lay",
      "std_lib/global/number/parse.lay",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn object() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/global/object/cls.lay",
      "std_lib/global/object/equals.lay",
      "std_lib/global/object/str.lay",
    ],
    ExecuteResult::Ok,
  )
}

#[test]
fn print() -> Result<(), std::io::Error> {
  test_file_with_stdio(
    "std_lib/global/print/basic.lay",
    Some(vec!["10", "true", "['cat']", "{}"]),
    None,
    ExecuteResult::Ok,
  )?;

  test_file_with_stdio(
    "std_lib/global/print/multi.lay",
    Some(vec!["10 false true ['dog'] { 'cat': nil }"]),
    None,
    ExecuteResult::Ok,
  )?;

  test_file_with_stdio(
    "std_lib/global/print/with_newline_char.lay",
    Some(vec!["hi!", "bye!"]),
    None,
    ExecuteResult::Ok,
  )
}

#[test]
fn str() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/global/str/str.lay",
      "std_lib/global/str/has.lay",
      "std_lib/global/str/iter.lay",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}
