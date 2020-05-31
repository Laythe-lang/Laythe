use spacelox_vm::vm::ExecuteResult;
use support::test_files_inner;

mod support;

fn test_files(paths: &[&str], result: ExecuteResult) -> Result<(), std::io::Error> {
  test_files_inner(paths, FILE_PATH, result)
}

const FILE_PATH: &str = file!();

#[test]
fn bool() -> Result<(), std::io::Error> {
  test_files(&vec!["std_lib/builtin/bool/str.lox"], ExecuteResult::Ok)?;

  test_files(&vec![], ExecuteResult::CompileError)?;

  test_files(
    &vec!["std_lib/builtin/bool/str_wrong_args.lox"],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn nil() -> Result<(), std::io::Error> {
  test_files(&vec!["std_lib/builtin/nil/str.lox"], ExecuteResult::Ok)?;

  test_files(
    &vec!["std_lib/builtin/nil/str_wrong_args.lox"],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn closure() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/builtin/closure/name.lox",
      "std_lib/builtin/closure/call.lox",
      "std_lib/builtin/closure/size.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![
      "std_lib/builtin/closure/name_wrong_args.lox",
      "std_lib/builtin/closure/call_wrong_args.lox",
      "std_lib/builtin/closure/size_wrong_args.lox",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn iter() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/builtin/iter/str.lox",
      "std_lib/builtin/iter/next.lox",
      "std_lib/builtin/iter/iter.lox",
      "std_lib/builtin/iter/map.lox",
      "std_lib/builtin/iter/filter.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec!["std_lib/builtin/list/str_wrong_args.lox"],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn list() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/builtin/list/clear.lox",
      "std_lib/builtin/list/has.lox",
      "std_lib/builtin/list/insert.lox",
      "std_lib/builtin/list/iter.lox",
      "std_lib/builtin/list/pop.lox",
      "std_lib/builtin/list/push.lox",
      "std_lib/builtin/list/remove.lox",
      "std_lib/builtin/list/size.lox",
      "std_lib/builtin/list/str.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![
      "std_lib/builtin/list/clear_wrong_args.lox",
      "std_lib/builtin/list/has_wrong_args.lox",
      "std_lib/builtin/list/insert_out_of_bounds.lox",
      "std_lib/builtin/list/insert_wrong_args.lox",
      "std_lib/builtin/list/iter_wrong_args.lox",
      "std_lib/builtin/list/pop_wrong_args.lox",
      "std_lib/builtin/list/push_wrong_args.lox",
      "std_lib/builtin/list/remove_out_of_bounds.lox",
      "std_lib/builtin/list/remove_wrong_args.lox",
      "std_lib/builtin/list/size_wrong_args.lox",
      "std_lib/builtin/list/str_wrong_args.lox",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn map() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/builtin/map/size.lox",
      "std_lib/builtin/map/str.lox",
      "std_lib/builtin/map/has.lox",
      "std_lib/builtin/map/get.lox",
      "std_lib/builtin/map/remove.lox",
      "std_lib/builtin/map/insert.lox",
      "std_lib/builtin/map/iter.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![
      "std_lib/builtin/map/size_wrong_args.lox",
      "std_lib/builtin/map/str_wrong_args.lox",
      "std_lib/builtin/map/has_wrong_args.lox",
      "std_lib/builtin/map/get_wrong_args.lox",
      "std_lib/builtin/map/remove_wrong_args.lox",
      "std_lib/builtin/map/remove_missing_key.lox",
      "std_lib/builtin/map/insert_wrong_args.lox",
      "std_lib/builtin/map/iter_wrong_args.lox",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn method() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "std_lib/builtin/method/name.lox",
      "std_lib/builtin/method/call.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}
