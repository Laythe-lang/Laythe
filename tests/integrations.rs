extern crate space_lox;
use space_lox::native::create_natives;
use space_lox::value::Value;
use space_lox::vm::{InterpretResult, Vm, DEFAULT_STACK_MAX};
use std::fs::File;
use std::io::prelude::*;
use std::path::{Path, PathBuf};

fn create_stack<'a>() -> Vec<Value<'a>> {
  vec![Value::Nil; DEFAULT_STACK_MAX]
}

fn create_vm<'a>() -> Vm<'a> {
  Vm::new(create_stack(), create_natives())
}

const FILE_PATH: &str = file!();

#[test]
fn build() {
  create_vm();
  assert_eq!(true, true);
}

fn fixture_path(path: &str) -> Option<PathBuf> {
  let test_path = Path::new(FILE_PATH);
  match test_path.parent() {
    Some(directory_path) => Some(directory_path.join("../fixture/").join(path)),
    None => None,
  }
}

fn test_files(paths: &[&str], result: InterpretResult) -> Result<(), std::io::Error> {
  for path in paths {
    let mut vm = create_vm();
    let assert = fixture_path(path).expect("No parent directory");

    let mut file = File::open(assert)?;
    let mut source = String::new();
    file.read_to_string(&mut source)?;

    assert_eq!(vm.run(&source), result);
  }

  Ok(())
}

#[test]
fn clock() -> Result<(), std::io::Error> {
  let mut vm = create_vm();
  let assert = fixture_path("assert/clock.lox").expect("No parent directory");

  let mut file = File::open(assert)?;
  let mut source = String::new();
  file.read_to_string(&mut source)?;

  assert_eq!(vm.run(&source), InterpretResult::Ok);
  Ok(())
}

#[test]
fn assert() -> Result<(), std::io::Error> {
  let mut vm = create_vm();
  let assert = fixture_path("assert/assert.lox").expect("No parent directory");

  let mut file = File::open(assert)?;
  let mut source = String::new();
  file.read_to_string(&mut source)?;

  assert_eq!(vm.run(&source), InterpretResult::Ok);
  Ok(())
}

#[test]
fn assert_eq() -> Result<(), std::io::Error> {
  let mut vm = create_vm();
  let assert = fixture_path("assert/assert_eq.lox").expect("No parent directory");
  // assert_eq!(assert, Path::new(""));

  let mut file = File::open(assert)?;
  let mut source = String::new();
  file.read_to_string(&mut source)?;

  assert_eq!(vm.run(&source), InterpretResult::Ok);
  Ok(())
}

#[test]
fn assert_ne() -> Result<(), std::io::Error> {
  let mut vm = create_vm();
  let assert = fixture_path("assert/assert_ne.lox").expect("No parent directory");
  // assert_eq!(assert, Path::new(""));

  let mut file = File::open(assert)?;
  let mut source = String::new();
  file.read_to_string(&mut source)?;

  assert_eq!(vm.run(&source), InterpretResult::Ok);
  Ok(())
}

#[test]
fn assignment() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "assignment/associativity.lox",
      "assignment/global.lox",
      "assignment/local.lox",
      "assignment/syntax.lox",
    ],
    InterpretResult::Ok,
  )?;

  test_files(
    &vec![
      "assignment/grouping.lox",
      "assignment/infix_operator.lox",
      "assignment/prefix_operator.lox",
      // "assignment/to_this.lox",
    ],
    InterpretResult::CompileError,
  )?;

  test_files(
    &vec!["assignment/undefined.lox"],
    InterpretResult::RuntimeError,
  )
}

#[test]
fn block() -> Result<(), std::io::Error> {
  test_files(
    &vec!["block/empty.lox", "block/empty.lox"],
    InterpretResult::Ok,
  )
}

#[test]
fn bool() -> Result<(), std::io::Error> {
  test_files(
    &vec!["bool/equality.lox", "bool/not.lox"],
    InterpretResult::Ok,
  )
}

#[test]
fn call() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "call/bool.lox",
      "call/nil.lox",
      "call/num.lox",
      // "call/object.lox",
      "call/string.lox",
    ],
    InterpretResult::RuntimeError,
  )
}

// #[test]
// fn class() -> Result<(), std::io::Error> {
//   test_files(&vec![
//   ], InterpretResult::Ok)?;

//   test_files(&vec![
//   ], InterpretResult::CompileError)?;

//   test_files(&vec![
//   ], InterpretResult::RuntimeError)
// }

// #[test]
// fn closure() -> Result<(), std::io::Error> {
//   test_files(&vec![
//   ], InterpretResult::Ok)?;

//   test_files(&vec![
//   ], InterpretResult::CompileError)?;

//   test_files(&vec![
//   ], InterpretResult::RuntimeError)
// }

#[test]
fn comments() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "comments/line_at_eof.lox",
      "comments/only_line_comment_and_line.lox",
      "comments/only_line_comment.lox",
      "comments/unicode.lox",
    ],
    InterpretResult::Ok,
  )
}

// #[test]
// fn constructor() -> Result<(), std::io::Error> {
//   test_files(&vec![
//   ], InterpretResult::Ok)?;

//   test_files(&vec![
//   ], InterpretResult::CompileError)?;

//   test_files(&vec![
//   ], InterpretResult::RuntimeError)
// }

#[test]
fn expressions() -> Result<(), std::io::Error> {
  test_files(&vec!["expressions/evaluate.lox"], InterpretResult::Ok)?;

  test_files(&vec![], InterpretResult::CompileError)?;

  test_files(&vec![], InterpretResult::RuntimeError)
}

// #[test]
// fn field() -> Result<(), std::io::Error> {
//   test_files(&vec![
//     "expressions/evaluate.lox"
//   ], InterpretResult::Ok)?;

//   test_files(&vec![
//   ], InterpretResult::CompileError)?;

//   test_files(&vec![
//   ], InterpretResult::RuntimeError)
// }

#[test]
fn for_loop() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "for/return_inside.lox",
      "for/scope.lox",
      "for/syntax.lox",
    ],
    InterpretResult::Ok,
  )?;

  test_files(
    &vec![
      // "for/class_in_body.lox"
      // "for/closure_in_body.lox"
      "for/fun_in_body.lox",
      "for/statement_condition.lox",
      "for/statement_increment.lox",
      "for/statement_initializer.lox",
      "for/var_in_body.lox",
    ],
    InterpretResult::CompileError,
  )?;

  test_files(&vec![], InterpretResult::RuntimeError)
}


#[test]
fn function() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "function/empty_body.lox",
      // "function/local_recursion.lox",
      "function/mutual_recursion.lox",
      "function/parameters.lox",
      "function/print.lox",
      "function/recursion.lox",
    ],
    InterpretResult::Ok,
  )?;

  test_files(
    &vec![
      "function/body_must_be_block.lox",
      "function/missing_comma_in_parameters.lox",
      "function/too_many_arguments.lox",
      "function/too_many_parameters.lox",
    ],
    InterpretResult::CompileError,
  )?;

  test_files(&vec![
    "function/extra_arguments.lox",
    "function/local_mutual_recursion.lox",
    "function/missing_arguments.lox",
  ], InterpretResult::RuntimeError)
}

#[test]
fn if_stmt() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "if/dangling_else.lox",
      "if/else.lox",
      "if/if.lox",
      "if/truth.lox",
    ],
    InterpretResult::Ok,
  )?;

  test_files(
    &vec![
      "if/class_in_else.lox",
      "if/class_in_then.lox",
      "if/fun_in_else.lox",
      "if/fun_in_then.lox",
      "if/var_in_then.lox",
      "if/var_in_then.lox",
    ],
    InterpretResult::CompileError,
  )?;

  test_files(&vec![
  ], InterpretResult::RuntimeError)
}

// #[test]
// fn inheritance() -> Result<(), std::io::Error> {
//   test_files(&vec![
//     "expressions/evaluate.lox"
//   ], InterpretResult::Ok)?;

//   test_files(&vec![
//   ], InterpretResult::CompileError)?;

//   test_files(&vec![
//   ], InterpretResult::RuntimeError)
// }

#[test]
fn limit() -> Result<(), std::io::Error> {
  test_files(&vec![
    "expressions/evaluate.lox"
  ], InterpretResult::Ok)?;

  test_files(&vec![
    "limit/loop_too_large.lox",
    "limit/no_reuse_constants.lox",
    "limit/too_many_constants.lox",
    "limit/too_many_locals.lox",
    
    // "limit/too_many_upvalues.lox",
  ], InterpretResult::CompileError)?;

  test_files(&vec![
    // "limit/stack_overflow.lox"
  ], InterpretResult::RuntimeError)
}

#[test]
fn logical_operator() -> Result<(), std::io::Error> {
  test_files(&vec![
    "logical_operator/and_truth.lox",
    // "logical_operator/and.lox",
    "logical_operator/or_truth.lox",
    "logical_operator/or.lox",
  ], InterpretResult::Ok)?;

  test_files(&vec![
  ], InterpretResult::CompileError)?;

  test_files(&vec![
  ], InterpretResult::RuntimeError)
}


// #[test]
// fn method() -> Result<(), std::io::Error> {
//   test_files(&vec![
//   ], InterpretResult::Ok)?;

//   test_files(&vec![
//   ], InterpretResult::CompileError)?;

//   test_files(&vec![
//   ], InterpretResult::RuntimeError)
// }

#[test]
fn nil() -> Result<(), std::io::Error> {
  test_files(&vec![
    "nil/literal.lox"
  ], InterpretResult::Ok)?;

  test_files(&vec![
  ], InterpretResult::CompileError)?;

  test_files(&vec![
  ], InterpretResult::RuntimeError)
}


#[test]
fn number() -> Result<(), std::io::Error> {
  test_files(&vec![
    "number/literals.lox",
  ], InterpretResult::Ok)?;

  test_files(&vec![
    "number/decimal_point_at_eof.lox",
    "number/leading_dot.lox",
    "number/trailing_dot.lox",
  ], InterpretResult::CompileError)?;

  test_files(&vec![
  ], InterpretResult::RuntimeError)
}

#[test]
fn print() -> Result<(), std::io::Error> {
  test_files(&vec![
  ], InterpretResult::Ok)?;

  test_files(&vec![
    "print/missing_argument.lox",
  ], InterpretResult::CompileError)?;

  test_files(&vec![
  ], InterpretResult::RuntimeError)
}

// #[test]
// fn scanning() -> Result<(), std::io::Error> {
//   test_files(&vec![
//   ], InterpretResult::Ok)?;

//   test_files(&vec![
//   ], InterpretResult::CompileError)?;

//   test_files(&vec![
//   ], InterpretResult::RuntimeError)
// }


// #[test]
fn string() -> Result<(), std::io::Error> {
  test_files(&vec![
  ], InterpretResult::Ok)?;

  test_files(&vec![
    "print/missing_argument.lox",
  ], InterpretResult::CompileError)?;

  test_files(&vec![
  ], InterpretResult::RuntimeError)
}

