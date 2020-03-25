use spacelox_core::managed::Managed;
use spacelox_core::native::create_natives;
use spacelox_core::value::{Closure, Fun, Value};
use spacelox_vm::constants::{DEFAULT_STACK_MAX, FRAME_MAX};
use spacelox_vm::memory::{Gc, NO_GC};
use spacelox_vm::{
  call_frame::CallFrame,
  vm::{InterpretResult, Vm},
};
use std::fs::File;
use std::io::prelude::*;
use std::path::{Path, PathBuf};

fn create_stack<'a>() -> Vec<Value> {
  vec![Value::Nil; DEFAULT_STACK_MAX]
}

fn create_frames<'a>(fun: Managed<Closure>) -> Vec<CallFrame> {
  vec![CallFrame::new(fun); FRAME_MAX]
}

fn create_vm<'a>(closure: Managed<Closure>) -> Vm {
  Vm::new(create_stack(), create_frames(closure), &create_natives())
}

fn fixture_path(fixture_path: &str) -> Option<PathBuf> {
  let test_path = Path::new(FILE_PATH);

  test_path
    .parent()
    .and_then(|path| path.parent())
    .and_then(|path| path.parent())
    .and_then(|path| Some(path.join("fixture").join(fixture_path)))
}

fn test_files(paths: &[&str], result: InterpretResult) -> Result<(), std::io::Error> {
  let fun = Fun::default();
  let gc = Gc::new();

  let managed_fun = gc.manage(fun, &NO_GC);
  let closure = gc.manage(Closure::new(managed_fun), &NO_GC);

  for path in paths {
    let mut vm = create_vm(closure.clone());

    let assert = fixture_path(path).expect("No parent directory");
    let debug_path = assert.to_str().map(|s| s.to_string());
    let mut file = File::open(assert)?;
    let mut source = String::new();
    file.read_to_string(&mut source)?;

    assert_eq!(vm.run(&source), result, "Failing file {:?}", debug_path);
  }

  Ok(())
}

const FILE_PATH: &str = file!();

#[test]
fn build() {
  let fun = Fun::default();
  let gc = Gc::new();

  let managed_fun = gc.manage(fun, &NO_GC);
  let closure = gc.manage(Closure::new(managed_fun), &NO_GC);
  create_vm(closure);
  assert!(true);
}

#[test]
fn clock() -> Result<(), std::io::Error> {
  let fun = Fun::default();
  let gc = Gc::new();

  let managed_fun = gc.manage(fun, &NO_GC);
  let closure = gc.manage(Closure::new(managed_fun), &NO_GC);
  let mut vm = create_vm(closure.clone());

  let assert = fixture_path("native/clock.lox").expect("No parent directory");

  let mut file = File::open(assert)?;
  let mut source = String::new();
  file.read_to_string(&mut source)?;

  assert_eq!(vm.run(&source), InterpretResult::Ok);
  Ok(())
}

#[test]
fn assert() -> Result<(), std::io::Error> {
  let fun = Fun::default();
  let gc = Gc::new();

  let managed_fun = gc.manage(fun, &NO_GC);
  let closure = gc.manage(Closure::new(managed_fun), &NO_GC);
  let mut vm = create_vm(closure.clone());

  let assert = fixture_path("native/assert.lox").expect("No parent directory");

  let mut file = File::open(assert)?;
  let mut source = String::new();
  file.read_to_string(&mut source)?;

  assert_eq!(vm.run(&source), InterpretResult::Ok);
  Ok(())
}

#[test]
fn assert_eq() -> Result<(), std::io::Error> {
  let fun = Fun::default();
  let gc = Gc::new();

  let managed_fun = gc.manage(fun, &NO_GC);
  let closure = gc.manage(Closure::new(managed_fun), &NO_GC);
  let mut vm = create_vm(closure.clone());

  let assert = fixture_path("native/assert_eq.lox").expect("No parent directory");

  let mut file = File::open(assert)?;
  let mut source = String::new();
  file.read_to_string(&mut source)?;

  assert_eq!(vm.run(&source), InterpretResult::Ok);
  Ok(())
}

#[test]
fn assert_ne() -> Result<(), std::io::Error> {
  let fun = Fun::default();
  let gc = Gc::new();

  let managed_fun = gc.manage(fun, &NO_GC);
  let closure = gc.manage(Closure::new(managed_fun), &NO_GC);
  let mut vm = create_vm(closure.clone());

  let assert = fixture_path("native/assert_ne.lox").expect("No parent directory");

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
      "assignment/to_this.lox",
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
      "call/object.lox",
      "call/string.lox",
    ],
    InterpretResult::RuntimeError,
  )
}

#[test]
fn class() -> Result<(), std::io::Error> {
  test_files(&vec!["class/empty.lox"], InterpretResult::Ok)?;

  test_files(&vec![], InterpretResult::CompileError)?;

  test_files(&vec![], InterpretResult::RuntimeError)
}

#[test]
fn closure() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "closure/assign_to_closure.lox",
      "closure/assign_to_shadowed_later.lox",
      "closure/close_over_function_parameter.lox",
      "closure/close_over_later_variable.lox",
      "closure/close_over_method_parameter.lox",
      "closure/closed_closure_in_function.lox",
      "closure/nested_closure.lox",
      "closure/open_closure_in_function.lox",
      "closure/reference_closure_multiple_times.lox",
      "closure/reuse_closure_slot.lox",
      "closure/shadow_closure_with_local.lox",
      "closure/unused_closure.lox",
      "closure/unused_later_closure.lox",
    ],
    InterpretResult::Ok,
  )?;

  test_files(&vec![], InterpretResult::CompileError)?;

  test_files(&vec![], InterpretResult::RuntimeError)
}

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

#[test]
fn constructor() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "constructor/arguments.lox",
      "constructor/call_init_early_return.lox",
      "constructor/call_init_explicitly.lox",
      "constructor/default.lox",
      "constructor/early_return.lox",
      "constructor/return_in_nested_function.lox",
    ],
    InterpretResult::Ok,
  )?;

  test_files(
    &vec!["constructor/return_value.lox"],
    InterpretResult::CompileError,
  )?;

  test_files(
    &vec![
      "constructor/default_arguments.lox",
      "constructor/extra_arguments.lox",
      "constructor/missing_arguments.lox",
    ],
    InterpretResult::RuntimeError,
  )
}

#[test]
fn expressions() -> Result<(), std::io::Error> {
  test_files(&vec!["expressions/evaluate.lox"], InterpretResult::Ok)?;

  test_files(&vec![], InterpretResult::CompileError)?;

  test_files(&vec![], InterpretResult::RuntimeError)
}

#[test]
fn field() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "field/call_function_field.lox",
      "field/get_and_set_method.lox",
      "field/many.lox",
      "field/method_binds_this.lox",
      "field/method.lox",
      "field/on_instance.lox",
    ],
    InterpretResult::Ok,
  )?;

  test_files(&vec![], InterpretResult::CompileError)?;

  test_files(
    &vec![
      "field/call_nonfunction_field.lox",
      "field/get_on_bool.lox",
      "field/get_on_class.lox",
      "field/get_on_function.lox",
      "field/get_on_nil.lox",
      "field/get_on_num.lox",
      "field/get_on_string.lox",
      "field/set_evaluation_order.lox",
      "field/set_on_bool.lox",
      "field/set_on_class.lox",
      "field/set_on_function.lox",
      "field/set_on_nil.lox",
      "field/set_on_num.lox",
      "field/set_on_string.lox",
      "field/undefined.lox",
    ],
    InterpretResult::RuntimeError,
  )
}

#[test]
fn for_loop() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "for/return_inside.lox",
      "for/scope.lox",
      "for/syntax.lox",
      "for/closure_in_body.lox",
    ],
    InterpretResult::Ok,
  )?;

  test_files(
    &vec![
      "for/class_in_body.lox",
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
      "function/local_recursion.lox",
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

  test_files(
    &vec![
      "function/extra_arguments.lox",
      "function/local_mutual_recursion.lox",
      "function/missing_arguments.lox",
    ],
    InterpretResult::RuntimeError,
  )
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

  test_files(&vec![], InterpretResult::RuntimeError)
}

#[test]
fn inheritance() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "inheritance/constructor.lox",
      "inheritance/inherit_methods.lox",
      "inheritance/set_fields_from_base_class.lox",
    ],
    InterpretResult::Ok,
  )?;

  test_files(
    &vec!["inheritance/parenthesized_superclass.lox"],
    InterpretResult::CompileError,
  )?;

  test_files(
    &vec![
      "inheritance/inherit_from_function.lox",
      "inheritance/inherit_from_nil.lox",
      "inheritance/inherit_from_number.lox",
    ],
    InterpretResult::RuntimeError,
  )
}

#[test]
fn limit() -> Result<(), std::io::Error> {
  test_files(&vec!["expressions/evaluate.lox"], InterpretResult::Ok)?;

  test_files(
    &vec![
      "limit/loop_too_large.lox",
      "limit/no_reuse_constants.lox",
      "limit/too_many_constants.lox",
      "limit/too_many_locals.lox",
      "limit/too_many_upvalues.lox",
    ],
    InterpretResult::CompileError,
  )?;

  test_files(
    &vec!["limit/stack_overflow.lox"],
    InterpretResult::RuntimeError,
  )
}

#[test]
fn logical_operator() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "logical_operator/and_truth.lox",
      "logical_operator/and.lox",
      "logical_operator/or_truth.lox",
      "logical_operator/or.lox",
    ],
    InterpretResult::Ok,
  )?;

  test_files(&vec![], InterpretResult::CompileError)?;

  test_files(&vec![], InterpretResult::RuntimeError)
}

#[test]
fn method() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "method/arity.lox",
      "method/empty_block.lox",
      "method/print_bound_method.lox",
    ],
    InterpretResult::Ok,
  )?;

  test_files(
    &vec![
      "method/too_many_arguments.lox",
      "method/too_many_parameters.lox",
    ],
    InterpretResult::CompileError,
  )?;

  test_files(
    &vec![
      "method/extra_arguments.lox",
      "method/missing_arguments.lox",
      "method/not_found.lox",
      "method/refer_to_name.lox",
    ],
    InterpretResult::RuntimeError,
  )
}

#[test]
fn nil() -> Result<(), std::io::Error> {
  test_files(&vec!["nil/literal.lox"], InterpretResult::Ok)?;

  test_files(&vec![], InterpretResult::CompileError)?;

  test_files(&vec![], InterpretResult::RuntimeError)
}

#[test]
fn number() -> Result<(), std::io::Error> {
  test_files(&vec!["number/literals.lox"], InterpretResult::Ok)?;

  test_files(
    &vec![
      "number/decimal_point_at_eof.lox",
      "number/leading_dot.lox",
      "number/trailing_dot.lox",
    ],
    InterpretResult::CompileError,
  )?;

  test_files(&vec![], InterpretResult::RuntimeError)
}

#[test]
fn operator() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "operator/add.lox",
      "operator/comparison.lox",
      "operator/divide.lox",
      "operator/equals_class.lox",
      "operator/equals_method.lox",
      "operator/equals.lox",
      "operator/multiply.lox",
      "operator/negate.lox",
      "operator/not_class.lox",
      "operator/not_equals.lox",
      "operator/not.lox",
      "operator/subtract.lox",
    ],
    InterpretResult::Ok,
  )?;

  test_files(&vec![], InterpretResult::CompileError)?;

  test_files(
    &vec![
      "operator/add_bool_nil.lox",
      "operator/add_bool_num.lox",
      "operator/add_bool_string.lox",
      "operator/add_nil_nil.lox",
      "operator/add_num_nil.lox",
      "operator/add_string_nil.lox",
      "operator/divide_nonnum_num.lox",
      "operator/divide_num_nonnum.lox",
      "operator/greater_nonnum_num.lox",
      "operator/greater_num_nonnum.lox",
      "operator/greater_or_equal_nonnum_num.lox",
      "operator/greater_or_equal_num_nonnum.lox",
      "operator/less_nonnum_num.lox",
      "operator/less_num_nonnum.lox",
      "operator/less_or_equal_nonnum_num.lox",
      "operator/less_or_equal_num_nonnum.lox",
      "operator/multiply_nonnum_num.lox",
      "operator/multiply_num_nonnum.lox",
      "operator/negate_nonnum.lox",
      "operator/subtract_nonnum_num.lox",
      "operator/subtract_num_nonnum.lox",
    ],
    InterpretResult::RuntimeError,
  )
}

#[test]
fn print() -> Result<(), std::io::Error> {
  test_files(&vec![], InterpretResult::Ok)?;

  test_files(
    &vec!["print/missing_argument.lox"],
    InterpretResult::CompileError,
  )?;

  test_files(&vec![], InterpretResult::RuntimeError)
}

#[test]
fn regression() -> Result<(), std::io::Error> {
  test_files(
    &vec!["regression/40.lox", "regression/394.lox"],
    InterpretResult::Ok,
  )
}

#[test]
fn return_test() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "return/after_else.lox",
      "return/after_if.lox",
      "return/after_while.lox",
      "return/in_function.lox",
      "return/return_nil_if_no_value.lox",
      "return/return_nil_if_no_value.lox",
    ],
    InterpretResult::Ok,
  )?;

  test_files(
    &vec!["return/at_top_level.lox"],
    InterpretResult::CompileError,
  )?;

  test_files(&vec![], InterpretResult::RuntimeError)
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

#[test]
fn string() -> Result<(), std::io::Error> {
  test_files(
    &vec!["string/literals.lox", "string/multiline.lox"],
    InterpretResult::Ok,
  )?;

  test_files(
    &vec!["string/unterminated.lox"],
    InterpretResult::CompileError,
  )?;

  test_files(
    &vec!["string/error_after_multiline.lox"],
    InterpretResult::RuntimeError,
  )
}

#[test]
fn super_() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "super/bound_method.lox",
      "super/call_other_method.lox",
      "super/call_same_method.lox",
      "super/closure.lox",
      "super/constructor.lox",
      "super/indirectly_inherited.lox",
      "super/reassign_superclass.lox",
      "super/super_in_closure_in_inherited_method.lox",
      "super/super_in_inherited_method.lox",
    ],
    InterpretResult::Ok,
  )?;

  test_files(
    &vec![
      "super/no_superclass_bind.lox",
      "super/no_superclass_call.lox",
      "super/parenthesized.lox",
      "super/super_at_top_level.lox",
      "super/super_in_top_level_function.lox",
      "super/super_without_dot.lox",
      "super/super_without_name.lox",
    ],
    InterpretResult::CompileError,
  )?;

  test_files(
    &vec![
      "super/extra_arguments.lox",
      "super/missing_arguments.lox",
      "super/no_superclass_method.lox",
    ],
    InterpretResult::RuntimeError,
  )
}

#[test]
fn variable() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "variable/early_bound.lox",
      "variable/in_middle_of_block.lox",
      "variable/in_nested_block.lox",
      "variable/local_from_method.lox",
      "variable/redeclare_global.lox",
      "variable/redefine_global.lox",
      "variable/scope_reuse_in_different_blocks.lox",
      "variable/shadow_and_local.lox",
      "variable/shadow_global.lox",
      "variable/shadow_local.lox",
      "variable/uninitialized.lox",
      "variable/unreached_undefined.lox",
      "variable/use_global_in_initializer.lox",
    ],
    InterpretResult::Ok,
  )?;

  test_files(
    &vec![
      "variable/collide_with_parameter.lox",
      "variable/duplicate_local.lox",
      "variable/duplicate_parameter.lox",
      "variable/use_false_as_var.lox",
      "variable/use_local_in_initializer.lox",
      "variable/use_nil_as_var.lox",
      "variable/use_this_as_var.lox",
    ],
    InterpretResult::CompileError,
  )?;

  test_files(
    &vec![
      "variable/undefined_global.lox",
      "variable/undefined_local.lox",
      "variable/undefined_local.lox",
    ],
    InterpretResult::RuntimeError,
  )
}

#[test]
fn while_test() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "while/closure_in_body.lox",
      "while/return_closure.lox",
      "while/return_inside.lox",
      "while/syntax.lox",
    ],
    InterpretResult::Ok,
  )?;

  test_files(
    &vec![
      "while/class_in_body.lox",
      "while/fun_in_body.lox",
      "while/var_in_body.lox",
    ],
    InterpretResult::CompileError,
  )?;

  test_files(&vec![], InterpretResult::RuntimeError)
}
