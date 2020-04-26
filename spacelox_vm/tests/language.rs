use spacelox_vm::vm::{default_native_vm, ExecuteResult};
use std::fs::File;
use std::io::prelude::*;
use std::path::{Path, PathBuf};

fn fixture_path(fixture_path: &str) -> Option<PathBuf> {
  let test_path = Path::new(FILE_PATH);

  test_path
    .parent()
    .and_then(|path| path.parent())
    .and_then(|path| path.parent())
    .and_then(|path| Some(path.join("fixture").join(fixture_path)))
}

fn test_files(paths: &[&str], result: ExecuteResult) -> Result<(), std::io::Error> {
  for path in paths {
    let mut vm = default_native_vm();

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
  default_native_vm();
  assert!(true);
}

#[test]
fn clock() -> Result<(), std::io::Error> {
  let mut vm = default_native_vm();

  let assert = fixture_path("native/clock.lox").expect("No parent directory");

  let mut file = File::open(assert)?;
  let mut source = String::new();
  file.read_to_string(&mut source)?;

  assert_eq!(vm.run(&source), ExecuteResult::Ok);
  Ok(())
}

#[test]
fn assert() -> Result<(), std::io::Error> {
  let mut vm = default_native_vm();

  let assert = fixture_path("native/assert.lox").expect("No parent directory");

  let mut file = File::open(assert)?;
  let mut source = String::new();
  file.read_to_string(&mut source)?;

  assert_eq!(vm.run(&source), ExecuteResult::Ok);
  Ok(())
}

#[test]
fn assert_eq() -> Result<(), std::io::Error> {
  let mut vm = default_native_vm();
  let assert = fixture_path("native/assert_eq.lox").expect("No parent directory");

  let mut file = File::open(assert)?;
  let mut source = String::new();
  file.read_to_string(&mut source)?;

  assert_eq!(vm.run(&source), ExecuteResult::Ok);
  Ok(())
}

#[test]
fn assert_ne() -> Result<(), std::io::Error> {
  let mut vm = default_native_vm();
  let assert = fixture_path("native/assert_ne.lox").expect("No parent directory");

  let mut file = File::open(assert)?;
  let mut source = String::new();
  file.read_to_string(&mut source)?;

  assert_eq!(vm.run(&source), ExecuteResult::Ok);
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
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![
      "assignment/grouping.lox",
      "assignment/infix_operator.lox",
      "assignment/prefix_operator.lox",
      "assignment/to_this.lox",
    ],
    ExecuteResult::CompileError,
  )?;

  test_files(
    &vec!["assignment/undefined.lox"],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn block() -> Result<(), std::io::Error> {
  test_files(
    &vec!["block/empty.lox", "block/empty.lox"],
    ExecuteResult::Ok,
  )
}

#[test]
fn bool() -> Result<(), std::io::Error> {
  test_files(
    &vec!["bool/equality.lox", "bool/not.lox"],
    ExecuteResult::Ok,
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
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn class() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "class/empty.lox",
      "class/inherited_method.lox",
      "class/local_inherit_other.lox",
      "class/local_reference_self.lox",
      "class/reference_self.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec!["class/inherit_self.lox", "class/local_inherit_self.lox"],
    ExecuteResult::CompileError,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
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
    ExecuteResult::Ok,
  )?;

  test_files(&vec![], ExecuteResult::CompileError)?;

  test_files(&vec![], ExecuteResult::RuntimeError)
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
    ExecuteResult::Ok,
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
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec!["constructor/return_value.lox"],
    ExecuteResult::CompileError,
  )?;

  test_files(
    &vec![
      "constructor/default_arguments.lox",
      "constructor/extra_arguments.lox",
      "constructor/missing_arguments.lox",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn expressions() -> Result<(), std::io::Error> {
  test_files(&vec!["expressions/evaluate.lox"], ExecuteResult::Ok)?;

  test_files(&vec![], ExecuteResult::CompileError)?;

  test_files(&vec![], ExecuteResult::RuntimeError)
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
    ExecuteResult::Ok,
  )?;

  test_files(&vec![], ExecuteResult::CompileError)?;

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
    ExecuteResult::RuntimeError,
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
    ExecuteResult::Ok,
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
    ExecuteResult::CompileError,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
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
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![
      "function/body_must_be_block.lox",
      "function/missing_comma_in_parameters.lox",
      "function/too_many_arguments.lox",
      "function/too_many_parameters.lox",
    ],
    ExecuteResult::CompileError,
  )?;

  test_files(
    &vec![
      "function/extra_arguments.lox",
      "function/local_mutual_recursion.lox",
      "function/missing_arguments.lox",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn hooks() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "hooks/inline.lox",
      "hooks/closure.lox",
      "hooks/class.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![],
    ExecuteResult::CompileError,
  )?;

  test_files(&vec![
    "hooks/pass_error_inline.lox",
    "hooks/pass_error_closure.lox",
    "hooks/pass_error_class.lox",
  ], ExecuteResult::RuntimeError)
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
    ExecuteResult::Ok,
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
    ExecuteResult::CompileError,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn indexing() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "indexing/list_get.lox",
      "indexing/list_set.lox",
      "indexing/list_nested_get.lox",
      "indexing/list_nested_set.lox",
      "indexing/map_na_get.lox",
      "indexing/map_ref_equal_get.lox",
      "indexing/map_nested_get.lox",
      "indexing/map_nested_set.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(&vec![], ExecuteResult::CompileError)?;

  test_files(
    &vec![
      "indexing/list_out_of_range.lox",
      "indexing/map_key_not_found.lox",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn inheritance() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "inheritance/constructor.lox",
      "inheritance/inherit_methods.lox",
      "inheritance/set_fields_from_base_class.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec!["inheritance/parenthesized_superclass.lox"],
    ExecuteResult::CompileError,
  )?;

  test_files(
    &vec![
      "inheritance/inherit_from_function.lox",
      "inheritance/inherit_from_nil.lox",
      "inheritance/inherit_from_number.lox",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn limit() -> Result<(), std::io::Error> {
  test_files(&vec!["expressions/evaluate.lox"], ExecuteResult::Ok)?;

  test_files(
    &vec![
      "limit/loop_too_large.lox",
      "limit/no_reuse_constants.lox",
      "limit/too_many_constants.lox",
      "limit/too_many_locals.lox",
      "limit/too_many_upvalues.lox",
    ],
    ExecuteResult::CompileError,
  )?;

  test_files(
    &vec!["limit/stack_overflow.lox"],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn list() -> Result<(), std::io::Error> {
  test_files(
    &vec!["list/empty.lox", "list/homogeneous.lox", "list/mixed.lox"],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![
      "list/missing_comma_in_initializer.lox",
      "list/missing_closing_bracket.lox",
    ],
    ExecuteResult::CompileError,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
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
    ExecuteResult::Ok,
  )?;

  test_files(&vec![], ExecuteResult::CompileError)?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn map() -> Result<(), std::io::Error> {
  test_files(
    &vec!["map/empty.lox", "map/homogeneous.lox", "map/mixed.lox"],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![
      "map/missing_closing_curly.lox",
      "map/missing_colon.lox",
      "map/statement_key.lox",
      "map/statement_value.lox",
    ],
    ExecuteResult::CompileError,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn method() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "method/arity.lox",
      "method/empty_block.lox",
      "method/print_bound_method.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![
      "method/too_many_arguments.lox",
      "method/too_many_parameters.lox",
    ],
    ExecuteResult::CompileError,
  )?;

  test_files(
    &vec![
      "method/extra_arguments.lox",
      "method/missing_arguments.lox",
      "method/not_found.lox",
      "method/refer_to_name.lox",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn nil() -> Result<(), std::io::Error> {
  test_files(&vec!["nil/literal.lox"], ExecuteResult::Ok)?;

  test_files(&vec![], ExecuteResult::CompileError)?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn number() -> Result<(), std::io::Error> {
  test_files(&vec!["number/literals.lox"], ExecuteResult::Ok)?;

  test_files(
    &vec![
      "number/decimal_point_at_eof.lox",
      "number/leading_dot.lox",
      "number/trailing_dot.lox",
    ],
    ExecuteResult::CompileError,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
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
    ExecuteResult::Ok,
  )?;

  test_files(&vec![], ExecuteResult::CompileError)?;

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
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn print() -> Result<(), std::io::Error> {
  test_files(&vec![], ExecuteResult::Ok)?;

  test_files(
    &vec!["print/missing_argument.lox"],
    ExecuteResult::CompileError,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn regression() -> Result<(), std::io::Error> {
  test_files(
    &vec!["regression/40.lox", "regression/394.lox"],
    ExecuteResult::Ok,
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
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec!["return/at_top_level.lox"],
    ExecuteResult::CompileError,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn string() -> Result<(), std::io::Error> {
  test_files(
    &vec!["string/literals.lox", "string/multiline.lox"],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec!["string/unterminated.lox"],
    ExecuteResult::CompileError,
  )?;

  test_files(
    &vec!["string/error_after_multiline.lox"],
    ExecuteResult::RuntimeError,
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
    ExecuteResult::Ok,
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
    ExecuteResult::CompileError,
  )?;

  test_files(
    &vec![
      "super/extra_arguments.lox",
      "super/missing_arguments.lox",
      "super/no_superclass_method.lox",
    ],
    ExecuteResult::RuntimeError,
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
    ExecuteResult::Ok,
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
    ExecuteResult::CompileError,
  )?;

  test_files(
    &vec![
      "variable/undefined_global.lox",
      "variable/undefined_local.lox",
      "variable/undefined_local.lox",
    ],
    ExecuteResult::RuntimeError,
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
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![
      "while/class_in_body.lox",
      "while/fun_in_body.lox",
      "while/var_in_body.lox",
    ],
    ExecuteResult::CompileError,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}
