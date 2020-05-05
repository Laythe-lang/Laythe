use spacelox_vm::vm::{default_native_vm, ExecuteResult};
use std::fs::File;
use std::io::prelude::*;
use std::path::PathBuf;
use support::{fixture_path_inner, test_files_inner};

mod support;

fn fixture_path(fixture_path: &str) -> Option<PathBuf> {
  fixture_path_inner(fixture_path, FILE_PATH)
}

fn test_files(paths: &[&str], result: ExecuteResult) -> Result<(), std::io::Error> {
  test_files_inner(paths, FILE_PATH, result)
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

  let assert = fixture_path("language/native/clock.lox").expect("No parent directory");

  let mut file = File::open(assert)?;
  let mut source = String::new();
  file.read_to_string(&mut source)?;

  assert_eq!(vm.run(&source), ExecuteResult::Ok);
  Ok(())
}

#[test]
fn assert() -> Result<(), std::io::Error> {
  let mut vm = default_native_vm();

  let assert = fixture_path("language/native/assert.lox").expect("No parent directory");

  let mut file = File::open(assert)?;
  let mut source = String::new();
  file.read_to_string(&mut source)?;

  assert_eq!(vm.run(&source), ExecuteResult::Ok);
  Ok(())
}

#[test]
fn assert_eq() -> Result<(), std::io::Error> {
  let mut vm = default_native_vm();
  let assert = fixture_path("language/native/assert_eq.lox").expect("No parent directory");

  let mut file = File::open(assert)?;
  let mut source = String::new();
  file.read_to_string(&mut source)?;

  assert_eq!(vm.run(&source), ExecuteResult::Ok);
  Ok(())
}

#[test]
fn assert_ne() -> Result<(), std::io::Error> {
  let mut vm = default_native_vm();
  let assert = fixture_path("language/native/assert_ne.lox").expect("No parent directory");

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
      "language/assignment/associativity.lox",
      "language/assignment/global.lox",
      "language/assignment/local.lox",
      "language/assignment/syntax.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![
      "language/assignment/grouping.lox",
      "language/assignment/infix_operator.lox",
      "language/assignment/prefix_operator.lox",
      "language/assignment/to_this.lox",
    ],
    ExecuteResult::CompileError,
  )?;

  test_files(
    &vec!["language/assignment/undefined.lox"],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn block() -> Result<(), std::io::Error> {
  test_files(
    &vec!["language/block/empty.lox", "language/block/empty.lox"],
    ExecuteResult::Ok,
  )
}

#[test]
fn bool() -> Result<(), std::io::Error> {
  test_files(
    &vec!["language/bool/equality.lox", "language/bool/not.lox"],
    ExecuteResult::Ok,
  )
}

#[test]
fn call() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "language/call/bool.lox",
      "language/call/nil.lox",
      "language/call/num.lox",
      "language/call/object.lox",
      "language/call/string.lox",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn class() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "language/class/empty.lox",
      "language/class/inherited_method.lox",
      "language/class/local_inherit_other.lox",
      "language/class/local_reference_self.lox",
      "language/class/reference_self.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![
      "language/class/inherit_self.lox",
      "language/class/local_inherit_self.lox",
    ],
    ExecuteResult::CompileError,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn closure() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "language/closure/assign_to_closure.lox",
      "language/closure/assign_to_shadowed_later.lox",
      "language/closure/close_over_function_parameter.lox",
      "language/closure/close_over_later_variable.lox",
      "language/closure/close_over_method_parameter.lox",
      "language/closure/closed_closure_in_function.lox",
      "language/closure/nested_closure.lox",
      "language/closure/open_closure_in_function.lox",
      "language/closure/reference_closure_multiple_times.lox",
      "language/closure/reuse_closure_slot.lox",
      "language/closure/shadow_closure_with_local.lox",
      "language/closure/unused_closure.lox",
      "language/closure/unused_later_closure.lox",
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
      "language/comments/line_at_eof.lox",
      "language/comments/only_line_comment_and_line.lox",
      "language/comments/only_line_comment.lox",
      "language/comments/unicode.lox",
    ],
    ExecuteResult::Ok,
  )
}

#[test]
fn constructor() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "language/constructor/arguments.lox",
      "language/constructor/call_init_early_return.lox",
      "language/constructor/call_init_explicitly.lox",
      "language/constructor/default.lox",
      "language/constructor/early_return.lox",
      "language/constructor/return_in_nested_function.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec!["language/constructor/return_value.lox"],
    ExecuteResult::CompileError,
  )?;

  test_files(
    &vec![
      "language/constructor/default_arguments.lox",
      "language/constructor/extra_arguments.lox",
      "language/constructor/missing_arguments.lox",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn expressions() -> Result<(), std::io::Error> {
  test_files(
    &vec!["language/expressions/evaluate.lox"],
    ExecuteResult::Ok,
  )?;

  test_files(&vec![], ExecuteResult::CompileError)?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn field() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "language/field/call_function_field.lox",
      "language/field/get_and_set_method.lox",
      "language/field/many.lox",
      "language/field/method_binds_this.lox",
      "language/field/method.lox",
      "language/field/on_instance.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(&vec![], ExecuteResult::CompileError)?;

  test_files(
    &vec![
      "language/field/call_nonfunction_field.lox",
      "language/field/get_on_bool.lox",
      "language/field/get_on_class.lox",
      "language/field/get_on_function.lox",
      "language/field/get_on_nil.lox",
      "language/field/get_on_num.lox",
      "language/field/get_on_string.lox",
      "language/field/set_evaluation_order.lox",
      "language/field/set_on_bool.lox",
      "language/field/set_on_class.lox",
      "language/field/set_on_function.lox",
      "language/field/set_on_nil.lox",
      "language/field/set_on_num.lox",
      "language/field/set_on_string.lox",
      "language/field/undefined.lox",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn for_loop() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "language/for/return_inside.lox",
      "language/for/scope.lox",
      "language/for/syntax.lox",
      "language/for/closure_in_body.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![
      "language/for/class_in_body.lox",
      "language/for/fun_in_body.lox",
      "language/for/statement_condition.lox",
      "language/for/statement_increment.lox",
      "language/for/statement_initializer.lox",
      "language/for/var_in_body.lox",
    ],
    ExecuteResult::CompileError,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn for_range_loop() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "language/for_range/return_inside.lox",
      "language/for_range/scope.lox",
      "language/for_range/closure_in_body.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![
      "language/for_range/class_in_body.lox",
      "language/for_range/fun_in_body.lox",
      "language/for_range/var_in_body.lox",
      "language/for_range/statement_iterator.lox",
    ],
    ExecuteResult::CompileError,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn function() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "language/function/empty_body.lox",
      "language/function/local_recursion.lox",
      "language/function/mutual_recursion.lox",
      "language/function/parameters.lox",
      "language/function/print.lox",
      "language/function/recursion.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![
      "language/function/body_must_be_block.lox",
      "language/function/missing_comma_in_parameters.lox",
      "language/function/too_many_arguments.lox",
      "language/function/too_many_parameters.lox",
    ],
    ExecuteResult::CompileError,
  )?;

  test_files(
    &vec![
      "language/function/extra_arguments.lox",
      "language/function/local_mutual_recursion.lox",
      "language/function/missing_arguments.lox",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn hooks() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "language/hooks/inline.lox",
      "language/hooks/closure.lox",
      "language/hooks/class.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(&vec![], ExecuteResult::CompileError)?;

  test_files(
    &vec![
      "language/hooks/pass_error_inline.lox",
      "language/hooks/pass_error_closure.lox",
      "language/hooks/pass_error_class.lox",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn if_stmt() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "language/if/dangling_else.lox",
      "language/if/else.lox",
      "language/if/if.lox",
      "language/if/truth.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![
      "language/if/class_in_else.lox",
      "language/if/class_in_then.lox",
      "language/if/fun_in_else.lox",
      "language/if/fun_in_then.lox",
      "language/if/var_in_then.lox",
      "language/if/var_in_then.lox",
    ],
    ExecuteResult::CompileError,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn indexing() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "language/indexing/list_get.lox",
      "language/indexing/list_set.lox",
      "language/indexing/list_nested_get.lox",
      "language/indexing/list_nested_set.lox",
      "language/indexing/map_na_get.lox",
      "language/indexing/map_ref_equal_get.lox",
      "language/indexing/map_nested_get.lox",
      "language/indexing/map_nested_set.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(&vec![], ExecuteResult::CompileError)?;

  test_files(
    &vec![
      "language/indexing/list_out_of_range.lox",
      "language/indexing/map_key_not_found.lox",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn inheritance() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "language/inheritance/constructor.lox",
      "language/inheritance/inherit_methods.lox",
      "language/inheritance/set_fields_from_base_class.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec!["language/inheritance/parenthesized_superclass.lox"],
    ExecuteResult::CompileError,
  )?;

  test_files(
    &vec![
      "language/inheritance/inherit_from_function.lox",
      "language/inheritance/inherit_from_nil.lox",
      "language/inheritance/inherit_from_number.lox",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn limit() -> Result<(), std::io::Error> {
  test_files(
    &vec!["language/expressions/evaluate.lox"],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![
      "language/limit/loop_too_large.lox",
      "language/limit/no_reuse_constants.lox",
      "language/limit/too_many_constants.lox",
      "language/limit/too_many_locals.lox",
      "language/limit/too_many_upvalues.lox",
    ],
    ExecuteResult::CompileError,
  )?;

  test_files(
    &vec!["language/limit/stack_overflow.lox"],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn list() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "language/list/empty.lox",
      "language/list/homogeneous.lox",
      "language/list/mixed.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![
      "language/list/missing_comma_in_initializer.lox",
      "language/list/missing_closing_bracket.lox",
    ],
    ExecuteResult::CompileError,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn logical_operator() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "language/logical_operator/and_truth.lox",
      "language/logical_operator/and.lox",
      "language/logical_operator/or_truth.lox",
      "language/logical_operator/or.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(&vec![], ExecuteResult::CompileError)?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn map() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "language/map/empty.lox",
      "language/map/homogeneous.lox",
      "language/map/mixed.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![
      "language/map/missing_closing_curly.lox",
      "language/map/missing_colon.lox",
      "language/map/statement_key.lox",
      "language/map/statement_value.lox",
    ],
    ExecuteResult::CompileError,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn method() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "language/method/arity.lox",
      "language/method/empty_block.lox",
      "language/method/print_bound_method.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![
      "language/method/too_many_arguments.lox",
      "language/method/too_many_parameters.lox",
    ],
    ExecuteResult::CompileError,
  )?;

  test_files(
    &vec![
      "language/method/extra_arguments.lox",
      "language/method/missing_arguments.lox",
      "language/method/not_found.lox",
      "language/method/refer_to_name.lox",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn nil() -> Result<(), std::io::Error> {
  test_files(&vec!["language/nil/literal.lox"], ExecuteResult::Ok)?;

  test_files(&vec![], ExecuteResult::CompileError)?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn number() -> Result<(), std::io::Error> {
  test_files(&vec!["language/number/literals.lox"], ExecuteResult::Ok)?;

  test_files(
    &vec![
      "language/number/decimal_point_at_eof.lox",
      "language/number/leading_dot.lox",
      "language/number/trailing_dot.lox",
    ],
    ExecuteResult::CompileError,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn operator() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "language/operator/add.lox",
      "language/operator/comparison.lox",
      "language/operator/divide.lox",
      "language/operator/equals_class.lox",
      "language/operator/equals_method.lox",
      "language/operator/equals.lox",
      "language/operator/multiply.lox",
      "language/operator/negate.lox",
      "language/operator/not_class.lox",
      "language/operator/not_equals.lox",
      "language/operator/not.lox",
      "language/operator/subtract.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(&vec![], ExecuteResult::CompileError)?;

  test_files(
    &vec![
      "language/operator/add_bool_nil.lox",
      "language/operator/add_bool_num.lox",
      "language/operator/add_bool_string.lox",
      "language/operator/add_nil_nil.lox",
      "language/operator/add_num_nil.lox",
      "language/operator/add_string_nil.lox",
      "language/operator/divide_nonnum_num.lox",
      "language/operator/divide_num_nonnum.lox",
      "language/operator/greater_nonnum_num.lox",
      "language/operator/greater_num_nonnum.lox",
      "language/operator/greater_or_equal_nonnum_num.lox",
      "language/operator/greater_or_equal_num_nonnum.lox",
      "language/operator/less_nonnum_num.lox",
      "language/operator/less_num_nonnum.lox",
      "language/operator/less_or_equal_nonnum_num.lox",
      "language/operator/less_or_equal_num_nonnum.lox",
      "language/operator/multiply_nonnum_num.lox",
      "language/operator/multiply_num_nonnum.lox",
      "language/operator/negate_nonnum.lox",
      "language/operator/subtract_nonnum_num.lox",
      "language/operator/subtract_num_nonnum.lox",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn print() -> Result<(), std::io::Error> {
  test_files(&vec![], ExecuteResult::Ok)?;

  test_files(
    &vec!["language/print/missing_argument.lox"],
    ExecuteResult::CompileError,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn regression() -> Result<(), std::io::Error> {
  test_files(
    &vec!["language/regression/40.lox", "language/regression/394.lox"],
    ExecuteResult::Ok,
  )
}

#[test]
fn return_test() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "language/return/after_else.lox",
      "language/return/after_if.lox",
      "language/return/after_while.lox",
      "language/return/in_function.lox",
      "language/return/return_nil_if_no_value.lox",
      "language/return/return_nil_if_no_value.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec!["language/return/at_top_level.lox"],
    ExecuteResult::CompileError,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn string() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "language/string/literals.lox",
      "language/string/multiline.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec!["language/string/unterminated.lox"],
    ExecuteResult::CompileError,
  )?;

  test_files(
    &vec!["language/string/error_after_multiline.lox"],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn super_() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "language/super/bound_method.lox",
      "language/super/call_other_method.lox",
      "language/super/call_same_method.lox",
      "language/super/closure.lox",
      "language/super/constructor.lox",
      "language/super/indirectly_inherited.lox",
      "language/super/reassign_superclass.lox",
      "language/super/super_in_closure_in_inherited_method.lox",
      "language/super/super_in_inherited_method.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![
      "language/super/no_superclass_bind.lox",
      "language/super/no_superclass_call.lox",
      "language/super/parenthesized.lox",
      "language/super/super_at_top_level.lox",
      "language/super/super_in_top_level_function.lox",
      "language/super/super_without_dot.lox",
      "language/super/super_without_name.lox",
    ],
    ExecuteResult::CompileError,
  )?;

  test_files(
    &vec![
      "language/super/extra_arguments.lox",
      "language/super/missing_arguments.lox",
      "language/super/no_superclass_method.lox",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn variable() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "language/variable/early_bound.lox",
      "language/variable/in_middle_of_block.lox",
      "language/variable/in_nested_block.lox",
      "language/variable/local_from_method.lox",
      "language/variable/redeclare_global.lox",
      "language/variable/redefine_global.lox",
      "language/variable/scope_reuse_in_different_blocks.lox",
      "language/variable/shadow_and_local.lox",
      "language/variable/shadow_global.lox",
      "language/variable/shadow_local.lox",
      "language/variable/uninitialized.lox",
      "language/variable/unreached_undefined.lox",
      "language/variable/use_global_in_initializer.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![
      "language/variable/collide_with_parameter.lox",
      "language/variable/duplicate_local.lox",
      "language/variable/duplicate_parameter.lox",
      "language/variable/use_false_as_var.lox",
      "language/variable/use_local_in_initializer.lox",
      "language/variable/use_nil_as_var.lox",
      "language/variable/use_this_as_var.lox",
    ],
    ExecuteResult::CompileError,
  )?;

  test_files(
    &vec![
      "language/variable/undefined_global.lox",
      "language/variable/undefined_local.lox",
      "language/variable/undefined_local.lox",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn while_test() -> Result<(), std::io::Error> {
  test_files(
    &vec![
      "language/while/closure_in_body.lox",
      "language/while/return_closure.lox",
      "language/while/return_inside.lox",
      "language/while/syntax.lox",
    ],
    ExecuteResult::Ok,
  )?;

  test_files(
    &vec![
      "language/while/class_in_body.lox",
      "language/while/fun_in_body.lox",
      "language/while/var_in_body.lox",
    ],
    ExecuteResult::CompileError,
  )?;

  test_files(&vec![], ExecuteResult::RuntimeError)
}
