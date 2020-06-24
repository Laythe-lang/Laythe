use laythe_env::io::NativeIo;
use laythe_vm::vm::{default_native_vm, ExecuteResult};
use support::{assert_file_exit_and_stdio, assert_files_exit};

mod support;

fn test_file_exits(paths: &[&str], result: ExecuteResult) -> Result<(), std::io::Error> {
  assert_files_exit(paths, FILE_PATH, NativeIo(), result)
}

fn test_file_with_stdio(
  path: &str,
  stdout: Option<Vec<&str>>,
  errout: Option<Vec<&str>>,
  result: ExecuteResult,
) -> Result<(), std::io::Error> {
  assert_file_exit_and_stdio(path, FILE_PATH, stdout, errout, result)
}

const FILE_PATH: &str = file!();

#[test]
fn build() {
  default_native_vm();
  assert!(true);
}

#[test]
fn assignment() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/assignment/associativity.ly",
      "language/assignment/global.ly",
      "language/assignment/local.ly",
      "language/assignment/syntax.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/assignment/grouping.ly",
      "language/assignment/infix_operator.ly",
      "language/assignment/prefix_operator.ly",
      "language/assignment/to_this.ly",
    ],
    ExecuteResult::CompileError,
  )?;

  test_file_exits(
    &vec!["language/assignment/undefined.ly"],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn block() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec!["language/block/empty.ly", "language/block/empty.ly"],
    ExecuteResult::Ok,
  )
}

#[test]
fn bool() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec!["language/bool/equality.ly", "language/bool/not.ly"],
    ExecuteResult::Ok,
  )
}

#[test]
fn call() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/call/bool.ly",
      "language/call/nil.ly",
      "language/call/num.ly",
      "language/call/object.ly",
      "language/call/string.ly",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn class() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/class/empty.ly",
      "language/class/inherited_method.ly",
      "language/class/local_inherit_other.ly",
      "language/class/local_reference_self.ly",
      "language/class/reference_self.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/class/inherit_self.ly",
      "language/class/local_inherit_self.ly",
    ],
    ExecuteResult::CompileError,
  )?;

  test_file_exits(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn closure() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/closure/assign_to_closure.ly",
      "language/closure/assign_to_shadowed_later.ly",
      "language/closure/close_over_function_parameter.ly",
      "language/closure/close_over_later_variable.ly",
      "language/closure/close_over_method_parameter.ly",
      "language/closure/closed_closure_in_function.ly",
      "language/closure/nested_closure.ly",
      "language/closure/open_closure_in_function.ly",
      "language/closure/reference_closure_multiple_times.ly",
      "language/closure/reuse_closure_slot.ly",
      "language/closure/shadow_closure_with_local.ly",
      "language/closure/unused_closure.ly",
      "language/closure/unused_later_closure.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_file_exits(&vec![], ExecuteResult::CompileError)?;

  test_file_exits(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn comments() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/comments/line_at_eof.ly",
      "language/comments/only_line_comment_and_line.ly",
      "language/comments/only_line_comment.ly",
      "language/comments/unicode.ly",
    ],
    ExecuteResult::Ok,
  )
}

#[test]
fn constructor() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/constructor/arguments.ly",
      "language/constructor/call_init_early_return.ly",
      "language/constructor/call_init_explicitly.ly",
      "language/constructor/default.ly",
      "language/constructor/early_return.ly",
      "language/constructor/return_in_nested_function.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_file_exits(
    &vec!["language/constructor/return_value.ly"],
    ExecuteResult::CompileError,
  )?;

  test_file_exits(
    &vec![
      "language/constructor/default_arguments.ly",
      "language/constructor/extra_arguments.ly",
      "language/constructor/missing_arguments.ly",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn exception() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/exception/top_level_catch.ly",
      "language/exception/one_deep_catch.ly",
      "language/exception/two_deep_catch.ly",
      "language/exception/top_level_catch_thrown.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/exception/catch_no_block.ly",
      "language/exception/try_no_block.ly",
      "language/exception/try_no_catch.ly",
    ],
    ExecuteResult::CompileError,
  )?;

  test_file_with_stdio(
    "language/exception/top_level_thrown.ly",
    None,
    Some(vec![
      "Index out of bounds. list was length 0 but attempted to index with 1.",
      "[line 0] in script",
    ]),
    ExecuteResult::RuntimeError,
  )?;

  test_file_with_stdio(
    "language/exception/one_deep_thrown.ly",
    None,
    Some(vec![
      "Index out of bounds. list was length 0 but attempted to index with 1.",
      "[line 1] in thrower()",
      "[line 4] in script",
    ]),
    ExecuteResult::RuntimeError,
  )?;

  test_file_with_stdio(
    "language/exception/two_deep_thrown.ly",
    None,
    Some(vec![
      "Index out of bounds. list was length 0 but attempted to index with 1.",
      "[line 5] in thrower()",
      "[line 1] in outer()",
      "[line 8] in script",
    ]),
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn export() -> Result<(), std::io::Error> {
  test_file_exits(&vec![], ExecuteResult::Ok)?;

  test_file_exits(
    &vec![
      "language/export/literal.ly",
      "language/export/local.ly",
      "language/export/non_declaration_class.ly",
      "language/export/non_declaration_fun.ly",
      "language/export/non_declaration_var.ly",
    ],
    ExecuteResult::CompileError,
  )?;

  test_file_exits(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn expressions() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec!["language/expressions/evaluate.ly"],
    ExecuteResult::Ok,
  )?;

  test_file_exits(&vec![], ExecuteResult::CompileError)?;

  test_file_exits(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn field() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/field/call_function_field.ly",
      "language/field/get_and_set_method.ly",
      "language/field/many.ly",
      "language/field/method_binds_this.ly",
      "language/field/method.ly",
      "language/field/on_instance.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_file_exits(&vec![], ExecuteResult::CompileError)?;

  test_file_exits(
    &vec![
      "language/field/call_nonfunction_field.ly",
      "language/field/get_on_bool.ly",
      "language/field/get_on_class.ly",
      "language/field/get_on_function.ly",
      "language/field/get_on_nil.ly",
      "language/field/get_on_num.ly",
      "language/field/get_on_string.ly",
      "language/field/set_evaluation_order.ly",
      "language/field/set_on_bool.ly",
      "language/field/set_on_class.ly",
      "language/field/set_on_function.ly",
      "language/field/set_on_nil.ly",
      "language/field/set_on_num.ly",
      "language/field/set_on_string.ly",
      "language/field/undefined.ly",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn for_loop() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/for/return_inside.ly",
      "language/for/scope.ly",
      "language/for/syntax.ly",
      "language/for/closure_in_body.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/for/class_in_body.ly",
      "language/for/fun_in_body.ly",
      "language/for/statement_condition.ly",
      "language/for/statement_increment.ly",
      "language/for/statement_initializer.ly",
      "language/for/var_in_body.ly",
    ],
    ExecuteResult::CompileError,
  )?;

  test_file_exits(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn for_range_loop() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/for_range/return_inside.ly",
      "language/for_range/scope.ly",
      "language/for_range/closure_in_body.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/for_range/class_in_body.ly",
      "language/for_range/fun_in_body.ly",
      "language/for_range/var_in_body.ly",
      "language/for_range/statement_iterator.ly",
    ],
    ExecuteResult::CompileError,
  )?;

  test_file_exits(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn function() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/function/empty_body.ly",
      "language/function/local_recursion.ly",
      "language/function/mutual_recursion.ly",
      "language/function/parameters.ly",
      "language/function/print.ly",
      "language/function/recursion.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/function/body_must_be_block.ly",
      "language/function/missing_comma_in_parameters.ly",
      "language/function/too_many_arguments.ly",
      "language/function/too_many_parameters.ly",
    ],
    ExecuteResult::CompileError,
  )?;

  test_file_exits(
    &vec![
      "language/function/extra_arguments.ly",
      "language/function/local_mutual_recursion.ly",
      "language/function/missing_arguments.ly",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn hooks() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/hooks/call_native.ly",
      "language/hooks/call_sl_function.ly",
      "language/hooks/call_sl_closure.ly",
      "language/hooks/call_sl_instance.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_file_exits(&vec![], ExecuteResult::CompileError)?;

  test_file_exits(
    &vec![
      "language/hooks/pass_error_native.ly",
      "language/hooks/pass_error_sl_function.ly",
      "language/hooks/pass_error_sl_closure.ly",
      "language/hooks/pass_error_sl_instance.ly",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn if_stmt() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/if/dangling_else.ly",
      "language/if/else.ly",
      "language/if/if.ly",
      "language/if/truth.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/if/class_in_else.ly",
      "language/if/class_in_then.ly",
      "language/if/fun_in_else.ly",
      "language/if/fun_in_then.ly",
      "language/if/var_in_then.ly",
      "language/if/var_in_then.ly",
    ],
    ExecuteResult::CompileError,
  )?;

  test_file_exits(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn import() -> Result<(), std::io::Error> {
  test_file_exits(&vec![], ExecuteResult::Ok)?;

  test_file_exits(
    &vec![
      "language/import/missing_name.ly",
      "language/import/missing_semicolon.ly",
      "language/import/non_identifier_name.ly",
      "language/import/non_string_literal.ly",
      "language/import/non_string_path.ly",
    ],
    ExecuteResult::CompileError,
  )?;

  test_file_exits(
    &vec!["language/import/std_lib_not_real.ly"],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn indexing() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/indexing/list_get.ly",
      "language/indexing/list_set.ly",
      "language/indexing/list_nested_get.ly",
      "language/indexing/list_nested_set.ly",
      "language/indexing/map_na_get.ly",
      "language/indexing/map_ref_equal_get.ly",
      "language/indexing/map_nested_get.ly",
      "language/indexing/map_nested_set.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_file_exits(&vec![], ExecuteResult::CompileError)?;

  test_file_exits(
    &vec![
      "language/indexing/list_out_of_range.ly",
      "language/indexing/map_key_not_found.ly",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn inheritance() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/inheritance/constructor.ly",
      "language/inheritance/inherit_methods.ly",
      "language/inheritance/set_fields_from_base_class.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_file_exits(
    &vec!["language/inheritance/parenthesized_superclass.ly"],
    ExecuteResult::CompileError,
  )?;

  test_file_exits(
    &vec![
      "language/inheritance/inherit_from_function.ly",
      "language/inheritance/inherit_from_nil.ly",
      "language/inheritance/inherit_from_number.ly",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn iterator() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/iterator/equality.ly",
      "language/iterator/assign_iter_keep_state.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_file_exits(&vec![], ExecuteResult::CompileError)?;

  test_file_exits(
    &vec!["language/iterator/cannot_set_current.ly"],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn limit() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec!["language/limit/reuse_constants.ly"],
    ExecuteResult::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/limit/loop_too_large.ly",
      "language/limit/too_many_constants.ly",
      "language/limit/too_many_locals.ly",
      "language/limit/too_many_upvalues.ly",
    ],
    ExecuteResult::CompileError,
  )?;

  test_file_exits(
    &vec!["language/limit/stack_overflow.ly"],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn lambda() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/lambda/expression_body.ly",
      "language/lambda/empty_body.ly",
      "language/lambda/mutual_recursion.ly",
      "language/lambda/recursion.ly",
      "language/lambda/parameters.ly",
      "language/lambda/print.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/lambda/local_recursion.ly",
      "language/lambda/local_mutual_recursion.ly",
      "language/lambda/body_must_be_block_or_expr.ly",
      "language/lambda/missing_comma_in_parameters.ly",
      "language/lambda/too_many_parameters.ly",
      "language/lambda/too_many_arguments.ly",
    ],
    ExecuteResult::CompileError,
  )?;

  test_file_exits(
    &vec![
      "language/lambda/extra_arguments.ly",
      "language/lambda/missing_arguments.ly",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn list() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/list/empty.ly",
      "language/list/homogeneous.ly",
      "language/list/mixed.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/list/missing_comma_in_initializer.ly",
      "language/list/missing_closing_bracket.ly",
    ],
    ExecuteResult::CompileError,
  )?;

  test_file_exits(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn logical_operator() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/logical_operator/and_truth.ly",
      "language/logical_operator/and.ly",
      "language/logical_operator/or_truth.ly",
      "language/logical_operator/or.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_file_exits(&vec![], ExecuteResult::CompileError)?;

  test_file_exits(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn map() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/map/empty.ly",
      "language/map/homogeneous.ly",
      "language/map/mixed.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/map/missing_closing_curly.ly",
      "language/map/missing_colon.ly",
      "language/map/statement_key.ly",
      "language/map/statement_value.ly",
    ],
    ExecuteResult::CompileError,
  )?;

  test_file_exits(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn method() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/method/arity.ly",
      "language/method/empty_block.ly",
      "language/method/print_bound_method.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/method/too_many_arguments.ly",
      "language/method/too_many_parameters.ly",
    ],
    ExecuteResult::CompileError,
  )?;

  test_file_exits(
    &vec![
      "language/method/extra_arguments.ly",
      "language/method/missing_arguments.ly",
      "language/method/not_found.ly",
      "language/method/refer_to_name.ly",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn native() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/native/assert.ly",
      "language/native/assert_eq.ly",
      "language/native/assert_ne.ly",
      "language/native/clock.ly",
      "language/native/signature_fixed_arity.ly",
      "language/native/signature_type.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_file_exits(&vec![], ExecuteResult::CompileError)?;

  test_file_exits(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn nil() -> Result<(), std::io::Error> {
  test_file_exits(&vec!["language/nil/literal.ly"], ExecuteResult::Ok)?;

  test_file_exits(&vec![], ExecuteResult::CompileError)?;

  test_file_exits(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn number() -> Result<(), std::io::Error> {
  test_file_exits(&vec!["language/number/literals.ly"], ExecuteResult::Ok)?;

  test_file_exits(
    &vec![
      "language/number/decimal_point_at_eof.ly",
      "language/number/leading_dot.ly",
      "language/number/trailing_dot.ly",
    ],
    ExecuteResult::CompileError,
  )?;

  test_file_exits(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn operator() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/operator/add.ly",
      "language/operator/comparison.ly",
      "language/operator/divide.ly",
      "language/operator/equals_class.ly",
      "language/operator/equals_method.ly",
      "language/operator/equals.ly",
      "language/operator/multiply.ly",
      "language/operator/negate.ly",
      "language/operator/not_class.ly",
      "language/operator/not_equals.ly",
      "language/operator/not.ly",
      "language/operator/subtract.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_file_exits(&vec![], ExecuteResult::CompileError)?;

  test_file_exits(
    &vec![
      "language/operator/add_bool_nil.ly",
      "language/operator/add_bool_num.ly",
      "language/operator/add_bool_string.ly",
      "language/operator/add_nil_nil.ly",
      "language/operator/add_num_nil.ly",
      "language/operator/add_string_nil.ly",
      "language/operator/divide_nonnum_num.ly",
      "language/operator/divide_num_nonnum.ly",
      "language/operator/greater_nonnum_num.ly",
      "language/operator/greater_num_nonnum.ly",
      "language/operator/greater_or_equal_nonnum_num.ly",
      "language/operator/greater_or_equal_num_nonnum.ly",
      "language/operator/less_nonnum_num.ly",
      "language/operator/less_num_nonnum.ly",
      "language/operator/less_or_equal_nonnum_num.ly",
      "language/operator/less_or_equal_num_nonnum.ly",
      "language/operator/multiply_nonnum_num.ly",
      "language/operator/multiply_num_nonnum.ly",
      "language/operator/negate_nonnum.ly",
      "language/operator/subtract_nonnum_num.ly",
      "language/operator/subtract_num_nonnum.ly",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn print() -> Result<(), std::io::Error> {
  test_file_exits(&vec![], ExecuteResult::Ok)?;

  test_file_exits(
    &vec!["language/print/missing_argument.ly"],
    ExecuteResult::CompileError,
  )?;

  test_file_exits(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn regression() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec!["language/regression/40.ly", "language/regression/394.ly"],
    ExecuteResult::Ok,
  )
}

#[test]
fn return_test() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/return/after_else.ly",
      "language/return/after_if.ly",
      "language/return/after_while.ly",
      "language/return/in_function.ly",
      "language/return/return_nil_if_no_value.ly",
      "language/return/return_nil_if_no_value.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_file_exits(
    &vec!["language/return/at_top_level.ly"],
    ExecuteResult::CompileError,
  )?;

  test_file_exits(&vec![], ExecuteResult::RuntimeError)
}

#[test]
fn string() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/string/literals.ly",
      "language/string/multiline.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/string/unterminated_double.ly",
      "language/string/unterminated_single.ly",
    ],
    ExecuteResult::CompileError,
  )?;

  test_file_exits(
    &vec!["language/string/error_after_multiline.ly"],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn super_() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/super/bound_method.ly",
      "language/super/call_other_method.ly",
      "language/super/call_same_method.ly",
      "language/super/closure.ly",
      "language/super/constructor.ly",
      "language/super/indirectly_inherited.ly",
      "language/super/reassign_superclass.ly",
      "language/super/super_in_closure_in_inherited_method.ly",
      "language/super/super_in_inherited_method.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/super/no_superclass_bind.ly",
      "language/super/no_superclass_call.ly",
      "language/super/parenthesized.ly",
      "language/super/super_at_top_level.ly",
      "language/super/super_in_top_level_function.ly",
      "language/super/super_without_dot.ly",
      "language/super/super_without_name.ly",
    ],
    ExecuteResult::CompileError,
  )?;

  test_file_exits(
    &vec![
      "language/super/extra_arguments.ly",
      "language/super/missing_arguments.ly",
      "language/super/no_superclass_method.ly",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn variable() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/variable/early_bound.ly",
      "language/variable/in_middle_of_block.ly",
      "language/variable/in_nested_block.ly",
      "language/variable/local_from_method.ly",
      "language/variable/redeclare_global.ly",
      "language/variable/redefine_global.ly",
      "language/variable/scope_reuse_in_different_blocks.ly",
      "language/variable/shadow_and_local.ly",
      "language/variable/shadow_global.ly",
      "language/variable/shadow_local.ly",
      "language/variable/uninitialized.ly",
      "language/variable/unreached_undefined.ly",
      "language/variable/use_global_in_initializer.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/variable/collide_with_parameter.ly",
      "language/variable/duplicate_local.ly",
      "language/variable/duplicate_parameter.ly",
      "language/variable/use_false_as_var.ly",
      "language/variable/use_local_in_initializer.ly",
      "language/variable/use_nil_as_var.ly",
      "language/variable/use_this_as_var.ly",
    ],
    ExecuteResult::CompileError,
  )?;

  test_file_exits(
    &vec![
      "language/variable/undefined_global.ly",
      "language/variable/undefined_local.ly",
      "language/variable/undefined_local.ly",
    ],
    ExecuteResult::RuntimeError,
  )
}

#[test]
fn while_test() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/while/closure_in_body.ly",
      "language/while/return_closure.ly",
      "language/while/return_inside.ly",
      "language/while/syntax.ly",
    ],
    ExecuteResult::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/while/class_in_body.ly",
      "language/while/fun_in_body.ly",
      "language/while/var_in_body.ly",
    ],
    ExecuteResult::CompileError,
  )?;

  test_file_exits(&vec![], ExecuteResult::RuntimeError)
}
