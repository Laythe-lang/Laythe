use laythe_vm::vm::{default_native_vm, VmExit};
use support::{assert_file_exit_and_stdio, assert_files_exit, assert_files_exit_with_cwd};

mod support;

fn test_file_exits(paths: &[&str], result: VmExit) -> Result<(), std::io::Error> {
  assert_files_exit(paths, FILE_PATH, result)
}

fn test_file_exits_with_cwd(
  paths: &[&str],
  cwd: &str,
  result: VmExit,
) -> Result<(), std::io::Error> {
  assert_files_exit_with_cwd(paths, FILE_PATH, cwd, result)
}

fn test_file_with_stdio(
  path: &str,
  stdout: Option<Vec<&str>>,
  stderr: Option<Vec<&str>>,
  result: VmExit,
) -> Result<(), std::io::Error> {
  assert_file_exit_and_stdio(path, FILE_PATH, None, None, stdout, stderr, result)
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
      "language/assignment/associativity.lay",
      "language/assignment/global.lay",
      "language/assignment/local.lay",
      "language/assignment/syntax.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/assignment/grouping.lay",
      "language/assignment/infix_operator.lay",
      "language/assignment/prefix_operator.lay",
      "language/assignment/to_this.lay",
      "language/assignment/undefined.lay",
    ],
    VmExit::CompileError,
  )?;

  test_file_exits(&vec![], VmExit::RuntimeError)
}

#[test]
fn binary_assignment() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/binary_assignment/local.lay",
      "language/binary_assignment/syntax.lay",
      "language/binary_assignment/associativity.lay",
      "language/binary_assignment/global.lay",
      "language/binary_assignment/operators.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/binary_assignment/grouping.lay",
      "language/binary_assignment/infix_operator.lay",
      "language/binary_assignment/prefix_operator.lay",
      "language/binary_assignment/to_this.lay",
      "language/binary_assignment/undefined.lay",
    ],
    VmExit::CompileError,
  )?;

  test_file_exits(&vec![], VmExit::RuntimeError)
}

#[test]
fn block() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec!["language/block/empty.lay", "language/block/scope.lay"],
    VmExit::Ok,
  )
}

#[test]
fn bool() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec!["language/bool/equality.lay", "language/bool/not.lay"],
    VmExit::Ok,
  )
}

#[test]
fn break_() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/break/additional_scopes.lay",
      "language/break/for_break.lay",
      "language/break/while_break.lay",
      "language/break/nested_while_loops.lay",
      "language/break/nested_for_loops.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(
    &vec!["language/break/outside_loop.lay"],
    VmExit::CompileError,
  )
}

#[test]
fn call() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/call/bool.lay",
      "language/call/nil.lay",
      "language/call/num.lay",
      "language/call/object.lay",
      "language/call/string.lay",
    ],
    VmExit::RuntimeError,
  )
}

#[test]
fn channel() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/channel/buffered.lay",
      "language/channel/channel_send_channel.lay",
      "language/channel/channel_sync.lay",
      "language/channel/fancy.lay",
      "language/channel/in_collection.lay",
      "language/channel/in_function.lay",
      "language/channel/in_instance.lay",
      "language/channel/multi_capture_increment.lay",
      "language/channel/receive_buffered.lay",
      "language/channel/receive_buffered_closed.lay",
      "language/channel/receive_sync.lay",
      "language/channel/receive_sync_closed.lay",
      "language/channel/receive_sync_no_send.lay",
      "language/channel/send_buffered.lay",
      "language/channel/send_sync.lay",
      "language/channel/sync.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/channel/missing_closing_paren.lay",
      "language/channel/missing_open_paren.lay",
    ],
    VmExit::CompileError,
  )?;

  test_file_exits(
    &vec![
      "language/channel/close_closed.lay",
      "language/channel/non_integer_capacity.lay",
      "language/channel/non_number_capacity.lay",
      "language/channel/receive_buffered_deadlock.lay",
      "language/channel/receive_sync_deadlock.lay",
      "language/channel/send_buffered_closed.lay",
      "language/channel/send_buffered_deadlock.lay",
      "language/channel/send_sync_closed.lay",
      "language/channel/send_sync_deadlock.lay",
    ],
    VmExit::RuntimeError,
  )
}

#[test]
fn class() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/class/empty.lay",
      "language/class/inherited_method.lay",
      "language/class/local_inherit_other.lay",
      "language/class/local_reference_self.lay",
      "language/class/reference_self.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/class/inherit_self.lay",
      "language/class/local_inherit_self.lay",
    ],
    VmExit::CompileError,
  )?;

  test_file_exits(&vec![], VmExit::RuntimeError)
}

#[test]
fn closure() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/closure/assign_to_closure.lay",
      "language/closure/assign_to_shadowed_later.lay",
      "language/closure/close_over_function_parameter.lay",
      "language/closure/close_over_later_variable.lay",
      "language/closure/close_over_method_parameter.lay",
      "language/closure/closed_closure_in_function.lay",
      "language/closure/nested_closure.lay",
      "language/closure/open_closure_in_function.lay",
      "language/closure/reference_closure_multiple_times.lay",
      "language/closure/reuse_closure_slot.lay",
      "language/closure/shadow_closure_with_local.lay",
      "language/closure/unused_closure.lay",
      "language/closure/unused_later_closure.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(&vec![], VmExit::CompileError)?;

  test_file_exits(&vec![], VmExit::RuntimeError)
}

#[test]
fn continue_() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/continue/additional_scopes.lay",
      "language/continue/for_continue.lay",
      "language/continue/while_continue.lay",
      "language/continue/nested_while_loops.lay",
      "language/continue/nested_for_loops.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(
    &vec!["language/continue/outside_loop.lay"],
    VmExit::CompileError,
  )
}

#[test]
fn comments() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/comments/line_at_eof.lay",
      "language/comments/only_line_comment_and_line.lay",
      "language/comments/only_line_comment.lay",
      "language/comments/unicode.lay",
    ],
    VmExit::Ok,
  )
}

#[test]
fn constructor() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/constructor/arguments.lay",
      "language/constructor/call_init_early_return.lay",
      "language/constructor/call_init_explicitly.lay",
      "language/constructor/default.lay",
      "language/constructor/early_return.lay",
      "language/constructor/return_in_nested_function.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(
    &vec!["language/constructor/return_value.lay"],
    VmExit::CompileError,
  )?;

  test_file_exits(
    &vec![
      "language/constructor/default_arguments.lay",
      "language/constructor/extra_arguments.lay",
      "language/constructor/missing_arguments.lay",
    ],
    VmExit::RuntimeError,
  )
}

#[test]
fn exception() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/exception/top_level_catch.lay",
      "language/exception/one_deep_catch.lay",
      "language/exception/two_deep_catch.lay",
      "language/exception/top_level_catch_thrown.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/exception/catch_no_block.lay",
      "language/exception/try_no_block.lay",
      "language/exception/try_no_catch.lay",
    ],
    VmExit::CompileError,
  )?;

  test_file_with_stdio(
    "language/exception/top_level_thrown.lay",
    None,
    Some(vec![
      "IndexError: Index out of bounds. list was length 0 but attempted to index with 1.",
      "  [line 0] in []()",
      "  [line 1] in script",
    ]),
    VmExit::RuntimeError,
  )?;

  test_file_with_stdio(
    "language/exception/one_deep_thrown.lay",
    None,
    Some(vec![
      "IndexError: Index out of bounds. list was length 0 but attempted to index with 1.",
      "  [line 0] in []()",
      "  [line 2] in thrower()",
      "  [line 5] in script",
    ]),
    VmExit::RuntimeError,
  )?;

  test_file_with_stdio(
    "language/exception/two_deep_thrown.lay",
    None,
    Some(vec![
      "IndexError: Index out of bounds. list was length 0 but attempted to index with 1.",
      "  [line 0] in []()",
      "  [line 6] in thrower()",
      "  [line 2] in outer()",
      "  [line 9] in script",
    ]),
    VmExit::RuntimeError,
  )
}

#[test]
fn export() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/export/declaration_class.lay",
      "language/export/declaration_fn.lay",
      "language/export/declaration_let.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/export/literal.lay",
      "language/export/local.lay",
      "language/export/non_declaration_class.lay",
      "language/export/non_declaration_fn.lay",
      "language/export/non_declaration_let.lay",
    ],
    VmExit::CompileError,
  )?;

  test_file_exits(&vec![], VmExit::RuntimeError)
}

#[test]
fn expressions() -> Result<(), std::io::Error> {
  test_file_exits(&vec!["language/expressions/evaluate.lay"], VmExit::Ok)?;

  test_file_exits(&vec![], VmExit::CompileError)?;

  test_file_exits(&vec![], VmExit::RuntimeError)
}

#[test]
fn field() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/field/call_function_field.lay",
      "language/field/many.lay",
      "language/field/method_binds_self.lay",
      "language/field/method.lay",
      "language/field/on_instance.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(
    &vec!["language/field/set_evaluation_order.lay"],
    VmExit::CompileError,
  )?;

  test_file_exits(
    &vec![
      "language/field/call_nonfunction_field.lay",
      "language/field/get_on_bool.lay",
      "language/field/get_on_class.lay",
      "language/field/get_on_function.lay",
      "language/field/get_on_nil.lay",
      "language/field/get_on_num.lay",
      "language/field/get_on_string.lay",
      "language/field/set_undefined.lay",
      "language/field/set_on_bool.lay",
      "language/field/set_on_class.lay",
      "language/field/set_on_function.lay",
      "language/field/set_on_nil.lay",
      "language/field/set_on_num.lay",
      "language/field/set_on_string.lay",
      "language/field/get_undefined.lay",
    ],
    VmExit::RuntimeError,
  )
}

#[test]
fn for_loop() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/for/return_inside.lay",
      "language/for/scope.lay",
      "language/for/closure_in_body.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/for/cannot_assign_iter.lay",
      "language/for/class_in_body.lay",
      "language/for/fn_in_body.lay",
      "language/for/let_in_body.lay",
      "language/for/statement_iterator.lay",
    ],
    VmExit::CompileError,
  )?;

  test_file_exits(&vec![], VmExit::RuntimeError)
}

#[test]
fn function() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/function/empty_body.lay",
      "language/function/local_recursion.lay",
      "language/function/mutual_recursion.lay",
      "language/function/parameters.lay",
      "language/function/print.lay",
      "language/function/recursion.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/function/body_must_be_block.lay",
      "language/function/missing_comma_in_parameters.lay",
      "language/function/too_many_arguments.lay",
      "language/function/too_many_parameters.lay",
      "language/function/local_mutual_recursion.lay",
    ],
    VmExit::CompileError,
  )?;

  test_file_exits(
    &vec![
      "language/function/extra_arguments.lay",
      "language/function/missing_arguments.lay",
    ],
    VmExit::RuntimeError,
  )
}

#[test]
fn hooks() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/hooks/call_native.lay",
      "language/hooks/call_ly_function.lay",
      "language/hooks/call_ly_closure.lay",
      "language/hooks/call_ly_instance.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(&vec![], VmExit::CompileError)?;

  test_file_exits(
    &vec![
      "language/hooks/pass_error_native.lay",
      "language/hooks/pass_error_ly_function.lay",
      "language/hooks/pass_error_ly_closure.lay",
      "language/hooks/pass_error_ly_instance.lay",
    ],
    VmExit::RuntimeError,
  )
}

#[test]
fn if_stmt() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/if/dangling_else.lay",
      "language/if/else.lay",
      "language/if/if.lay",
      "language/if/truth.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/if/class_in_else.lay",
      "language/if/class_in_then.lay",
      "language/if/fun_in_else.lay",
      "language/if/fun_in_then.lay",
      "language/if/let_in_then.lay",
      "language/if/let_in_then.lay",
    ],
    VmExit::CompileError,
  )?;

  test_file_exits(&vec![], VmExit::RuntimeError)
}

#[test]
fn implicit_return() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/implicit_return/in_function.lay",
      "language/implicit_return/in_method.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/implicit_return/after_else.lay",
      "language/implicit_return/after_if.lay",
      "language/implicit_return/after_while.lay",
      "language/implicit_return/at_top_level.lay",
      "language/implicit_return/in_function_middle.lay",
      "language/implicit_return/in_method_middle.lay",
      "language/implicit_return/in_init.lay",
    ],
    VmExit::CompileError,
  )?;

  test_file_exits(&vec![], VmExit::RuntimeError)
}

#[test]
fn import() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/import/module.lay",
      "language/import/module_rename.lay",
      "language/import/symbol.lay",
      "language/import/symbol_rename.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits_with_cwd(
    &vec![
      "language/import/user_import/symbol.lay",
      "language/import/user_import/module.lay",
    ],
    "language/import/user_import",
    VmExit::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/import/import_in_fun.lay",
      "language/import/import_in_class.lay",
      "language/import/import_in_scope.lay",
      "language/import/missing_path.lay",
      "language/import/missing_semicolon.lay",
      "language/import/non_identifier_path.lay",
      "language/import/rename_missing.lay",
      "language/import/rename_not_identifer.lay",
      "language/import/rename_redefine.lay",
      "language/import/symbols_redefine.lay",
      "language/import/symbols_rename_missing.lay",
      "language/import/symbols_rename_not_identifer.lay",
    ],
    VmExit::CompileError,
  )?;

  test_file_exits(
    &vec![
      "language/import/module_not_real.lay",
      "language/import/symbols_not_real.lay",
    ],
    VmExit::RuntimeError,
  )?;

  test_file_exits_with_cwd(
    &vec!["language/import/user_import/cycle_1.lay"],
    "language/import/user_import",
    VmExit::RuntimeError,
  )
}

#[test]
fn inheritance() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/inheritance/constructor.lay",
      "language/inheritance/inherit_methods.lay",
      "language/inheritance/set_fields_from_base_class.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(
    &vec!["language/inheritance/parenthesized_superclass.lay"],
    VmExit::CompileError,
  )?;

  test_file_exits(
    &vec![
      "language/inheritance/inherit_from_function.lay",
      "language/inheritance/inherit_from_nil.lay",
      "language/inheritance/inherit_from_number.lay",
    ],
    VmExit::RuntimeError,
  )
}

#[test]
fn inline_cache() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/inline_cache/invoke_thrash.lay",
      "language/inline_cache/property_get_thrash.lay",
      "language/inline_cache/property_set_thrash.lay",
      "language/inline_cache/super_invoke_thrash.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(&vec![], VmExit::CompileError)?;

  test_file_exits(&vec![], VmExit::RuntimeError)
}

#[test]
fn iterator() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/iterator/equality.lay",
      "language/iterator/assign_iter_keep_state.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(&vec![], VmExit::CompileError)?;

  test_file_exits(&vec![], VmExit::RuntimeError)
}

#[test]
fn launch() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/launch/launch_bound_method.lay",
      "language/launch/launch_exit.lay",
      "language/launch/launch_init.lay",
      "language/launch/launch_method.lay",
      "language/launch/launch_multi.lay",
      "language/launch/launch_multi_channel_join.lay",
      "language/launch/launch_native.lay",
      "language/launch/launch_single.lay",
      "language/launch/launch_with_capture_global.lay",
      "language/launch/launch_with_capture_local.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/launch/no_call.lay",
      "language/launch/no_expr.lay",
      "language/launch/no_semi.lay",
    ],
    VmExit::CompileError,
  )?;

  test_file_exits(
    &vec!["language/launch/launch_error.lay"],
    VmExit::RuntimeError,
  )
}

#[test]
fn limit() -> Result<(), std::io::Error> {
  test_file_exits(&vec!["language/limit/reuse_constants.lay"], VmExit::Ok)?;

  test_file_exits(
    &vec![
      "language/limit/loop_too_large.lay",
      "language/limit/too_many_constants.lay",
      "language/limit/too_many_locals.lay",
      "language/limit/too_many_captures.lay",
    ],
    VmExit::CompileError,
  )?;

  test_file_exits(
    &vec!["language/limit/stack_overflow.lay"],
    VmExit::RuntimeError,
  )
}

#[test]
fn lambda() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/lambda/expression_body.lay",
      "language/lambda/empty_body.lay",
      "language/lambda/mutual_recursion.lay",
      "language/lambda/recursion.lay",
      "language/lambda/parameters.lay",
      "language/lambda/str.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/lambda/local_recursion.lay",
      "language/lambda/body_must_be_block_or_expr.lay",
      "language/lambda/missing_comma_in_parameters.lay",
      "language/lambda/too_many_parameters.lay",
      "language/lambda/too_many_arguments.lay",
      "language/lambda/local_mutual_recursion.lay",
    ],
    VmExit::CompileError,
  )?;

  test_file_exits(
    &vec![
      "language/lambda/extra_arguments.lay",
      "language/lambda/missing_arguments.lay",
    ],
    VmExit::RuntimeError,
  )
}

#[test]
fn list() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/list/empty.lay",
      "language/list/homogeneous.lay",
      "language/list/mixed.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/list/missing_comma_in_initializer.lay",
      "language/list/missing_closing_bracket.lay",
    ],
    VmExit::CompileError,
  )?;

  test_file_exits(&vec![], VmExit::RuntimeError)
}

#[test]
fn logical_operator() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/logical_operator/and.lay",
      "language/logical_operator/and_truth.lay",
      "language/logical_operator/nested_ternary.lay",
      "language/logical_operator/or.lay",
      "language/logical_operator/or_truth.lay",
      "language/logical_operator/ternary.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(&vec![], VmExit::CompileError)?;

  test_file_exits(&vec![], VmExit::RuntimeError)
}

#[test]
fn map() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/map/empty.lay",
      "language/map/homogeneous.lay",
      "language/map/mixed.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/map/missing_closing_curly.lay",
      "language/map/missing_colon.lay",
      "language/map/statement_key.lay",
      "language/map/statement_value.lay",
    ],
    VmExit::CompileError,
  )?;

  test_file_exits(&vec![], VmExit::RuntimeError)
}

#[test]
fn method() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/method/arity.lay",
      "language/method/empty_block.lay",
      "language/method/print_bound_method.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/method/too_many_arguments.lay",
      "language/method/too_many_parameters.lay",
      "language/method/refer_to_name.lay",
    ],
    VmExit::CompileError,
  )?;

  test_file_exits(
    &vec![
      "language/method/extra_arguments.lay",
      "language/method/missing_arguments.lay",
      "language/method/not_found.lay",
    ],
    VmExit::RuntimeError,
  )
}

#[test]
fn native() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/native/assert.lay",
      "language/native/assert_eq.lay",
      "language/native/assert_ne.lay",
      "language/native/clock.lay",
      "language/native/signature_fixed_arity.lay",
      "language/native/signature_type.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(&vec![], VmExit::CompileError)?;

  test_file_exits(&vec![], VmExit::RuntimeError)
}

#[test]
fn nil() -> Result<(), std::io::Error> {
  test_file_exits(&vec!["language/nil/literal.lay"], VmExit::Ok)?;

  test_file_exits(&vec![], VmExit::CompileError)?;

  test_file_exits(&vec![], VmExit::RuntimeError)
}

#[test]
fn number() -> Result<(), std::io::Error> {
  test_file_exits(&vec!["language/number/literals.lay"], VmExit::Ok)?;

  test_file_exits(
    &vec![
      "language/number/decimal_point_at_eof.lay",
      "language/number/leading_dot.lay",
      "language/number/trailing_dot.lay",
    ],
    VmExit::CompileError,
  )?;

  test_file_exits(&vec![], VmExit::RuntimeError)
}

#[test]
fn operator() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/operator/add.lay",
      "language/operator/comparison.lay",
      "language/operator/divide.lay",
      "language/operator/equals_class.lay",
      "language/operator/equals_method.lay",
      "language/operator/equals.lay",
      "language/operator/multiply.lay",
      "language/operator/negate.lay",
      "language/operator/not_class.lay",
      "language/operator/not_equals.lay",
      "language/operator/not.lay",
      "language/operator/subtract.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(&vec![], VmExit::CompileError)?;

  test_file_exits(
    &vec![
      "language/operator/add_bool_nil.lay",
      "language/operator/add_bool_num.lay",
      "language/operator/add_bool_string.lay",
      "language/operator/add_nil_nil.lay",
      "language/operator/add_num_nil.lay",
      "language/operator/add_string_nil.lay",
      "language/operator/divide_nonnum_num.lay",
      "language/operator/divide_num_nonnum.lay",
      "language/operator/greater_nonnum_num.lay",
      "language/operator/greater_num_nonnum.lay",
      "language/operator/greater_or_equal_nonnum_num.lay",
      "language/operator/greater_or_equal_num_nonnum.lay",
      "language/operator/less_nonnum_num.lay",
      "language/operator/less_num_nonnum.lay",
      "language/operator/less_or_equal_nonnum_num.lay",
      "language/operator/less_or_equal_num_nonnum.lay",
      "language/operator/multiply_nonnum_num.lay",
      "language/operator/multiply_num_nonnum.lay",
      "language/operator/negate_nonnum.lay",
      "language/operator/subtract_nonnum_num.lay",
      "language/operator/subtract_num_nonnum.lay",
    ],
    VmExit::RuntimeError,
  )
}

#[test]
fn regression() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/regression/40.lay",
      "language/regression/394.lay",
      "language/regression/continue.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(
    &vec!["language/regression/missing_symbol.lay"],
    VmExit::CompileError,
  )
}

#[test]
fn return_test() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/return/after_else.lay",
      "language/return/after_if.lay",
      "language/return/after_while.lay",
      "language/return/in_function.lay",
      "language/return/return_nil_if_no_value.lay",
      "language/return/return_nil_if_no_value.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(
    &vec!["language/return/at_top_level.lay"],
    VmExit::CompileError,
  )?;

  test_file_exits(&vec![], VmExit::RuntimeError)
}

#[test]
fn static_method() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/static_method/arity.lay",
      "language/static_method/call_bound.lay",
      "language/static_method/empty_block.lay",
      "language/static_method/print_bound_method.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/static_method/no_self.lay",
      "language/static_method/not_inherited.lay",
      "language/static_method/too_many_arguments.lay",
      "language/static_method/too_many_parameters.lay",
      "language/static_method/refer_to_name.lay",
    ],
    VmExit::CompileError,
  )?;

  test_file_exits(
    &vec![
      "language/static_method/extra_arguments.lay",
      "language/static_method/missing_arguments.lay",
      "language/static_method/not_found.lay",
    ],
    VmExit::RuntimeError,
  )
}

#[test]
fn string() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/string/interpolation.lay",
      "language/string/literals.lay",
      "language/string/multiline.lay",
      "language/string/escape.lay",
      "language/string/unicode_escape.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/string/invalid_escape.lay",
      "language/string/invalid_interpolation_missing_close.lay",
      "language/string/invalid_unicode_hex.lay",
      "language/string/invalid_unicode_missing_close.lay",
      "language/string/invalid_unicode_missing_open.lay",
      "language/string/invalid_unicode_no_hex.lay",
      "language/string/invalid_unicode_too_long.lay",
      "language/string/unterminated_double.lay",
      "language/string/unterminated_single.lay",
      "language/string/error_after_multiline.lay",
    ],
    VmExit::CompileError,
  )?;

  test_file_exits(&vec![], VmExit::RuntimeError)
}

#[test]
fn super_() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/super/bound_method.lay",
      "language/super/call_other_method.lay",
      "language/super/call_same_method.lay",
      "language/super/closure.lay",
      "language/super/constructor.lay",
      "language/super/indirectly_inherited.lay",
      "language/super/reassign_superclass.lay",
      "language/super/super_in_closure_in_inherited_method.lay",
      "language/super/super_in_inherited_method.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/super/parenthesized.lay",
      "language/super/super_at_top_level.lay",
      "language/super/super_in_top_level_function.lay",
      "language/super/super_without_dot.lay",
      "language/super/super_without_name.lay",
    ],
    VmExit::CompileError,
  )?;

  test_file_exits(
    &vec![
      "language/super/extra_arguments.lay",
      "language/super/missing_arguments.lay",
      "language/super/no_superclass_method.lay",
    ],
    VmExit::RuntimeError,
  )
}

#[test]
fn variable() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/variable/early_bound.lay",
      "language/variable/in_middle_of_block.lay",
      "language/variable/in_nested_block.lay",
      "language/variable/local_from_method.lay",
      "language/variable/scope_reuse_in_different_blocks.lay",
      "language/variable/shadow_and_local.lay",
      "language/variable/shadow_global.lay",
      "language/variable/shadow_local.lay",
      "language/variable/uninitialized.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/variable/collide_with_parameter.lay",
      "language/variable/duplicate_local.lay",
      "language/variable/duplicate_parameter.lay",
      "language/variable/redeclare_global.lay",
      "language/variable/redefine_global.lay",
      "language/variable/undefined_global.lay",
      "language/variable/undefined_local.lay",
      "language/variable/unreached_undefined.lay",
      "language/variable/use_false_as_var.lay",
      "language/variable/use_global_in_initializer.lay",
      "language/variable/use_local_in_initializer.lay",
      "language/variable/use_nil_as_var.lay",
      "language/variable/use_this_as_var.lay",
    ],
    VmExit::CompileError,
  )?;

  test_file_exits(&vec![], VmExit::RuntimeError)
}

#[test]
fn tuple() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/tuple/empty.lay",
      "language/tuple/homogeneous.lay",
      "language/tuple/mixed.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/tuple/empty_comma.lay",
      "language/tuple/missing_comma_in_initializer.lay",
      "language/tuple/missing_closing_bracket.lay",
    ],
    VmExit::CompileError,
  )?;

  test_file_exits(&vec![], VmExit::RuntimeError)
}

#[test]
fn while_test() -> Result<(), std::io::Error> {
  test_file_exits(
    &vec![
      "language/while/closure_in_body.lay",
      "language/while/return_closure.lay",
      "language/while/return_inside.lay",
      "language/while/syntax.lay",
    ],
    VmExit::Ok,
  )?;

  test_file_exits(
    &vec![
      "language/while/class_in_body.lay",
      "language/while/fun_in_body.lay",
      "language/while/var_in_body.lay",
    ],
    VmExit::CompileError,
  )?;

  test_file_exits(&vec![], VmExit::RuntimeError)
}
