use crate::{native, support::export_and_insert};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{MetaData, Native, NativeMeta, NativeMetaBuilder},
  package::Package,
  signature::{Arity, ParameterBuilder, ParameterKind},
  val,
  value::{Value, VALUE_NIL},
  CallResult, LyError, LyResult,
};
use laythe_env::{
  managed::{Managed, Trace},
  stdio::Stdio,
};
use smol_str::SmolStr;

pub fn add_misc_funs(
  hooks: &GcHooks,
  mut module: Managed<Module>,
  _package: Managed<Package>,
) -> LyResult<()> {
  declare_misc_funs(hooks, &mut module)
}

const PRINT_META: NativeMetaBuilder = NativeMetaBuilder::fun("print", Arity::Variadic(0))
  .with_params(&[ParameterBuilder::new("values", ParameterKind::Any)])
  .with_stack();

const EXIT_META: NativeMetaBuilder = NativeMetaBuilder::fun("exit", Arity::Default(0, 1))
  .with_params(&[ParameterBuilder::new("code", ParameterKind::Number)]);

// const RANGE_META: NativeMetaBuilder = NativeMetaBuilder::fun("range", Arity::Default(2, 3))
//   .with_params(&[
//     ParameterBuilder::new("lower", ParameterKind::Number),
//     ParameterBuilder::new("upper", ParameterKind::Number),
//     ParameterBuilder::new("stide", ParameterKind::Number),
//   ]);

pub fn declare_misc_funs(hooks: &GcHooks, self_module: &mut Module) -> LyResult<()> {
  let str_name = hooks.manage_str("str");

  export_and_insert(
    hooks,
    self_module,
    hooks.manage_str(PRINT_META.name),
    val!(hooks.manage(Box::new(Print::new(PRINT_META.to_meta(hooks), str_name)) as Box<dyn Native>)),
  )?;

  export_and_insert(
    hooks,
    self_module,
    hooks.manage_str(EXIT_META.name),
    val!(hooks.manage(Box::new(Exit::from(hooks)) as Box<dyn Native>)),
  )
}

#[derive(Debug)]
/// A native method to assert that for a boolean true value
pub struct Print {
  /// reference to 'str'
  method_str: Managed<SmolStr>,
  meta: NativeMeta,
}

impl Print {
  /// Construct a new instance of the native assert function
  pub fn new(meta: NativeMeta, method_str: Managed<SmolStr>) -> Self {
    Self { meta, method_str }
  }
}

impl MetaData for Print {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }
}

impl Native for Print {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> CallResult {
    let mut output = String::from(
      hooks
        .call_method_by_name(args[0], self.method_str, &[])?
        .to_str()
        .as_str(),
    );

    for s in args.iter().skip(1) {
      output.push(' ');
      output.push_str(
        hooks
          .call_method_by_name(*s, self.method_str, &[])?
          .to_str()
          .as_str(),
      )
    }

    let mut stdio = hooks.to_io().stdio();
    match writeln!(stdio.stdout(), "{}", output) {
      Ok(_) => Ok(VALUE_NIL),
      Err(err) => hooks.error(err.to_string()),
    }
  }
}

impl Trace for Print {
  fn trace(&self) -> bool {
    self.meta.trace();
    self.method_str.trace()
  }

  fn trace_debug(&self, stdio: &mut Stdio) -> bool {
    self.meta.trace_debug(stdio);
    self.method_str.trace_debug(stdio)
  }
}

native!(Exit, EXIT_META);

impl Native for Exit {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> CallResult {
    let code = if args.is_empty() {
      0.0
    } else {
      args[0].to_num() as f64
    };

    Err(Box::new(LyError::exit(
      hooks.manage_str(&format!("Exit code {}", code)),
    )))
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use laythe_core::hooks::support::TestContext;

  #[cfg(test)]
  mod print {
    use super::*;
    use crate::support::MockedContext;
    use laythe_env::managed::Allocation;
    use std::ptr::NonNull;

    #[test]
    fn new() {
      let mut context = TestContext::default();
      let hooks = Hooks::new(&mut context);
      let assert = Print::new(
        PRINT_META.to_meta(&hooks.to_gc()),
        hooks.manage_str("str".to_string()),
      );

      assert_eq!(&*assert.meta().name, "print");
      assert_eq!(assert.meta().signature.arity, Arity::Variadic(0));
      assert_eq!(
        assert.meta().signature.parameters[0].kind,
        ParameterKind::Any
      );
    }

    #[test]
    fn call() {
      let response = SmolStr::from("true");
      let allocation = Allocation::new(response);

      let managed = Managed::from(NonNull::from(&allocation));

      let mut context = MockedContext::new(&[val!(managed)]);
      let mut hooks = Hooks::new(&mut context);

      let print = Print::new(
        PRINT_META.to_meta(&hooks.to_gc()),
        hooks.manage_str("str".to_string()),
      );
      let values = &[val!(true)];

      let result = match print.call(&mut hooks, None, values) {
        Ok(res) => res,
        Err(_) => panic!(),
      };

      assert_eq!(result, VALUE_NIL);
    }
  }
}
