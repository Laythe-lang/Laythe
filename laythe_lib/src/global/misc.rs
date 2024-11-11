use crate::{native, support::export_and_insert_native, StdResult};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  managed::Trace,
  module::Module,
  object::{LyNative, LyStr, Native, NativeMetaBuilder},
  signature::{Arity, ParameterBuilder, ParameterKind},
  value::{Value, VALUE_NIL},
  Call, LyError, ObjRef, Ref,
};
use std::io::Write;

pub fn add_misc_funs(hooks: &GcHooks, module: Ref<Module>) -> StdResult<()> {
  declare_misc_funs(hooks, module)
}

const PRINT: NativeMetaBuilder = NativeMetaBuilder::fun("print", Arity::Variadic(0))
  .with_params(&[ParameterBuilder::new("values", ParameterKind::Object)])
  .with_stack();

const EXIT_META: NativeMetaBuilder = NativeMetaBuilder::fun("exit", Arity::Default(0, 1))
  .with_params(&[ParameterBuilder::new("code", ParameterKind::Number)]);

pub fn declare_misc_funs(hooks: &GcHooks, module: Ref<Module>) -> StdResult<()> {
  let str_name = hooks.manage_str("str");

  export_and_insert_native(hooks, module, Print::native(hooks, str_name))?;
  export_and_insert_native(hooks, module, Exit::native(hooks))
}

#[derive(Debug)]
/// A native method to assert that for a boolean true value
pub struct Print {
  /// reference to 'str'
  method_str: LyStr,
}

impl Print {
  /// Construct a new instance of the native assert function
  pub fn native(hooks: &GcHooks, method_str: LyStr) -> ObjRef<Native> {
    let native = Box::new(Self { method_str }) as Box<dyn LyNative>;

    hooks.manage_obj(Native::new(PRINT.build(hooks), native))
  }
}

impl LyNative for Print {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let str_method = hooks.get_method(args[0], self.method_str)?;
    let mut output = String::from(
      &*hooks
        .call_method(args[0], str_method, &[])?
        .to_obj()
        .to_str(),
    );

    for s in args.iter().skip(1) {
      let str_method = hooks.get_method(*s, self.method_str)?;

      output.push(' ');
      output.push_str(&hooks.call_method(*s, str_method, &[])?.to_obj().to_str())
    }

    let mut stdio = hooks.as_io().stdio();
    match writeln!(stdio.stdout(), "{output}") {
      Ok(_) => Call::Ok(VALUE_NIL),
      Err(err) => panic!("TODO return some sort of io error {err}"),
    }
  }
}

impl Trace for Print {
  fn trace(&self) {
    self.method_str.trace();
  }

  fn trace_debug(&self, stdout: &mut dyn Write) {
    self.method_str.trace_debug(stdout);
  }
}

native!(Exit, EXIT_META);

impl LyNative for Exit {
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
    let code = if args.is_empty() {
      0
    } else {
      args[0].to_num() as u16
    };

    Err(LyError::Exit(code))
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use laythe_core::val;

  #[cfg(test)]
  mod print {
    use laythe_core::NO_GC;

    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn call() {
      let mut context = MockedContext::with_std(&[]).unwrap();
      let response = context.gc.borrow_mut().manage_str("true", &NO_GC);
      context.add_responses(&[val!(response)]);

      let mut hooks = Hooks::new(&mut context);

      let print = Print::native(&hooks.as_gc(), hooks.manage_str("str"));
      let values = &[val!(true)];

      let result = print.call(&mut hooks, values).unwrap();

      assert_eq!(result, VALUE_NIL);
    }
  }
}
