use crate::support::{
  default_class_inheritance, export_and_insert, load_instance_from_module, to_dyn_method,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{NativeMeta, NativeMethod},
  object::Instance,
  package::Package,
  signature::{Arity, Parameter, ParameterKind},
  value::{Value, VALUE_NIL},
  CallResult, LyResult,
};
use laythe_env::{managed::Trace, stdio::Stdio};

const STDOUT_CLASS_NAME: &str = "Stdout";
const STDOUT_INSTANCE_NAME: &str = "stdout";

const STDOUT_WRITE: NativeMeta = NativeMeta::new(
  "write",
  Arity::Fixed(1),
  &[Parameter::new("string", ParameterKind::String)],
);

const STDOUT_WRITELN: NativeMeta = NativeMeta::new(
  "writeln",
  Arity::Fixed(1),
  &[Parameter::new("string", ParameterKind::String)],
);

const STDOUT_FLUSH: NativeMeta = NativeMeta::new("flush", Arity::Fixed(0), &[]);

pub fn declare_stdout(hooks: &GcHooks, module: &mut Module, std: &Package) -> LyResult<()> {
  let class = default_class_inheritance(hooks, std, STDOUT_CLASS_NAME)?;
  let instance = hooks.manage(Instance::new(class));

  export_and_insert(
    hooks,
    module,
    hooks.manage_str(STDOUT_INSTANCE_NAME.to_string()),
    Value::from(instance),
  )
}

pub fn define_stdout(hooks: &GcHooks, module: &Module, _: &Package) -> LyResult<()> {
  let instance = load_instance_from_module(hooks, module, STDOUT_INSTANCE_NAME)?;
  let mut class = instance.class;

  class.add_method(
    hooks,
    hooks.manage_str(String::from(STDOUT_WRITE.name)),
    Value::from(to_dyn_method(hooks, StdoutWrite())),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(STDOUT_WRITELN.name)),
    Value::from(to_dyn_method(hooks, StdoutWriteln())),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(STDOUT_FLUSH.name)),
    Value::from(to_dyn_method(hooks, StdoutFlush())),
  );

  Ok(())
}

#[derive(Clone, Debug, Trace)]
struct StdoutWrite();

impl NativeMethod for StdoutWrite {
  fn meta(&self) -> &NativeMeta {
    &STDOUT_WRITE
  }

  fn call(&self, hooks: &mut Hooks, _this: Value, args: &[Value]) -> CallResult {
    let io = hooks.to_io();
    let mut stdio = io.stdio();
    let stdout = stdio.stdout();

    match stdout.write(args[0].to_str().as_bytes()) {
      Ok(_) => Ok(VALUE_NIL),
      Err(err) => Err(hooks.make_error(err.to_string())),
    }
  }
}

#[derive(Clone, Debug, Trace)]
struct StdoutWriteln();

impl NativeMethod for StdoutWriteln {
  fn meta(&self) -> &NativeMeta {
    &STDOUT_WRITELN
  }

  fn call(&self, hooks: &mut Hooks, _this: Value, args: &[Value]) -> CallResult {
    let io = hooks.to_io();
    let mut stdio = io.stdio();
    let stdout = stdio.stdout();

    match writeln!(stdout, "{}", args[0].to_str()) {
      Ok(_) => Ok(VALUE_NIL),
      Err(err) => Err(hooks.make_error(err.to_string())),
    }
  }
}

#[derive(Clone, Debug, Trace)]
struct StdoutFlush();

impl NativeMethod for StdoutFlush {
  fn meta(&self) -> &NativeMeta {
    &STDOUT_FLUSH
  }
  fn call(&self, hooks: &mut Hooks, _this: Value, _args: &[Value]) -> CallResult {
    let io = hooks.to_io();
    let mut stdio = io.stdio();
    let stdout = stdio.stdout();

    match stdout.flush() {
      Ok(_) => Ok(VALUE_NIL),
      Err(err) => Err(hooks.make_error(err.to_string())),
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod write {
    use super::*;
    use crate::support::MockedContext;
    use laythe_env::stdio::support::StdioTestContainer;
    use std::{rc::Rc, str};

    #[test]
    fn new() {
      let stdout_write = StdoutWrite();

      assert_eq!(stdout_write.meta().name, "write");
      assert_eq!(stdout_write.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        stdout_write.meta().signature.parameters[0].kind,
        ParameterKind::String
      );
    }

    #[test]
    fn call() {
      let stdout_write = StdoutWrite();
      let stdio_container = Rc::new(StdioTestContainer::default());

      let mut context = MockedContext::new_with_io(&stdio_container);
      let mut hooks = Hooks::new(&mut context);

      let string = Value::from(hooks.manage_str("some string".to_string()));
      let result = stdout_write.call(&mut hooks, VALUE_NIL, &[string]);

      assert!(result.is_ok());
      assert!(result.unwrap().is_nil());

      let stdout = str::from_utf8(&*stdio_container.stdout);
      assert!(stdout.is_ok());
      assert_eq!(stdout.unwrap(), "some string");
    }
  }

  mod writeln {
    use super::*;
    use crate::support::MockedContext;
    use laythe_env::stdio::support::StdioTestContainer;
    use std::{rc::Rc, str};

    #[test]
    fn new() {
      let stdout_writeln = StdoutWriteln();

      assert_eq!(stdout_writeln.meta().name, "writeln");
      assert_eq!(stdout_writeln.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        stdout_writeln.meta().signature.parameters[0].kind,
        ParameterKind::String
      );
    }

    #[test]
    fn call() {
      let stdout_write = StdoutWriteln();
      let stdio_container = Rc::new(StdioTestContainer::default());

      let mut context = MockedContext::new_with_io(&stdio_container);
      let mut hooks = Hooks::new(&mut context);

      let string = Value::from(hooks.manage_str("some string".to_string()));
      let result = stdout_write.call(&mut hooks, VALUE_NIL, &[string]);

      assert!(result.is_ok());
      assert!(result.unwrap().is_nil());

      let stdout = str::from_utf8(&*stdio_container.stdout);
      assert!(stdout.is_ok());
      assert_eq!(stdout.unwrap(), "some string\n");
    }
  }
}
