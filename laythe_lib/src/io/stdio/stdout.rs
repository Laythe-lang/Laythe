use crate::{
  io::{global::IO_ERROR, IO_MODULE_PATH},
  native_with_error,
  support::load_class_from_package,
  support::{default_class_inheritance, export_and_insert, load_instance_from_module},
  StdResult,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  managed::Trace,
  module::{Module, Package},
  object::{LyNative, Native, NativeMetaBuilder, ObjectKind},
  signature::{Arity, ParameterBuilder, ParameterKind},
  val,
  value::{Value, VALUE_NIL},
  Call, LyError, Ref,
};
use std::io::Write;

const STDOUT_CLASS_NAME: &str = "Stdout";
const STDOUT_INSTANCE_NAME: &str = "stdout";

const STDOUT_WRITE: NativeMetaBuilder = NativeMetaBuilder::method("write", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("string", ParameterKind::String)])
  .with_stack();

const STDOUT_WRITELN: NativeMetaBuilder = NativeMetaBuilder::method("writeln", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("string", ParameterKind::String)])
  .with_stack();

const STDOUT_FLUSH: NativeMetaBuilder =
  NativeMetaBuilder::method("flush", Arity::Fixed(0)).with_stack();

pub fn declare_stdout(hooks: &GcHooks, module: Ref<Module>, package: Ref<Package>) -> StdResult<()> {
  let class = default_class_inheritance(hooks, package, STDOUT_CLASS_NAME)?;
  let instance = hooks.manage_obj(class);

  export_and_insert(
    hooks,
    module,
    hooks.manage_str(STDOUT_INSTANCE_NAME),
    val!(instance),
  )
}

pub fn define_stdout(hooks: &GcHooks, module: Ref<Module>, package: Ref<Package>) -> StdResult<()> {
  let instance = load_instance_from_module(hooks, module, STDOUT_INSTANCE_NAME)?;
  let mut class = instance.class();
  let io_error = val!(load_class_from_package(
    hooks,
    package,
    IO_MODULE_PATH,
    IO_ERROR
  )?);

  class.add_method(
    hooks.manage_str(STDOUT_WRITE.name),
    val!(StdoutWrite::native(hooks, io_error)),
  );

  class.add_method(
    hooks.manage_str(STDOUT_WRITELN.name),
    val!(StdoutWriteln::native(hooks, io_error)),
  );

  class.add_method(
    hooks.manage_str(STDOUT_FLUSH.name),
    val!(StdoutFlush::native(hooks, io_error)),
  );

  Ok(())
}

native_with_error!(StdoutWrite, STDOUT_WRITE);

impl LyNative for StdoutWrite {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let io = hooks.as_io();
    let mut stdio = io.stdio();
    let stdout = stdio.stdout();

    match stdout.write_all(args[1].to_obj().to_str().as_bytes()) {
      Ok(_) => Call::Ok(VALUE_NIL),
      Err(err) => self.call_error(hooks, err.to_string()),
    }
  }
}

native_with_error!(StdoutWriteln, STDOUT_WRITELN);

impl LyNative for StdoutWriteln {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let io = hooks.as_io();
    let mut stdio = io.stdio();
    let stdout = stdio.stdout();

    match writeln!(stdout, "{}", &*args[1].to_obj().to_str()) {
      Ok(_) => Call::Ok(VALUE_NIL),
      Err(err) => self.call_error(hooks, err.to_string()),
    }
  }
}

native_with_error!(StdoutFlush, STDOUT_FLUSH);

impl LyNative for StdoutFlush {
  fn call(&self, hooks: &mut Hooks, _args: &[Value]) -> Call {
    let io = hooks.as_io();
    let mut stdio = io.stdio();
    let stdout = stdio.stdout();

    match stdout.flush() {
      Ok(_) => Call::Ok(VALUE_NIL),
      Err(err) => self.call_error(hooks, err.to_string()),
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod write {
    use super::*;
    use crate::support::{test_error_class, MockedContext};
    use laythe_env::stdio::support::StdioTestContainer;
    use std::{str, sync::Arc};

    #[test]
    fn call() {
      let stdio_container = Arc::new(StdioTestContainer::default());

      let mut context = MockedContext::with_test_stdio(&stdio_container);
      let mut hooks = Hooks::new(&mut context);
      let error = val!(test_error_class(&hooks.as_gc()));

      let stdout_write = StdoutWrite::native(&hooks.as_gc(), error);

      let string = val!(hooks.manage_str("some string"));
      let result = stdout_write.call(&mut hooks, &[VALUE_NIL, string]);

      assert!(result.is_ok());
      assert!(result.unwrap().is_nil());

      let stdout = str::from_utf8(&stdio_container.stdout);
      assert!(stdout.is_ok());
      assert_eq!(stdout.unwrap(), "some string");
    }
  }

  mod writeln {
    use super::*;
    use crate::support::{test_error_class, MockedContext};
    use laythe_env::stdio::support::StdioTestContainer;
    use std::{str, sync::Arc};

    #[test]
    fn call() {
      let stdio_container = Arc::new(StdioTestContainer::default());

      let mut context = MockedContext::with_test_stdio(&stdio_container);
      let mut hooks = Hooks::new(&mut context);
      let error = val!(test_error_class(&hooks.as_gc()));

      let stdout_write = StdoutWriteln::native(&hooks.as_gc(), error);

      let string = val!(hooks.manage_str("some string"));
      let result = stdout_write.call(&mut hooks, &[VALUE_NIL, string]);

      assert!(result.is_ok());
      assert!(result.unwrap().is_nil());

      let stdout = str::from_utf8(&stdio_container.stdout);
      assert!(stdout.is_ok());
      assert_eq!(stdout.unwrap(), "some string\n");
    }
  }
}
