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
  managed::{Gc, GcObj},
  module::{Module, Package},
  object::{LyNative, Native, NativeMetaBuilder, ObjectKind},
  signature::{Arity, ParameterBuilder, ParameterKind},
  val,
  value::{Value, VALUE_NIL},
  Call, LyError,
};
use std::io::Write;

const STDERR_CLASS_NAME: &str = "Stderr";
const STDERR_INSTANCE_NAME: &str = "stderr";

const STDERR_WRITE: NativeMetaBuilder = NativeMetaBuilder::method("write", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("string", ParameterKind::String)])
  .with_stack();

const STDERR_WRITELN: NativeMetaBuilder = NativeMetaBuilder::method("writeln", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("string", ParameterKind::String)])
  .with_stack();

const STDERR_FLUSH: NativeMetaBuilder =
  NativeMetaBuilder::method("flush", Arity::Fixed(0)).with_stack();

pub fn declare_stderr(hooks: &GcHooks, module: Gc<Module>, package: Gc<Package>) -> StdResult<()> {
  let class = default_class_inheritance(hooks, package, STDERR_CLASS_NAME)?;
  let instance = hooks.manage_obj(class);

  export_and_insert(
    module,
    hooks.manage_str(STDERR_INSTANCE_NAME),
    val!(instance),
  )
}

pub fn define_stderr(hooks: &GcHooks, module: Gc<Module>, package: Gc<Package>) -> StdResult<()> {
  let instance = load_instance_from_module(hooks, module, STDERR_INSTANCE_NAME)?;
  let mut class = instance.class();
  let io_error = val!(load_class_from_package(
    hooks,
    package,
    IO_MODULE_PATH,
    IO_ERROR
  )?);

  class.add_method(
    hooks.manage_str(STDERR_WRITE.name),
    val!(StderrWrite::native(hooks, io_error)),
  );

  class.add_method(
    hooks.manage_str(STDERR_WRITELN.name),
    val!(StderrWriteln::native(hooks, io_error)),
  );

  class.add_method(
    hooks.manage_str(STDERR_FLUSH.name),
    val!(StderrFlush::native(hooks, io_error)),
  );

  Ok(())
}

native_with_error!(StderrWrite, STDERR_WRITE);

impl LyNative for StderrWrite {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let io = hooks.as_io();
    let mut stdio = io.stdio();
    let stderr = stdio.stderr();

    match stderr.write_all(args[1].to_obj().to_str().as_bytes()) {
      Ok(_) => Call::Ok(VALUE_NIL),
      Err(err) => self.call_error(hooks, err.to_string()),
    }
  }
}

native_with_error!(StderrWriteln, STDERR_WRITELN);

impl LyNative for StderrWriteln {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let io = hooks.as_io();
    let mut stdio = io.stdio();
    let stderr = stdio.stderr();

    match writeln!(stderr, "{}", &*args[1].to_obj().to_str()) {
      Ok(_) => Call::Ok(VALUE_NIL),
      Err(err) => self.call_error(hooks, err.to_string()),
    }
  }
}

native_with_error!(StderrFlush, STDERR_FLUSH);

impl LyNative for StderrFlush {
  fn call(&self, hooks: &mut Hooks, _args: &[Value]) -> Call {
    let io = hooks.as_io();
    let mut stdio = io.stdio();
    let stderr = stdio.stderr();

    match stderr.flush() {
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

      let stderr_write = StderrWrite::native(&hooks.as_gc(), error);

      let string = val!(hooks.manage_str("some string".to_string()));
      let result = stderr_write.call(&mut hooks, &[VALUE_NIL, string]);

      assert!(result.is_ok());
      assert!(result.unwrap().is_nil());

      let stderr = str::from_utf8(&*stdio_container.stderr);
      assert!(stderr.is_ok());
      assert_eq!(stderr.unwrap(), "some string");
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

      let stderr_write = StderrWriteln::native(&hooks.as_gc(), error);

      let string = val!(hooks.manage_str("some string"));
      let result = stderr_write.call(&mut hooks, &[VALUE_NIL, string]);

      assert!(result.is_ok());
      assert!(result.unwrap().is_nil());

      let stderr = str::from_utf8(&*stdio_container.stderr);
      assert!(stderr.is_ok());
      assert_eq!(stderr.unwrap(), "some string\n");
    }
  }
}
