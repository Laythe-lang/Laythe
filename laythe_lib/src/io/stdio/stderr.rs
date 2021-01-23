use crate::{
  io::global::{ERROR_PATH, IO_ERROR},
  native_with_error,
  support::load_class_from_package,
  support::{
    default_class_inheritance, export_and_insert, load_instance_from_module, to_dyn_native,
  },
  InitResult,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{MetaData, Native, NativeMeta, NativeMetaBuilder},
  object::Instance,
  package::Package,
  signature::{Arity, ParameterBuilder, ParameterKind},
  val,
  value::{Value, VALUE_NIL},
  Call,
};
use laythe_env::managed::Trace;
use std::io::Write;

const STDERR_CLASS_NAME: &str = "Stderr";
const STDERR_INSTANCE_NAME: &str = "stderr";

const STDERR_WRITE: NativeMetaBuilder = NativeMetaBuilder::method("write", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("string", ParameterKind::String)]);

const STDERR_WRITELN: NativeMetaBuilder = NativeMetaBuilder::method("writeln", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("string", ParameterKind::String)]);

const STDERR_FLUSH: NativeMetaBuilder = NativeMetaBuilder::method("flush", Arity::Fixed(0));

pub fn declare_stderr(hooks: &GcHooks, module: &mut Module, std: &Package) -> InitResult<()> {
  let class = default_class_inheritance(hooks, std, STDERR_CLASS_NAME)?;
  let instance = hooks.manage(Instance::new(class));

  export_and_insert(
    hooks,
    module,
    hooks.manage_str(STDERR_INSTANCE_NAME),
    val!(instance),
  )
}

pub fn define_stderr(hooks: &GcHooks, module: &Module, package: &Package) -> InitResult<()> {
  let instance = load_instance_from_module(hooks, module, STDERR_INSTANCE_NAME)?;
  let mut class = instance.class();
  let io_error = val!(load_class_from_package(
    hooks, package, ERROR_PATH, IO_ERROR
  )?);

  class.add_method(
    hooks,
    hooks.manage_str(STDERR_WRITE.name),
    val!(to_dyn_native(hooks, StderrWrite::new(hooks, io_error))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(STDERR_WRITELN.name),
    val!(to_dyn_native(hooks, StderrWriteln::new(hooks, io_error))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(STDERR_FLUSH.name),
    val!(to_dyn_native(hooks, StderrFlush::new(hooks, io_error))),
  );

  Ok(())
}

native_with_error!(StderrWrite, STDERR_WRITE);

impl Native for StderrWrite {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> Call {
    let io = hooks.as_io();
    let mut stdio = io.stdio();
    let stderr = stdio.stderr();

    match stderr.write(args[0].to_str().as_bytes()) {
      Ok(_) => Call::Ok(VALUE_NIL),
      Err(err) => self.call_error(hooks, err.to_string()),
    }
  }
}

native_with_error!(StderrWriteln, STDERR_WRITELN);

impl Native for StderrWriteln {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> Call {
    let io = hooks.as_io();
    let mut stdio = io.stdio();
    let stderr = stdio.stderr();

    match writeln!(stderr, "{}", args[0].to_str()) {
      Ok(_) => Call::Ok(VALUE_NIL),
      Err(err) => self.call_error(hooks, err.to_string()),
    }
  }
}

native_with_error!(StderrFlush, STDERR_FLUSH);

impl Native for StderrFlush {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, _args: &[Value]) -> Call {
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
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);
      let error = val!(test_error_class(&hooks));

      let stderr_write = StderrWrite::new(&hooks, error);

      assert_eq!(stderr_write.meta().name, "write");
      assert_eq!(stderr_write.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        stderr_write.meta().signature.parameters[0].kind,
        ParameterKind::String
      );
    }

    #[test]
    fn call() {
      let stdio_container = Arc::new(StdioTestContainer::default());

      let mut context = MockedContext::with_test_stdio(&stdio_container);
      let mut hooks = Hooks::new(&mut context);
      let error = val!(test_error_class(&hooks.as_gc()));

      let stderr_write = StderrWrite::new(&hooks.as_gc(), error);

      let string = val!(hooks.manage_str("some string".to_string()));
      let result = stderr_write.call(&mut hooks, Some(VALUE_NIL), &[string]);

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
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);
      let error = val!(test_error_class(&hooks));

      let stderr_writeln = StderrWriteln::new(&hooks, error);

      assert_eq!(stderr_writeln.meta().name, "writeln");
      assert_eq!(stderr_writeln.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        stderr_writeln.meta().signature.parameters[0].kind,
        ParameterKind::String
      );
    }

    #[test]
    fn call() {
      let stdio_container = Arc::new(StdioTestContainer::default());

      let mut context = MockedContext::with_test_stdio(&stdio_container);
      let mut hooks = Hooks::new(&mut context);
      let error = val!(test_error_class(&hooks.as_gc()));

      let stderr_write = StderrWriteln::new(&hooks.as_gc(), error);

      let string = val!(hooks.manage_str("some string".to_string()));
      let result = stderr_write.call(&mut hooks, Some(VALUE_NIL), &[string]);

      assert!(result.is_ok());
      assert!(result.unwrap().is_nil());

      let stderr = str::from_utf8(&*stdio_container.stderr);
      assert!(stderr.is_ok());
      assert_eq!(stderr.unwrap(), "some string\n");
    }
  }
}
