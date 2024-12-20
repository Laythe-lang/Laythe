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
  signature::Arity,
  val,
  value::{Value, VALUE_NIL},
  Call, LyError, Ref,
};
use std::io::Write;

const STDIN_CLASS_NAME: &str = "Stdin";
const STDIN_INSTANCE_NAME: &str = "stdin";

const STDIN_READ: NativeMetaBuilder =
  NativeMetaBuilder::method("read", Arity::Fixed(0)).with_stack();
const STDIN_READ_LINE: NativeMetaBuilder =
  NativeMetaBuilder::method("readLine", Arity::Fixed(0)).with_stack();

pub fn declare_stdin(hooks: &GcHooks, module: Ref<Module>, package: Ref<Package>) -> StdResult<()> {
  let class = default_class_inheritance(hooks, package, STDIN_CLASS_NAME)?;
  let instance = hooks.manage_obj(class);

  export_and_insert(
    hooks,
    module,
    hooks.manage_str(STDIN_INSTANCE_NAME),
    val!(instance),
  )
}

pub fn define_stdin(hooks: &GcHooks, module: Ref<Module>, package: Ref<Package>) -> StdResult<()> {
  let instance = load_instance_from_module(hooks, module, STDIN_INSTANCE_NAME)?;
  let mut class = instance.class();
  let io_error = val!(load_class_from_package(
    hooks,
    package,
    IO_MODULE_PATH,
    IO_ERROR
  )?);

  class.add_method(
    hooks.manage_str(STDIN_READ.name),
    val!(StdinRead::native(hooks, io_error)),
  );

  class.add_method(
    hooks.manage_str(STDIN_READ_LINE.name),
    val!(StdinReadLine::native(hooks, io_error)),
  );

  Ok(())
}

native_with_error!(StdinRead, STDIN_READ);

impl LyNative for StdinRead {
  fn call(&self, hooks: &mut Hooks, _args: &[Value]) -> Call {
    let io = hooks.as_io();
    let mut stdio = io.stdio();
    let stdin = stdio.stdin();

    let mut buf = String::new();
    match stdin.read_to_string(&mut buf) {
      Ok(_) => Call::Ok(val!(hooks.manage_str(buf))),
      Err(err) => self.call_error(hooks, err.to_string()),
    }
  }
}

native_with_error!(StdinReadLine, STDIN_READ_LINE);

impl LyNative for StdinReadLine {
  fn call(&self, hooks: &mut Hooks, _args: &[Value]) -> Call {
    let io = hooks.as_io();
    let stdio = io.stdio();

    let mut buf = String::new();

    match stdio.read_line(&mut buf) {
      Ok(0) => Call::Ok(VALUE_NIL),
      Ok(_) => {
        if buf.ends_with('\n') {
          buf.pop();
        }
        Call::Ok(val!(hooks.manage_str(buf)))
      },
      Err(err) => self.call_error(hooks, err.to_string()),
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod read {
    use super::*;
    use crate::support::{test_error_class, MockedContext};
    use laythe_env::stdio::support::StdioTestContainer;
    use std::sync::Arc;

    #[test]
    fn call() {
      let stdio_container = Arc::new(StdioTestContainer::with_stdin("dude".as_bytes()));

      let mut context = MockedContext::with_test_stdio(&stdio_container);
      let mut hooks = Hooks::new(&mut context);
      let error = val!(test_error_class(&hooks.as_gc()));

      let stdin_read = StdinRead::native(&hooks.as_gc(), error);

      let result = stdin_read.call(&mut hooks, &[]);

      assert!(result.is_ok());
      let unwrapped = result.unwrap();

      assert!(unwrapped.is_obj_kind(ObjectKind::String));
      assert_eq!(unwrapped.to_obj().to_str(), "dude");
    }
  }

  mod readlines {
    use super::*;
    use crate::support::{test_error_class, MockedContext};
    use laythe_env::stdio::support::StdioTestContainer;
    use std::sync::Arc;

    #[test]
    fn call() {
      let stdio_container = Arc::new(StdioTestContainer::with_lines(vec![
        "dude".to_string(),
        "sup".to_string(),
      ]));

      let mut context = MockedContext::with_test_stdio(&stdio_container);
      let mut hooks = Hooks::new(&mut context);
      let error = val!(test_error_class(&hooks.as_gc()));

      let stdin_readline = StdinReadLine::native(&hooks.as_gc(), error);

      let result = stdin_readline.call(&mut hooks, &[]);

      assert!(result.is_ok());
      let unwrapped = result.unwrap();

      assert!(unwrapped.is_obj_kind(ObjectKind::String));
      assert_eq!(unwrapped.to_obj().to_str(), "dude");
    }
  }
}
