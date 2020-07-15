use crate::support::{
  default_class_inheritance, export_and_insert, load_instance_from_module, to_dyn_method,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{NativeMeta, NativeMethod},
  object::Instance,
  package::Package,
  signature::Arity,
  value::Value,
  CallResult, LyResult,
  val
};
use laythe_env::{managed::Trace, stdio::Stdio};

const STDIN_CLASS_NAME: &str = "Stdin";
const STDIN_INSTANCE_NAME: &str = "stdin";

const STDIN_READ: NativeMeta = NativeMeta::new("read", Arity::Fixed(0), &[]);
const STDIN_READ_LINE: NativeMeta = NativeMeta::new("readLine", Arity::Fixed(0), &[]);

pub fn declare_stdin(hooks: &GcHooks, module: &mut Module, std: &Package) -> LyResult<()> {
  let class = default_class_inheritance(hooks, std, STDIN_CLASS_NAME)?;
  let instance = hooks.manage(Instance::new(class));

  export_and_insert(
    hooks,
    module,
    hooks.manage_str(STDIN_INSTANCE_NAME.to_string()),
    val!(instance),
  )
}

pub fn define_stdin(hooks: &GcHooks, module: &Module, _: &Package) -> LyResult<()> {
  let instance = load_instance_from_module(hooks, module, STDIN_INSTANCE_NAME)?;
  let mut class = instance.class;

  class.add_method(
    hooks,
    hooks.manage_str(String::from(STDIN_READ.name)),
    val!(to_dyn_method(hooks, StdinRead())),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(STDIN_READ_LINE.name)),
    val!(to_dyn_method(hooks, StdinReadLine())),
  );

  Ok(())
}

#[derive(Clone, Debug, Trace)]
struct StdinRead();

impl NativeMethod for StdinRead {
  fn meta(&self) -> &NativeMeta {
    &STDIN_READ
  }

  fn call(&self, hooks: &mut Hooks, _this: Value, _args: &[Value]) -> CallResult {
    let io = hooks.to_io();
    let mut stdio = io.stdio();
    let stdin = stdio.stdin();

    let mut buf = String::new();
    match stdin.read_to_string(&mut buf) {
      Ok(_) => Ok(val!(hooks.manage_str(buf))),
      Err(err) => Err(hooks.make_error(err.to_string())),
    }
  }
}

#[derive(Clone, Debug, Trace)]
struct StdinReadLine();

impl NativeMethod for StdinReadLine {
  fn meta(&self) -> &NativeMeta {
    &STDIN_READ_LINE
  }

  fn call(&self, hooks: &mut Hooks, _this: Value, _args: &[Value]) -> CallResult {
    let io = hooks.to_io();
    let stdio = io.stdio();

    let mut buf = String::new();

    match stdio.read_line(&mut buf) {
      Ok(_) => {
        if buf.ends_with('\n') {
          buf.pop();
        }
        Ok(val!(hooks.manage_str(buf)))
      }
      Err(err) => Err(hooks.make_error(err.to_string())),
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod read {
    use super::*;
    use crate::support::MockedContext;
    use laythe_core::value::VALUE_NIL;
    use laythe_env::stdio::support::StdioTestContainer;
    use std::rc::Rc;

    #[test]
    fn new() {
      let stdin_read = StdinRead();

      assert_eq!(stdin_read.meta().name, "read");
      assert_eq!(stdin_read.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let stdin_read = StdinRead();
      let stdio_container = Rc::new(StdioTestContainer::with_stdin(&"dude".as_bytes()));

      let mut context = MockedContext::new_with_io(&stdio_container);
      let mut hooks = Hooks::new(&mut context);

      let result = stdin_read.call(&mut hooks, VALUE_NIL, &[]);

      assert!(result.is_ok());
      let unwrapped = result.unwrap();

      assert!(unwrapped.is_str());
      assert_eq!(&**unwrapped.to_str(), "dude");
    }
  }

  mod readlines {
    use super::*;
    use crate::support::MockedContext;
    use laythe_core::value::VALUE_NIL;
    use laythe_env::stdio::support::StdioTestContainer;
    use std::rc::Rc;

    #[test]
    fn new() {
      let stdin_readline = StdinReadLine();

      assert_eq!(stdin_readline.meta().name, "readLine");
      assert_eq!(stdin_readline.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let stdin_readline = StdinReadLine();
      let stdio_container = Rc::new(StdioTestContainer::with_lines(vec![
        "dude".to_string(),
        "sup".to_string(),
      ]));

      let mut context = MockedContext::new_with_io(&stdio_container);
      let mut hooks = Hooks::new(&mut context);

      let result = stdin_readline.call(&mut hooks, VALUE_NIL, &[]);

      assert!(result.is_ok());
      let unwrapped = result.unwrap();

      assert!(unwrapped.is_str());
      assert_eq!(&**unwrapped.to_str(), "dude");
    }
  }
}
