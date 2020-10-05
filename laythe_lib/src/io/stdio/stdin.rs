use crate::{
  native,
  support::{
    default_class_inheritance, export_and_insert, load_instance_from_module, to_dyn_native,
  },
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{MetaData, Native, NativeMeta, NativeMetaBuilder},
  object::Instance,
  package::Package,
  signature::Arity,
  val,
  value::Value,
  CallResult, LyResult,
};
use laythe_env::managed::Trace;
use std::io::Write;

const STDIN_CLASS_NAME: &str = "Stdin";
const STDIN_INSTANCE_NAME: &str = "stdin";

const STDIN_READ: NativeMetaBuilder = NativeMetaBuilder::method("read", Arity::Fixed(0));
const STDIN_READ_LINE: NativeMetaBuilder = NativeMetaBuilder::method("readLine", Arity::Fixed(0));

pub fn declare_stdin(hooks: &GcHooks, module: &mut Module, std: &Package) -> LyResult<()> {
  let class = default_class_inheritance(hooks, std, STDIN_CLASS_NAME)?;
  let instance = hooks.manage(Instance::new(class));

  export_and_insert(
    hooks,
    module,
    hooks.manage_str(STDIN_INSTANCE_NAME),
    val!(instance),
  )
}

pub fn define_stdin(hooks: &GcHooks, module: &Module, _: &Package) -> LyResult<()> {
  let instance = load_instance_from_module(hooks, module, STDIN_INSTANCE_NAME)?;
  let mut class = instance.class;

  class.add_method(
    hooks,
    hooks.manage_str(STDIN_READ.name),
    val!(to_dyn_native(hooks, StdinRead::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(STDIN_READ_LINE.name),
    val!(to_dyn_native(hooks, StdinReadLine::from(hooks))),
  );

  Ok(())
}

native!(StdinRead, STDIN_READ);

impl Native for StdinRead {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, _args: &[Value]) -> CallResult {
    let io = hooks.as_io();
    let mut stdio = io.stdio();
    let stdin = stdio.stdin();

    let mut buf = String::new();
    match stdin.read_to_string(&mut buf) {
      Ok(_) => Ok(val!(hooks.manage_str(buf))),
      Err(err) => hooks.error(err.to_string()),
    }
  }
}

native!(StdinReadLine, STDIN_READ_LINE);

impl Native for StdinReadLine {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, _args: &[Value]) -> CallResult {
    let io = hooks.as_io();
    let stdio = io.stdio();

    let mut buf = String::new();

    match stdio.read_line(&mut buf) {
      Ok(_) => {
        if buf.ends_with('\n') {
          buf.pop();
        }
        Ok(val!(hooks.manage_str(buf)))
      }
      Err(err) => hooks.error(err.to_string()),
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
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let stdin_read = StdinRead::from(&hooks);

      assert_eq!(stdin_read.meta().name, "read");
      assert_eq!(stdin_read.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let stdio_container = Rc::new(StdioTestContainer::with_stdin(&"dude".as_bytes()));

      let mut context = MockedContext::new_with_io(&stdio_container);
      let mut hooks = Hooks::new(&mut context);
      let stdin_read = StdinRead::from(&hooks);

      let result = stdin_read.call(&mut hooks, Some(VALUE_NIL), &[]);

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
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let stdin_readline = StdinReadLine::from(&hooks);

      assert_eq!(stdin_readline.meta().name, "readLine");
      assert_eq!(stdin_readline.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let stdio_container = Rc::new(StdioTestContainer::with_lines(vec![
        "dude".to_string(),
        "sup".to_string(),
      ]));

      let mut context = MockedContext::new_with_io(&stdio_container);
      let mut hooks = Hooks::new(&mut context);
      let stdin_readline = StdinReadLine::from(&hooks);

      let result = stdin_readline.call(&mut hooks, Some(VALUE_NIL), &[]);

      assert!(result.is_ok());
      let unwrapped = result.unwrap();

      assert!(unwrapped.is_str());
      assert_eq!(&**unwrapped.to_str(), "dude");
    }
  }
}
