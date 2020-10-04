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
  signature::{Arity, ParameterBuilder, ParameterKind},
  val,
  value::{Value, VALUE_NIL},
  CallResult, LyResult,
};
use laythe_env::managed::Trace;
use std::io::Write;

const STDOUT_CLASS_NAME: &str = "Stdout";
const STDOUT_INSTANCE_NAME: &str = "stdout";

const STDOUT_WRITE: NativeMetaBuilder = NativeMetaBuilder::method("write", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("string", ParameterKind::String)]);

const STDOUT_WRITELN: NativeMetaBuilder = NativeMetaBuilder::method("writeln", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("string", ParameterKind::String)]);

const STDOUT_FLUSH: NativeMetaBuilder = NativeMetaBuilder::method("flush", Arity::Fixed(0));

pub fn declare_stdout(hooks: &GcHooks, module: &mut Module, std: &Package) -> LyResult<()> {
  let class = default_class_inheritance(hooks, std, STDOUT_CLASS_NAME)?;
  let instance = hooks.manage(Instance::new(class));

  export_and_insert(
    hooks,
    module,
    hooks.manage_str(STDOUT_INSTANCE_NAME),
    val!(instance),
  )
}

pub fn define_stdout(hooks: &GcHooks, module: &Module, _: &Package) -> LyResult<()> {
  let instance = load_instance_from_module(hooks, module, STDOUT_INSTANCE_NAME)?;
  let mut class = instance.class;

  class.add_method(
    hooks,
    hooks.manage_str(STDOUT_WRITE.name),
    val!(to_dyn_native(hooks, StdoutWrite::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(STDOUT_WRITELN.name),
    val!(to_dyn_native(hooks, StdoutWriteln::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(STDOUT_FLUSH.name),
    val!(to_dyn_native(hooks, StdoutFlush::from(hooks))),
  );

  Ok(())
}

native!(StdoutWrite, STDOUT_WRITE);

impl Native for StdoutWrite {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> CallResult {
    let io = hooks.as_io();
    let mut stdio = io.stdio();
    let stdout = stdio.stdout();

    match stdout.write(args[0].to_str().as_bytes()) {
      Ok(_) => Ok(VALUE_NIL),
      Err(err) => hooks.error(err.to_string()),
    }
  }
}

native!(StdoutWriteln, STDOUT_WRITELN);

impl Native for StdoutWriteln {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> CallResult {
    let io = hooks.as_io();
    let mut stdio = io.stdio();
    let stdout = stdio.stdout();

    match writeln!(stdout, "{}", args[0].to_str()) {
      Ok(_) => Ok(VALUE_NIL),
      Err(err) => hooks.error(err.to_string()),
    }
  }
}

native!(StdoutFlush, STDOUT_FLUSH);

impl Native for StdoutFlush {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, _args: &[Value]) -> CallResult {
    let io = hooks.as_io();
    let mut stdio = io.stdio();
    let stdout = stdio.stdout();

    match stdout.flush() {
      Ok(_) => Ok(VALUE_NIL),
      Err(err) => hooks.error(err.to_string()),
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
      let mut context = MockedContext::default();
      let hooks = Hooks::new(&mut context);

      let stdout_write = StdoutWrite::from(&hooks);

      assert_eq!(stdout_write.meta().name, "write");
      assert_eq!(stdout_write.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        stdout_write.meta().signature.parameters[0].kind,
        ParameterKind::String
      );
    }

    #[test]
    fn call() {
      let stdio_container = Rc::new(StdioTestContainer::default());

      let mut context = MockedContext::new_with_io(&stdio_container);
      let mut hooks = Hooks::new(&mut context);
      let stdout_write = StdoutWrite::from(&hooks);

      let string = val!(hooks.manage_str("some string".to_string()));
      let result = stdout_write.call(&mut hooks, Some(VALUE_NIL), &[string]);

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
      let mut context = MockedContext::default();
      let hooks = Hooks::new(&mut context);

      let stdout_writeln = StdoutWriteln::from(&hooks);

      assert_eq!(stdout_writeln.meta().name, "writeln");
      assert_eq!(stdout_writeln.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        stdout_writeln.meta().signature.parameters[0].kind,
        ParameterKind::String
      );
    }

    #[test]
    fn call() {
      let stdio_container = Rc::new(StdioTestContainer::default());

      let mut context = MockedContext::new_with_io(&stdio_container);
      let mut hooks = Hooks::new(&mut context);
      let stdout_write = StdoutWriteln::from(&hooks);

      let string = val!(hooks.manage_str("some string".to_string()));
      let result = stdout_write.call(&mut hooks, Some(VALUE_NIL), &[string]);

      assert!(result.is_ok());
      assert!(result.unwrap().is_nil());

      let stdout = str::from_utf8(&*stdio_container.stdout);
      assert!(stdout.is_ok());
      assert_eq!(stdout.unwrap(), "some string\n");
    }
  }
}
