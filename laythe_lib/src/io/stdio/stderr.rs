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

const STDERR_CLASS_NAME: &str = "Stderr";
const STDERR_INSTANCE_NAME: &str = "stderr";

const STDERR_WRITE: NativeMetaBuilder = NativeMetaBuilder::method("write", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("string", ParameterKind::String)]);

const STDERR_WRITELN: NativeMetaBuilder = NativeMetaBuilder::method("writeln", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("string", ParameterKind::String)]);

const STDERR_FLUSH: NativeMetaBuilder = NativeMetaBuilder::method("flush", Arity::Fixed(0));

pub fn declare_stderr(hooks: &GcHooks, module: &mut Module, std: &Package) -> LyResult<()> {
  let class = default_class_inheritance(hooks, std, STDERR_CLASS_NAME)?;
  let instance = hooks.manage(Instance::new(class));

  export_and_insert(
    hooks,
    module,
    hooks.manage_str(STDERR_INSTANCE_NAME),
    val!(instance),
  )
}

pub fn define_stderr(hooks: &GcHooks, module: &Module, _: &Package) -> LyResult<()> {
  let instance = load_instance_from_module(hooks, module, STDERR_INSTANCE_NAME)?;
  let mut class = instance.class;

  class.add_method(
    hooks,
    hooks.manage_str(STDERR_WRITE.name),
    val!(to_dyn_native(hooks, StderrWrite::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(STDERR_WRITELN.name),
    val!(to_dyn_native(hooks, StderrWriteln::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(STDERR_FLUSH.name),
    val!(to_dyn_native(hooks, StderrFlush::from(hooks))),
  );

  Ok(())
}

native!(StderrWrite, STDERR_WRITE);

impl Native for StderrWrite {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> CallResult {
    let io = hooks.as_io();
    let mut stdio = io.stdio();
    let stderr = stdio.stderr();

    match stderr.write(args[0].to_str().as_bytes()) {
      Ok(_) => Ok(VALUE_NIL),
      Err(err) => hooks.error(err.to_string()),
    }
  }
}

native!(StderrWriteln, STDERR_WRITELN);

impl Native for StderrWriteln {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> CallResult {
    let io = hooks.as_io();
    let mut stdio = io.stdio();
    let stderr = stdio.stderr();

    match writeln!(stderr, "{}", args[0].to_str()) {
      Ok(_) => Ok(VALUE_NIL),
      Err(err) => hooks.error(err.to_string()),
    }
  }
}

native!(StderrFlush, STDERR_FLUSH);

impl Native for StderrFlush {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, _args: &[Value]) -> CallResult {
    let io = hooks.as_io();
    let mut stdio = io.stdio();
    let stderr = stdio.stderr();

    match stderr.flush() {
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

      let stderr_write = StderrWrite::from(&hooks);

      assert_eq!(stderr_write.meta().name, "write");
      assert_eq!(stderr_write.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        stderr_write.meta().signature.parameters[0].kind,
        ParameterKind::String
      );
    }

    #[test]
    fn call() {
      let stdio_container = Rc::new(StdioTestContainer::default());

      let mut context = MockedContext::new_with_io(&stdio_container);
      let mut hooks = Hooks::new(&mut context);
      let stderr_write = StderrWrite::from(&hooks);

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
    use crate::support::MockedContext;
    use laythe_env::stdio::support::StdioTestContainer;
    use std::{rc::Rc, str};

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = Hooks::new(&mut context);

      let stderr_writeln = StderrWriteln::from(&hooks);

      assert_eq!(stderr_writeln.meta().name, "writeln");
      assert_eq!(stderr_writeln.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        stderr_writeln.meta().signature.parameters[0].kind,
        ParameterKind::String
      );
    }

    #[test]
    fn call() {
      let stdio_container = Rc::new(StdioTestContainer::default());

      let mut context = MockedContext::new_with_io(&stdio_container);
      let mut hooks = Hooks::new(&mut context);
      let stderr_write = StderrWriteln::from(&hooks);

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
