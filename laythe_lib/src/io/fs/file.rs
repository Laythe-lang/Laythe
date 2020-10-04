use crate::{
  native,
  support::{default_class_inheritance, export_and_insert, load_class_from_module, to_dyn_native},
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{MetaData, Native, NativeMeta, NativeMetaBuilder},
  package::Package,
  signature::{Arity, ParameterBuilder, ParameterKind},
  val,
  value::Value,
  CallResult, LyResult,
};
use laythe_env::managed::Trace;
use std::io::Write;
use std::path::Path;

const FILE_CLASS_NAME: &str = "File";

const FILE_READ_ALL_TEXT: NativeMetaBuilder =
  NativeMetaBuilder::fun("readAllText", Arity::Fixed(1))
    .with_params(&[ParameterBuilder::new("path", ParameterKind::String)]);

pub fn declare_file(hooks: &GcHooks, module: &mut Module, std: &Package) -> LyResult<()> {
  let class = default_class_inheritance(hooks, std, FILE_CLASS_NAME)?;
  export_and_insert(hooks, module, class.name, val!(class))
}

pub fn define_file(hooks: &GcHooks, module: &Module, _: &Package) -> LyResult<()> {
  let mut class = load_class_from_module(hooks, module, FILE_CLASS_NAME)?;

  class.meta().expect("Meta class not set.").add_method(
    hooks,
    hooks.manage_str(FILE_READ_ALL_TEXT.name),
    val!(to_dyn_native(hooks, FileReadAllText::from(hooks))),
  );

  Ok(())
}

native!(FileReadAllText, FILE_READ_ALL_TEXT);

impl Native for FileReadAllText {
  fn call(&self, hook: &mut Hooks, _this: Option<Value>, args: &[Value]) -> CallResult {
    let io = hook.to_io();
    let managed = args[0].to_str();
    let path = managed.as_str();

    match io.fs().read_to_string(&Path::new(path)) {
      Ok(result) => Ok(val!(hook.manage_str(result))),
      Err(err) => hook.error(err.to_string()),
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod read_all_text {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = Hooks::new(&mut context);

      let stdout_write = FileReadAllText::from(&hooks);

      assert_eq!(stdout_write.meta().name, "readAllText");
      assert_eq!(stdout_write.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        stdout_write.meta().signature.parameters[0].kind,
        ParameterKind::String
      );
    }

    // TODO call
  }
}
