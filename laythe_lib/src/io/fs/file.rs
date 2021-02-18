use crate::{
  io::{global::IO_ERROR, IO_MODULE_PATH},
  native_with_error,
  support::load_class_from_package,
  support::{default_class_inheritance, export_and_insert, load_class_from_module},
  StdResult,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  managed::GcObj,
  managed::Trace,
  module::{Module, Package},
  object::{LyNative, Native, NativeMetaBuilder, ObjectKind},
  signature::{Arity, ParameterBuilder, ParameterKind},
  val,
  value::Value,
  Call,
};
use std::io::Write;
use std::path::Path;

const FILE_CLASS_NAME: &str = "File";

const FILE_READ_ALL_TEXT: NativeMetaBuilder =
  NativeMetaBuilder::fun("readAllText", Arity::Fixed(1))
    .with_params(&[ParameterBuilder::new("path", ParameterKind::String)]);

pub fn declare_file(hooks: &GcHooks, module: &mut Module, std: &Package) -> StdResult<()> {
  let class = default_class_inheritance(hooks, std, FILE_CLASS_NAME)?;
  export_and_insert(hooks, module, class.name(), val!(class))
}

pub fn define_file(hooks: &GcHooks, module: &Module, std: &Package) -> StdResult<()> {
  let class = load_class_from_module(hooks, module, FILE_CLASS_NAME)?;
  let io_error = val!(load_class_from_package(
    hooks,
    std,
    IO_MODULE_PATH,
    IO_ERROR
  )?);

  class.meta_class().expect("Meta class not set.").add_method(
    hooks,
    hooks.manage_str(FILE_READ_ALL_TEXT.name),
    val!(FileReadAllText::native(hooks, io_error)),
  );

  Ok(())
}

native_with_error!(FileReadAllText, FILE_READ_ALL_TEXT);

impl LyNative for FileReadAllText {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> Call {
    let io = hooks.as_io();
    let path = args[0].to_obj().to_str();

    match io.fs().read_to_string(&Path::new(&*path)) {
      Ok(result) => Call::Ok(val!(hooks.manage_str(result))),
      Err(err) => self.call_error(hooks, err.to_string()),
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod read_all_text {
    use super::*;
    use crate::support::{test_error_class, MockedContext};

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);
      let error = val!(test_error_class(&hooks));

      let stdout_write = FileReadAllText::native(&hooks, error);

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
