use crate::{
  io::{global::IO_ERROR, IO_MODULE_PATH},
  native_with_error,
  support::{export_and_insert, load_class_from_package},
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
use std::path::Path;

const READ_FILE: NativeMetaBuilder = NativeMetaBuilder::fun("readFile", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("path", ParameterKind::String)])
  .with_stack();

const WRITE_FILE: NativeMetaBuilder = NativeMetaBuilder::fun("writeFile", Arity::Fixed(2))
  .with_params(&[
    ParameterBuilder::new("path", ParameterKind::String),
    ParameterBuilder::new("contents", ParameterKind::String),
  ])
  .with_stack();

const REMOVE_FILE: NativeMetaBuilder = NativeMetaBuilder::fun("removeFile", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("path", ParameterKind::String)])
  .with_stack();

pub fn declare_fs_module(
  _hooks: &GcHooks,
  _module: Gc<Module>,
  _std: Gc<Package>,
) -> StdResult<()> {
  Ok(())
}

pub fn define_fs_module(hooks: &GcHooks, module: Gc<Module>, std: Gc<Package>) -> StdResult<()> {
  let io_error = val!(load_class_from_package(
    hooks,
    std,
    IO_MODULE_PATH,
    IO_ERROR
  )?);

  export_and_insert(
    hooks,
    module,
    READ_FILE.name,
    val!(ReadFile::native(hooks, io_error)),
  )?;

  export_and_insert(
    hooks,
    module,
    WRITE_FILE.name,
    val!(WriteFile::native(hooks, io_error)),
  )?;

  export_and_insert(
    hooks,
    module,
    REMOVE_FILE.name,
    val!(RemoveFile::native(hooks, io_error)),
  )?;

  Ok(())
}

native_with_error!(ReadFile, READ_FILE);

impl LyNative for ReadFile {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let io = hooks.as_io();
    let path = args[0].to_obj().to_str();

    match io.fs().read_file(Path::new(&*path)) {
      Ok(result) => Call::Ok(val!(hooks.manage_str(result))),
      Err(err) => self.call_error(hooks, err.to_string()),
    }
  }
}

native_with_error!(WriteFile, WRITE_FILE);

impl LyNative for WriteFile {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let io = hooks.as_io();
    let path = args[0].to_obj().to_str();
    let contents = args[1].to_obj().to_str();

    match io.fs().write_file(Path::new(&*path), &contents) {
      Ok(_) => Call::Ok(VALUE_NIL),
      Err(err) => self.call_error(hooks, err.to_string()),
    }
  }
}

native_with_error!(RemoveFile, REMOVE_FILE);

impl LyNative for RemoveFile {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let io = hooks.as_io();
    let path = args[0].to_obj().to_str();

    match io.fs().remove_file(Path::new(&*path)) {
      Ok(_) => Call::Ok(VALUE_NIL),
      Err(err) => self.call_error(hooks, err.to_string()),
    }
  }
}

#[cfg(test)]
mod test {
  mod read_file {
    // TODO: call
  }

  mod write_file {
    // TODO: call
  }

  mod remove_file {
    // TODO: call
  }
}
