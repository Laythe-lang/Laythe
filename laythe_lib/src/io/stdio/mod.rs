mod stderr;
mod stdin;
mod stdout;

use crate::{global::MODULE_CLASS_NAME, support::load_class_from_package, StdResult, STD};
use laythe_core::{
  hooks::GcHooks,
  managed::Gc,
  module::{Module, Package},
  object::Class,
  utils::IdEmitter,
};
use stderr::{declare_stderr, define_stderr};
use stdin::{declare_stdin, define_stdin};
use stdout::{declare_stdout, define_stdout};

use super::IO_MODULE_NAME;

const STDIO_MODULE_NAME: &str = "stdio";

pub fn stdio_module(
  hooks: &GcHooks,
  std: Gc<Package>,
  emitter: &mut IdEmitter,
) -> StdResult<Gc<Module>> {
  let module_class = load_class_from_package(hooks, std, STD, MODULE_CLASS_NAME)?;
  let stdio_module_class =
    Class::with_inheritance(hooks, hooks.manage_str(STDIO_MODULE_NAME), module_class);

  let module = hooks.manage(Module::new(
    hooks,
    stdio_module_class,
    &format!("native/{}/{}", IO_MODULE_NAME, STDIO_MODULE_NAME),
    emitter.emit(),
  ));

  declare_stderr(hooks, module, std)?;
  declare_stdin(hooks, module, std)?;
  declare_stdout(hooks, module, std)?;

  define_stderr(hooks, module, std)?;
  define_stdin(hooks, module, std)?;
  define_stdout(hooks, module, std)?;

  Ok(module)
}
