mod stderr;
mod stdin;
mod stdout;

use laythe_core::{hooks::GcHooks, module::Module, package::Package, LyResult};
use laythe_env::managed::Managed;
use std::path::PathBuf;
use stderr::{declare_stderr, define_stderr};
use stdin::{declare_stdin, define_stdin};
use stdout::{declare_stdout, define_stdout};

const STDIO_PATH: &str = "std/io/stdio.ly";

pub fn stdio_module(hooks: &GcHooks, std: Managed<Package>) -> LyResult<Managed<Module>> {
  let mut module = hooks.manage(Module::from_path(
    &hooks,
    hooks.manage(PathBuf::from(STDIO_PATH)),
  )?);

  declare_stderr(hooks, &mut module, &*std)?;
  declare_stdin(hooks, &mut module, &*std)?;
  declare_stdout(hooks, &mut module, &*std)?;

  define_stderr(hooks, &module, &*std)?;
  define_stdin(hooks, &module, &*std)?;
  define_stdout(hooks, &module, &*std)?;

  Ok(module)
}
