mod fs;
mod global;
mod stdio;

use self::global::io_module;
use crate::{StdError, StdResult};
use fs::fs_module;
use laythe_core::{hooks::GcHooks, managed::Gc, module::Package, utils::IdEmitter};
use stdio::stdio_module;
pub const IO_MODULE_PATH: &str = "std/io";

const IO_MODULE_NAME: &str = "io";

pub fn add_io_package(
  hooks: &GcHooks,
  std: Gc<Package>,
  emitter: &mut IdEmitter,
) -> StdResult<()> {
  let mut root = std.root_module();

  let mut io_module = io_module(hooks, std, emitter)?;
  root.insert_module(io_module)?;

  let stdio = stdio_module(hooks, std, emitter)?;
  let fs = fs_module(hooks, std, emitter)?;

  io_module.insert_module(stdio)?;
  io_module.insert_module(fs).map_err(StdError::from)
}
