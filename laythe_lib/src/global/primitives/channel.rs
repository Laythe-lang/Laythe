use super::class_inheritance;
use crate::{support::export_and_insert, StdError, StdResult};
use laythe_core::{hooks::GcHooks, module::Module, val, value::Value};

pub const CHANNEL_CLASS_NAME: &str = "Channel";

pub fn declare_channel_class(hooks: &GcHooks, module: &mut Module) -> StdResult<()> {
  let channel_class = class_inheritance(hooks, module, CHANNEL_CLASS_NAME)?;
  export_and_insert(hooks, module, channel_class.name(), val!(channel_class))
    .map_err(StdError::from)
}

pub fn define_channel_class(_hooks: &GcHooks, _module: &Module) -> StdResult<()> {
  Ok(())
}
