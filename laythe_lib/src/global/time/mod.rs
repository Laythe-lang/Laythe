mod clock;

use clock::declare_clock_funs;
use laythe_core::{hooks::GcHooks, module::Module};

use crate::StdResult;

pub(crate) fn add_clock_funs(hooks: &GcHooks, module: &mut Module) -> StdResult<()> {
  declare_clock_funs(hooks, module)
}
