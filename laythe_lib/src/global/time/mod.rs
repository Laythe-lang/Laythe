mod clock;

use clock::declare_clock_funs;
use laythe_core::{hooks::GcHooks, module::Module, Ref};

use crate::StdResult;

pub(crate) fn add_clock_funs(hooks: &GcHooks, module: Ref<Module>) -> StdResult<()> {
  declare_clock_funs(hooks, module)
}
