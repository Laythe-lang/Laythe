mod clock;

use clock::declare_clock_funs;
use laythe_core::{hooks::GcHooks, module::Module, package::Package};

use crate::InitResult;

pub(crate) fn add_clock_funs(
  hooks: &GcHooks,
  module: &mut Module,
  _package: &Package,
) -> InitResult<()> {
  declare_clock_funs(hooks, module)
}
