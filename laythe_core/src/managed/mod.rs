mod allocate;
mod allocation;
mod header;
mod manage;

pub use allocate::{AllocObjResult, AllocResult, Allocate, AllocateObj};
pub use allocation::Allocation;
pub use header::Header;
pub use manage::{
  DebugHeap, DebugWrap, DebugWrapDyn, Manage, Mark, Marked, Trace, TraceAny, TraceRoot, Unmark,
};
