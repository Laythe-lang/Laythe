mod allocation;
mod gc;
mod gc_array;
mod gc_str;
mod manage;

pub use allocation::Allocation;
pub use gc::Gc;
pub use gc_array::{GcArray, GcArrayHandle};
pub use gc_str::{GcStr, GcStrHandle};
pub use manage::{
  DebugHeap, DebugWrap, DebugWrapDyn, Manage, Mark, Marked, Trace, TraceRoot, Unmark,
};
