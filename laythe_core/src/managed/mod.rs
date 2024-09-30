mod allocation;
mod gc;
mod gc_array;
mod gc_obj;
mod gc_str;
mod header;
mod manage;
mod utils;
mod allocate;

pub use allocation::Allocation;
pub use gc::Gc;
pub use gc_array::{
  Array, GcArray, GcArrayHandle, Instance, Tuple
};
pub use gc_obj::{GcObj, GcObject, GcObjectHandle, GcObjectHandleBuilder, Object};
pub use gc_str::{GcStr, GcStrHandle};
pub use manage::{
  DebugHeap, DebugWrap, DebugWrapDyn, Manage, Mark, Marked,
  Trace, TraceRoot, Unmark,
};
pub use allocate::{AllocResult, Allocate, AllocateObj};
