mod allocation;
mod gc;
mod gc_array;
mod gc_obj;
mod gc_str;
mod manage;
mod utils;

pub use allocation::Allocation;
pub use gc::Gc;
pub use gc_array::{tuple_handle, GcArray, GcArrayHandle, Tuple};
pub use gc_obj::{GcObj, GcObject, GcObjectHandle, GcObjectHandleBuilder, Object};
pub use gc_str::{GcStr, GcStrHandle};
pub use manage::{
  DebugHeap, DebugWrap, DebugWrapDyn, Manage, Mark, Marked, Trace, TraceRoot, Unmark,
};
