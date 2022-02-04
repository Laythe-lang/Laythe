mod allocation;
mod gc;
mod gc_array;
mod gc_obj;
mod gc_str;
mod header;
mod manage;
mod utils;

pub use allocation::Allocation;
pub use gc::Gc;
pub use gc_array::{
  instance_handle, tuple_handle, Array, GcArray, GcArrayHandle, Instance, Tuple, TupleHandle,
};
pub use gc_obj::{GcObj, GcObject, GcObjectHandle, GcObjectHandleBuilder, Object};
pub use gc_str::{GcStr, GcStrHandle};
pub use manage::{
  AllocResult, Allocate, DebugHeap, DebugHeapRef, DebugWrap, DebugWrapDyn, Manage, Mark, Marked,
  Trace, TraceRoot, Unmark,
};
