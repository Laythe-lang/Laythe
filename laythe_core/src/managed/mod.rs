mod allocate;
mod allocation;
mod gc;
mod gc_array;
mod gc_list;
mod gc_obj;
mod gc_str;
mod header;
mod manage;
mod utils;

#[macro_export]
macro_rules! list {
  () => {{
    use $crate::managed::ListBuilder;

    ListBuilder::new(&[] as &[Value], 4)
  }};
  ( $x:expr ) => {{
    use $crate::managed::ListBuilder;

    let len = $x.len();
    ListBuilder::new($x, std::cmp::max(len, 4))
  }};
}

#[macro_export]
macro_rules! to_obj_kind {
  ($o:expr, Channel) => {
    $o.to_channel()
  };
  ($o:expr, Class) => {
    $o.to_class()
  };
  ($o:expr, Closure) => {
    $o.to_closure()
  };
  ($o:expr, Fun) => {
    $o.to_fun()
  };
  ($o:expr, Fiber) => {
    $o.to_fiber()
  };
  ($o:expr, Instance) => {
    $o.to_instance()
  };
  ($o:expr, Enumerator) => {
    $o.to_enumerator()
  };
  ($o:expr, List) => {
    $o.to_list()
  };
  ($o:expr, Map) => {
    $o.to_map()
  };
  ($o:expr, Method) => {
    $o.to_method()
  };
  ($o:expr, Native) => {
    $o.to_native()
  };
  ($o:expr, String) => {
    $o.to_str()
  };
  ($o:expr, LyBox) => {
    $o.to_box()
  };
  ($o:expr, Tuple) => {
    $o.to_tuple()
  };
}

#[macro_export]
macro_rules! match_obj {
  (($scrutinee:expr) {
    $(ObjectKind::$obj_kind:ident($p:pat) => $e:expr,)*
    $(_ => $d:expr,)?
  }) => {
    {
      use $crate::to_obj_kind;

      let object: &GcObject = $scrutinee;
      match object.kind() {
        $(ObjectKind::$obj_kind => {
          let $p = to_obj_kind!(object, $obj_kind);
          $e
        })*
        $(_ => $d)?
      }
    }
  };
}

pub use allocate::{AllocResult, Allocate, AllocateObj};
pub use allocation::Allocation;
pub use gc::Gc;
pub use gc_array::{Array, GcArray, GcArrayHandle, Instance, Tuple};
pub use gc_list::{List, ListBuilder, IndexedResult};
pub use gc_obj::{GcObj, Object, GcObject, GcObjectHandle, GcObjectHandleBuilder};
pub use gc_str::{GcStr, GcStrHandle};
pub use manage::{
  DebugHeap, DebugWrap, DebugWrapDyn, Manage, Mark, Marked, Trace, TraceRoot, Unmark,
};