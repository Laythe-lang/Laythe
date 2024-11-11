use std::sync::atomic::{AtomicBool, Ordering};

use crate::{
  managed::{Mark, Marked, Trace, Unmark},
  object::{Class, ObjectKind}, reference::ObjRef,
};

/// The `Header` meta data for `ObjRef<T>` and `ObjRefect`. This struct
/// is positioned at the front of the array such that the layout looks like
/// this
/// ```markdown
/// [Header (potential padding)| T ]
/// ```
#[repr(C)]
pub struct Header {
  /// Has this allocation been marked by the garbage collector
  marked: AtomicBool,

  /// The underlying value kind of this object
  kind: ObjectKind,

  /// The instance class
  class: ObjRef<Class>,
}

impl Header {
  /// Create a new object header
  #[inline]
  pub fn new(class: ObjRef<Class>) -> Self {
    Self {
      marked: AtomicBool::new(false),
      kind: ObjectKind::Instance,
      class,
    }
  }

  #[inline]
  pub fn class(&self) -> ObjRef<Class> {
    self.class
  }
}

impl Mark for Header {
  #[inline]
  fn mark(&self) -> bool {
    self.marked.swap(true, Ordering::Release)
  }
}

impl Unmark for Header {
  #[inline]
  fn unmark(&self) -> bool {
    self.marked.swap(false, Ordering::Release)
  }
}

impl Marked for Header {
  #[inline]
  fn marked(&self) -> bool {
    self.marked.load(Ordering::Acquire)
  }
}

impl Trace for Header {
  #[inline]
  fn trace(&self) {
    self.class.trace();
  }

  fn trace_debug(&self, log: &mut dyn std::io::Write) {
    self.class.trace_debug(log);
  }
}

#[cfg(test)]
mod test {
  use crate::{
    hooks::{GcHooks, NoContext}, managed::Marked, object::{header::Header as ObjHeader, instance::header::Header, ObjectKind}, support::test_class
  };
  use std::mem;

  #[test]
  fn size() {
    assert_eq!(mem::size_of::<Header>(), 16);
  }

  #[test]
  fn alignment() {
    assert_eq!(mem::align_of::<Header>(), 8);
  }

  #[test]
  fn interpret_as_obj_header() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let class = test_class(&hooks, "example");
    let header = Box::new(Header::new(class));

    unsafe {
      let ptr: *const ObjHeader = ((&*header) as *const Header) as *const ObjHeader;
      let obj_header: &ObjHeader = &*ptr;


      assert!(!obj_header.marked());
      assert_eq!(obj_header.kind(), ObjectKind::Instance);
    };
  }
}
