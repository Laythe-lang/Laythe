use super::{GcObj, Mark, Marked, Trace, Unmark};
use crate::{
  impl_trace,
  object::{Class, ObjectKind},
};
use std::sync::atomic::{AtomicBool, Ordering};

/// The header of an allocation indicate meta data about the object
#[derive(Debug, Default)]
#[repr(C)]
pub struct Header {
  // has this allocation been marked by the garbage collector
  marked: AtomicBool,
}

impl Header {
  pub fn new(marked: bool) -> Self {
    Self {
      marked: AtomicBool::new(marked),
    }
  }
}

impl Mark for Header {
  /// Mark this allocation as visited, returning
  /// the existing marked status
  #[inline]
  fn mark(&self) -> bool {
    self.marked.swap(true, Ordering::Release)
  }
}

impl Marked for Header {
  #[inline]
  fn marked(&self) -> bool {
    self.marked.load(Ordering::Acquire)
  }
}

impl Unmark for Header {
  #[inline]
  fn unmark(&self) -> bool {
    self.marked.swap(false, Ordering::Release)
  }
}

impl_trace!(Header);

/// The `Header` meta data for `GcObj<T>` and `GcObject`. This struct
/// is positioned at the front of the array such that the layout looks like
/// this
/// ```markdown
/// [Header (potential padding)| T ]
/// ```
#[repr(C)]
pub struct ObjHeader {
  /// Has this allocation been marked by the garbage collector
  marked: AtomicBool,

  /// The underlying value kind of this object
  kind: ObjectKind,
}

impl ObjHeader {
  /// Create a new object header
  #[inline]
  pub fn new(kind: ObjectKind) -> Self {
    Self {
      marked: AtomicBool::new(false),
      kind,
    }
  }

  /// What is the value kind of this object
  #[inline]
  pub fn kind(&self) -> ObjectKind {
    self.kind
  }
}

impl Mark for ObjHeader {
  #[inline]
  fn mark(&self) -> bool {
    self.marked.swap(true, Ordering::Release)
  }
}

impl Unmark for ObjHeader {
  #[inline]
  fn unmark(&self) -> bool {
    self.marked.swap(false, Ordering::Release)
  }
}

impl Marked for ObjHeader {
  #[inline]
  fn marked(&self) -> bool {
    self.marked.load(Ordering::Acquire)
  }
}

impl_trace!(ObjHeader);

/// The `Header` meta data for `GcObj<T>` and `GcObject`. This struct
/// is positioned at the front of the array such that the layout looks like
/// this
/// ```markdown
/// [Header (potential padding)| T ]
/// ```
#[repr(C)]
pub struct InstanceHeader {
  /// Has this allocation been marked by the garbage collector
  marked: AtomicBool,

  /// The underlying value kind of this object
  kind: ObjectKind,

  /// The instance class
  class: GcObj<Class>,
}

impl InstanceHeader {
  /// Create a new object header
  #[inline]
  pub fn new(class: GcObj<Class>) -> Self {
    Self {
      marked: AtomicBool::new(false),
      kind: ObjectKind::Instance,
      class,
    }
  }

  /// What is the value kind of this object
  #[inline]
  pub fn kind(&self) -> ObjectKind {
    self.kind
  }

  #[inline]
  pub fn class(&self) -> GcObj<Class> {
    self.class
  }
}

impl Mark for InstanceHeader {
  #[inline]
  fn mark(&self) -> bool {
    self.marked.swap(true, Ordering::Release)
  }
}

impl Unmark for InstanceHeader {
  #[inline]
  fn unmark(&self) -> bool {
    self.marked.swap(false, Ordering::Release)
  }
}

impl Marked for InstanceHeader {
  #[inline]
  fn marked(&self) -> bool {
    self.marked.load(Ordering::Acquire)
  }
}

impl Trace for InstanceHeader {
  fn trace(&self) {
    self.class.trace();
  }

  fn trace_debug(&self, log: &mut dyn std::io::Write) {
    self.class.trace_debug(log);
  }
}

#[cfg(test)]
mod test {
  mod header {
    use crate::managed::header::Header;
    use std::mem;

    #[test]
    fn size() {
      assert_eq!(mem::size_of::<Header>(), 1);
    }

    #[test]
    fn alignment() {
      assert_eq!(mem::align_of::<Header>(), 1);
    }
  }

  mod obj_header {
    use crate::managed::header::ObjHeader;
    use std::mem;

    #[test]
    fn size() {
      assert_eq!(mem::size_of::<ObjHeader>(), 2);
    }

    #[test]
    fn alignment() {
      assert_eq!(mem::align_of::<ObjHeader>(), 1);
    }
  }

  mod instance_header {
    use crate::{
      hooks::{GcHooks, NoContext},
      managed::{
        header::{InstanceHeader, ObjHeader},
        Marked,
      },
      object::ObjectKind,
      support::test_class,
    };
    use std::mem;

    #[test]
    fn size() {
      assert_eq!(mem::size_of::<InstanceHeader>(), 16);
    }

    #[test]
    fn alignment() {
      assert_eq!(mem::align_of::<InstanceHeader>(), 8);
    }

    #[test]
    fn interpret_as_obj_header() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let class = test_class(&hooks, "example");
      let header = Box::new(InstanceHeader::new(class));

      unsafe {
        let ptr = ((&*header) as *const InstanceHeader) as *const ObjHeader;
        let obj_header = &*ptr;

        assert_eq!(obj_header.marked(), false);
        assert_eq!(obj_header.kind, ObjectKind::Instance);
      };
    }
  }
}
