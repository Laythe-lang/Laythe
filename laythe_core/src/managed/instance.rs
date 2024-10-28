use std::{
  fmt::{self, Debug, Display, Pointer},
  ops::{Deref, DerefMut},
  ptr::NonNull,
};

use super::{
  allocate::AllocObjResult,
  gc_array::{GcArray, GcArrayHandle},
  header::InstanceHeader,
  AllocateObj, DebugHeap, GcObj, GcStr, Trace,
};
use crate::{
  object::Class,
  value::{Value, VALUE_NIL},
};

#[cfg(not(feature = "nan_boxing"))]
use super::gc_obj::GcObject;

const MAX_FIELD_COUNT: usize = u16::MAX as usize;
const NIL_ARRAY: [Value; MAX_FIELD_COUNT] = [VALUE_NIL; MAX_FIELD_COUNT];

pub struct Instance(GcArray<Value, InstanceHeader>);

impl Instance {
  /// Create a usize from the buffer pointer. This is used
  /// when the value is boxed
  pub fn to_usize(self) -> usize {
    self.0.to_usize()
  }

  /// Degrade this Instance into the more generic GcObject.
  /// This allows the string to meet the same interface
  /// as the other managed objects
  #[cfg(not(feature = "nan_boxing"))]
  pub fn degrade(self) -> GcObject {
    GcObject::new(self.0.ptr())
  }

  #[inline]
  pub fn class(&self) -> GcObj<Class> {
    self.0.header().class()
  }

  #[inline]
  pub fn set_field(&mut self, name: GcStr, value: Value) -> bool {
    match self.class().get_field_index(&name) {
      Some(index) => {
        self[index as usize] = value;
        true
      },
      None => false,
    }
  }

  #[inline]
  pub fn get_field(&self, name: GcStr) -> Option<&Value> {
    self
      .class()
      .get_field_index(&name)
      .map(|index| &self[index as usize])
  }

  /// Construct a `Tuple` from `NonNull<u8>`
  ///
  /// ## Safety
  /// This should only be constructed from a box value
  pub unsafe fn from_alloc_ptr(ptr: NonNull<u8>) -> Self {
    Instance(GcArray::from_alloc_ptr(ptr))
  }
}

impl Deref for Instance {
  type Target = [Value];

  #[inline]
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl DerefMut for Instance {
  #[inline]
  fn deref_mut(&mut self) -> &mut Self::Target {
    self.0.deref_mut()
  }
}

impl AllocateObj<Instance> for GcObj<Class> {
  fn alloc(self) -> AllocObjResult<Instance> {
    if self.fields() > 256 {
      panic!("Cannot allocate class with more than 256 fields")
    }

    let slice = &NIL_ARRAY[..self.fields()];
    let handle = GcArrayHandle::from_slice(slice, InstanceHeader::new(self));

    let size = handle.size();
    let reference = Instance(handle.value());

    AllocObjResult {
      handle: handle.degrade(),
      size,
      reference,
    }
  }
}

impl Trace for Instance {
  #[inline]
  fn trace(&self) {
    self.0.trace();
  }

  fn trace_debug(&self, log: &mut dyn std::io::Write) {
    self.0.trace_debug(log);
  }
}

impl DebugHeap for Instance {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    self.0.fmt_heap(f, depth)
  }
}

impl fmt::Pointer for Instance {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    Pointer::fmt(&self, f)
  }
}

impl Copy for Instance {}
impl Clone for Instance {
  fn clone(&self) -> Self {
    *self
  }
}

impl PartialEq<Instance> for Instance {
  #[inline]
  fn eq(&self, other: &Instance) -> bool {
    self.0.eq(&other.0)
  }
}
impl Eq for Instance {}

impl Debug for Instance {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    Debug::fmt(&self.0, f)
  }
}

impl Display for Instance {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<instance {} {:p}>", self.class().name(), self)
  }
}

#[cfg(test)]
mod test {
  mod instance_handle {
    use crate::{
      hooks::{GcHooks, NoContext},
      managed::AllocateObj,
      support::{test_object_class, ClassBuilder},
    };

    #[test]
    fn dude() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let obj_class = test_object_class(&hooks);

      let class = ClassBuilder::default()
        .name("example")
        .super_cls(obj_class)
        .fields(
          ["foo", "bar"]
            .iter()
            .map(|field| hooks.manage_str(field))
            .collect(),
        )
        .build(&hooks);

      let result = class.alloc();
      let instance_value = result.reference;

      assert_eq!(instance_value.len(), 2);
      assert_eq!(instance_value.class(), class);
    }
  }
}
