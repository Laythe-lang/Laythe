use std::{
  fmt,
  io::Write,
  mem,
  ops::{Index, IndexMut},
};

use super::Class;
use crate::value::{Value, VALUE_NIL};
use laythe_env::managed::{DebugHeap, DebugWrap, Gc, GcStr, Manage, Trace};

#[derive(PartialEq, Clone)]
pub struct Instance {
  class: Gc<Class>,
  fields: Box<[Value]>,
}

impl Instance {
  pub fn new(class: Gc<Class>) -> Self {
    Instance {
      class,
      fields: vec![VALUE_NIL; class.fields()].into_boxed_slice(),
    }
  }

  #[inline]
  pub fn class(&self) -> Gc<Class> {
    self.class
  }

  #[inline]
  pub fn fields(&self) -> &[Value] {
    &self.fields
  }

  #[inline]
  pub fn set_field(&mut self, name: GcStr, value: Value) -> bool {
    match self.class.get_field_index(&name) {
      Some(index) => {
        self.fields[index as usize] = value;
        true
      }
      None => false,
    }
  }

  #[inline]
  pub fn get_field(&self, name: &GcStr) -> Option<&Value> {
    self
      .class
      .get_field_index(&name)
      .map(|index| &self.fields[index as usize])
  }
}

impl Index<usize> for Instance {
  type Output = Value;

  #[inline]
  fn index(&self, index: usize) -> &Value {
    &self.fields[index]
  }
}

impl IndexMut<usize> for Instance {
  #[inline]
  fn index_mut(&mut self, index: usize) -> &mut Value {
    &mut self.fields[index]
  }
}

impl fmt::Debug for Instance {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.fmt_heap(f, 1)
  }
}

impl Trace for Instance {
  fn trace(&self) {
    self.class.trace();

    self.fields.iter().for_each(|val| {
      val.trace();
    });
  }

  fn trace_debug(&self, stdio: &mut dyn Write) {
    self.class.trace_debug(stdio);

    self.fields.iter().for_each(|val| {
      val.trace_debug(stdio);
    });
  }
}

impl DebugHeap for Instance {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    let depth = depth.saturating_sub(1);

    f.debug_struct("Instance")
      .field("class", &DebugWrap(&self.class, depth))
      .field("fields", &DebugWrap(&&*self.fields, depth))
      .finish()
  }
}

impl Manage for Instance {
  fn size(&self) -> usize {
    mem::size_of::<Instance>()
      + (mem::size_of::<GcStr>() + mem::size_of::<Value>()) * self.fields.len()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}
