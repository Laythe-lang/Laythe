use super::Class;
use crate::{
  managed::{GcObj, GcObject, GcStr, Instance},
  value::Value,
};
use std::fmt;

impl Instance {
  /// Degrade this GcArray into the more generic GcObject.
  /// This allows the array to meet the same interface
  /// as the other managed objects
  pub fn degrade(self) -> GcObject {
    GcObject::new(self.ptr())
  }

  #[inline]
  pub fn class(&self) -> GcObj<Class> {
    self.header().class()
  }

  #[inline]
  pub fn fields(&self) -> &[Value] {
    self
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
}

impl fmt::Display for Instance {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<{} {:p}>", &*self.class().name(), self)
  }
}
