use crate::object::{Obj, ObjValue};
use std::collections::HashSet;
use std::cell::Cell;
use std::ptr::NonNull;

pub struct Allocator<'a> {
  objects: Cell<Option<&'a Obj<'a>>>,
  string_store: HashSet<String>,
}

impl<'a> Allocator<'a> {
  pub fn new() -> Self {
    Allocator {
      objects: Cell::new(None),
      string_store: HashSet::new(),
    }
  }

  pub fn allocate(&mut self, value: ObjValue<'a>) -> Obj<'a> {
    let obj = Obj::new(value);
    obj.next.set(self.objects.get());
    self.objects.set(obj.next.get());
  
    obj
  }

  pub fn allocate_string(&mut self, string: String) -> Obj<'a> {
    let str_ptr = NonNull::from(self.intern_string(string));
    self.allocate(ObjValue::String(str_ptr))
  }

  pub fn copy_string(&mut self, string_ptr: NonNull<str>) -> Obj<'a> {
    self.allocate(ObjValue::String(string_ptr))
  }

  fn intern_string(&mut self, string: String) -> &str {
    &**self.string_store.get_or_insert(string)
  }

  pub fn free_objects(&mut self) {
    loop {
      match self.objects.get() {
        Some(obj) => {
          self.objects.replace(obj.next.get());
        }
        None => {
          return;
        }
      }
    }
  }
}
