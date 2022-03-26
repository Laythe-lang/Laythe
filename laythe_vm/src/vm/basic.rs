use super::Vm;
use crate::cache::InlineCache;
use laythe_core::{
  managed::{Allocate, DebugHeapRef, GcObj, GcStr, Instance, Object, Trace, Tuple},
  object::Class,
  value::Value,
};
use std::{convert::TryInto, ptr};

impl Vm {
  pub(super) fn value_class(&self, value: Value) -> GcObj<Class> {
    self.builtin.primitives.for_value(value)
  }

  pub(super) fn manage<R: 'static + Trace + Copy + DebugHeapRef, T: Allocate<R>>(
    &self,
    data: T,
  ) -> R {
    self.gc.borrow_mut().manage(data, self)
  }

  pub(super) fn manage_obj<T: 'static + Object>(&self, data: T) -> GcObj<T> {
    self.gc.borrow_mut().manage_obj(data, self)
  }

  pub(super) fn manage_tuple(&self, slice: &[Value]) -> Tuple {
    self.gc.borrow_mut().manage_tuple(slice, self)
  }

  pub(super) fn manage_instance(&self, class: GcObj<Class>) -> Instance {
    self.gc.borrow_mut().manage_instance(class, self)
  }

  pub(super) fn manage_str<S: AsRef<str>>(&self, string: S) -> GcStr {
    self.gc.borrow_mut().manage_str(string, self)
  }

  pub(super) fn push_root<T: 'static + Trace>(&self, data: T) {
    self.gc.borrow_mut().push_root(data)
  }

  pub(super) fn pop_roots(&self, count: usize) {
    self.gc.borrow_mut().pop_roots(count)
  }

  /// Update the current instruction pointer
  pub(super) unsafe fn update_ip(&mut self, offset: isize) {
    self.ip = self.ip.offset(offset)
  }

  /// Store the ip in the current frame
  pub(super) fn load_ip(&mut self) {
    self.ip = self.fiber.load_ip()
  }

  /// Store the ip in the current frame
  pub(super) fn store_ip(&mut self) {
    self.fiber.store_ip(self.ip)
  }

  /// Get the current frame slots
  pub(super) fn stack_start(&self) -> *mut Value {
    self.fiber.stack_start()
  }

  /// Get the current closure
  pub(super) unsafe fn inline_cache(&mut self) -> &InlineCache {
    self
      .inline_cache
      .get_unchecked(self.current_fun.module_id())
  }

  /// Get the current closure
  pub(super) unsafe fn inline_cache_mut(&mut self) -> &mut InlineCache {
    self
      .inline_cache
      .get_unchecked_mut(self.current_fun.module_id())
  }

  /// read a u8 out of the bytecode
  pub(super) unsafe fn read_byte(&mut self) -> u8 {
    let byte = ptr::read(self.ip);
    self.update_ip(1);
    byte
  }

  /// read a u16 out of the bytecode
  pub(super) unsafe fn read_short(&mut self) -> u16 {
    let slice = std::slice::from_raw_parts(self.ip, 2);
    let buffer = slice.try_into().expect("slice of incorrect length.");
    let short = u16::from_ne_bytes(buffer);
    self.update_ip(2);

    short
  }

  /// read a u32 out of the bytecode
  pub(super) unsafe fn read_slot(&mut self) -> u32 {
    let slice = std::slice::from_raw_parts(self.ip, 4);
    let buffer = slice.try_into().expect("slice of incorrect length.");
    let short = u32::from_ne_bytes(buffer);
    self.update_ip(4);

    short
  }

  /// read a constant from the current chunk
  pub(super) unsafe fn read_constant(&self, index: u16) -> Value {
    self
      .current_fun
      .chunk()
      .get_constant_unchecked(index as usize)
  }

  /// read a constant as a string from the current chunk
  pub(super) unsafe fn read_string(&self, index: u16) -> GcStr {
    self.read_constant(index).to_obj().to_str()
  }
}
