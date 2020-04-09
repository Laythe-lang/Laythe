use crate::{
  io::StdIo,
  managed::{Manage, Trace},
  memory::Gc,
  value::{ArityKind, Value},
};
use std::fmt;
use std::{mem, ptr};

#[derive(Clone, Debug)]
pub struct NativeMeta {
  pub name: &'static str,
  pub arity: ArityKind,
}

impl NativeMeta {
  pub const fn new(name: &'static str, arity: ArityKind) -> Self {
    NativeMeta { name, arity }
  }
}

pub enum NativeResult {
  /// The result of the native function call was a success with this value
  Success(Value),

  /// The result of the native function call was an error with this runtime
  /// message
  RuntimeError(String),
}

pub trait NativeFun {
  /// Meta data to this native function
  fn meta(&self) -> &NativeMeta;

  /// Call the native functions
  fn call(&self, gc: &Gc, context: &dyn Trace, values: &[Value]) -> NativeResult;
}

impl PartialEq<dyn NativeFun> for dyn NativeFun {
  fn eq(&self, rhs: &dyn NativeFun) -> bool {
    ptr::eq(self.meta(), rhs.meta())
  }
}

impl fmt::Debug for dyn NativeFun {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let meta = self.meta();
    f.debug_struct("NativeFun")
      .field("name", &meta.name)
      .field("arity", &meta.arity)
      .finish()
  }
}

impl fmt::Display for dyn NativeFun {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let meta = self.meta();
    write!(f, "<native {}>", meta.name)
  }
}

impl Trace for Box<dyn NativeFun> {
  fn trace(&self) -> bool {
    true
  }

  fn trace_debug(&self, _: &dyn StdIo) -> bool {
    true
  }
}

impl Manage for Box<dyn NativeFun> {
  fn alloc_type(&self) -> &str {
    "native"
  }

  fn debug(&self) -> String {
    format!("{:?}", self)
  }

  fn debug_free(&self) -> String {
    format!("{:?}", self)
  }

  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }
}

pub trait NativeMethod {
  /// Meta data to this native function
  fn meta(&self) -> &NativeMeta;

  /// Call the native functions
  fn call(&self, gc: &Gc, context: &dyn Trace, this: Value, values: &[Value]) -> NativeResult;
}

impl PartialEq<dyn NativeMethod> for dyn NativeMethod {
  fn eq(&self, rhs: &dyn NativeMethod) -> bool {
    ptr::eq(self.meta(), rhs.meta())
  }
}

impl fmt::Debug for dyn NativeMethod {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let meta = self.meta();
    f.debug_struct("NativeFun")
      .field("name", &meta.name)
      .field("arity", &meta.arity)
      .finish()
  }
}

impl fmt::Display for dyn NativeMethod {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let meta = self.meta();
    write!(f, "<native {}>", meta.name)
  }
}

impl Trace for Box<dyn NativeMethod> {
  fn trace(&self) -> bool {
    true
  }

  fn trace_debug(&self, _: &dyn StdIo) -> bool {
    true
  }
}

impl Manage for Box<dyn NativeMethod> {
  fn alloc_type(&self) -> &str {
    "native"
  }

  fn debug(&self) -> String {
    format!("{:?}", self)
  }

  fn debug_free(&self) -> String {
    format!("{:?}", self)
  }

  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }
}
