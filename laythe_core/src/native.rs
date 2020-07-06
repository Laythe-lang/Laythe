use crate::{
  hooks::Hooks,
  signature::{Arity, Parameter, Signature},
  value::Value,
  CallResult,
};
use laythe_env::{
  managed::{Manage, Trace},
  stdio::Stdio,
};
use std::fmt;
use std::{mem, ptr};

#[derive(Clone, Debug)]
pub struct NativeMeta {
  /// The name of the native function
  pub name: &'static str,

  /// The signature of this native function or method
  pub signature: Signature,
}

impl NativeMeta {
  /// Create a new set of meta date for a native function
  pub const fn new(name: &'static str, arity: Arity, parameters: &'static [Parameter]) -> Self {
    NativeMeta {
      name,
      signature: Signature::new(arity, parameters),
    }
  }
}

pub trait NativeFun: Trace {
  /// Meta data to this native function
  fn meta(&self) -> &NativeMeta;

  /// Call the native functions
  fn call(&self, hooks: &mut Hooks, values: &[Value]) -> CallResult;
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
      .field("signature", &meta.signature)
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
    let inner: &dyn NativeFun = &**self;
    inner.trace()
  }

  fn trace_debug(&self, stdio: &mut Stdio) -> bool {
    let inner: &dyn NativeFun = &**self;
    inner.trace_debug(stdio)
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

pub trait NativeMethod: Trace {
  /// Meta data to this native function
  fn meta(&self) -> &NativeMeta;

  /// Call the native functions
  fn call(&self, hooks: &mut Hooks, this: Value, values: &[Value]) -> CallResult;
}

impl PartialEq<dyn NativeMethod> for dyn NativeMethod {
  fn eq(&self, rhs: &dyn NativeMethod) -> bool {
    ptr::eq(self.meta(), rhs.meta())
  }
}

impl fmt::Debug for dyn NativeMethod {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let meta = self.meta();
    f.debug_struct("NativeMethod")
      .field("name", &meta.name)
      .field("signature", &meta.signature)
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
    let inner: &dyn NativeMethod = &**self;
    inner.trace()
  }

  fn trace_debug(&self, stdio: &mut Stdio) -> bool {
    let inner: &dyn NativeMethod = &**self;
    inner.trace_debug(stdio)
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
