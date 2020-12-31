use crate::{
  hooks::{GcHooks, Hooks},
  signature::{Arity, Environment, ParameterBuilder, Signature, SignatureBuilder},
  value::Value,
  Call,
};
use laythe_env::managed::{DebugHeap, DebugWrap, Gc, Manage, Trace};
use smol_str::SmolStr;
use std::{fmt, io::Write};
use std::{mem, ptr};

#[derive(Clone, Debug)]
pub struct NativeMetaBuilder {
  /// The name of the native function
  pub name: &'static str,

  /// Is this native function used as a method
  pub is_method: bool,

  /// Does this
  pub environment: Environment,

  /// The signature of this native function or method
  pub signature: SignatureBuilder,
}

impl NativeMetaBuilder {
  /// Create a new set of meta date for a native function
  pub const fn fun(name: &'static str, arity: Arity) -> Self {
    NativeMetaBuilder {
      name,
      is_method: false,
      environment: Environment::StackLess,
      signature: SignatureBuilder::new(arity),
    }
  }

  /// Create a new set of meta date for a native function
  pub const fn method(name: &'static str, arity: Arity) -> Self {
    NativeMetaBuilder {
      name,
      is_method: true,
      environment: Environment::StackLess,
      signature: SignatureBuilder::new(arity),
    }
  }

  /// Provide parameters this native function needs
  pub const fn with_params(self, parameters: &'static [ParameterBuilder]) -> Self {
    Self {
      name: self.name,
      is_method: self.is_method,
      environment: self.environment,
      signature: self.signature.with_params(parameters),
    }
  }

  /// Indicate this native function requires the stack
  pub const fn with_stack(self) -> Self {
    Self {
      name: self.name,
      is_method: self.is_method,
      environment: Environment::Normal,
      signature: self.signature,
    }
  }

  /// Create a native meta data struct from this builder
  pub fn to_meta(&self, hooks: &GcHooks) -> NativeMeta {
    NativeMeta {
      name: hooks.manage_str(self.name),
      is_method: self.is_method,
      environment: self.environment,
      signature: self.signature.to_sig(hooks),
    }
  }
}

#[derive(Clone, Debug)]
pub struct NativeMeta {
  /// The name of the native function
  pub name: Gc<SmolStr>,

  /// Is this native function used as a method
  pub is_method: bool,

  /// Does this
  pub environment: Environment,

  /// The signature of this native function or method
  pub signature: Signature,
}

impl Trace for NativeMeta {
  fn trace(&self) {
    self.name.trace();
    self.signature.trace();
  }

  fn trace_debug(&self, log: &mut dyn Write) {
    self.name.trace_debug(log);
    self.signature.trace_debug(log);
  }
}

pub trait Native: MetaData + Trace {
  /// Call the native functions
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, values: &[Value]) -> Call;
}

pub trait MetaData {
  /// Meta data to this native function
  fn meta(&self) -> &NativeMeta;
}

impl PartialEq<dyn Native> for dyn Native {
  fn eq(&self, rhs: &dyn Native) -> bool {
    ptr::eq(self.meta(), rhs.meta())
  }
}

impl fmt::Debug for Box<dyn Native> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.fmt_heap(f, 1)
  }
}

impl fmt::Display for Box<dyn Native> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let meta = self.meta();
    write!(f, "<native {}>", meta.name)
  }
}

impl Trace for Box<dyn Native> {
  fn trace(&self) {
    let inner: &dyn Native = &**self;
    inner.trace();
  }

  fn trace_debug(&self, log: &mut dyn Write) {
    let inner: &dyn Native = &**self;
    inner.trace_debug(log);
  }
}

impl DebugHeap for Box<dyn Native> {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    let meta = self.meta();
    let depth = depth.saturating_sub(1);

    f.debug_struct("Native")
      .field("name", &DebugWrap(&meta.name, depth))
      .field("signature", &meta.signature)
      .finish()
  }
}

impl Manage for Box<dyn Native> {
  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}
