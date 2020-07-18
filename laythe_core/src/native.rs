use crate::{
  hooks::{GcHooks, Hooks},
  signature::{Arity, Environment, ParameterBuilder, Signature, SignatureBuilder},
  value::Value,
  CallResult,
};
use laythe_env::{
  managed::{Manage, Managed, Trace},
  stdio::Stdio,
};
use std::fmt;
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
      name: hooks.manage_str(self.name.to_string()),
      is_method: self.is_method,
      environment: self.environment,
      signature: self.signature.to_sig(hooks),
    }
  }
}

#[derive(Clone, Debug)]
pub struct NativeMeta {
  /// The name of the native function
  pub name: Managed<String>,

  /// Is this native function used as a method
  pub is_method: bool,

  /// Does this
  pub environment: Environment,

  /// The signature of this native function or method
  pub signature: Signature,
}

impl Trace for NativeMeta {
  fn trace(&self) -> bool {
    self.name.trace();
    self.signature.trace()
  }

  fn trace_debug(&self, stdio: &mut Stdio) -> bool {
    self.name.trace_debug(stdio);
    self.signature.trace_debug(stdio)
  }
}

pub trait Native: MetaData + Trace {
  /// Call the native functions
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, values: &[Value]) -> CallResult;
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

impl fmt::Debug for dyn Native {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let meta = self.meta();
    f.debug_struct("NativeFun")
      .field("name", &meta.name)
      .field("signature", &meta.signature)
      .finish()
  }
}

impl fmt::Display for dyn Native {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let meta = self.meta();
    write!(f, "<native {}>", meta.name)
  }
}

impl Trace for Box<dyn Native> {
  fn trace(&self) -> bool {
    let inner: &dyn Native = &**self;
    inner.trace()
  }

  fn trace_debug(&self, stdio: &mut Stdio) -> bool {
    let inner: &dyn Native = &**self;
    inner.trace_debug(stdio)
  }
}

impl Manage for Box<dyn Native> {
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
