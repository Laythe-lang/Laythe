use crate::{
  hooks::{GcHooks, Hooks},
  managed::{DebugHeap, DebugWrap, GcStr, Object, Trace},
  signature::{
    Arity, NativeEnvironment, NativeSignature, ParameterBuilder, ParameterKind, SignatureBuilder,
  },
  value::Value,
  Call,
};
use std::{fmt, io::Write};

use super::ObjectKind;

#[derive(Clone, Debug)]
pub struct NativeMetaBuilder {
  /// The name of the native function
  pub name: &'static str,

  /// Is this native function used as a method
  pub is_method: bool,

  /// Does this
  pub environment: NativeEnvironment,

  /// The signature of this native function or method
  pub signature: SignatureBuilder,
}

impl NativeMetaBuilder {
  /// Create a new set of meta date for a native function
  pub const fn fun(name: &'static str, arity: Arity) -> Self {
    NativeMetaBuilder {
      name,
      is_method: false,
      environment: NativeEnvironment::StackLess,
      signature: SignatureBuilder::new(arity),
    }
  }

  /// Create a new set of meta date for a native function
  pub const fn method(name: &'static str, arity: Arity) -> Self {
    NativeMetaBuilder {
      name,
      is_method: true,
      environment: NativeEnvironment::StackLess,
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
      environment: NativeEnvironment::Normal,
      signature: self.signature,
    }
  }

  /// Create a native meta data struct from this builder
  pub fn build(&self, hooks: &GcHooks) -> NativeMeta {
    if self.is_method {
      NativeMeta {
        name: hooks.manage_str(self.name),
        is_method: true,
        environment: self.environment,
        signature: self.signature.to_method_sig(hooks),
      }
    } else {
      NativeMeta {
        name: hooks.manage_str(self.name),
        is_method: false,
        environment: self.environment,
        signature: self.signature.to_sig(hooks),
      }
    }
  }
}

#[derive(Clone, Debug)]
pub struct NativeMeta {
  /// The name of the native function
  pub name: GcStr,

  /// Is this native function used as a method
  pub is_method: bool,

  /// Does this
  pub environment: NativeEnvironment,

  /// The signature of this native function or method
  pub signature: NativeSignature,
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

pub struct Native {
  /// The meta data for this native function
  meta: NativeMeta,

  /// The underlying native function
  native: Box<dyn LyNative>,
}

impl Native {
  pub fn new(meta: NativeMeta, native: Box<dyn LyNative>) -> Self {
    Self { meta, native }
  }

  #[inline]
  pub fn name(&self) -> GcStr {
    self.meta.name
  }

  #[inline]
  pub fn is_method(&self) -> bool {
    self.meta.is_method
  }

  #[inline]
  pub fn environment(&self) -> NativeEnvironment {
    self.meta.environment
  }

  #[inline]
  pub fn check_if_valid_call(&self, hooks: &GcHooks, args: &[Value]) -> Result<(), GcStr> {
    let args_count = args.len();

    let parameters = &*self.meta.signature.parameters;

    match self.meta.signature.arity {
      // if fixed we need exactly the correct amount
      Arity::Fixed(arity) => {
        if args_count != arity as usize {
          return Err(hooks.manage_str(&format!(
            "{} expected {} argument(s) but received {}.",
            self.name(),
            self.real_arg_count(arity as usize),
            self.real_arg_count(args.len()),
          )));
        }

        for (argument, parameter) in args.iter().zip(parameters.iter()) {
          if !parameter.kind.is_valid(*argument) {
            return Err(hooks.manage_str(&format!(
              "{}'s parameter \"{}\" required a {} but received a {}.",
              self.name(),
              parameter.name,
              parameter.kind,
              ParameterKind::from(*argument)
            )));
          }
        }
      },
      // if variadic and ending with ... take arity +
      Arity::Variadic(arity) => {
        if args_count < arity as usize {
          return Err(hooks.manage_str(&format!(
            "{} expected at least {} argument(s) but received {}.",
            self.name(),
            self.real_arg_count(arity as usize),
            self.real_arg_count(args_count),
          )));
        }

        let variadic_type = parameters[arity as usize];

        if arity != 0 {
          for (argument, parameter) in args.iter().zip(parameters.iter()).take(arity as usize) {
            if !parameter.kind.is_valid(*argument) {
              return Err(hooks.manage_str(&format!(
                "{}'s parameter \"{}\" required a {} but received a {}.",
                self.name(),
                parameter.name,
                parameter.kind,
                ParameterKind::from(*argument)
              )));
            }
          }
        }

        for argument in args[arity as usize..].iter() {
          if !variadic_type.kind.is_valid(*argument) {
            return Err(hooks.manage_str(&format!(
              "{}'s parameter \"{}\" required a {} but received a {}.",
              self.name(),
              variadic_type.name,
              variadic_type.kind,
              ParameterKind::from(*argument)
            )));
          }
        }
      },
      // if defaulted we need between the min and max
      Arity::Default(min_arity, max_arity) => {
        if args_count < min_arity as usize {
          return Err(hooks.manage_str(&format!(
            "{} expected at least {} argument(s) but received {}.",
            self.name(),
            self.real_arg_count(min_arity as usize),
            self.real_arg_count(args_count),
          )));
        }
        if args_count > max_arity as usize {
          return Err(hooks.manage_str(&format!(
            "{} expected at most {} argument(s) but received {}.",
            self.name(),
            self.real_arg_count(max_arity as usize),
            self.real_arg_count(args_count),
          )));
        }

        for (argument, parameter) in args.iter().zip(parameters.iter()) {
          if !parameter.kind.is_valid(*argument) {
            // return Err(SignatureError::TypeWrong(index as u8));
            return Err(hooks.manage_str("todo"));
          }
        }
      },
    }

    Ok(())
  }

  #[inline]
  pub fn call(&self, hooks: &mut Hooks, values: &[Value]) -> Call {
    self.native.call(hooks, values)
  }

  fn real_arg_count(&self, arg_count: usize) -> usize {
    if self.is_method() {
      arg_count - 1
    } else {
      arg_count
    }
  }
}

impl fmt::Display for Native {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<{} native {:p}>", &*self.name(), self)
  }
}

impl fmt::Debug for Native {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.fmt_heap(f, 2)
  }
}

impl Trace for Native {
  #[inline]
  fn trace(&self) {
    self.meta.trace();
    self.native.trace();
  }

  #[inline]
  fn trace_debug(&self, log: &mut dyn Write) {
    self.meta.trace_debug(log);
    self.native.trace_debug(log);
  }
}

impl DebugHeap for Native {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    let name = self.name();

    f.debug_struct("Native")
      .field("name", &DebugWrap(&name, depth))
      .finish()
  }
}

impl Object for Native {
  fn kind(&self) -> ObjectKind {
    ObjectKind::Native
  }
}

pub trait LyNative: Trace {
  /// Call the native functions
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call;
}

impl Trace for Box<dyn LyNative> {
  #[inline]
  fn trace(&self) {
    let inner: &dyn LyNative = &**self;
    inner.trace();
  }

  #[inline]
  fn trace_debug(&self, log: &mut dyn Write) {
    let inner: &dyn LyNative = &**self;
    inner.trace_debug(log);
  }
}
