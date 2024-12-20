use crate::{
  hooks::GcHooks,
  managed::Trace,
  object::{LyStr, ObjectKind},
  value::{Value, ValueKind},
};
use std::{fmt::Display, io::Write};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Arity {
  Fixed(u8),
  Variadic(u8),
  Default(u8, u8),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ArityError {
  Fixed(u8),
  Variadic(u8),
  DefaultLow(u8),
  DefaultHigh(u8),
}

pub type ArityResult = Result<(), ArityError>;

impl Default for Arity {
  fn default() -> Self {
    Self::Fixed(0)
  }
}

impl Arity {
  /// Check that the given number of arguments is valid for this
  /// Arity Kind
  ///
  /// # Examples
  /// ```
  /// use laythe_core::signature::{Arity, ArityError};
  ///
  /// let fixed_arity = Arity::Fixed(3);
  /// assert_eq!(fixed_arity.check(3), Ok(()));
  /// assert_eq!(fixed_arity.check(5), Err(ArityError::Fixed(3)));
  /// ```
  pub fn check(&self, arg_count: u8) -> ArityResult {
    match *self {
      // if fixed we need exactly the correct amount
      Self::Fixed(arity) => {
        if arg_count != arity {
          return Err(ArityError::Fixed(arity));
        }
      },
      // if variadic and ending with ... take arity +
      Self::Variadic(arity) => {
        if arg_count < arity {
          return Err(ArityError::Variadic(arity));
        }
      },
      // if defaulted we need between the min and max
      Self::Default(min_arity, max_arity) => {
        if arg_count < min_arity {
          return Err(ArityError::DefaultLow(min_arity));
        }
        if arg_count > max_arity {
          return Err(ArityError::DefaultHigh(max_arity));
        }
      },
    }

    Ok(())
  }

  pub fn required_parameter(&self) -> usize {
    match *self {
      Self::Fixed(i) => i as usize,
      Self::Variadic(i) => i as usize + 1,
      Self::Default(_, i) => i as usize,
    }
  }
}

/// A native parameter indicating the name and kind of the parameter
#[derive(Copy, Clone, Debug)]
pub struct ParameterBuilder {
  pub name: &'static str,
  pub kind: ParameterKind,
}

impl ParameterBuilder {
  /// Create a new parameter
  pub const fn new(name: &'static str, kind: ParameterKind) -> Self {
    ParameterBuilder { name, kind }
  }

  /// Build a parameter from this builder
  pub fn to_param(self, hooks: &GcHooks) -> Parameter {
    Parameter {
      name: hooks.manage_str(self.name),
      kind: self.kind,
    }
  }
}

/// A native parameter indicating the name and kind of the parameter
#[derive(Copy, Clone, Debug)]
pub struct Parameter {
  pub name: LyStr,
  pub kind: ParameterKind,
}

impl Parameter {
  /// Create a new parameter
  pub const fn new(name: LyStr, kind: ParameterKind) -> Self {
    Self { name, kind }
  }
}

impl Trace for Parameter {
  #[inline]
  fn trace(&self) {
    self.name.trace()
  }

  fn trace_debug(&self, log: &mut dyn Write) {
    self.name.trace_debug(log)
  }
}

/// Indicating the type of the parameter passed in
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ParameterKind {
  Object,
  Bool,
  Number,
  String,
  Callable,
}

impl ParameterKind {
  pub fn is_valid(&self, value: Value) -> bool {
    if *self == ParameterKind::Object {
      return true;
    }

    match (self, value.kind()) {
      (ParameterKind::Bool, ValueKind::Bool) => true,
      (ParameterKind::Number, ValueKind::Number) => true,
      (ParameterKind::Object, ValueKind::Nil) => true,
      (ParameterKind::Callable, ValueKind::Obj) => matches!(
        value.to_obj().kind(),
        ObjectKind::Closure | ObjectKind::Fun | ObjectKind::Native | ObjectKind::Method
      ),
      (ParameterKind::String, ValueKind::Obj) => value.is_obj_kind(ObjectKind::String),
      _ => false,
    }
  }
}

impl From<Value> for ParameterKind {
  fn from(value: Value) -> Self {
    match value.kind() {
      ValueKind::Bool => ParameterKind::Bool,
      ValueKind::Nil => ParameterKind::Object,
      ValueKind::Number => ParameterKind::Number,
      ValueKind::Undefined => panic!("Should not pass in undefined directly"),
      ValueKind::Obj => match value.to_obj().kind() {
        ObjectKind::Closure => ParameterKind::Callable,
        ObjectKind::Method => ParameterKind::Callable,
        ObjectKind::Native => ParameterKind::Callable,
        ObjectKind::Fun => ParameterKind::Callable,
        ObjectKind::LyBox => panic!("Should not pass in box directly"),
        _ => ParameterKind::Object,
      },
    }
  }
}

impl Display for ParameterKind {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match *self {
      ParameterKind::Object => write!(f, "object"),
      ParameterKind::Bool => write!(f, "boolean"),
      ParameterKind::Number => write!(f, "number"),
      ParameterKind::String => write!(f, "string"),
      ParameterKind::Callable => write!(f, "callable"),
    }
  }
}

#[derive(Clone, Debug, Copy)]
pub enum NativeEnvironment {
  StackLess,
  Normal,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum SignatureError {
  LengthFixed(u8),
  LengthVariadic(u8),
  LengthDefaultLow(u8),
  LengthDefaultHigh(u8),
  TypeWrong(u8),
}

pub type SignatureResult = Result<(), SignatureError>;

const NO_PARAMETERS: &[ParameterBuilder] = &[];

#[derive(Clone, Debug)]
pub struct SignatureBuilder {
  pub arity: Arity,
  pub parameters: &'static [ParameterBuilder],
}

impl SignatureBuilder {
  /// Create a new default signature with provided arity
  pub const fn new(arity: Arity) -> Self {
    Self {
      arity,
      parameters: NO_PARAMETERS,
    }
  }

  /// Indicate that this signature has a set of parameters
  pub const fn with_params(self, parameters: &'static [ParameterBuilder]) -> Self {
    // TODO: wait for const match to be stabilized
    // here I think we want to use the static_assertion library
    // to check that the parameters pass in work given the arity

    Self {
      arity: self.arity,
      parameters,
    }
  }

  /// Build a full signature struct
  pub fn to_sig(&self, hooks: &GcHooks) -> NativeSignature {
    let required_parameters = self.arity.required_parameter();
    let parameters = self
      .parameters
      .iter()
      .map(|p| p.to_param(hooks))
      .collect::<Vec<Parameter>>()
      .into_boxed_slice();

    assert_eq!(
      parameters.len(),
      required_parameters,
      "Incorrect number of parameters."
    );

    NativeSignature {
      arity: self.arity,
      parameters,
    }
  }

  pub fn to_method_sig(&self, hooks: &GcHooks) -> NativeSignature {
    let arity = self.method_arity();
    let required_parameters = arity.required_parameter();

    let parameters = [ParameterBuilder::new("self", ParameterKind::Object)]
      .iter()
      .chain(self.parameters.iter())
      .map(|p| p.to_param(hooks))
      .collect::<Vec<Parameter>>()
      .into_boxed_slice();

    assert_eq!(
      parameters.len(),
      required_parameters,
      "Incorrect number of parameters."
    );

    NativeSignature { arity, parameters }
  }

  fn method_arity(&self) -> Arity {
    match self.arity {
      Arity::Fixed(i) => Arity::Fixed(i + 1),
      Arity::Variadic(i) => Arity::Variadic(i + 1),
      Arity::Default(required, total) => Arity::Default(required + 1, total + 1),
    }
  }
}

#[derive(Clone, Debug)]
pub struct NativeSignature {
  pub arity: Arity,
  pub parameters: Box<[Parameter]>,
}

impl NativeSignature {
  /// Check if the provides arguments are valid for this signature
  pub fn check(&self, args: &[Value]) -> SignatureResult {
    let count = args.len();

    match self.arity {
      // if fixed we need exactly the correct amount
      Arity::Fixed(arity) => {
        if count != arity as usize {
          return Err(SignatureError::LengthFixed(arity));
        }

        for (index, (argument, parameter)) in args.iter().zip(self.parameters.iter()).enumerate() {
          if !parameter.kind.is_valid(*argument) {
            return Err(SignatureError::TypeWrong(index as u8));
          }
        }
      },
      // if variadic and ending with ... take arity +
      Arity::Variadic(arity) => {
        if count < arity as usize {
          return Err(SignatureError::LengthVariadic(arity));
        }

        let variadic_type = self.parameters[arity as usize];

        if arity != 0 {
          for (index, (argument, parameter)) in args
            .iter()
            .zip(self.parameters.iter())
            .take(arity as usize)
            .enumerate()
          {
            if !parameter.kind.is_valid(*argument) {
              return Err(SignatureError::TypeWrong(index as u8));
            }
          }
        }

        if variadic_type.kind == ParameterKind::Object {
          return Ok(());
        }

        for (index, argument) in args[arity as usize..].iter().enumerate() {
          if !variadic_type.kind.is_valid(*argument) {
            return Err(SignatureError::TypeWrong(arity + index as u8));
          }
        }
      },
      // if defaulted we need between the min and max
      Arity::Default(min_arity, max_arity) => {
        if count < min_arity as usize {
          return Err(SignatureError::LengthDefaultLow(min_arity));
        }
        if count > max_arity as usize {
          return Err(SignatureError::LengthDefaultHigh(max_arity));
        }

        for (index, (argument, parameter)) in args.iter().zip(self.parameters.iter()).enumerate() {
          if !parameter.kind.is_valid(*argument) {
            return Err(SignatureError::TypeWrong(index as u8));
          }
        }
      },
    }

    Ok(())
  }
}

impl Trace for NativeSignature {
  fn trace(&self) {
    self.parameters.iter().for_each(|p| {
      p.trace();
    });
  }
  fn trace_debug(&self, stdio: &mut dyn Write) {
    self.parameters.iter().for_each(|p| {
      p.trace_debug(stdio);
    });
  }
}
#[cfg(test)]
mod test {
  use super::*;

  mod arity {
    use super::*;

    #[test]
    fn check_fixed() {
      let fixed_arity = Arity::Fixed(3);
      assert_eq!(fixed_arity.check(3), Ok(()));
      assert_eq!(fixed_arity.check(5), Err(ArityError::Fixed(3)));
      assert_eq!(fixed_arity.check(2), Err(ArityError::Fixed(3)));
    }

    #[test]
    fn check_variadic() {
      let fixed_arity = Arity::Variadic(2);
      assert_eq!(fixed_arity.check(2), Ok(()));
      assert_eq!(fixed_arity.check(18), Ok(()));
      assert_eq!(fixed_arity.check(1), Err(ArityError::Variadic(2)));
    }

    #[test]
    fn check_default() {
      let fixed_arity = Arity::Default(2, 4);
      assert_eq!(fixed_arity.check(2), Ok(()));
      assert_eq!(fixed_arity.check(3), Ok(()));
      assert_eq!(fixed_arity.check(4), Ok(()));
      assert_eq!(fixed_arity.check(1), Err(ArityError::DefaultLow(2)));
      assert_eq!(fixed_arity.check(6), Err(ArityError::DefaultHigh(4)));
    }
  }

  mod signature {
    use super::*;
    use crate::{hooks::NoContext, val};

    const PARAMETERS_FIXED: [ParameterBuilder; 2] = [
      ParameterBuilder::new("index", ParameterKind::Number),
      ParameterBuilder::new("val", ParameterKind::Object),
    ];

    #[test]
    fn check_fixed() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let fixed_signature = SignatureBuilder::new(Arity::Fixed(2))
        .with_params(&PARAMETERS_FIXED)
        .to_sig(&hooks);

      assert_eq!(
        fixed_signature.check(&[]),
        Err(SignatureError::LengthFixed(2))
      );
      assert_eq!(
        fixed_signature.check(&[val!(10.0), val!(true), val!(false)]),
        Err(SignatureError::LengthFixed(2))
      );
      assert_eq!(
        fixed_signature.check(&[val!(true), val!(true)]),
        Err(SignatureError::TypeWrong(0))
      );
      assert_eq!(fixed_signature.check(&[val!(10.0), val!(true)]), Ok(()));
    }

    const PARAMETERS_VARIADIC: [ParameterBuilder; 2] = [
      ParameterBuilder::new("stuff", ParameterKind::Bool),
      ParameterBuilder::new("values", ParameterKind::Number),
    ];

    #[test]
    fn check_variadic() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let fixed_signature = SignatureBuilder::new(Arity::Variadic(1))
        .with_params(&PARAMETERS_VARIADIC)
        .to_sig(&hooks);

      assert_eq!(
        fixed_signature.check(&[]),
        Err(SignatureError::LengthVariadic(1))
      );
      assert_eq!(
        fixed_signature.check(&[val!(10.0), val!(10.0)]),
        Err(SignatureError::TypeWrong(0))
      );
      assert_eq!(
        fixed_signature.check(&[val!(true), val!(false)]),
        Err(SignatureError::TypeWrong(1))
      );
      assert_eq!(
        fixed_signature.check(&[val!(true), val!(10.0), val!(false)]),
        Err(SignatureError::TypeWrong(2))
      );
      assert_eq!(fixed_signature.check(&[val!(true)]), Ok(()));
      assert_eq!(
        fixed_signature.check(&[val!(true), val!(10.0), val!(10.0)]),
        Ok(())
      );
    }

    const PARAMETERS_DEFAULT: [ParameterBuilder; 2] = [
      ParameterBuilder::new("stuff", ParameterKind::Bool),
      ParameterBuilder::new("values", ParameterKind::Number),
    ];

    #[test]
    fn check_default() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let fixed_signature = SignatureBuilder::new(Arity::Default(1, 2))
        .with_params(&PARAMETERS_DEFAULT)
        .to_sig(&hooks);

      assert_eq!(
        fixed_signature.check(&[]),
        Err(SignatureError::LengthDefaultLow(1))
      );
      assert_eq!(
        fixed_signature.check(&[val!(10.0), val!(10.0), val!(10.0)]),
        Err(SignatureError::LengthDefaultHigh(2))
      );
      assert_eq!(
        fixed_signature.check(&[val!(10.0)]),
        Err(SignatureError::TypeWrong(0))
      );
      assert_eq!(
        fixed_signature.check(&[val!(false), val!(true)]),
        Err(SignatureError::TypeWrong(1))
      );
      assert_eq!(fixed_signature.check(&[val!(true)]), Ok(()));
      assert_eq!(fixed_signature.check(&[val!(true), val!(10.0)]), Ok(()));
    }
  }
}
