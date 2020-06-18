use crate::value::{Value, ValueVariant};
use std::fmt::Display;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Arity {
  Fixed(u8),
  Variadic(u8),
  Default(u8, u8),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ArityError {
  Fixed(u8),
  Variadic(u8),
  DefaultLow(u8),
  DefaultHigh(u8),
}

pub type ArityResult = Result<(), ArityError>;

impl Arity {
  /// Check that the given number of arguments is valid for this
  /// Arity Kind
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::signature::{Arity, ArityError};
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
      }
      // if variadic and ending with ... take arity +
      Self::Variadic(arity) => {
        if arg_count < arity {
          return Err(ArityError::Variadic(arity));
        }
      }
      // if defaulted we need between the min and max
      Self::Default(min_arity, max_arity) => {
        if arg_count < min_arity {
          return Err(ArityError::DefaultLow(min_arity));
        }
        if arg_count > max_arity {
          return Err(ArityError::DefaultHigh(max_arity));
        }
      }
    }

    Ok(())
  }
}

/// A native parameter indicating the name and kind of the parameter
#[derive(Copy, Clone, Debug)]
pub struct Parameter {
  pub name: &'static str,
  pub kind: ParameterKind,
}

impl Parameter {
  /// Create a new parameter
  pub const fn new(name: &'static str, kind: ParameterKind) -> Self {
    Parameter { name, kind }
  }
}

/// Indicating the type of the parameter passed in
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ParameterKind {
  Any,
  Bool,
  Number,
  String,
  List,
  Map,
  Class,
  Instance,
  Iter,
  Nil,
  Fun,
}

impl ParameterKind {
  fn is_valid(&self, value: Value) -> bool {
    if *self == ParameterKind::Any {
      return true;
    }

    match (self, value.kind()) {
      (ParameterKind::Bool, ValueVariant::Bool) => true,
      (ParameterKind::Number, ValueVariant::Number) => true,
      (ParameterKind::String, ValueVariant::String) => true,
      (ParameterKind::List, ValueVariant::List) => true,
      (ParameterKind::Map, ValueVariant::Map) => true,
      (ParameterKind::Class, ValueVariant::Class) => true,
      (ParameterKind::Instance, ValueVariant::Instance) => true,
      (ParameterKind::Iter, ValueVariant::Iter) => true,
      (ParameterKind::Fun, ValueVariant::Closure) => true,
      (ParameterKind::Fun, ValueVariant::Method) => true,
      (ParameterKind::Fun, ValueVariant::NativeFun) => true,
      (ParameterKind::Fun, ValueVariant::NativeMethod) => true,
      _ => false,
    }
  }
}

impl From<Value> for ParameterKind {
  fn from(value: Value) -> Self {
    match value.kind() {
      ValueVariant::Bool => ParameterKind::Bool,
      ValueVariant::Nil => ParameterKind::Nil,
      ValueVariant::Number => ParameterKind::Number,
      ValueVariant::String => ParameterKind::String,
      ValueVariant::List => ParameterKind::List,
      ValueVariant::Map => ParameterKind::Map,
      ValueVariant::Fun => ParameterKind::Fun,
      ValueVariant::Closure => ParameterKind::Fun,
      ValueVariant::Class => ParameterKind::Class,
      ValueVariant::Instance => ParameterKind::Instance,
      ValueVariant::Iter => ParameterKind::Iter,
      ValueVariant::Method => ParameterKind::Fun,
      ValueVariant::NativeFun => ParameterKind::Fun,
      ValueVariant::NativeMethod => ParameterKind::Fun,
      ValueVariant::Upvalue => panic!("Should not pass in upvalue directly")
    }
  }
}

impl Display for ParameterKind {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match *self {
      ParameterKind::Any => write!(f, "anything"),
      ParameterKind::Bool => write!(f, "boolean"),
      ParameterKind::Nil => write!(f, "nil"),
      ParameterKind::Number => write!(f, "number"),
      ParameterKind::String => write!(f, "string"),
      ParameterKind::List => write!(f, "list"),
      ParameterKind::Map => write!(f, "map"),
      ParameterKind::Class => write!(f, "class"),
      ParameterKind::Instance => write!(f, "instance"),
      ParameterKind::Iter => write!(f, "iterator"),
      ParameterKind::Fun => write!(f, "function")
    }
  }
}

#[derive(Clone, Debug)]
pub struct Signature {
  pub arity: Arity,
  pub parameters: &'static [Parameter],
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum SignatureError {
  LengthFixed(u8),
  LengthVariadic(u8),
  LengthDefaultLow(u8),
  LengthDefaultHigh(u8),
  TypeWrong(u8),
}

pub type SignatureResult = Result<(), SignatureError>;

impl Signature {
  pub const fn new(arity: Arity, parameters: &'static [Parameter]) -> Self {
    Signature { arity, parameters }
  }

  pub fn check(&self, args: &[Value]) -> SignatureResult {
    let count = args.len();

    match self.arity {
      // if fixed we need exactly the correct amount
      Arity::Fixed(arity) => {
        if count != arity as usize {
          return Err(SignatureError::LengthFixed(arity));
        }

        for (index, (argument, parameter)) in args.iter().zip(self.parameters).enumerate() {
          if !parameter.kind.is_valid(*argument) {
            return Err(SignatureError::TypeWrong(index as u8));
          }
        }
      }
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

        if variadic_type.kind == ParameterKind::Any {
          return Ok(());
        }

        for (index, argument) in args[arity as usize..].iter().enumerate() {
          if !variadic_type.kind.is_valid(*argument) {
            return Err(SignatureError::TypeWrong(arity + index as u8));
          }
        }
      }
      // if defaulted we need between the min and max
      Arity::Default(min_arity, max_arity) => {
        if count < min_arity as usize {
          return Err(SignatureError::LengthDefaultLow(min_arity));
        }
        if count > max_arity as usize {
          return Err(SignatureError::LengthDefaultHigh(max_arity));
        }

        for (index, (argument, parameter)) in args.iter().zip(self.parameters).enumerate() {
          if !parameter.kind.is_valid(*argument) {
            return Err(SignatureError::TypeWrong(index as u8));
          }
        }
      }
    }

    Ok(())
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

    const PARAMETERS_FIXED: [Parameter; 2] = [
      Parameter::new("index", ParameterKind::Number),
      Parameter::new("val", ParameterKind::Any),
    ];

    #[test]
    fn check_fixed() {
      let fixed_signature = Signature::new(Arity::Fixed(2), &PARAMETERS_FIXED);

      assert_eq!(
        fixed_signature.check(&[]),
        Err(SignatureError::LengthFixed(2))
      );
      assert_eq!(
        fixed_signature.check(&[Value::from(10.0), Value::from(true), Value::from(false)]),
        Err(SignatureError::LengthFixed(2))
      );
      assert_eq!(
        fixed_signature.check(&[Value::from(true), Value::from(true)]),
        Err(SignatureError::TypeWrong(0))
      );
      assert_eq!(
        fixed_signature.check(&[Value::from(10.0), Value::from(true)]),
        Ok(())
      );
    }

    const PARAMETERS_VARIADIC: [Parameter; 2] = [
      Parameter::new("stuff", ParameterKind::Bool),
      Parameter::new("values", ParameterKind::Number),
    ];

    #[test]
    fn check_variadic() {
      let fixed_signature = Signature::new(Arity::Variadic(1), &PARAMETERS_VARIADIC);

      assert_eq!(
        fixed_signature.check(&[]),
        Err(SignatureError::LengthVariadic(1))
      );
      assert_eq!(
        fixed_signature.check(&[Value::from(10.0), Value::from(10.0)]),
        Err(SignatureError::TypeWrong(0))
      );
      assert_eq!(
        fixed_signature.check(&[Value::from(true), Value::from(false)]),
        Err(SignatureError::TypeWrong(1))
      );
      assert_eq!(
        fixed_signature.check(&[Value::from(true), Value::from(10.0), Value::from(false)]),
        Err(SignatureError::TypeWrong(2))
      );
      assert_eq!(fixed_signature.check(&[Value::from(true)]), Ok(()));
      assert_eq!(
        fixed_signature.check(&[Value::from(true), Value::from(10.0), Value::from(10.0)]),
        Ok(())
      );
    }

    const PARAMETERS_DEFAULT: [Parameter; 2] = [
      Parameter::new("stuff", ParameterKind::Bool),
      Parameter::new("values", ParameterKind::Number),
    ];

    #[test]
    fn check_default() {
      let fixed_signature = Signature::new(Arity::Default(1, 2), &PARAMETERS_DEFAULT);

      assert_eq!(
        fixed_signature.check(&[]),
        Err(SignatureError::LengthDefaultLow(1))
      );
      assert_eq!(
        fixed_signature.check(&[Value::from(10.0), Value::from(10.0), Value::from(10.0)]),
        Err(SignatureError::LengthDefaultHigh(2))
      );
      assert_eq!(
        fixed_signature.check(&[Value::from(10.0)]),
        Err(SignatureError::TypeWrong(0))
      );
      assert_eq!(
        fixed_signature.check(&[Value::from(false), Value::from(true)]),
        Err(SignatureError::TypeWrong(1))
      );
      assert_eq!(fixed_signature.check(&[Value::from(true)]), Ok(()));
      assert_eq!(
        fixed_signature.check(&[Value::from(true), Value::from(10.0)]),
        Ok(())
      );
    }
  }
}
