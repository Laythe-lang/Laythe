#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ArityKind {
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

impl ArityKind {
  /// Check that the given number of arguments is valid for this
  /// Arity Kind
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::arity::{ArityKind, ArityError};
  ///
  /// let fixed_arity = ArityKind::Fixed(3);
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
