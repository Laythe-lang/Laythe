use crate::object::Obj;
use std::fmt;
use std::mem::discriminant;

/// Enum of value types in spacelox
#[derive(Debug, Clone)]
pub enum Value<'a> {
  Bool(bool),
  Nil,
  Number(f64),
  Obj(Obj<'a>),
}

impl<'a> fmt::Display for Value<'a> {
  /// Implement display for value in spacelox
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Value::Number(num) => write!(f, "{}", num),
      Value::Bool(b) => write!(f, "{}", b),
      Value::Nil => write!(f, "nil"),
      Value::Obj(obj) => write!(f, "{}", obj),
    }
  }
}

impl<'a> PartialEq for Value<'a> {
  /// Determine if this `Value` and another `Value` are equal inside
  /// of the spacelox runtime
  ///
  /// # Examples
  /// ```
  /// use space_lox::value::Value;
  ///
  /// let val1 = Value::Bool(false);
  /// let val2 = Value::Bool(true);
  ///
  /// assert_eq!(val1 == val2, false);
  /// ```
  fn eq(&self, other: &Value<'a>) -> bool {
    // check we're the same variant
    if discriminant(self) != discriminant(other) {
      return false;
    }

    // check the the variants have the same value
    match self {
      Value::Number(num1) => match other {
        Value::Number(num2) => num1 == num2,
        _ => unreachable!(),
      },
      Value::Bool(b1) => match other {
        Value::Bool(b2) => b1 == b2,
        _ => unreachable!(),
      },
      Value::Nil => true,
      Value::Obj(obj1) => match other {
        Value::Obj(obj2) => obj1 == obj2,
        _ => unreachable!(),
      },
    }
  }
}

impl<'a> Value<'a> {
  /// Convert spacelox value to number, panics if not a number
  ///
  /// # Examples
  /// ```
  /// use space_lox::value::Value;
  ///
  /// let val1 = Value::Number(20.0);
  /// assert_eq!(val1.to_num(), 20.0);
  /// ```
  pub fn to_num(&self) -> f64 {
    match self {
      Value::Number(num) => *num,
      _ => panic!("Value is not number"),
    }
  }

  /// Convert spacelox value to boolean, panics if not a bool
  ///
  /// # Examples
  /// ```
  /// use space_lox::value::Value;
  ///
  /// let b1 = Value::Bool(false);
  /// assert_eq!(b1.to_bool(), false);
  /// ```
  pub fn to_bool(&self) -> bool {
    match self {
      Value::Bool(b1) => *b1,
      _ => panic!("Value is not boolean"),
    }
  }

  /// Convert spacelox value to an object, panics if not a object
  ///
  /// # Examples
  /// ```
  /// use space_lox::value::Value;
  /// use space_lox::object::{Obj, ObjValue};
  ///
  /// let str1 = Value::Obj(Obj::new(ObjValue::String("example".to_string())));
  /// assert_eq!(str1.move_obj().move_string(), "example");
  /// ```
  pub fn move_obj(self) -> Obj<'a> {
    match self {
      Value::Obj(obj) => obj,
      _ => panic!("Value is not string"),
    }
  }

  /// Get a string representation of the underlying type this value representing
  ///
  /// # Examples
  /// use space_lox::value::Value;
  /// use space_lox::object::{Obj, ObjValue};
  ///
  /// let nil = Value::Nil;
  /// let bool = Value::Bool(true);
  /// let number = Value::Number(10);
  /// let string = Value::Obj(Obj::new(ObjValue::String("something")));
  ///
  /// assert_eq!(nil.value_type(), "nil");
  /// assert_eq!(bool.value_type(), "bool");
  /// assert_eq!(number.value_type(), "number");
  /// assert_eq!(string.value_type(), "string");
  pub fn value_type(&self) -> String {
    match self {
      Value::Nil => "nil".to_string(),
      Value::Bool(_) => "bool".to_string(),
      Value::Number(_) => "number".to_string(),
      Value::Obj(obj) => obj.obj_type(),
    }
  }
}

// Represents a collection of values
#[derive(Debug, Clone, PartialEq, Default)]
pub struct ValueVec<'a> {
  pub values: Vec<Value<'a>>,
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::object::ObjValue;

  fn example_each<'a>() -> Vec<Value<'a>> {
    vec![
      Value::Bool(true),
      Value::Nil,
      Value::Number(10.0),
      Value::Obj(Obj::new(ObjValue::String("example".to_string()))),
    ]
  }

  #[test]
  fn test_diff_type_no_equal() {
    let examples = example_each();
    for i in 0..examples.len() {
      for j in 0..examples.len() {
        if i == j {
          assert_eq!(examples[i] == examples[j], true);
        } else {
          assert_eq!(examples[i] == examples[j], false);
        }
      }
    }
  }
}
