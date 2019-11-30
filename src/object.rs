use crate::scanner::{Token};
use crate::utils::{next_boundary, previous_boundary};
use std::fmt;
use std::mem::{discriminant};
use std::cell::{Cell};

#[derive(Debug, Clone)]
pub struct Obj<'a> {
  pub next: Cell<Option<&'a Obj<'a>>>,
  pub value: ObjValue
}

#[derive(Debug, PartialEq, Clone)]
pub enum ObjValue {
  String(String)
}

impl<'a> PartialEq for Obj<'a> {
  /// Determine if this `Obj` and another `Obj` are equal inside
  /// of the spacelox runtime
  /// 
  /// # Examples
  /// ```
  /// use lox_runtime::object::{Obj, ObjValue};
  /// 
  /// let obj1 = Obj::new(ObjValue::String("example1".to_string()));
  /// let obj2 = Obj::new(ObjValue::String("example2".to_string()));
  /// 
  /// assert_eq!(obj1 == obj2, false);
  /// ```
  fn eq(&self, other: &Obj) -> bool {
    if discriminant(&self.value) != discriminant(&other.value) {
      return false
    }

    match &self.value {
      ObjValue::String(str1) => match &other.value {
        ObjValue::String(str2) => str1 == str2,
      }
    }
  }
}

impl<'a> fmt::Display for Obj<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.value {
      ObjValue::String(store) => write!(f, "{}", store),
    }
  }
}

impl<'a> Obj<'a> {
  /// Construct a new object for spacelox
  pub fn new(value: ObjValue) -> Obj<'a> {
    Obj { value, next: Cell::new(Option::None) }
  }

  /// Convert spacelox value to string, panics if not a string
  /// 
  /// # Examples
  /// ```
  /// use lox_runtime::object::{Obj, ObjValue};
  /// 
  /// let obj1 = Obj::new(ObjValue::String("example".to_string()));
  /// assert_eq!(obj1.to_string(), "example");
  /// ```
  pub fn move_string(self) -> String {
    match self.value {
      ObjValue::String(str1) => str1,
    }
  }

  // pub fn equals(&self, other: &Obj) -> bool{
  //   if discriminant(&self) != discriminant(&other) {
  //     return false
  //   }

  //   match &self.value {
  //     ObjValue::String(str1) => match &other.value {
  //       ObjValue::String(str2) => str1 == str2,
  //     }
  //   }
  // }
}

pub fn copy_string(token: &Token) -> String {
  let start = next_boundary(&token.lexeme, 0);
  let end = previous_boundary(&token.lexeme, token.lexeme.len());

  token.lexeme[start..end].to_string()
}

#[cfg(test)]
mod test {
  use super::*;

  fn example_each<'a>() -> Vec<Obj<'a>> {
    vec![
      Obj::new(ObjValue::String("example".to_string()))
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