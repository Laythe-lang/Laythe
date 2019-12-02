use crate::scanner::{Token};
use crate::utils::{next_boundary, previous_boundary};
use std::fmt;
use std::mem::{discriminant};
use std::cell::{Cell};
use std::hash::{Hash, Hasher};

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

impl <'a> Eq for Obj<'a> {}

impl<'a> Hash for Obj<'a> {
  /// Produces a hash for the lox object provided.alloc
  /// 
  /// # Example
  /// ```
  /// use lox_runtime::object::{Obj, ObjValue};
  /// use std::collections::{HashMap};
  /// 
  /// let mut hash_map: HashMap<Obj, i32> = HashMap::new();
  /// let string_obj1 = Obj::new(ObjValue::String("example".to_string()));
  /// let string_obj2 = Obj::new(ObjValue::String("example".to_string()));
  /// let string_obj3 = Obj::new(ObjValue::String("example".to_string()));
  /// 
  /// hash_map.insert(string_obj1, 10);
  /// assert_eq!(*hash_map.get(&string_obj3).unwrap(), 10);
  /// 
  /// hash_map.insert(string_obj2, 20);
  /// assert_eq!(*hash_map.get(&string_obj3).unwrap(), 20);
  /// ```
  fn hash<H: Hasher>(&self, state: &mut H) {
    match &self.value {
      ObjValue::String(string) => string.hash(state), 
    }
  }
}

impl<'a> fmt::Display for Obj<'a> {
  /// Produce a human readable version of the display object
  /// 
  /// # Example
  /// ```
  /// use lox_runtime::object::{Obj, ObjValue};
  /// 
  /// let string_obj = Obj::new(ObjValue::String("example".to_string()));
  /// assert_eq!(format!("{}", string_obj), "example");
  /// ```
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
}

/// Copy a string from the str backing the provided token. Note this copy
/// emits the enclosing quotes
/// 
/// # Examples
/// ```
/// use lox_runtime::object::{Obj, ObjValue, copy_string};
/// use lox_runtime::scanner::{Token, TokenKind};
/// 
/// let token = Token {
///   kind: TokenKind::String,
///   lexeme: "\"a cat in a hat\"".to_string(),
///   line: 0
/// };
/// 
/// let copy = copy_string(&token);
/// assert_eq!(copy, "a cat in a hat".to_string());
/// assert_ne!(copy, "\"a cat in a hat\"".to_string());
/// 
/// ```
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