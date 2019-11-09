use std::fmt;
use crate::object::{Obj};

/// Enum of value types in spacelox
#[derive(Debug, PartialEq, Clone)]
pub enum Value {
  Bool(bool),
  Nil,
  Number(f64),
  Obj(Obj)
}

impl fmt::Display for Value {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Value::Number(num) => write!(f, "{}", num),
      Value::Bool(b) => write!(f, "{}", b),
      Value::Nil => write!(f, "nil"),
      Value::Obj(obj) => match obj {
        Obj::String(string) => write!(f, "{}", string)
      }
    }
  }
}

// Represents a collection of values
#[derive(Debug, Clone)]
pub struct ValueVec {
  pub values: Vec<Value>
}

impl ValueVec {

  /// Create a new value vec
  pub fn new() -> ValueVec {
    ValueVec { values: Vec::new() }
  }
}