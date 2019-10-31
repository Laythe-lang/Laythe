use std::fmt;

/// Enum of value types in spacelox
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Value {
  Bool(bool),
  Nil,
  Number(f64)
}

impl fmt::Display for Value {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Value::Number(num) => write!(f, "{}", num),
      Value::Bool(b) => write!(f, "{}", b),
      Value::Nil => write!(f, "nil"),
    }
  }
}

// Represents a collection of values
#[derive(Debug)]
pub struct ValueVec {
  pub values: Vec<Value>
}

impl ValueVec {

  /// Create a new value vec
  pub fn new() -> ValueVec {
    ValueVec { values: Vec::new() }
  }
}