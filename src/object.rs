use std::fmt;
use crate::scanner::{Token};
use crate::utils::{next_boundary, previous_boundary};

#[derive(Debug, PartialEq, Clone)]
pub enum Obj {
  String(String)
}

impl fmt::Display for Obj {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Obj::String(store) => write!(f, "{}", store)
    }
  }
}

pub fn copy_string<'a>(token: &'a Token) -> String {
  let start = next_boundary(&token.lexeme, 0);
  let end = previous_boundary(&token.lexeme, token.lexeme.len());

  token.lexeme[start..end].to_string()
}