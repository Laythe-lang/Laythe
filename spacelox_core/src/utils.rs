use crate::token::Token;

/// What is the previous unicode code point
pub fn previous_boundary(source: &str, start: usize) -> usize {
  let mut current = start - 1;
  while !source.is_char_boundary(current) && current > 0 {
    current -= 1;
  }

  current
}

/// What is next unicode code point
pub fn next_boundary(source: &str, start: usize) -> usize {
  let mut current = start + 1;
  while !source.is_char_boundary(current) && current < source.len() {
    current += 1;
  }

  current
}

/// Copy a string from the str backing the provided token. Note this copy
/// emits the enclosing quotes
///
/// # Examples
/// ```
/// use spacelox_core::object::{Obj, ObjValue, copy_string};
/// use spacelox_core::token::{Token, TokenKind};
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

  #[test]
  fn test_previous_boundary() {
    let example1 = "abc";
    let example2 = "αβγ";

    assert_eq!(previous_boundary(example1, 2), 1);
    assert_eq!(previous_boundary(example1, 1), 0);
    assert_eq!(previous_boundary(example2, 4), 2);
    assert_eq!(previous_boundary(example2, 3), 2);
  }

  #[test]
  fn test_next_boundary() {
    let example1 = "abc";
    let example2 = "αβγ";

    assert_eq!(next_boundary(example1, 1), 2);
    assert_eq!(next_boundary(example1, 0), 1);
    assert_eq!(next_boundary(example2, 2), 4);
    assert_eq!(next_boundary(example2, 3), 4);
  }
}
