use crate::{token::Token, value::Value};
use std::mem;

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

/// Is the provided `value` falsey according to spacelox rules
#[inline]
pub fn is_falsey(value: Value) -> bool {
  value.is_nil() || (value.is_bool() && !value.to_bool())
}

/// Copy a string from the str backing the provided token. Note this copy
/// emits the enclosing quotes
///
/// # Examples
/// ```
/// use spacelox_core::utils::copy_string;
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

pub fn do_if_some<T, F: FnOnce(T)>(val: Option<T>, op: F) {
  if let Some(some) = val {
    op(some);
  }
}

pub fn ptr_len<T>(start: *const T, end: *const T) -> usize {
  let end_u = end as usize;
  let start_u = start as usize;

  if end_u < start_u {
    return 0;
  }
  let byte_len: usize = end_u - start_u;
  byte_len / mem::size_of::<T>()
}

pub fn use_sentinel_nan(val: f64) -> f64 {
  if val.is_nan() {
    std::f64::NAN
  } else {
    val
  }
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
