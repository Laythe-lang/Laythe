use crate::value::Value;
use std::mem;

/// What is the previous unicode code point
pub fn previous_boundary(source: &str, start: usize) -> usize {
  let mut current = start - 1;
  while current > 0 && !source.is_char_boundary(current) {
    current -= 1;
  }

  current
}

/// What is next unicode code point
pub fn next_boundary(source: &str, start: usize) -> usize {
  let mut current = start + 1;
  while current < source.len() && !source.is_char_boundary(current) {
    current += 1;
  }

  current
}

/// Is the provided `value` falsey according to laythe rules
#[inline]
pub fn is_falsey(value: Value) -> bool {
  value.is_false() || value.is_nil()
}

/// Length in T between two points. Assumes aligned
pub fn ptr_len<T>(start: *const T, end: *const T) -> usize {
  let end_u = end as usize;
  let start_u = start as usize;

  if end_u < start_u {
    return 0;
  }
  let byte_len: usize = end_u - start_u;
  byte_len / mem::size_of::<T>()
}

#[derive(Default, Debug)]
pub struct IdEmitter(usize);

impl IdEmitter {
  pub fn emit(&mut self) -> usize {
    let result = self.0;
    self.0 += 1;
    result
  }

  pub fn id_count(&self) -> usize {
    self.0
  }
}

/// Get a specific NAN value to sit in place
/// of any nan
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
