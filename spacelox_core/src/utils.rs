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
