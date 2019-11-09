
/// Determine if all are some or none
pub fn option_all<T>(option1: Option<T>, option2: Option<T>) -> Option<(T, T)> {
  match option1 {
    Some(opt1) => match option2 {
      Some(opt2) => Some((opt1, opt2)),
      None => None
    },
    None => None
  }
}


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
  fn test_option_all() {
    let some = Some(true);
    let none: Option<bool> = None;

    assert_eq!(option_all(some, some).is_some(), true);
    assert_eq!(option_all(some, none).is_some(), false);
    assert_eq!(option_all(none, some).is_some(), false);
    assert_eq!(option_all(none, none).is_some(), false);
  }
}