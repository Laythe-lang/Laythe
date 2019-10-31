
pub fn option_all<T>(option1: Option<T>, option2: Option<T>) -> Option<(T, T)> {
  match option1 {
    Some(opt1) => match option2 {
      Some(opt2) => Some((opt1, opt2)),
      None => None
    },
    None => None
  }
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