use crate::value::Value;
use std::collections::HashMap;

#[derive(Debug, Clone, Default)]
pub struct Table<'a> {
  /// Underlying hashmap store for the table struct. In crafting interpreters
  /// a hashmap is created as there isn't one on the standard lib
  pub store: HashMap<String, Value<'a>>,
}

impl<'a> Table<'a> {}
