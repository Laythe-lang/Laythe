use super::ast::Span;
use bumpalo::collections::vec::Vec;

/// Provides the state of a given
/// symbol
#[derive(Debug, PartialEq, Eq, Clone, Copy, Default)]
pub enum SymbolState {
  #[default]
  Uninitialized,
  Initialized,
  GlobalInitialized,
  Captured,
}

/// Was the local successfully added
/// to this table
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AddSymbolResult {
  Ok,
  DuplicateSymbol(Symbol),
}

/// A symbol representing a named
/// entity inside a Laythe program
#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct Symbol {
  /// name of the local
  name: String,

  /// the local where this symbol was declared
  span: Span,

  /// What is the state of this local
  state: SymbolState,
}

impl Symbol {
  /// Create a new symbol
  pub fn new(name: String, span: Span, state: SymbolState) -> Self {
    Self { name, span, state }
  }

  /// Retrieve this symbol's name
  pub fn name(&self) -> &str {
    &self.name
  }

  /// Retrieve this symbol's current state
  pub fn state(&self) -> SymbolState {
    self.state
  }

  /// Retrieve the source span of this symbol
  pub fn span(&self) -> Span {
    self.span
  }

  /// Mark this symbol as initialized
  pub fn initialize(&mut self) {
    if let SymbolState::Uninitialized = self.state {
      self.state = SymbolState::Initialized
    }
  }

  /// Mark this symbol as initialized
  pub fn global_initialize(&mut self) {
    if let SymbolState::Uninitialized = self.state {
      self.state = SymbolState::GlobalInitialized
    }
  }

  /// Mark this symbol as captured
  pub fn capture(&mut self) {
    if let SymbolState::Initialized = self.state {
      self.state = SymbolState::Captured
    }
  }
}

/// A table representing a collection of symbols
/// for a particular scope
#[derive(Debug)]
pub struct SymbolTable<'a>(Vec<'a, Symbol>);

impl<'a> SymbolTable<'a> {
  /// Create a new Symbol table
  pub fn new(symbols: Vec<'a, Symbol>) -> Self {
    Self(symbols)
  }

  /// Add a new symbol to this table
  pub fn add_symbol(&mut self, name: &str, span: Span) -> AddSymbolResult {
    match self.get_any_state(name) {
      Some(symbol) => AddSymbolResult::DuplicateSymbol(symbol.clone()),
      None => {
        self.0.push(Symbol {
          name: name.to_string(),
          span,
          state: SymbolState::Uninitialized,
        });
        AddSymbolResult::Ok
      },
    }
  }

  /// Add a new symbol to this table
  pub fn add_global_symbol(&mut self, name: &str, span: Span) -> AddSymbolResult {
    match self.get_any_state(name) {
      Some(symbol) => AddSymbolResult::DuplicateSymbol(symbol.clone()),
      None => {
        self.0.push(Symbol {
          name: name.to_string(),
          span,
          state: SymbolState::GlobalInitialized,
        });
        AddSymbolResult::Ok
      },
    }
  }

  /// Get a reference to all symbols in this table
  pub fn len(&self) -> usize {
    self.0.len()
  }

  /// Attempt to find a symbol regardless of state it
  /// was declared
  fn get_any_state(&self, name: &str) -> Option<&Symbol> {
    self.0.iter().find(|local| name == local.name)
  }

  /// Retrieve a symbol from this table if it
  /// exists
  pub fn get(&self, name: &str) -> Option<&Symbol> {
    if self.0.is_empty() {
      return None;
    }

    self.0.iter().rev().find(|local| name == local.name)
  }

  /// Retrieve a symbol from this table if it
  /// exists that allows modification
  pub fn get_mut(&mut self, name: &str) -> Option<&mut Symbol> {
    if self.0.is_empty() {
      return None;
    }

    self.0.iter_mut().rev().find(|local| name == local.name)
  }
}

#[cfg(test)]
mod test {
  use super::*;

  fn test_span() -> Span {
    Span { start: 0, end: 1 }
  }

  fn test_symbol(name: &str, state: SymbolState) -> Symbol {
    Symbol {
      name: name.to_string(),
      span: test_span(),
      state,
    }
  }

  mod symbol {
    use super::*;

    #[test]
    fn initialize() {
      let mut symbol = test_symbol("example", SymbolState::Uninitialized);

      symbol.initialize();
      assert_eq!(symbol.state(), SymbolState::Initialized)
    }

    #[test]
    fn initialize_wrong_state() {
      let mut symbol = test_symbol("example", SymbolState::Captured);

      symbol.initialize();
      assert_eq!(symbol.state(), SymbolState::Captured)
    }

    #[test]
    fn capture() {
      let mut symbol = test_symbol("example", SymbolState::Initialized);

      symbol.capture();
      assert_eq!(symbol.state(), SymbolState::Captured)
    }

    #[test]
    fn capture_wrong_state() {
      let mut symbol = test_symbol("example", SymbolState::Uninitialized);

      symbol.capture();
      assert_eq!(symbol.state(), SymbolState::Uninitialized)
    }
  }

  mod symbol_table {
    use bumpalo::Bump;

    use super::*;

    #[test]
    fn add_local_ok() {
      let bump = Bump::new();
      let mut table = SymbolTable::new(Vec::new_in(&bump));

      assert_eq!(
        table.add_symbol("example", test_span()),
        AddSymbolResult::Ok
      );
    }

    #[test]
    fn add_local_duplicate() {
      let bump = Bump::new();
      let mut table = SymbolTable::new(Vec::new_in(&bump));

      table.add_symbol("example", test_span());
      assert_eq!(
        table.add_symbol("example", test_span()),
        AddSymbolResult::DuplicateSymbol(Symbol {
          name: "example".to_string(),
          state: SymbolState::Uninitialized,
          span: test_span(),
        })
      );
    }

    #[test]
    fn get_present() {
      let bump = Bump::new();
      let mut table = SymbolTable::new(Vec::new_in(&bump));

      table.add_symbol("example", test_span());
      assert!(table.get("example").is_some());
    }

    #[test]
    fn get_empty() {
      let bump = Bump::new();
      let table = SymbolTable::new(Vec::new_in(&bump));

      assert!(table.get("example").is_none());
    }

    #[test]
    fn get_present_wrong_state() {
      let bump = Bump::new();
      let mut table = SymbolTable::new(Vec::new_in(&bump));

      table.add_symbol("example1", test_span());
      table.add_symbol("example2", test_span());

      assert!(table.get("example1").is_some());
      assert!(table.get("example2").is_some());
    }

    #[test]
    fn get_mut_present() {
      let bump = Bump::new();
      let mut table = SymbolTable::new(Vec::new_in(&bump));

      table.add_symbol("example", test_span());
      assert!(table.get("example").is_some());
    }

    #[test]
    fn get_mut_empty() {
      let bump = Bump::new();
      let table = SymbolTable::new(Vec::new_in(&bump));

      assert!(table.get("example").is_none());
    }

    #[test]
    fn get_mut_present_wrong_state() {
      let bump = Bump::new();
      let mut table = SymbolTable::new(Vec::new_in(&bump));

      table.add_symbol("example1", test_span());
      table.add_symbol("example2", test_span());

      assert!(table.get_mut("example1").is_some());
      assert!(table.get_mut("example2").is_some());
    }
  }
}
