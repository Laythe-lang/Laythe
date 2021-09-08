/// Provides the state of a given
/// symbol
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum SymbolState {
  Uninitialized,
  Local,
  Captured,
}

/// Was the local successfully added
/// to this table
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum AddSymbolResult {
  Ok,
  DuplicateSymbol,
}

/// A symbol representing a named
/// entity inside a Laythe program
#[derive(Debug, Clone)]
pub struct Symbol<'a> {
  /// name of the local
  name: &'a str,

  /// What is the state of this local
  state: SymbolState,
}

impl<'a> Symbol<'a> {
  pub fn name(&self) -> &str {
    self.name
  }

  pub fn state(&self) -> SymbolState {
    self.state
  }

  /// Mark this symbol as initialized
  pub fn initialize(&mut self) {
    assert!(matches!(self.state, SymbolState::Uninitialized));
    self.state = SymbolState::Local
  }

  /// Mark this symbol as captured
  pub fn capture(&mut self) {
    assert!(matches!(self.state, SymbolState::Local));
    self.state = SymbolState::Captured
  }
}

/// A table representing a collection of symbols
/// for a particular scope
#[derive(Default, Debug)]
pub struct SymbolTable<'a>(Vec<Symbol<'a>>);

impl<'a, 'src: 'a> SymbolTable<'a> {
  /// Add a new symbol to this table
  pub fn add_symbol(&mut self, name: &'a str) -> AddSymbolResult {
    match self.get(name) {
      Some(_) => AddSymbolResult::DuplicateSymbol,
      None => {
        self.0.push(Symbol {
          name,
          state: SymbolState::Uninitialized,
        });
        AddSymbolResult::Ok
      }
    }
  }

  /// Retrieve a symbol from this table if it
  /// exists
  pub fn get(&self, name: &str) -> Option<&Symbol<'a>> {
    self.0.iter().find(|local| name == local.name)
  }

  /// Retrieve a symbol from this table if it
  /// exists that allows modification
  pub fn get_mut(&mut self, name: &str) -> Option<&mut Symbol<'a>> {
    self.0.iter_mut().find(|local| name == local.name)
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod symbol {
    use super::*;

    #[test]
    fn initialize() {
      let mut symbol = Symbol {
        name: "example",
        state: SymbolState::Uninitialized,
      };

      symbol.initialize();
      assert_eq!(symbol.state(), SymbolState::Local)
    }

    #[test]
    #[should_panic]
    fn initialize_wrong_state() {
      let mut symbol = Symbol {
        name: "example",
        state: SymbolState::Local,
      };

      symbol.initialize();
    }

    #[test]
    fn capture() {
      let mut symbol = Symbol {
        name: "example",
        state: SymbolState::Local,
      };

      symbol.capture();
      assert_eq!(symbol.state(), SymbolState::Captured)
    }

    #[test]
    #[should_panic]
    fn capture_wrong_state() {
      let mut symbol = Symbol {
        name: "example",
        state: SymbolState::Uninitialized,
      };

      symbol.capture();
    }
  }

  mod symbol_table {
    use crate::compiler::ir::symbol_table::{AddSymbolResult, SymbolTable};

    #[test]
    fn add_local_ok() {
      let mut table = SymbolTable::default();

      assert_eq!(table.add_symbol("example"), AddSymbolResult::Ok);
    }

    #[test]
    fn add_local_duplicate() {
      let mut table = SymbolTable::default();

      table.add_symbol("example");
      assert_eq!(
        table.add_symbol("example"),
        AddSymbolResult::DuplicateSymbol
      );
    }

    #[test]
    fn get_present() {
      let mut table = SymbolTable::default();

      table.add_symbol("example");
      assert!(table.get("example").is_some());
    }

    #[test]
    fn get_empty() {
      let table = SymbolTable::default();

      assert!(table.get("example").is_none());
    }

    #[test]
    fn get_mut_present() {
      let mut table = SymbolTable::default();

      table.add_symbol("example");
      assert!(table.get("example").is_some());
    }

    #[test]
    fn get_mut_empty() {
      let table = SymbolTable::default();

      assert!(table.get("example").is_none());
    }
  }
}
