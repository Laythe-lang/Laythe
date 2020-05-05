use crate::{
  hooks::Hooks, managed::Managed, value::Value, ModuleResult, SlHashMap, SpaceloxError,
  SymbolResult,
};
use hashbrown::{hash_map::Entry, HashMap};

/// A struct representing a collection of class functions and variable of shared functionality
#[derive(Clone)]
pub struct Module {
  /// The name of the module
  pub name: Managed<String>,

  /// A key value set of named exports from the provided modules
  pub exports: SlHashMap<Managed<String>, Value>,
}

impl Module {
  /// Create a new spacelox module
  ///
  /// # Example
  /// ```
  /// use spacelox_core::module::Module;
  /// use spacelox_core::memory::{Gc, NO_GC};
  ///
  /// let gc = Gc::default();
  /// let module = Module::new(gc.manage_str(String::from("example"), &NO_GC));
  /// ```
  pub fn new(name: Managed<String>) -> Self {
    Module {
      name,
      exports: HashMap::with_hasher(Default::default()),
    }
  }

  /// Add export a new symbol from this module. Exported names must be unique
  ///
  /// # Example
  /// ```
  /// use spacelox_core::module::Module;
  /// use spacelox_core::memory::{Gc};
  /// use spacelox_core::value::Value;
  /// use spacelox_core::hooks::{NoContext, Hooks, HookContext};
  ///
  /// let gc = Gc::default();
  /// let mut context = NoContext::new(&gc);
  /// let hooks = Hooks::new(&mut context);
  ///
  /// let mut module = Module::new(hooks.manage_str(String::from("module")));
  ///
  /// let export_name = hooks.manage_str(String::from("exported"));
  ///
  /// let result1 = module.add_export(&hooks, export_name, Value::Bool(true));
  /// let result2 = module.add_export(&hooks, export_name, Value::Bool(false));
  ///
  /// assert_eq!(result1.is_ok(), true);
  /// assert_eq!(result2.is_err(), true);
  /// ```
  pub fn add_export(
    &mut self,
    hooks: &Hooks,
    name: Managed<String>,
    symbol: Value,
  ) -> ModuleResult<()> {
    match self.exports.entry(name) {
      Entry::Occupied(_) => Err(hooks.make_error(format!(
        "{} has already been exported from {}",
        name, self.name
      ))),
      Entry::Vacant(entry) => {
        entry.insert(symbol);
        Ok(())
      }
    }
  }

  /// Retrieve a symbol from this module
  ///
  /// # Example
  /// ```
  /// use spacelox_core::module::Module;
  /// use spacelox_core::memory::{Gc};
  /// use spacelox_core::value::Value;
  /// use spacelox_core::hooks::{NoContext, Hooks, HookContext};
  ///
  /// let gc = Gc::default();
  /// let mut context = NoContext::new(&gc);
  /// let hooks = Hooks::new(&mut context);
  ///
  /// let mut module = Module::new(hooks.manage_str(String::from("module")));
  ///
  /// let export_name = hooks.manage_str(String::from("exported"));
  /// module.add_export(&hooks, export_name, Value::Bool(true));
  ///
  /// let successful = vec![hooks.manage_str(String::from("exported"))];
  /// let failing = vec![hooks.manage_str(String::from("not_exported"))];
  ///
  /// let symbols1 = module.get_symbols(&hooks, &successful);
  /// let symbols2 = module.get_symbols(&hooks, &failing);
  ///
  /// assert_eq!(symbols1.is_ok(), true);
  /// assert_eq!(symbols2.is_err(), true);
  ///
  /// if let Ok(result) = symbols1 {
  ///   assert_eq!(result.len(), 1);
  ///   assert_eq!(*result.get(&export_name).unwrap(), Value::Bool(true));
  /// }
  /// ```
  pub fn get_symbols(&self, hooks: &Hooks, symbols: &[Managed<String>]) -> SymbolResult {
    let mut missed: Vec<Managed<String>> = Vec::new();
    let mut results: SlHashMap<Managed<String>, Value> = HashMap::with_hasher(Default::default());

    // try to get every symbols requrest in the import
    for symbol in symbols {
      match self.exports.get(symbol) {
        Some(value) => {
          results.insert(*symbol, *value);
        }
        None => {
          missed.push(*symbol);
        }
      }
    }

    // if we miss one generate a spacelox error
    if missed.len() > 0 {
      let mut missing_str = format!("{{ {}", missed[0]);
      for i in 1..missed.len() {
        missing_str.push_str(&format!(", {}", missed[i]));
      }
      missing_str.push_str(" }");

      return Err(hooks.make_error(format!(
        "Could not find exports {} in {}",
        missing_str, self.name
      )));
    }

    Ok(results)
  }

  /// Retrieve a single item from this module
  pub fn get_symbol(&self, hooks: &Hooks, symbol: Managed<String>) -> Result<Value, SpaceloxError> {
    match self.get_symbols(hooks, &vec![symbol]) {
      Ok(hash_map) => match hash_map.get(&symbol) {
        Some(symbol) => Ok(*symbol),
        None => Err(hooks.make_error(format!(
          "Module {} does not export an item named {}.",
          self.name, symbol
        ))),
      },
      Err(err) => Err(err),
    }
  }

  /// Get a reference to all exported symbols in this module
  pub fn get_all_symbols(&self) -> &SlHashMap<Managed<String>, Value> {
    &self.exports
  }
}
