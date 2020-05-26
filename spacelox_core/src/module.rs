use crate::{
  hooks::Hooks, managed::Managed, value::Value, ModuleResult, SlHashMap,
};
use hashbrown::{hash_map::Entry, HashMap};

/// A struct representing a collection of class functions and variable of shared functionality
#[derive(Clone)]
pub struct Module {
  /// The name of the module
  pub name: Managed<String>,

  /// A key value set of named exports from the provided modules
  exports: SlHashMap<Managed<String>, Value>,

  /// All of the top level symbols in this module
  symbols: SlHashMap<Managed<String>, Value>,
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
      symbols: HashMap::with_hasher(Default::default()),
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
  /// let result1 = module.export_symbol(&hooks, export_name, Value::from(true));
  /// let result2 = module.export_symbol(&hooks, export_name, Value::from(false));
  ///
  /// assert_eq!(result1.is_ok(), true);
  /// assert_eq!(result2.is_err(), true);
  /// ```
  pub fn export_symbol(
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

  /// Get a reference to all exported symbols in this module
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
  /// module.export_symbol(&hooks, export_name, Value::from(true));
  ///
  /// let symbols = module.import();
  ///
  /// assert_eq!(symbols.len(), 1);
  ///
  /// if let Some(result) = symbols.get(&export_name) {
  ///   assert_eq!(*result, Value::from(true));
  /// } else {
  ///   assert!(false);
  /// }
  /// ```
  pub fn import(&self) -> &SlHashMap<Managed<String>, Value> {
    &self.exports
  }

  /// Insert a symbol into this module's symbol table
  /// 
  /// #Examples
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
  /// let name = hooks.manage_str(String::from("exported"));
  /// module.insert_symbol(name, Value::from(true));
  ///
  /// let symbol = module.get_symbol(name);
  ///
  /// if let Some(result) = symbol {
  ///   assert_eq!(*result, Value::from(true));
  /// } else {
  ///   assert!(false);
  /// } 
  /// ```
  pub fn insert_symbol(&mut self, name: Managed<String>, symbol: Value) {
    self.symbols.insert(name, symbol);
  }

  /// Get a symbol from this module's symbol table
  /// 
  /// #Examples
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
  /// let name = hooks.manage_str(String::from("exported"));
  ///
  /// let symbol = module.get_symbol(name);
  ///
  /// if let Some(result) = symbol {
  ///   assert!(false);
  /// } else {
  ///   assert!(true);
  /// } 
  /// ```
  pub fn get_symbol(&mut self, name: Managed<String>) -> Option<&Value> {
    self.symbols.get(&name)
  }
}
