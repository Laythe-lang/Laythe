use crate::{hooks::Hooks, object::SlHashMap, value::Value, ModuleResult, SlHashSet};
use spacelox_env::{
  managed::{Manage, Managed, Trace},
  stdio::StdIo,
};
use std::fmt;
use std::mem;

/// A struct representing a collection of class functions and variable of shared functionality
#[derive(Clone)]
pub struct Module {
  /// The name of the module
  pub name: Managed<String>,

  /// A key value set of named exports from the provided modules
  exports: SlHashSet<Managed<String>>,

  /// All of the top level symbols in this module
  symbols: SlHashMap<Managed<String>, Value>,
}

impl Module {
  /// Create a new spacelox module
  ///
  /// # Example
  /// ```
  /// use spacelox_core::module::Module;
  /// use spacelox_env::memory::{Gc, NO_GC};
  ///
  /// let gc = Gc::default();
  /// let module = Module::new(gc.manage_str("example".to_string(), &NO_GC));
  /// ```
  pub fn new(name: Managed<String>) -> Self {
    Module {
      name,
      exports: SlHashSet::default(),
      symbols: SlHashMap::default(),
    }
  }

  /// Add export a new symbol from this module. Exported names must be unique
  ///
  /// # Example
  /// ```
  /// use spacelox_core::module::Module;
  /// use spacelox_env::memory::{Gc};
  /// use spacelox_core::value::Value;
  /// use spacelox_core::hooks::{NoContext, Hooks, HookContext};
  ///
  /// let gc = Gc::default();
  /// let mut context = NoContext::new(&gc);
  /// let hooks = Hooks::new(&mut context);
  ///
  /// let mut module = Module::new(hooks.manage_str("module".to_string()));
  ///
  /// let export_name = hooks.manage_str("exported".to_string());
  ///
  /// module.insert_symbol(&hooks, export_name, Value::from(true));
  /// let result1 = module.export_symbol(&hooks, export_name);
  /// let result2 = module.export_symbol(&hooks, export_name);
  ///
  /// assert_eq!(result1.is_ok(), true);
  /// assert_eq!(result2.is_err(), true);
  /// ```
  pub fn export_symbol(&mut self, hooks: &Hooks, name: Managed<String>) -> ModuleResult<()> {
    if self.exports.contains(&name) {
      Err(hooks.make_error(format!(
        "{} has already been exported from {}",
        name, self.name
      )))
    } else {
      hooks.grow(self, |module| module.exports.insert(name));
      Ok(())
    }
  }

  /// Get a reference to all exported symbols in this module
  ///
  /// # Example
  /// ```
  /// use spacelox_core::module::Module;
  /// use spacelox_env::memory::{Gc};
  /// use spacelox_core::value::Value;
  /// use spacelox_core::hooks::{NoContext, Hooks, HookContext};
  ///
  /// let gc = Gc::default();
  /// let mut context = NoContext::new(&gc);
  /// let hooks = Hooks::new(&mut context);
  ///
  /// let mut module = Module::new(hooks.manage_str("module".to_string()));
  ///
  /// let export_name = hooks.manage_str("exported".to_string());
  /// module.insert_symbol(&hooks, export_name, Value::from(true));
  /// module.export_symbol(&hooks, export_name);
  ///
  /// let symbols = module.import();
  ///
  /// assert_eq!(symbols.len(), 1);
  ///
  /// if let Some(result) = symbols.get(&Value::from(export_name)) {
  ///   assert_eq!(*result, Value::from(true));
  /// } else {
  ///   assert!(false);
  /// }
  /// ```
  pub fn import(&self) -> SlHashMap<Value, Value> {
    let mut import = SlHashMap::default();

    self.exports.iter().for_each(|export| {
      import.insert(
        Value::from(*export),
        *self
          .symbols
          .get(export)
          .expect("Exports should mirror symbols"),
      );
    });

    import
  }

  /// Insert a symbol into this module's symbol table
  ///
  /// #Examples
  /// ```
  /// use spacelox_core::module::Module;
  /// use spacelox_env::memory::{Gc};
  /// use spacelox_core::value::Value;
  /// use spacelox_core::hooks::{NoContext, Hooks, HookContext};
  ///
  /// let gc = Gc::default();
  /// let mut context = NoContext::new(&gc);
  /// let hooks = Hooks::new(&mut context);
  ///
  /// let mut module = Module::new(hooks.manage_str("module".to_string()));
  ///
  /// let name = hooks.manage_str("exported".to_string());
  /// module.insert_symbol(&hooks, name, Value::from(true));
  ///
  /// let symbol = module.get_symbol(name);
  ///
  /// if let Some(result) = symbol {
  ///   assert_eq!(*result, Value::from(true));
  /// } else {
  ///   assert!(false);
  /// }
  /// ```
  pub fn insert_symbol(
    &mut self,
    hooks: &Hooks,
    name: Managed<String>,
    symbol: Value,
  ) -> Option<Value> {
    hooks.grow(self, |module| module.symbols.insert(name, symbol))
  }

  /// Get a symbol from this module's symbol table
  ///
  /// #Examples
  /// ```
  /// use spacelox_core::module::Module;
  /// use spacelox_env::memory::{Gc};
  /// use spacelox_core::value::Value;
  /// use spacelox_core::hooks::{NoContext, Hooks, HookContext};
  ///
  /// let gc = Gc::default();
  /// let mut context = NoContext::new(&gc);
  /// let hooks = Hooks::new(&mut context);
  ///
  /// let mut module = Module::new(hooks.manage_str("module".to_string()));
  ///
  /// let name = hooks.manage_str("exported".to_string());
  ///
  /// let symbol = module.get_symbol(name);
  ///
  /// if let Some(result) = symbol {
  ///   assert!(false);
  /// } else {
  ///   assert!(true);
  /// }
  /// ```
  pub fn get_symbol(&self, name: Managed<String>) -> Option<&Value> {
    self.symbols.get(&name)
  }

  pub fn len(&self) -> usize {
    self.symbols.len()
  }

  /// Remove a symbol from this module
  pub fn remove_symbol(&mut self, hooks: &Hooks, name: Managed<String>) {
    hooks.shrink(self, |module| {
      module.symbols.remove(&name);
      module.exports.remove(&name);
    });
  }

  /// Transfer the export symbols to another module
  pub fn transfer_exported(&self, hooks: &Hooks, other: &mut Module) -> ModuleResult<()> {
    for export in &self.exports {
      other.insert_symbol(
        hooks,
        *export,
        *self
          .symbols
          .get(export)
          .expect("Exported value not in symbol table."),
      );
    }

    Ok(())
  }
}

impl fmt::Debug for Module {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    f.debug_struct("Module")
      .field("name", &*self.name)
      .field("exports", &"SlHashMap: { ... }")
      .field("symbols", &"SlHashMap: { ... }")
      .finish()
  }
}

impl Trace for Module {
  fn trace(&self) -> bool {
    self.name.trace();

    self.exports.iter().for_each(|key| {
      key.trace();
    });
    self.symbols.iter().for_each(|(key, value)| {
      key.trace();
      value.trace();
    });

    true
  }
  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    self.name.trace_debug(stdio);

    self.exports.iter().for_each(|key| {
      key.trace_debug(stdio);
    });
    self.symbols.iter().for_each(|(key, value)| {
      key.trace_debug(stdio);
      value.trace_debug(stdio);
    });

    true
  }
}

impl Manage for Module {
  fn alloc_type(&self) -> &str {
    "module"
  }
  fn debug(&self) -> String {
    format!("{:?}", self)
  }
  fn debug_free(&self) -> String {
    "Module: {{ name: {{...}}, exports: {{...}}, symbols: {{...}}}}".to_string()
  }

  fn size(&self) -> usize {
    mem::size_of::<Self>()
      + (mem::size_of::<Managed<String>>() + mem::size_of::<Value>())
        * (self.exports.capacity() + self.symbols.capacity())
  }
}
