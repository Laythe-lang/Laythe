mod error;
mod import;
mod package;

pub use error::{
  ImportError, ImportResult, ModuleInsertError, ModuleInsertResult, SymbolExportError,
  SymbolExportResult, SymbolInsertError, SymbolInsertResult,
};
pub use import::Import;
pub use package::Package;

use crate::{
  hooks::GcHooks,
  managed::{AllocResult, Allocate, DebugHeap, DebugWrap, Gc, GcObj, GcStr, Instance, Trace},
  object::{Class, Map},
  value::Value,
  LyHashSet,
};
use hashbrown::hash_map;
use std::{
  fmt,
  io::Write,
};

pub fn module_class<S: AsRef<str>>(
  hooks: &GcHooks,
  name: S,
  base_class: GcObj<Class>,
) -> GcObj<Class> {
  let name = hooks.manage_str(name);
  Class::with_inheritance(hooks, name, base_class)
}

/// A struct representing a collection of class functions and variable of shared functionality
#[derive(Clone, Debug)]
pub struct Module {
  // What is the id of this module for this execution
  id: usize,

  /// The class that represents this module when imported
  module_class: GcObj<Class>,

  /// A key value set of named exports from the provided modules
  exports: LyHashSet<GcStr>,

  /// All of the top level symbols in this module
  symbols: Map<GcStr, Value>,

  /// The path this module is located at
  path: GcStr,

  /// All the child modules to this module
  modules: Map<GcStr, Gc<Module>>,
}

impl Module {
  /// Create a new laythe module
  pub fn new(module_class: GcObj<Class>, path: GcStr, id: usize) -> Self {
    Module {
      id,
      module_class,
      exports: LyHashSet::default(),
      symbols: Map::default(),
      modules: Map::default(),
      path,
    }
  }

  /// Get the name of this module
  pub fn name(&self) -> GcStr {
    self.module_class.name()
  }

  /// The path of this module
  pub fn path(&self) -> &str {
    &self.path
  }

  /// The id of this module
  pub fn id(&self) -> usize {
    self.id
  }

  /// A symbols iterator
  pub fn symbols(&self) -> hash_map::Iter<'_, GcStr, Value> {
    self.symbols.iter()
  }

  /// A symbols iterator
  pub fn modules(&self) -> hash_map::Iter<'_, GcStr, Gc<Module>> {
    self.modules.iter()
  }

  /// Get the instance that represents
  pub fn module_instance(&self, hooks: &GcHooks) -> Instance {
    let class = self.module_class;

    let mut import = hooks.manage_instance(class);
    hooks.push_root(import);

    self.exports.iter().for_each(|export| {
      import.set_field(
        *export,
        *self
          .symbols
          .get(export)
          .expect("Exports should mirror symbols"),
      );
    });

    hooks.pop_roots(1);

    import
  }

  /// Insert a module into this module
  pub fn insert_module(&mut self, sub_module: Gc<Module>) -> ModuleInsertResult {
    let name = sub_module.name();

    match self.modules.insert(name, sub_module) {
      Some(_) => Err(ModuleInsertError::ModuleAlreadyExists),
      None => Ok(()),
    }
  }

  /// Retrieve a module for the given that if it exists
  pub fn get_module(&self, name: GcStr) -> Option<Gc<Module>> {
    self.modules.get(&name).copied()
  }

  /// Attempt to import a module from the provided path
  pub fn import(&self, path: &[GcStr]) -> ImportResult<Gc<Module>> {
    if path.is_empty() {
      Err(ImportError::MalformedPath)
    } else {
      match self.modules.get(&path[0]) {
        Some(module) => {
          if path.len() == 1 {
            Ok(*module)
          } else {
            module.import(&path[1..])
          }
        },
        None => Err(ImportError::ModuleDoesNotExist),
      }
    }
  }

  /// Add export a new symbol from this module. Exported names must be unique
  pub fn export_symbol(&mut self, name: GcStr) -> SymbolExportResult {
    if !self.symbols.contains_key(&name) {
      return Err(SymbolExportError::SymbolDoesNotExist);
    }

    if self.exports.contains(&name) {
      Err(SymbolExportError::SymbolAlreadyExported)
    } else {
      self.module_class.add_field(name);
      self.exports.insert(name);
      Ok(())
    }
  }

  /// Set the value of a symbol in this module symbol table
  #[inline]
  pub fn set_symbol(&mut self, name: GcStr, symbol: Value) -> ImportResult<()> {
    match self.symbols.get_mut(&name) {
      Some(value) => {
        *value = symbol;
        Ok(())
      },
      None => Err(ImportError::SymbolDoesNotExist),
    }
  }

  /// Insert a symbol into this module's symbol table
  #[inline]
  pub fn insert_symbol(&mut self, name: GcStr, symbol: Value) -> SymbolInsertResult {
    match self.symbols.insert(name, symbol) {
      Some(_) => Err(SymbolInsertError::SymbolAlreadyExists),
      None => Ok(()),
    }
  }

  /// Insert a symbol into this module's symbol table. Overrides existing symbols
  #[inline]
  fn insert_symbol_unchecked(&mut self, _hooks: &GcHooks, name: GcStr, symbol: Value) {
    self.symbols.insert(name, symbol);
  }

  /// Get a symbol from this module's symbol table
  #[inline]
  pub fn get_symbol(&self, name: GcStr) -> Option<Value> {
    self.symbols.get(&name).copied()
  }

  /// Get an exported symbom from this module's symbol table
  pub fn get_exported_symbol(&self, name: GcStr) -> Option<Value> {
    self.get_symbol(name).and_then(|symbol| {
      if self.exports.contains(&name) {
        Some(symbol)
      } else {
        None
      }
    })
  }

  /// how many symbols are in this module
  pub fn len(&self) -> usize {
    self.symbols.len()
  }

  /// Is this module empty
  pub fn is_empty(&self) -> bool {
    self.symbols.is_empty()
  }

  /// Transfer the export symbols to another module
  pub fn transfer_exported(&self, hooks: &GcHooks, other: &mut Module) {
    for export in &self.exports {
      other.insert_symbol_unchecked(
        hooks,
        *export,
        *self
          .symbols
          .get(export)
          .expect("Exported value not in symbol table."),
      );
    }
  }
}

impl Trace for Module {
  fn trace(&self) {
    self.module_class.trace();

    self.exports.iter().for_each(|key| {
      key.trace();
    });
    self.symbols.trace();
    self.modules.trace();
  }

  fn trace_debug(&self, log: &mut dyn Write) {
    self.module_class.trace_debug(log);

    self.exports.iter().for_each(|key| {
      key.trace_debug(log);
    });
    self.symbols.trace_debug(log);
    self.modules.trace_debug(log);
  }
}

impl DebugHeap for Module {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    f.debug_struct("Module")
      .field("module_class", &DebugWrap(&self.module_class, depth))
      .field("exports", &DebugWrap(&self.exports, depth))
      .field("symbols", &DebugWrap(&self.symbols, depth))
      .finish()
  }
}

impl Allocate<Gc<Self>> for Module {
  fn alloc(self) -> AllocResult<Gc<Self>> {
    Gc::alloc_result(self)
  }
}

#[cfg(test)]
mod test {
  use super::error::SymbolInsertError;
  use super::Module;
  use crate::hooks::{GcHooks, NoContext};
  use crate::module::SymbolExportError;
  use crate::{
    module::{error::ModuleInsertError, ImportError},
    object::Class,
    support::test_module,
    val,
    value::Value,
  };
  use std::error;

  #[test]
  fn new() {
    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);

    Module::new(
      hooks.manage_obj(Class::bare(hooks.manage_str("example".to_string()))),
      hooks.manage_str("example"),
      0,
    );

    assert!(true);
  }

  #[test]
  fn path() {
    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);

    let module = Module::new(
      hooks.manage_obj(Class::bare(hooks.manage_str("example".to_string()))),
      hooks.manage_str("example"),
      0,
    );

    assert_eq!(module.path(), "example");
  }

  #[test]
  fn module_instance() {
    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);

    let mut module = Module::new(
      hooks.manage_obj(Class::bare(hooks.manage_str("module".to_string()))),
      hooks.manage_str("example"),
      0,
    );

    let export_name = hooks.manage_str("exported".to_string());
    assert!(module.insert_symbol(export_name, val!(true)).is_ok());
    assert!(module.export_symbol(export_name).is_ok());

    let symbols = module.module_instance(&hooks);

    if let Some(result) = symbols.get_field(export_name) {
      assert_eq!(*result, val!(true));
    } else {
      assert!(false);
    }
  }

  #[test]
  fn insert_module() {
    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);

    let name_root = "self";
    let mut module_root = test_module(&hooks, name_root);

    let name_child = "child";
    let module_child = test_module(&hooks, name_child);

    assert!(module_root.insert_module(module_child).is_ok());
    assert_eq!(
      module_root.insert_module(module_child),
      Err(ModuleInsertError::ModuleAlreadyExists)
    );
  }

  #[test]
  fn export_symbol() {
    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);

    let mut module = test_module(&hooks, "example");
    let export_name = hooks.manage_str("exported");

    assert!(module.insert_symbol(export_name, val!(true)).is_ok());
    let result1 = module.export_symbol(export_name);
    let result2 = module.export_symbol(export_name);

    assert!(result1.is_ok());
    assert_eq!(result2, Err(SymbolExportError::SymbolAlreadyExported));
  }

  #[test]
  fn set_symbol() -> Result<(), Box<dyn error::Error>> {
    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);

    let name = "self";
    let mut module = test_module(&hooks, name);

    assert_eq!(
      module.set_symbol(hooks.manage_str("does not exist"), val!(false)),
      Err(ImportError::SymbolDoesNotExist)
    );

    let symbol_name = hooks.manage_str("test");
    module.insert_symbol(symbol_name, val!(true))?;

    assert!(module.set_symbol(symbol_name, val!(false)).is_ok());

    assert_eq!(module.get_symbol(symbol_name), Some(val!(false)));

    Ok(())
  }

  #[test]
  fn insert_symbol() {
    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);

    let mut module = test_module(&hooks, "test");

    let name = hooks.manage_str("exported".to_string());
    assert!(module.insert_symbol(name, val!(true)).is_ok());

    let symbol = module.get_symbol(name);

    if let Some(result) = symbol {
      assert_eq!(result, val!(true));
    } else {
      assert!(false);
    }
  }

  #[test]
  fn get_symbol() -> Result<(), SymbolInsertError> {
    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);

    let name = "self";
    let mut module = test_module(&hooks, name);

    let name = hooks.manage_str("exported".to_string());

    assert!(module.get_symbol(name).is_none());

    module.insert_symbol(name, val!(10.0))?;
    assert_eq!(module.get_symbol(name), Some(val!(10.0)));

    Ok(())
  }

  #[test]
  fn get_exported_symbol() -> Result<(), Box<dyn error::Error>> {
    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);

    let name = "self";
    let mut module = test_module(&hooks, name);

    let name = hooks.manage_str("exported".to_string());

    assert_eq!(module.get_exported_symbol(name), None);

    module.insert_symbol(name, val!(10.0))?;
    assert_eq!(module.get_exported_symbol(name), None);

    module.export_symbol(name)?;
    assert_eq!(module.get_exported_symbol(name), Some(val!(10.0)));

    Ok(())
  }
}
