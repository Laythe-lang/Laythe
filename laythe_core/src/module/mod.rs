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
  list,
  managed::{AllocResult, Allocate, DebugHeap, DebugWrap, Trace},
  object::{Class, Instance, List, ListLocation, LyStr, Map},
  reference::{ObjRef, Ref},
  value::Value,
  LyHashSet,
};
use hashbrown::hash_map;
use std::{fmt, io::Write, slice::Iter};

pub fn module_class<S: AsRef<str>>(
  hooks: &GcHooks,
  name: S,
  base_class: ObjRef<Class>,
) -> ObjRef<Class> {
  let name = hooks.manage_str(name);
  Class::with_inheritance(hooks, name, base_class)
}

/// A struct representing a collection of class functions and variable of shared functionality
#[derive(Clone, Debug)]
pub struct Module {
  // What is the id of this module for this execution
  id: usize,

  /// The class that represents this module when imported
  module_class: ObjRef<Class>,

  /// A key value set of named exports from the provided modules
  exports: LyHashSet<LyStr>,

  /// All of the top level symbols in this module
  symbols_by_name: Map<LyStr, usize>,

  /// All the symbols
  symbols: List,

  /// The path this module is located at
  path: LyStr,

  /// All the child modules to this module
  modules: Map<LyStr, Ref<Module>>,
}

impl Module {
  /// Create a new laythe module
  pub fn new(hooks: &GcHooks, module_class: ObjRef<Class>, path: &str, id: usize) -> Self {
    let symbols = List::new(hooks.manage_obj(list!()));
    hooks.push_root(symbols);

    let path = hooks.manage_str(path);
    hooks.pop_roots(1);

    Module {
      id,
      module_class,
      exports: LyHashSet::default(),
      symbols_by_name: Map::default(),
      modules: Map::default(),
      symbols,
      path,
    }
  }

  /// Get the name of this module
  pub fn name(&self) -> LyStr {
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

  pub fn symbols_by_name(&self) -> impl Iterator<Item = (LyStr, Value, usize)> + '_ {
    self
      .symbols_by_name
      .iter()
      .map(|(name, id)| (*name, self.symbols[*id], *id))
  }

  /// A symbols iterator
  pub fn symbols(&self) -> Iter<'_, Value> {
    self.symbols.iter()
  }

  /// A module iterator
  pub fn modules(&self) -> hash_map::Iter<'_, LyStr, Ref<Module>> {
    self.modules.iter()
  }

  /// Get the instance that represents
  pub fn module_instance(&self, hooks: &GcHooks) -> Instance {
    let class = self.module_class;

    let mut import = hooks.manage_obj(class);
    hooks.push_root(import);

    self.exports.iter().for_each(|export| {
      let index = *self
        .symbols_by_name
        .get(export)
        .expect("Exports should mirror symbols");

      import.set_field(*export, self.symbols[index]);
    });

    hooks.pop_roots(1);

    import
  }

  /// Insert a module into this module
  pub fn insert_module(&mut self, sub_module: Ref<Module>) -> ModuleInsertResult {
    let name = sub_module.name();

    match self.modules.insert(name, sub_module) {
      Some(_) => Err(ModuleInsertError::ModuleAlreadyExists),
      None => Ok(()),
    }
  }

  /// Retrieve a module for the given that if it exists
  pub fn get_module(&self, name: LyStr) -> Option<Ref<Module>> {
    self.modules.get(&name).copied()
  }

  /// Attempt to import a module from the provided path
  pub fn import(&self, path: &[LyStr]) -> ImportResult<Ref<Module>> {
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
  pub fn export_symbol(&mut self, name: LyStr) -> SymbolExportResult {
    if !self.symbols_by_name.contains_key(&name) {
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

  /// Set the value of a symbol in this module symbol table by name
  #[inline]
  pub fn set_symbol_by_name(&mut self, name: LyStr, symbol: Value) -> ImportResult<()> {
    match self.symbols_by_name.get(&name) {
      Some(index) => {
        self.symbols[*index] = symbol;
        Ok(())
      },
      None => Err(ImportError::SymbolDoesNotExist),
    }
  }

  /// Set the value of a symbol in this module symbol table by slot
  #[inline]
  pub fn set_symbol_by_slot(&mut self, slot: usize, symbol: Value) -> ImportResult<()> {
    if self.symbols.len() > slot {
      self.symbols[slot] = symbol;
      Ok(())
    } else {
      Err(ImportError::SymbolDoesNotExist)
    }
  }

  /// Insert a symbol into this module's symbol table
  #[inline]
  pub fn insert_symbol(
    &mut self,
    hooks: &GcHooks,
    name: LyStr,
    symbol: Value,
  ) -> SymbolInsertResult {
    let slot = self.symbols.len();
    match self.symbols_by_name.insert(name, slot) {
      Some(_) => Err(SymbolInsertError::SymbolAlreadyExists),
      None => {
        self.symbols.push(symbol, hooks);
        match self.symbols.state() {
          ListLocation::Forwarded(symbols) => self.symbols = symbols,
          ListLocation::Here(_) => (),
        }
        Ok(slot)
      },
    }
  }

  /// Get a symbol from this module's symbol table by name
  #[inline]
  pub fn get_symbol_by_name(&self, name: LyStr) -> Option<Value> {
    self
      .symbols_by_name
      .get(&name)
      .map(|index| self.symbols[*index])
  }

  /// Get a symbol from this module's symbol table by slot
  #[inline]
  pub fn get_symbol_by_slot(&self, slot: usize) -> Option<Value> {
    if self.symbols.len() > slot {
      Some(self.symbols[slot])
    } else {
      None
    }
  }

  /// Using a module slot determine it's symbol name. Note
  /// this method isn't very efficient but it should be in the slow
  /// path of us bailing out so it shouldn't matter too much
  pub fn get_symbol_name_by_slot(&self, slot: usize) -> Option<LyStr> {
    if self.symbols.len() > slot {
      self
        .symbols_by_name
        .iter()
        .find(|(_, value)| **value == slot)
        .map(|(key, _)| *key)
    } else {
      None
    }
  }

  /// Get an exported symbol from this module's symbol table
  pub fn get_exported_symbol_by_name(&self, name: LyStr) -> Option<Value> {
    self.get_symbol_by_name(name).and_then(|symbol| {
      if self.exports.contains(&name) {
        Some(symbol)
      } else {
        None
      }
    })
  }

  /// how many symbols are in this module
  pub fn len(&self) -> usize {
    self.symbols_by_name.len()
  }

  /// Is this module empty
  pub fn is_empty(&self) -> bool {
    self.symbols_by_name.is_empty()
  }
}

impl Trace for Module {
  #[inline]
  fn trace(&self) {
    self.module_class.trace();

    self.exports.iter().for_each(|key| {
      key.trace();
    });
    self.symbols_by_name.keys().for_each(|key| key.trace());
    self.path.trace();
    self.symbols.trace();
    self.modules.trace();
  }

  fn trace_debug(&self, log: &mut dyn Write) {
    self.module_class.trace_debug(log);

    self.exports.iter().for_each(|key| {
      key.trace_debug(log);
    });
    self
      .symbols_by_name
      .keys()
      .for_each(|key| key.trace_debug(log));
    self.path.trace_debug(log);
    self.symbols.trace_debug(log);
    self.modules.trace_debug(log);
  }
}

impl DebugHeap for Module {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    f.debug_struct("Module")
      .field("module_class", &DebugWrap(&self.module_class, depth))
      .field("exports", &DebugWrap(&self.exports, depth))
      .field("symbols_by_name", &DebugWrap(&self.symbols_by_name, depth))
      .field("symbols", &DebugWrap(&self.symbols, depth))
      .finish()
  }
}

impl Allocate<Ref<Self>> for Module {
  fn alloc(self) -> AllocResult<Ref<Self>> {
    Ref::alloc_result(self)
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
  };
  use std::error;

  #[test]
  fn new() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    Module::new(
      &hooks,
      hooks.manage_obj(Class::bare(hooks.manage_str("example"))),
      "example",
      0,
    );
  }

  #[test]
  fn path() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let module = Module::new(
      &hooks,
      hooks.manage_obj(Class::bare(hooks.manage_str("example"))),
      "example",
      0,
    );

    assert_eq!(module.path(), "example");
  }

  #[test]
  fn module_instance() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut module = Module::new(
      &hooks,
      hooks.manage_obj(Class::bare(hooks.manage_str("module"))),
      "example",
      0,
    );

    let export_name = hooks.manage_str("exported");
    assert!(module
      .insert_symbol(&hooks, export_name, val!(true))
      .is_ok());
    assert!(module.export_symbol(export_name).is_ok());

    let symbols = module.module_instance(&hooks);

    if let Some(result) = symbols.get_field(export_name) {
      assert_eq!(*result, val!(true));
    } else {
      panic!();
    }
  }

  #[test]
  fn insert_module() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

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
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut module = test_module(&hooks, "example");
    let export_name = hooks.manage_str("exported");

    assert!(module
      .insert_symbol(&hooks, export_name, val!(true))
      .is_ok());
    let result1 = module.export_symbol(export_name);
    let result2 = module.export_symbol(export_name);

    assert!(result1.is_ok());
    assert_eq!(result2, Err(SymbolExportError::SymbolAlreadyExported));
  }

  #[test]
  fn set_symbol() -> Result<(), Box<dyn error::Error>> {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let name = "self";
    let mut module = test_module(&hooks, name);

    assert_eq!(
      module.set_symbol_by_name(hooks.manage_str("does not exist"), val!(false)),
      Err(ImportError::SymbolDoesNotExist)
    );

    let symbol_name = hooks.manage_str("test");
    module.insert_symbol(&hooks, symbol_name, val!(true))?;

    assert!(module.set_symbol_by_name(symbol_name, val!(false)).is_ok());

    assert_eq!(module.get_symbol_by_name(symbol_name), Some(val!(false)));

    Ok(())
  }

  #[test]
  fn insert_symbol() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let mut module = test_module(&hooks, "test");

    let name = hooks.manage_str("exported");
    assert!(module.insert_symbol(&hooks, name, val!(true)).is_ok());

    let symbol = module.get_symbol_by_name(name);

    if let Some(result) = symbol {
      assert_eq!(result, val!(true));
    } else {
      panic!();
    }
  }

  #[test]
  fn get_symbol() -> Result<(), SymbolInsertError> {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let name = "self";
    let mut module = test_module(&hooks, name);

    let name = hooks.manage_str("exported");

    assert!(module.get_symbol_by_name(name).is_none());

    module.insert_symbol(&hooks, name, val!(10.0))?;
    assert_eq!(module.get_symbol_by_name(name), Some(val!(10.0)));

    Ok(())
  }

  #[test]
  fn get_exported_symbol() -> Result<(), Box<dyn error::Error>> {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);

    let name = "self";
    let mut module = test_module(&hooks, name);

    let name = hooks.manage_str("exported");

    assert_eq!(module.get_exported_symbol_by_name(name), None);

    module.insert_symbol(&hooks, name, val!(10.0))?;
    assert_eq!(module.get_exported_symbol_by_name(name), None);

    module.export_symbol(name)?;
    assert_eq!(module.get_exported_symbol_by_name(name), Some(val!(10.0)));

    Ok(())
  }
}
