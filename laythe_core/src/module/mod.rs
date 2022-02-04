mod error;
mod import;
mod package;

pub use error::{ModuleError, ModuleResult};
pub use import::Import;
pub use package::Package;

use crate::{
  hooks::GcHooks,
  managed::{AllocResult, Allocate, DebugHeap, DebugWrap, Gc, GcObj, GcStr, Trace, Instance},
  object::{Class, Map, MapEntry},
  value::Value,
  LyHashSet,
};
use hashbrown::hash_map;
use std::path::PathBuf;
use std::{fmt, io::Write};

/// A struct representing a collection of class functions and variable of shared functionality
#[derive(Clone)]
pub struct Module {
  /// The full filepath to this module
  path: PathBuf,

  // What is the id of this module for this execution
  id: usize,

  /// The class that represents this module when imported
  module_class: GcObj<Class>,

  /// A key value set of named exports from the provided modules
  exports: LyHashSet<GcStr>,

  /// All of the top level symbols in this module
  symbols: Map<GcStr, Value>,

  /// All the child modules to this module
  modules: Map<GcStr, Gc<Module>>,
}

impl Module {
  /// Create a new laythe module
  pub fn new(module_class: GcObj<Class>, path: PathBuf, id: usize) -> Self {
    Module {
      path,
      id,
      module_class,
      exports: LyHashSet::default(),
      symbols: Map::default(),
      modules: Map::default(),
    }
  }

  /// Get the name of this module
  pub fn name(&self) -> GcStr {
    self.module_class.name()
  }

  /// Retrieve the path for this module
  pub fn path(&self) -> &PathBuf {
    &self.path
  }

  /// The id of this module
  pub fn id(&self) -> usize {
    self.id
  }

  /// Create a module from a filepath
  pub fn from_path(
    hooks: &GcHooks,
    path: PathBuf,
    base_class: GcObj<Class>,
    id: usize,
  ) -> ModuleResult<Self> {
    let module_name = path
      .file_stem()
      .and_then(|m| m.to_str())
      .ok_or(ModuleError::ModulePathMalformed)?;

    let name = hooks.manage_str(module_name);
    let module_class = Class::with_inheritance(hooks, name, base_class);

    Ok(Self {
      path,
      id,
      module_class,
      exports: LyHashSet::default(),
      symbols: Map::default(),
      modules: Map::default(),
    })
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
  pub fn insert_module(&mut self, hooks: &GcHooks, sub_module: Gc<Module>) -> ModuleResult<()> {
    let relative = sub_module
      .path()
      .strip_prefix(self.path())
      .or(Err(ModuleError::ModuleNotDecedent))?;

    match relative.parent() {
      Some(parent) => {
        if parent.to_str() != Some("") {
          return Err(ModuleError::ModuleNotDirectDecedent);
        }
      },
      None => return Err(ModuleError::ModuleNotDecedent),
    }

    let root = relative.to_str().ok_or(ModuleError::ModuleNotDecedent)?;
    let name = hooks.manage_str(root);

    match self.modules.insert(name, sub_module) {
      Some(_) => Err(ModuleError::SymbolAlreadyExists),
      None => Ok(()),
    }
  }

  /// Get a reference to all exported symbols in this module
  pub fn import(&self, hooks: &GcHooks, path: &[GcStr]) -> ModuleResult<Instance> {
    if path.is_empty() {
      Ok(self.module_instance(hooks))
    } else {
      self
        .modules
        .get(&path[0])
        .ok_or(ModuleError::ModuleDoesNotExist)
        .and_then(|module| module.import(hooks, &path[1..]))
    }
  }

  /// Get a reference to all exported symbols in this module
  pub fn import_symbol(&self, hooks: &GcHooks, path: &[GcStr], name: GcStr) -> ModuleResult<Value> {
    if path.is_empty() {
      self.get_exported_symbol(name)
    } else {
      self
        .modules
        .get(&path[0])
        .ok_or(ModuleError::ModuleDoesNotExist)
        .and_then(|module| module.import_symbol(hooks, &path[1..], name))
    }
  }

  /// Add export a new symbol from this module. Exported names must be unique
  pub fn export_symbol(&mut self, name: GcStr) -> ModuleResult<()> {
    if !self.symbols.contains_key(&name) {
      return Err(ModuleError::SymbolDoesNotExist);
    }

    if self.exports.contains(&name) {
      Err(ModuleError::SymbolAlreadyExported)
    } else {
      self.module_class.add_field(name);
      self.exports.insert(name);
      Ok(())
    }
  }

  /// Set the value of a symbol in this module symbol table
  #[inline]
  pub fn set_symbol(&mut self, name: GcStr, symbol: Value) -> ModuleResult<()> {
    match self.symbols.get_mut(&name) {
      Some(value) => {
        *value = symbol;
        Ok(())
      },
      None => Err(ModuleError::SymbolDoesNotExist),
    }
  }

  /// Insert a symbol into this module's symbol table
  #[inline]
  pub fn insert_symbol(
    &mut self,
    _hooks: &GcHooks,
    name: GcStr,
    symbol: Value,
  ) -> ModuleResult<()> {
    match self.symbols.insert(name, symbol) {
      Some(_) => Err(ModuleError::SymbolAlreadyExists),
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

  /// Get this symbol entry if it exists
  #[inline]
  pub fn symbol_entry(&mut self, name: GcStr) -> MapEntry<'_, GcStr, Value> {
    self.symbols.entry(name)
  }

  /// Import a single symbol from this module
  pub fn get_exported_symbol(&self, name: GcStr) -> ModuleResult<Value> {
    self
      .get_symbol(name)
      .ok_or(ModuleError::SymbolDoesNotExist)
      .and_then(|symbol| {
        if self.exports.contains(&name) {
          Ok(symbol)
        } else {
          Err(ModuleError::SymbolNotExported)
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
      .field("path", &self.path)
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
  use super::Module;
  use crate::{
    hooks::{GcHooks, NoContext},
    module::{ModuleError, ModuleResult},
    object::Class,
    support::test_class,
    val,
  };
  use std::path::PathBuf;

  fn test_module(hooks: &GcHooks, path: PathBuf) -> ModuleResult<Module> {
    let module_class = test_class(hooks, "Module");
    Module::from_path(&hooks, path, module_class, 0)
  }

  #[test]
  fn new() {
    use std::path::PathBuf;

    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);

    let path = PathBuf::from("self/path.ly");

    Module::new(
      hooks.manage_obj(Class::bare(hooks.manage_str("example".to_string()))),
      path,
      0,
    );

    assert!(true);
  }

  #[test]
  fn from_path() {
    use crate::hooks::{GcHooks, NoContext};

    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);

    let path = PathBuf::from("self/path.ly");
    let module_class = test_class(&hooks, "Module");
    let module = Module::from_path(&hooks, path, module_class, 0);

    assert!(module.is_ok());
    assert_eq!(&*module.unwrap().name(), "path");
  }

  #[test]
  fn module_instance() {
    use crate::hooks::{GcHooks, NoContext};
    use crate::value::Value;
    use std::path::PathBuf;

    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);

    let mut module = Module::new(
      hooks.manage_obj(Class::bare(hooks.manage_str("module".to_string()))),
      PathBuf::from("self/module.ly"),
      0,
    );

    let export_name = hooks.manage_str("exported".to_string());
    assert!(module
      .insert_symbol(&hooks, export_name, val!(true))
      .is_ok());
    assert!(module.export_symbol(export_name).is_ok());

    let symbols = module.module_instance(&hooks);

    if let Some(result) = symbols.get_field(export_name) {
      assert_eq!(*result, val!(true));
    } else {
      assert!(false);
    }
  }

  #[test]
  fn insert_module() -> ModuleResult<()> {
    use crate::hooks::{GcHooks, NoContext};

    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);

    let path = PathBuf::from("self");
    let mut module = hooks.manage(test_module(&hooks, path)?);

    let path = PathBuf::from("self/inner");
    let inner_module = hooks.manage(test_module(&hooks, path)?);

    let path = PathBuf::from("other/inner");
    let invalid_parent_module = hooks.manage(test_module(&hooks, path)?);

    let path = PathBuf::from("self/inner/innerer");
    let invalid_depth_module = hooks.manage(test_module(&hooks, path)?);

    assert!(module.insert_module(&hooks, inner_module).is_ok());
    assert_eq!(
      module.insert_module(&hooks, invalid_parent_module),
      Err(ModuleError::ModuleNotDecedent)
    );
    assert_eq!(
      module.insert_module(&hooks, invalid_depth_module),
      Err(ModuleError::ModuleNotDirectDecedent)
    );

    Ok(())
  }

  #[test]
  fn import() -> ModuleResult<()> {
    use crate::hooks::{GcHooks, NoContext};

    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);

    let path = PathBuf::from("self");
    let mut module = hooks.manage(test_module(&hooks, path)?);

    let path = PathBuf::from("self/inner");
    let inner_module = hooks.manage(test_module(&hooks, path)?);
    assert!(module.insert_module(&hooks, inner_module).is_ok());

    assert!(module.import(&hooks, &[hooks.manage_str("inner"),]).is_ok());

    assert_eq!(
      module.import(&hooks, &[hooks.manage_str("other"),]),
      Err(ModuleError::ModuleDoesNotExist)
    );

    Ok(())
  }

  #[test]
  fn import_symbol() -> ModuleResult<()> {
    use crate::hooks::{GcHooks, NoContext};
    use crate::value::Value;

    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);

    let path = PathBuf::from("self");
    let mut module = hooks.manage(test_module(&hooks, path)?);

    let path = PathBuf::from("self/inner");
    let mut inner_module = hooks.manage(test_module(&hooks, path)?);

    let symbol_name1 = hooks.manage_str("test1");
    let symbol_name2 = hooks.manage_str("test2");
    let not_symbol_name = hooks.manage_str("not_test");
    inner_module.insert_symbol(&hooks, symbol_name1, val!(false))?;
    inner_module.export_symbol(symbol_name1)?;
    inner_module.insert_symbol(&hooks, symbol_name2, val!(true))?;

    assert!(module.insert_module(&hooks, inner_module).is_ok());

    assert_eq!(
      module.import_symbol(&hooks, &[hooks.manage_str("inner")], symbol_name1),
      Ok(val!(false))
    );

    assert_eq!(
      module.import_symbol(&hooks, &[hooks.manage_str("inner")], symbol_name2),
      Err(ModuleError::SymbolNotExported)
    );

    assert_eq!(
      module.import_symbol(&hooks, &[hooks.manage_str("other")], symbol_name1),
      Err(ModuleError::ModuleDoesNotExist)
    );

    assert_eq!(
      module.import_symbol(&hooks, &[hooks.manage_str("inner")], not_symbol_name),
      Err(ModuleError::SymbolDoesNotExist)
    );

    Ok(())
  }

  #[test]
  fn export_symbol() {
    use crate::hooks::{GcHooks, NoContext};
    use crate::value::Value;
    use std::path::PathBuf;

    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);

    let mut module = Module::new(
      hooks.manage_obj(Class::bare(hooks.manage_str("module".to_string()))),
      PathBuf::from("self/module.ly"),
      0,
    );

    let export_name = hooks.manage_str("exported".to_string());

    assert!(module
      .insert_symbol(&hooks, export_name, val!(true))
      .is_ok());
    let result1 = module.export_symbol(export_name);
    let result2 = module.export_symbol(export_name);

    assert!(result1.is_ok());
    assert_eq!(result2, Err(ModuleError::SymbolAlreadyExported));
  }

  #[test]
  fn set_symbol() -> ModuleResult<()> {
    use crate::hooks::{GcHooks, NoContext};
    use crate::value::Value;

    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);

    let path = PathBuf::from("self");
    let mut module = hooks.manage(test_module(&hooks, path)?);

    assert_eq!(
      module.set_symbol(hooks.manage_str("does not exist"), val!(false)),
      Err(ModuleError::SymbolDoesNotExist)
    );

    let symbol_name = hooks.manage_str("test");
    module.insert_symbol(&hooks, symbol_name, val!(true))?;

    assert!(module.set_symbol(symbol_name, val!(false)).is_ok());

    assert_eq!(module.get_symbol(symbol_name), Some(val!(false)));

    Ok(())
  }

  #[test]
  fn insert_symbol() {
    use crate::hooks::{GcHooks, NoContext};
    use crate::value::Value;
    use std::path::PathBuf;

    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);

    let mut module = Module::new(
      hooks.manage_obj(Class::bare(hooks.manage_str("module".to_string()))),
      PathBuf::from("self/module.ly"),
      0,
    );

    let name = hooks.manage_str("exported".to_string());
    assert!(module.insert_symbol(&hooks, name, val!(true)).is_ok());

    let symbol = module.get_symbol(name);

    if let Some(result) = symbol {
      assert_eq!(result, val!(true));
    } else {
      assert!(false);
    }
  }

  #[test]
  fn get_symbol() -> ModuleResult<()> {
    use crate::hooks::{GcHooks, NoContext};
    use crate::{val, value::Value};
    use std::path::PathBuf;

    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);

    let path = PathBuf::from("self");
    let mut module = test_module(&hooks, path)?;

    let name = hooks.manage_str("exported".to_string());

    assert!(module.get_symbol(name).is_none());

    module.insert_symbol(&hooks, name, val!(10.0))?;
    assert_eq!(module.get_symbol(name), Some(val!(10.0)));

    Ok(())
  }

  #[test]
  fn get_exported_symbol() -> ModuleResult<()> {
    use crate::hooks::{GcHooks, NoContext};
    use crate::{val, value::Value};
    use std::path::PathBuf;

    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);

    let path = PathBuf::from("self");
    let mut module = test_module(&hooks, path)?;

    let name = hooks.manage_str("exported".to_string());

    assert_eq!(
      module.get_exported_symbol(name),
      Err(ModuleError::SymbolDoesNotExist)
    );

    module.insert_symbol(&hooks, name, val!(10.0))?;
    assert_eq!(
      module.get_exported_symbol(name),
      Err(ModuleError::SymbolNotExported)
    );

    module.export_symbol(name)?;
    assert_eq!(module.get_exported_symbol(name), Ok(val!(10.0)));

    Ok(())
  }
}
