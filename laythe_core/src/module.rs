use crate::{
  hooks::GcHooks,
  object::{Class, Instance, Map},
  value::Value,
  LyHashSet, LyResult,
};
use laythe_env::managed::{DebugHeap, DebugWrap, Gc, GcStr, Manage, Trace};
use std::{fmt, io::Write};
use std::{mem, path::PathBuf};

/// A struct representing a collection of class functions and variable of shared functionality
#[derive(Clone)]
pub struct Module {
  /// The full filepath to this module
  pub path: Gc<PathBuf>,

  /// The class that represents this module when imported
  module_class: Gc<Class>,

  /// A key value set of named exports from the provided modules
  exports: LyHashSet<GcStr>,

  /// All of the top level symbols in this module
  symbols: Map<GcStr, Value>,
}

impl Module {
  /// Create a new laythe module
  pub fn new(module_class: Gc<Class>, path: Gc<PathBuf>) -> Self {
    Module {
      path,
      module_class,
      exports: LyHashSet::default(),
      symbols: Map::default(),
    }
  }

  /// Get the name of this module
  pub fn name(&self) -> GcStr {
    self.module_class.name
  }

  /// Create a module from a filepath
  pub fn from_path(hooks: &GcHooks, path: Gc<PathBuf>) -> Result<Self, GcStr> {
    let module_name = path.file_stem().and_then(|m| m.to_str());

    let module_name = match module_name {
      Some(module_name) => module_name,
      None => {
        return Err(hooks.manage_str(format!(
          "Could not create module from {}, path malformed.",
          path.to_str().unwrap_or("invalid path")
        )))
      }
    };

    let name = hooks.manage_str(module_name);

    Ok(Self {
      path,
      module_class: hooks.manage(Class::bare(name)),
      exports: LyHashSet::default(),
      symbols: Map::default(),
    })
  }

  /// Add export a new symbol from this module. Exported names must be unique
  pub fn export_symbol(&mut self, hooks: &GcHooks, name: GcStr) -> Result<(), GcStr> {
    if self.exports.contains(&name) {
      Err(hooks.manage_str(format!(
        "{} has already been exported from {}",
        name,
        self.name()
      )))
    } else {
      hooks.grow(self, |module| {
        module.exports.insert(name);
      });
      self.module_class.add_field(hooks, name);
      Ok(())
    }
  }

  /// Get a reference to all exported symbols in this module
  pub fn import(&self, hooks: &GcHooks) -> Gc<Instance> {
    let class = self.module_class;

    let mut import = hooks.manage(Instance::new(class));
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

  /// Insert a symbol into this module's symbol table
  pub fn insert_symbol(&mut self, hooks: &GcHooks, name: GcStr, symbol: Value) -> Option<Value> {
    hooks.grow(self, |module| module.symbols.insert(name, symbol))
  }

  /// Get a symbol from this module's symbol table
  pub fn get_symbol(&self, name: GcStr) -> Option<&Value> {
    self.symbols.get(&name)
  }

  /// how many symbols are in this module
  pub fn len(&self) -> usize {
    self.symbols.len()
  }

  /// Is this module empty
  pub fn is_empty(&self) -> bool {
    self.symbols.is_empty()
  }

  /// Remove a symbol from this module
  pub fn remove_symbol(&mut self, hooks: &GcHooks, name: GcStr) {
    hooks.shrink(self, |module| {
      module.symbols.remove(&name);
      module.exports.remove(&name);
    });
  }

  /// Transfer the export symbols to another module
  pub fn transfer_exported(&self, hooks: &GcHooks, other: &mut Module) -> LyResult<()> {
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

    LyResult::Ok(())
  }
}

impl Trace for Module {
  fn trace(&self) {
    self.module_class.trace();
    self.path.trace();

    self.exports.iter().for_each(|key| {
      key.trace();
    });
    self.symbols.iter().for_each(|(key, value)| {
      key.trace();
      value.trace();
    });
  }
  fn trace_debug(&self, stdout: &mut dyn Write) {
    self.module_class.trace();
    self.path.trace_debug(stdout);

    self.exports.iter().for_each(|key| {
      key.trace_debug(stdout);
    });
    self.symbols.iter().for_each(|(key, value)| {
      key.trace_debug(stdout);
      value.trace_debug(stdout);
    });
  }
}

impl DebugHeap for Module {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    let depth = depth.saturating_sub(1);

    f.debug_struct("Module")
      .field("path", &DebugWrap(&self.path, depth))
      .field("module_class", &DebugWrap(&self.module_class, depth))
      .field("exports", &DebugWrap(&self.exports, depth))
      .field("symbols", &DebugWrap(&self.symbols, depth))
      .finish()
  }
}

impl Manage for Module {
  fn size(&self) -> usize {
    mem::size_of::<Self>()
      + (mem::size_of::<GcStr>() + mem::size_of::<Value>()) * self.symbols.capacity()
      + mem::size_of::<GcStr>() * self.exports.capacity()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

#[cfg(test)]
mod test {
  use crate::{
    hooks::{GcHooks, NoContext},
    object::Class,
    val,
  };

  #[test]
  fn new() {
    use crate::module::Module;
    use std::path::PathBuf;

    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);

    let path = PathBuf::from("self/path.ly");

    Module::new(
      hooks.manage(Class::bare(hooks.manage_str("example".to_string()))),
      hooks.manage(path),
    );

    assert!(true);
  }

  #[test]
  fn from_path() {
    use crate::hooks::{GcHooks, NoContext};
    use crate::module::Module;
    use std::path::PathBuf;

    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);

    let path = hooks.manage(PathBuf::from("self/path.ly"));
    let module = Module::from_path(&hooks, path);

    assert!(module.is_ok());
    assert_eq!(&*module.unwrap().name(), "path");
  }

  #[test]
  fn export_symbol() {
    use crate::hooks::{GcHooks, NoContext};
    use crate::module::Module;
    use crate::value::Value;
    use std::path::PathBuf;

    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);

    let mut module = Module::new(
      hooks.manage(Class::bare(hooks.manage_str("module".to_string()))),
      hooks.manage(PathBuf::from("self/module.ly")),
    );

    let export_name = hooks.manage_str("exported".to_string());

    module.insert_symbol(&hooks, export_name, val!(true));
    let result1 = module.export_symbol(&hooks, export_name);
    let result2 = module.export_symbol(&hooks, export_name);

    assert_eq!(result1.is_ok(), true);
    assert_eq!(result2.is_err(), true);
  }

  #[test]
  fn import() {
    use crate::hooks::{GcHooks, NoContext};
    use crate::module::Module;
    use crate::value::Value;
    use std::path::PathBuf;

    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);

    let mut module = Module::new(
      hooks.manage(Class::bare(hooks.manage_str("module".to_string()))),
      hooks.manage(PathBuf::from("self/module.ly")),
    );

    let export_name = hooks.manage_str("exported".to_string());
    module.insert_symbol(&hooks, export_name, val!(true));
    assert!(module.export_symbol(&hooks, export_name).is_ok());

    let symbols = module.import(&hooks);

    if let Some(result) = symbols.get_field(&export_name) {
      assert_eq!(*result, val!(true));
    } else {
      assert!(false);
    }
  }

  #[test]
  fn insert_symbol() {
    use crate::hooks::{GcHooks, NoContext};
    use crate::module::Module;
    use crate::value::Value;
    use std::path::PathBuf;

    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);

    let mut module = Module::new(
      hooks.manage(Class::bare(hooks.manage_str("module".to_string()))),
      hooks.manage(PathBuf::from("self/module.ly")),
    );

    let name = hooks.manage_str("exported".to_string());
    module.insert_symbol(&hooks, name, val!(true));

    let symbol = module.get_symbol(name);

    if let Some(result) = symbol {
      assert_eq!(*result, val!(true));
    } else {
      assert!(false);
    }
  }

  #[test]
  fn get_symbol() {
    use crate::hooks::{GcHooks, NoContext};
    use crate::module::Module;
    use std::path::PathBuf;

    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);

    let module = Module::new(
      hooks.manage(Class::bare(hooks.manage_str("module".to_string()))),
      hooks.manage(PathBuf::from("self/module.ly")),
    );

    let name = hooks.manage_str("exported".to_string());
    let symbol = module.get_symbol(name);

    if let Some(_) = symbol {
      assert!(false);
    } else {
      assert!(true);
    }
  }
}
