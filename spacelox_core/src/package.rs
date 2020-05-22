use crate::{
  hooks::Hooks, managed::Managed, module::Module, value::Value, PackageResult, SlHashMap,
  SymbolResult,
};
use hashbrown::{hash_map::Entry, HashMap};

/// An object representing an import request from a file
pub struct Import {
  /// The path as sequence of string segments
  pub path: Vec<Managed<String>>,

  /// The symbols that have been requested for import
  pub symbols: Vec<Managed<String>>,
}

impl Import {
  /// Create a new import
  pub fn new(path: Vec<Managed<String>>, symbols: Vec<Managed<String>>) -> Self {
    Self { path, symbols }
  }

  pub fn from_strs(hooks: &Hooks, symbols: &str, path: &str) -> Self {
    let symbols = symbols
      .split(",")
      .map(|str| hooks.manage_str(String::from(str)))
      .collect();

    let path = path
      .split("/")
      .map(|segment| hooks.manage_str(String::from(segment)))
      .collect();

    Self { symbols, path }
  }

  /// Generate a path string from the internal representation
  pub fn path_str(&self) -> String {
    let mut path = format!("{}", self.path[0]);
    for segment in &self.path[1..] {
      path.push_str(&format!("/{}", segment))
    }

    path
  }
}

/// Enum of the entities in a package
#[derive(Clone)]
enum PackageEntity {
  /// A module in a package
  Module(Module),

  /// A sub package in this package
  Package(Package),
}

#[derive(Clone)]
pub struct Package {
  /// The name of the package
  name: Managed<String>,

  /// A hash of names to sub packages and modules
  entities: HashMap<Managed<String>, PackageEntity>,
}

impl Package {
  /// Create a new package
  ///
  /// # Example
  /// ```
  /// use spacelox_core::package::Package;
  /// use spacelox_core::memory::{Gc, NO_GC};
  ///
  /// let gc = Gc::default();
  /// let package = Package::new(gc.manage_str(String::from("example"), &NO_GC));
  /// ```
  pub fn new(name: Managed<String>) -> Self {
    Self {
      name,
      entities: HashMap::with_hasher(Default::default()),
    }
  }

  /// Add a module to this package
  ///
  /// # Example
  /// ```
  /// use spacelox_core::package::Package;
  /// use spacelox_core::module::Module;
  /// use spacelox_core::memory::{Gc};
  /// use spacelox_core::hooks::{NoContext, Hooks, HookContext};
  ///
  /// let gc = Gc::default();
  /// let mut context = NoContext::new(&gc);
  /// let hooks = Hooks::new(&mut context);
  ///
  /// let mut package = Package::new(hooks.manage_str(String::from("package")));
  /// let module = Module::new(hooks.manage_str(String::from("module")));
  ///
  /// let result1 = package.add_module(&hooks, module.clone());
  /// let result2 = package.add_module(&hooks, module);
  ///
  /// assert_eq!(result1.is_ok(), true);
  /// assert_eq!(result2.is_err(), true);
  /// ```
  pub fn add_module(&mut self, hooks: &Hooks, module: Module) -> PackageResult<()> {
    match self.entities.entry(module.name) {
      Entry::Occupied(_) => Err(hooks.make_error(format!(
        "Cannot add module {} to package {}",
        module.name, self.name
      ))),
      Entry::Vacant(entry) => {
        entry.insert(PackageEntity::Module(module));
        Ok(())
      }
    }
  }

  /// Add a sub package to this package
  ///
  /// # Example
  /// ```
  /// use spacelox_core::package::Package;
  /// use spacelox_core::memory::{Gc};
  /// use spacelox_core::hooks::{NoContext, Hooks, HookContext};
  ///
  /// let gc = Gc::default();
  /// let mut context = NoContext::new(&gc);
  /// let hooks = Hooks::new(&mut context);
  ///
  /// let mut package = Package::new(hooks.manage_str(String::from("package")));
  /// let sub_package = Package::new(hooks.manage_str(String::from("sub_module")));
  ///
  /// let result1 = package.add_sub_package(&hooks, sub_package.clone());
  /// let result2 = package.add_sub_package(&hooks, sub_package);
  ///
  /// assert_eq!(result1.is_ok(), true);
  /// assert_eq!(result2.is_err(), true);
  /// ```
  pub fn add_sub_package(&mut self, hooks: &Hooks, sub_package: Package) -> PackageResult<()> {
    match self.entities.entry(sub_package.name) {
      Entry::Occupied(_) => Err(hooks.make_error(format!(
        "Cannot add sub package {} to package {}",
        sub_package.name, self.name
      ))),
      Entry::Vacant(entry) => {
        entry.insert(PackageEntity::Package(sub_package));
        Ok(())
      }
    }
  }

  /// Get a set of symbols from this package using a requested import. This
  /// operation can fail if some or all of the symbols are not found.
  ///
  /// # Example
  /// ```
  /// use spacelox_core::module::Module;
  /// use spacelox_core::package::{Package, Import};
  /// use spacelox_core::memory::{Gc};
  /// use spacelox_core::value::Value;
  /// use spacelox_core::hooks::{NoContext, Hooks, HookContext};
  ///
  /// let gc = Gc::default();
  /// let mut context = NoContext::new(&gc);
  /// let hooks = Hooks::new(&mut context);
  ///
  /// let mut module = Module::new(hooks.manage_str(String::from("my_module")));
  ///
  /// let export_name = hooks.manage_str(String::from("exported"));
  /// module.add_export(&hooks, export_name, Value::from(true));
  ///
  /// let mut package = Package::new(hooks.manage_str(String::from("my_package")));
  /// package.add_module(&hooks, module);
  ///
  /// let successful = Import::from_strs(&hooks, "exported", "my_package/my_module");
  /// let failing = Import::from_strs(&hooks, "exported", "my_package/not_my_module");
  ///
  /// let symbols1 = package.get_symbols(&hooks, successful);
  /// let symbols2 = package.get_symbols(&hooks, failing);
  ///
  /// assert_eq!(symbols1.is_ok(), true);
  /// assert_eq!(symbols2.is_err(), true);
  ///
  /// if let Ok(result) = symbols1 {
  ///   assert_eq!(result.len(), 1);
  ///   assert_eq!(*result.get(&export_name).unwrap(), Value::from(true));
  /// }
  /// ```
  pub fn get_symbols(&self, hooks: &Hooks, import: Import) -> SymbolResult {
    if import.symbols.len() == 0 {
      panic!("No symbols imported");
    }

    if import.path.len() == 0 {
      panic!("No path in import");
    }

    if import.path[0] == self.name {
      self._get_symbols(hooks, 1, import)
    } else {
      panic!("import resolution should not have selected this package");
    }
  }

  pub fn get_all_symbols(&self) -> SymbolResult {
    let mut result = HashMap::with_hasher(Default::default());

    self._get_all_symbols(&mut result);
    Ok(result)
  }

  fn _get_all_symbols(&self, results: &mut SlHashMap<Managed<String>, Value>) {
    self.entities.iter().for_each(|(_, entity)| {
      match entity {
        PackageEntity::Module(module) => {
          module.get_all_symbols().iter().for_each(|(name, symbol)| {
            let replaced = results.insert(*name, *symbol);

            // since this should only be used in std check we don't have multiple
            // of the same name
            debug_assert!(
              replaced.is_none(),
              "{} has multiple symbols with name {}",
              self.name,
              name
            );
          });
        }
        PackageEntity::Package(package) => {
          package._get_all_symbols(results);
        }
      }
    })
  }

  /// Get a set of symbols from this packages using a requested. This method
  /// is used internally to track how far down the import path has currently been resolved
  fn _get_symbols(&self, hooks: &Hooks, depth: usize, import: Import) -> SymbolResult {
    if depth >= import.path.len() {
      return Err(hooks.make_error(format!("Could not resolve module {}", import.path_str())));
    }

    let key = import.path[depth];
    match self.entities.get(&key) {
      Some(entity) => match entity {
        PackageEntity::Package(sub_package) => sub_package._get_symbols(hooks, depth + 1, import),
        PackageEntity::Module(module) => module.get_symbols(hooks, &import.symbols),
      },
      None => Err(hooks.make_error(format!("Could not resolve module {}", import.path_str()))),
    }
  }
}
