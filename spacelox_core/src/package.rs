use crate::{hooks::Hooks, module::Module, object::SlHashMap, PackageResult};
use hashbrown::hash_map::Entry;
use spacelox_env::{
  managed::{Manage, Managed, Trace},
  stdio::StdIo,
};
use std::fmt;
use std::mem;

/// An object representing an import request from a file
pub struct Import(Vec<Managed<String>>);

impl Import {
  /// Create a new import
  pub fn new(path: Vec<Managed<String>>) -> Self {
    Self(path)
  }

  pub fn from_strs(hooks: &Hooks, path: &str) -> Self {
    let path = path
      .split("/")
      .map(|segment| hooks.manage_str(String::from(segment)))
      .collect();

    Self(path)
  }

  /// Generate a path string from the internal representation
  pub fn path_str(&self) -> String {
    let mut path = format!("{}", self.0[0]);
    for segment in &self.0[1..] {
      path.push_str(&format!("/{}", segment))
    }

    path
  }
}

/// Enum of the entities in a package
#[derive(Clone)]
enum PackageEntity {
  /// A module in a package
  Module(Managed<Module>),

  /// A sub package in this package
  Package(Managed<Package>),
}

#[derive(Clone)]
pub struct Package {
  /// The name of the package
  name: Managed<String>,

  /// A hash of names to sub packages and modules
  entities: SlHashMap<Managed<String>, PackageEntity>,
}

impl Package {
  /// Create a new package
  ///
  /// # Example
  /// ```
  /// use spacelox_core::package::Package;
  /// use spacelox_env::memory::{Gc, NO_GC};
  ///
  /// let gc = Gc::default();
  /// let package = Package::new(gc.manage_str("example".to_string(), &NO_GC));
  /// ```
  pub fn new(name: Managed<String>) -> Self {
    Self {
      name,
      entities: SlHashMap::default(),
    }
  }

  /// Add a module to this package
  ///
  /// # Example
  /// ```
  /// use spacelox_core::package::Package;
  /// use spacelox_core::module::Module;
  /// use spacelox_core::hooks::{NoContext, Hooks, HookContext};
  /// use spacelox_env::memory::{Gc};
  ///
  /// let gc = Gc::default();
  /// let mut context = NoContext::new(&gc);
  /// let hooks = Hooks::new(&mut context);
  ///
  /// let mut package = Package::new(hooks.manage_str("package".to_string()));
  /// let module = hooks.manage(Module::new(hooks.manage_str("module".to_string())));
  ///
  /// let result1 = package.add_module(&hooks, module.clone());
  /// let result2 = package.add_module(&hooks, module);
  ///
  /// assert_eq!(result1.is_ok(), true);
  /// assert_eq!(result2.is_err(), true);
  /// ```
  pub fn add_module(&mut self, hooks: &Hooks, module: Managed<Module>) -> PackageResult<()> {
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
  /// use spacelox_core::hooks::{NoContext, Hooks, HookContext};
  /// use spacelox_env::memory::{Gc};
  ///
  /// let gc = Gc::default();
  /// let mut context = NoContext::new(&gc);
  /// let hooks = Hooks::new(&mut context);
  ///
  /// let mut package = Package::new(hooks.manage_str("package".to_string()));
  /// let sub_package = hooks.manage(Package::new(hooks.manage_str("sub_module".to_string())));
  ///
  /// let result1 = package.add_package(&hooks, sub_package);
  /// let result2 = package.add_package(&hooks, sub_package);
  ///
  /// assert_eq!(result1.is_ok(), true);
  /// assert_eq!(result2.is_err(), true);
  /// ```
  pub fn add_package(&mut self, hooks: &Hooks, sub_package: Managed<Package>) -> PackageResult<()> {
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
  /// use spacelox_core::value::Value;
  /// use spacelox_core::hooks::{NoContext, Hooks, HookContext};
  /// use spacelox_env::memory::{Gc};
  ///
  /// let gc = Gc::default();
  /// let mut context = NoContext::new(&gc);
  /// let hooks = Hooks::new(&mut context);
  ///
  /// let mut module = hooks.manage(Module::new(hooks.manage_str("my_module".to_string())));
  ///
  /// let export_name = hooks.manage_str("exported".to_string());
  /// module.insert_symbol(&hooks, export_name, Value::from(true));
  /// module.export_symbol(&hooks, export_name);
  ///
  /// let mut package = Package::new(hooks.manage_str("my_package".to_string()));
  /// package.add_module(&hooks, module);
  ///
  /// let successful = Import::from_strs(&hooks, "my_package/my_module");
  /// let failing = Import::from_strs(&hooks, "my_package/not_my_module");
  ///
  /// let symbols1 = package.import(&hooks, successful);
  /// let symbols2 = package.import(&hooks, failing);
  ///
  /// assert_eq!(symbols1.is_ok(), true);
  /// assert_eq!(symbols2.is_err(), true);
  ///
  /// if let Ok(result) = symbols1 {
  ///   assert_eq!(result.len(), 1);
  ///   assert_eq!(*result.get_symbol(export_name).unwrap(), Value::from(true));
  /// }
  /// ```
  pub fn import(&self, hooks: &Hooks, import: Import) -> PackageResult<Managed<Module>> {
    if import.0.len() == 0 {
      panic!("No path in import");
    }

    if import.0[0] == self.name {
      self._import(hooks, 1, import)
    } else {
      panic!("import resolution should not have selected this package");
    }
  }

  /// Get a set of symbols from this packages using a requested. This method
  /// is used internally to track how far down the import path has currently been resolved
  fn _import(&self, hooks: &Hooks, depth: usize, import: Import) -> PackageResult<Managed<Module>> {
    if depth >= import.0.len() {
      return Err(hooks.make_error(format!("Could not resolve module {}", import.path_str())));
    }

    let key = import.0[depth];
    match self.entities.get(&key) {
      Some(entity) => match entity {
        PackageEntity::Package(sub_package) => sub_package._import(hooks, depth + 1, import),
        PackageEntity::Module(module) => Ok(*module),
      },
      None => Err(hooks.make_error(format!("Could not resolve module {}", import.path_str()))),
    }
  }
}

impl fmt::Debug for Package {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    f.debug_struct("Package")
      .field("name", &*self.name)
      .field("entities", &"SlHashMap: { ... }")
      .finish()
  }
}

impl Trace for Package {
  fn trace(&self) -> bool {
    self.name.trace();

    self.entities.iter().for_each(|(key, value)| {
      key.trace();

      match value {
        PackageEntity::Module(module) => module.trace(),
        PackageEntity::Package(package) => package.trace(),
      };
    });

    true
  }
  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    self.name.trace_debug(stdio);

    self.entities.iter().for_each(|(key, value)| {
      key.trace();

      match value {
        PackageEntity::Module(module) => module.trace_debug(stdio),
        PackageEntity::Package(package) => package.trace_debug(stdio),
      };
    });

    true
  }
}

impl Manage for Package {
  fn alloc_type(&self) -> &str {
    "package"
  }
  fn debug(&self) -> String {
    format!("{:?}", self)
  }
  fn debug_free(&self) -> String {
    "Package: {{ name: {{...}}, entities: {{...}}}}".to_string()
  }

  fn size(&self) -> usize {
    mem::size_of::<Self>()
      + (mem::size_of::<Managed<String>>() + mem::size_of::<PackageEntity>())
        * (self.entities.capacity())
  }
}
