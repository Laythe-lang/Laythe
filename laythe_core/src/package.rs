use crate::{hooks::GcHooks, module::Module, object::Map, LyResult};
use hashbrown::hash_map::{Entry, Iter};
use laythe_env::{
  managed::{Manage, Managed, Trace},
  stdio::Stdio,
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

  /// Get the package name
  pub fn package(&self) -> Option<Managed<String>> {
    self.0.first().copied()
  }

  pub fn from_str(hooks: &GcHooks, path: &str) -> Self {
    let path = path
      .trim_end_matches(".ly")
      .split('/')
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
pub enum PackageEntity {
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
  entities: Map<Managed<String>, PackageEntity>,
}

impl Package {
  /// Create a new package
  pub fn new(name: Managed<String>) -> Self {
    Self {
      name,
      entities: Map::default(),
    }
  }

  /// Retrieve the name of this package
  pub fn name(&self) -> Managed<String> {
    self.name
  }

  /// Add a module to this package
  pub fn add_module(&mut self, hooks: &GcHooks, module: Managed<Module>) -> LyResult<()> {
    match self.entities.entry(module.name()) {
      Entry::Occupied(_) => Err(hooks.make_error(format!(
        "Cannot add module {} to package {}",
        module.name(),
        self.name
      ))),
      Entry::Vacant(entry) => {
        entry.insert(PackageEntity::Module(module));
        Ok(())
      }
    }
  }

  /// Add a sub package to this package
  pub fn add_package(&mut self, hooks: &GcHooks, sub_package: Managed<Package>) -> LyResult<()> {
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

  /// Get an iterator to the elements in this package
  pub fn entities(&self) -> Iter<'_, Managed<String>, PackageEntity> {
    self.entities.iter()
  }

  /// Get a set of symbols from this package using a requested import. This
  /// operation can fail if some or all of the symbols are not found.
  pub fn import(&self, hooks: &GcHooks, import: Import) -> LyResult<Managed<Module>> {
    if import.0.is_empty() {
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
  fn _import(&self, hooks: &GcHooks, depth: usize, import: Import) -> LyResult<Managed<Module>> {
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
      .field("entities", &"LyHashMap: { ... }")
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
  fn trace_debug(&self, stdio: &mut Stdio) -> bool {
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

#[cfg(test)]
mod test {
  use crate::object::Class;

  #[test]
  fn new() {
    use crate::package::Package;
    use laythe_env::memory::{Gc, NO_GC};

    let gc = Gc::default();
    Package::new(gc.manage_str("example".to_string(), &NO_GC));
  }

  #[test]
  fn name() {
    use crate::package::Package;
    use laythe_env::memory::{Gc, NO_GC};

    let gc = Gc::default();
    let package = Package::new(gc.manage_str("example".to_string(), &NO_GC));

    assert_eq!(&*package.name(), "example");
  }

  #[test]
  fn add_module() {
    use crate::hooks::{support::TestContext, GcHooks};
    use crate::module::Module;
    use crate::package::Package;
    use std::path::PathBuf;

    let mut context = TestContext::default();
    let hooks = GcHooks::new(&mut context);

    let mut package = Package::new(hooks.manage_str("package".to_string()));
    let module = hooks.manage(Module::new(
      hooks.manage(Class::bare(hooks.manage_str("module".to_string()))),
      hooks.manage(PathBuf::from("self/module.ly")),
    ));

    let result1 = package.add_module(&hooks, module.clone());
    let result2 = package.add_module(&hooks, module);

    assert_eq!(result1.is_ok(), true);
    assert_eq!(result2.is_err(), true);
  }

  #[test]
  fn add_package() {
    use crate::hooks::{support::TestContext, GcHooks};
    use crate::package::Package;

    let mut context = TestContext::default();
    let hooks = GcHooks::new(&mut context);

    let mut package = Package::new(hooks.manage_str("package".to_string()));
    let sub_package = hooks.manage(Package::new(hooks.manage_str("sub_module".to_string())));

    let result1 = package.add_package(&hooks, sub_package);
    let result2 = package.add_package(&hooks, sub_package);

    assert_eq!(result1.is_ok(), true);
    assert_eq!(result2.is_err(), true);
  }

  #[test]
  fn import() {
    use crate::hooks::{support::TestContext, GcHooks};
    use crate::module::Module;
    use crate::package::{Import, Package};
    use crate::value::Value;
    use std::path::PathBuf;

    let mut context = TestContext::default();
    let hooks = GcHooks::new(&mut context);

    let mut module = hooks.manage(
      Module::from_path(
        &hooks,
        hooks.manage(PathBuf::from("my_package/my_module.ly")),
      )
      .unwrap(),
    );
    let export_name = hooks.manage_str("exported".to_string());
    module.insert_symbol(&hooks, export_name, Value::from(true));
    assert!(module.export_symbol(&hooks, export_name).is_ok());

    let mut package = Package::new(hooks.manage_str("my_package".to_string()));
    assert!(package.add_module(&hooks, module).is_ok());

    let successful = Import::from_str(&hooks, "my_package/my_module");
    let failing = Import::from_str(&hooks, "my_package/not_my_module");

    let symbols1 = package.import(&hooks, successful);
    let symbols2 = package.import(&hooks, failing);

    assert_eq!(symbols1.is_ok(), true);
    assert_eq!(symbols2.is_err(), true);

    if let Ok(result) = symbols1 {
      assert_eq!(result.len(), 1);
      assert_eq!(*result.get_symbol(export_name).unwrap(), Value::from(true));
    }
  }
}
