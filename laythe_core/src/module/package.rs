use super::{error::ModuleResult, import::Import, Module, ModuleError};
use crate::{
  hooks::GcHooks,
  managed::{AllocResult, Allocate, DebugHeap, DebugWrap, Gc, GcStr, Trace, Instance},
  value::Value,
};
use std::{fmt, io::Write};

#[derive(Clone)]
pub struct Package {
  /// The name of the package
  name: GcStr,

  /// A hash of names to sub packages and modules
  root_module: Gc<Module>,
}

impl Package {
  /// Create a new package
  pub fn new(name: GcStr, root_module: Gc<Module>) -> Self {
    assert_eq!(name, root_module.name());
    Self { name, root_module }
  }

  /// Retrieve the name of this package
  pub fn name(&self) -> GcStr {
    self.name
  }

  /// Retrieve the root module of this package
  pub fn root_module(&self) -> Gc<Module> {
    self.root_module
  }

  /// Get a set of symbols from this package using a requested import. This
  /// operation can fail if some or all of the symbols are not found.
  pub fn import(&self, hooks: &GcHooks, import: Gc<Import>) -> ModuleResult<Instance> {
    if import.package() == self.name {
      self.root_module.import(hooks, import.path())
    } else {
      Err(ModuleError::PackageDoesNotMatch)
    }
  }

  /// Get a set of symbols from this package using a requested import. This
  /// operation can fail if some or all of the symbols are not found.
  pub fn import_symbol(
    &self,
    hooks: &GcHooks,
    import: Gc<Import>,
    name: GcStr,
  ) -> ModuleResult<Value> {
    if import.package() == self.name {
      self.root_module.import_symbol(hooks, import.path(), name)
    } else {
      Err(ModuleError::PackageDoesNotMatch)
    }
  }
}

impl fmt::Debug for Package {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.fmt_heap(f, 2)
  }
}

impl Trace for Package {
  fn trace(&self) {
    self.name.trace();
    self.root_module.trace();
  }

  fn trace_debug(&self, log: &mut dyn Write) {
    self.name.trace_debug(log);
    self.root_module.trace_debug(log);
  }
}

impl DebugHeap for Package {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    f.debug_struct("Package")
      .field("name", &DebugWrap(&self.name, depth))
      .field("module", &DebugWrap(&self.root_module, depth))
      .finish()
  }
}

impl Allocate<Gc<Self>> for Package {
  fn alloc(self) -> AllocResult<Gc<Self>> {
    Gc::alloc_result(self)
  }
}

#[cfg(test)]
mod test {
  use std::path::PathBuf;

  use super::Package;
  use crate::{
    managed::Gc,
    memory::{Allocator, NO_GC},
    module::{Import, Module, ModuleError},
    object::Class,
    support::test_class,
    val,
  };

  fn test_module(alloc: &mut Allocator, name: &str) -> Gc<Module> {
    let name = alloc.manage_str(name, &NO_GC);
    let class = alloc.manage_obj(Class::bare(name), &NO_GC);
    alloc.manage(Module::new(class, PathBuf::new(), 0), &NO_GC)
  }

  #[test]
  fn new() {
    let mut gc = Allocator::default();
    let name = "example";

    let module = test_module(&mut gc, name);
    Package::new(gc.manage_str(name, &NO_GC), module);
  }

  #[test]
  fn name() {
    let mut gc = Allocator::default();
    let name = "example";

    let module = test_module(&mut gc, name);
    let package = Package::new(gc.manage_str(name, &NO_GC), module);

    assert_eq!(&*package.name(), name);
  }

  #[test]
  fn import() {
    use crate::hooks::{GcHooks, NoContext};
    use crate::value::Value;
    use std::path::PathBuf;

    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);

    let module_class = test_class(&hooks, "Module");

    let mut module = hooks
      .manage(Module::from_path(&hooks, PathBuf::from("my_package"), module_class, 0).unwrap());
    let mut inner_module = hooks.manage(
      Module::from_path(
        &hooks,
        PathBuf::from("my_package/my_module"),
        module_class,
        0,
      )
      .unwrap(),
    );

    let export_name = hooks.manage_str("exported".to_string());
    assert!(inner_module
      .insert_symbol(&hooks, export_name, val!(true))
      .is_ok());
    assert!(inner_module.export_symbol(export_name).is_ok());
    assert!(module.insert_module(&hooks, inner_module).is_ok());

    let package = Package::new(hooks.manage_str("my_package".to_string()), module);

    let successful = Import::from_str(&hooks, "my_package/my_module").unwrap();
    let failing = Import::from_str(&hooks, "my_package/not_my_module").unwrap();

    let symbols1 = package.import(&hooks, successful);
    let symbols2 = package.import(&hooks, failing);

    assert_eq!(symbols1.is_ok(), true);
    assert_eq!(symbols2, Err(ModuleError::ModuleDoesNotExist));

    if let Ok(result) = symbols1 {
      assert_eq!(*result.get_field(export_name).unwrap(), val!(true));
    }
  }

  #[test]
  fn import_symbol() {
    use crate::hooks::{GcHooks, NoContext};
    use crate::value::Value;
    use std::path::PathBuf;

    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);

    let module_class = test_class(&hooks, "Module");

    let mut module = hooks
      .manage(Module::from_path(&hooks, PathBuf::from("my_package"), module_class, 0).unwrap());
    let mut inner_module = hooks.manage(
      Module::from_path(
        &hooks,
        PathBuf::from("my_package/my_module"),
        module_class,
        0,
      )
      .unwrap(),
    );

    let export_name = hooks.manage_str("exported".to_string());
    assert!(inner_module
      .insert_symbol(&hooks, export_name, val!(true))
      .is_ok());
    assert!(inner_module.export_symbol(export_name).is_ok());
    assert!(module.insert_module(&hooks, inner_module).is_ok());

    let package = Package::new(hooks.manage_str("my_package".to_string()), module);

    let successful = Import::from_str(&hooks, "my_package/my_module").unwrap();
    let failing = Import::from_str(&hooks, "my_package/not_my_module").unwrap();

    let symbols1 = package.import_symbol(&hooks, successful, export_name);
    let symbols2 = package.import_symbol(&hooks, failing, export_name);

    assert_eq!(symbols1, Ok(val!(true)));
    assert_eq!(symbols2, Err(ModuleError::ModuleDoesNotExist));
  }
}
