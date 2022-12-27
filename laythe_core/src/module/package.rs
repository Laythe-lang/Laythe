use super::{error::ImportResult, import::Import, ImportError, Module};
use crate::managed::{AllocResult, Allocate, DebugHeap, DebugWrap, Gc, GcStr, Trace};
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

  /// Attempt to import a module from the provided import object
  pub fn import(&self, import: Gc<Import>) -> ImportResult<Gc<Module>> {
    if import.package() == self.name {
      let path = import.path();
      if path.is_empty() {
        Ok(self.root_module)
      } else {
        self.root_module.import(import.path())
      }
    } else {
      Err(ImportError::PackageDoesNotMatch)
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
  use super::Package;
  use crate::{
    hooks::{GcHooks, NoContext},
    support::test_module,
  };

  #[test]
  fn new() {
    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);

    let module = test_module(&hooks, "package");
    Package::new(hooks.manage_str("package"), module);
  }

  #[test]
  fn name() {
    let mut context = NoContext::default();
    let hooks = GcHooks::new(&mut context);
    let name = "example";

    let module = test_module(&hooks, name);
    let package = Package::new(hooks.manage_str(name), module);

    assert_eq!(&*package.name(), name);
  }
}
