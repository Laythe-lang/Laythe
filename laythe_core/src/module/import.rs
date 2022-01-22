use super::{ModuleError, ModuleResult};
use crate::{
  hooks::GcHooks,
  managed::{AllocResult, Allocate, Array, DebugHeap, DebugWrap, Gc, GcStr, Trace},
};

/// An object representing an import request from a file
pub struct Import {
  package: GcStr,
  path: Array<GcStr>,
}

impl Import {
  /// Create a new import
  pub fn new(package: GcStr, path: Array<GcStr>) -> Self {
    Self { package, path }
  }

  /// Get the package name
  pub fn package(&self) -> GcStr {
    self.package
  }

  pub fn path(&self) -> &[GcStr] {
    &self.path
  }

  /// Generate an import from a path
  pub fn from_str(hooks: &GcHooks, path: &str) -> ModuleResult<Gc<Self>> {
    let segments: Vec<&str> = path.trim_end_matches(".ly").split('/').collect();

    if let Some((package, path_slice)) = segments.split_first() {
      let package = hooks.manage_str(package);
      hooks.push_root(package);

      let mut path: Vec<GcStr> = Vec::with_capacity(path.len());
      for segment in path_slice {
        let segment = hooks.manage_str(segment);
        path.push(segment);
        hooks.push_root(segment);
      }

      let import = hooks.manage(Self::new(package, hooks.manage(&*path)));
      hooks.pop_roots(path.len());

      Ok(import)
    } else {
      Err(ModuleError::InvalidImport)
    }
  }
}

impl Trace for Import {
  fn trace(&self) {
    self.package.trace();
    self.path.trace();
  }

  fn trace_debug(&self, log: &mut dyn std::io::Write) {
    self.package.trace_debug(log);
    self.path.trace_debug(log);
  }
}

impl Allocate<Gc<Self>> for Import {
  fn alloc(self) -> AllocResult<Gc<Self>> {
    Gc::alloc_result(self)
  }
}

impl DebugHeap for Import {
  fn fmt_heap(&self, f: &mut std::fmt::Formatter, depth: usize) -> std::fmt::Result {
    f.debug_struct("Import")
      .field("package", &DebugWrap(&self.package, depth))
      .field("path", &DebugWrap(&self.path, depth))
      .finish()
  }
}
