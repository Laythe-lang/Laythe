use std::mem;

use super::{ModuleError, ModuleResult};
use crate::{hooks::GcHooks, object::List};
use laythe_env::managed::{DebugHeap, DebugWrap, Gc, GcStr, Manage, Trace};

/// An object representing an import request from a file
pub struct Import {
  package: GcStr,
  path: Gc<List<GcStr>>,
}

impl Import {
  /// Create a new import
  pub fn new(package: GcStr, path: Gc<List<GcStr>>) -> Self {
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

      let path: Gc<List<GcStr>> = hooks.manage(List::with_capacity(path.len()));
      hooks.push_root(path);

      let mut import = hooks.manage(Self::new(package, path));
      hooks.push_root(import);

      import
        .path
        .extend(path_slice.iter().map(|segment| hooks.manage_str(segment)));

      hooks.pop_roots(3);
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

impl Manage for Import {
  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }

  fn as_debug(&self) -> &dyn laythe_env::managed::DebugHeap {
    self
  }
}

impl DebugHeap for Import {
  fn fmt_heap(&self, f: &mut std::fmt::Formatter, depth: usize) -> std::fmt::Result {
    let depth = depth.saturating_sub(1);

    f.debug_struct("Import")
      .field("package", &DebugWrap(&self.package, depth))
      .field("path", &DebugWrap(&self.path, depth))
      .finish()
  }
}
