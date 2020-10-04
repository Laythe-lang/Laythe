use crate::constants::IMPORT_SEPARATOR;
use laythe_core::{
  hooks::GcHooks,
  module::Module,
  object::{BuiltIn, BuiltInDependencies, BuiltinPrimitives, Map},
  package::{Import, Package},
  LyResult,
};
use laythe_env::{
  io::Io,
  managed::{DebugHeap, DebugWrap, Manage, Managed, Trace},
};
use smol_str::SmolStr;
use std::{fmt, io::Write, mem, path::PathBuf};

pub struct DepManager {
  /// The directory of the entry point
  pub src_dir: Managed<PathBuf>,

  /// interface to the current environments io
  io: Io,

  /// A collection of builtin in classes, values and functions
  builtin: BuiltIn,

  /// A collection of packages that have already been loaded
  packages: Map<Managed<SmolStr>, Managed<Package>>,

  /// A cache for full filepath to individual modules
  cache: Map<Managed<SmolStr>, Managed<Module>>,
}

impl DepManager {
  /// Create a new dependency manager
  pub fn new(io: Io, builtin: BuiltIn, src_dir: Managed<PathBuf>) -> Self {
    Self {
      io,
      src_dir,
      builtin,
      packages: Map::default(),
      cache: Map::default(),
    }
  }

  /// Get the class for primitives in laythe
  pub fn primitive_classes(&self) -> &BuiltinPrimitives {
    &self.builtin.primitives
  }

  /// Get the classes for dependency entities in laythe
  pub fn dependency_classes(&self) -> &BuiltInDependencies {
    &self.builtin.dependencies
  }

  /// Import a module using the given path
  pub fn import(
    &mut self,
    hooks: &GcHooks,
    module: Managed<Module>,
    path: Managed<SmolStr>,
  ) -> LyResult<Managed<Module>> {
    let mut module_dir = (*module.path).clone();
    module_dir.pop();

    // determine relative position of module relative to the src directory
    let relative = match self.io.fs().relative_path(&*self.src_dir, &module_dir) {
      Ok(relative) => relative,
      Err(err) => return hooks.error(err.to_string()),
    };

    // split path into segments
    let relative: Vec<String> = relative
      .ancestors()
      .map(|p| p.display().to_string())
      .filter(|p| p != "")
      .collect();

    // split import into segments and join to relative path
    let resolved_segments: Vec<String> = if &path[..1] == "." {
      relative
        .into_iter()
        .rev()
        .chain(path.split(IMPORT_SEPARATOR).map(|s| s.to_string()))
        .collect()
    } else {
      path
        .split(IMPORT_SEPARATOR)
        .map(|s| s.to_string())
        .collect()
    };

    // let resolved_segments: Vec<String> = relative
    //   .into_iter()
    //   .rev()
    //   .chain(path.split(IMPORT_SEPARATOR).map(|s| s.to_string()))
    //   .collect();

    // check if fully resolved module has already been loaded
    let resolved = hooks.manage_str(resolved_segments.join("/"));
    if let Some(module) = self.cache.get(&resolved) {
      return Ok(*module);
    }

    // generate a new import object
    let import = Import::new(
      resolved_segments
        .iter()
        .map(|segment| hooks.manage_str(segment))
        .collect(),
    );

    // match by package then resolve inside the package
    match import.package() {
      Some(pkg) => match self.packages.get(&pkg) {
        Some(package) => package.import(hooks, import),
        None => hooks.error(format!("Package {} does not exist", pkg)),
      },
      None => hooks.error("Import needs to be prepended with a package name".to_string()),
    }
  }

  /// Add a package to the dependency manager
  pub fn add_package(&mut self, package: Managed<Package>) {
    self.packages.insert(package.name(), package);
  }
}

impl fmt::Debug for DepManager {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("DepManager")
      .field("io", &self.io)
      .field("src_directory", &self.src_dir)
      .field("packages", &"LyHashMap { ... })")
      .field("cache", &"LyHashMap { ... }")
      .finish()
  }
}

impl Trace for DepManager {
  fn trace(&self) -> bool {
    self.src_dir.trace();
    self.packages.iter().for_each(|(key, value)| {
      key.trace();
      value.trace();
    });

    true
  }

  fn trace_debug(&self, stdout: &mut dyn Write) -> bool {
    self.src_dir.trace_debug(stdout);
    self.packages.iter().for_each(|(key, value)| {
      key.trace_debug(stdout);
      value.trace_debug(stdout);
    });

    true
  }
}

impl DebugHeap for DepManager {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    let depth = depth.checked_sub(1).unwrap_or(0);

    f.debug_struct("DepManager")
      .field("io", &self.io)
      .field("src_directory", &DebugWrap(&self.src_dir, depth))
      .field("packages", &DebugWrap(&self.packages, depth))
      .field("cache", &DebugWrap(&self.cache, depth))
      .finish()
  }
}

impl Manage for DepManager {
  fn alloc_type(&self) -> &str {
    "Dependency Manager"
  }

  fn size(&self) -> usize {
    mem::size_of::<Self>()
      + self.cache.capacity() * 2 * mem::size_of::<Managed<SmolStr>>()
      + self.packages.capacity() * 2 * mem::size_of::<Managed<SmolStr>>()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}
