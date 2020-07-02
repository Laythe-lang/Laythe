use crate::constants::IMPORT_SEPARATOR;
use laythe_core::{
  hooks::GcHooks,
  module::Module,
  object::{BuiltIn, BuiltInDependencies, BuiltinPrimitives, LyHashMap},
  package::{Import, Package},
  LyResult,
};
use laythe_env::{
  fs::FsIo,
  io::Io,
  managed::{Manage, Managed, Trace},
  stdio::StdIo,
};
use std::{fmt, mem, path::PathBuf};

pub struct DepManager<I: Io> {
  /// The directory of the entry point
  pub src_dir: Managed<PathBuf>,

  /// interface to the current environments io
  io: I,

  /// A collection of builtin in classes, values and functions
  builtin: BuiltIn,

  /// A collection of packages that have already been loaded
  packages: LyHashMap<Managed<String>, Managed<Package>>,

  /// A cache for full filepath to individual modules
  cache: LyHashMap<Managed<String>, Managed<Module>>,
}

impl<I: Io> DepManager<I> {
  /// Create a new dependency manager
  pub fn new(io: I, builtin: BuiltIn, src_dir: Managed<PathBuf>) -> Self {
    Self {
      io,
      src_dir,
      builtin,
      packages: LyHashMap::default(),
      cache: LyHashMap::default(),
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
    path: Managed<String>,
  ) -> LyResult<Managed<Module>> {
    let mut module_dir = (*module.path).clone();
    module_dir.pop();

    // determine relative position of module relative to the src directory
    let relative = match self.io.fsio().relative_path(&*self.src_dir, &module_dir) {
      Ok(relative) => relative,
      Err(err) => return Err(hooks.make_error(err.message)),
    };

    // split path into segments
    let relative: Vec<String> = relative
      .ancestors()
      .map(|p| p.display().to_string())
      .filter(|p| p != "")
      .collect();

    // split import into segments and join to relative path
    let resolved_segments: Vec<String> = relative
      .into_iter()
      .rev()
      .chain(path.split(IMPORT_SEPARATOR).map(|s| s.to_string()))
      .collect();

    // check if fully resolved module has already been loaded
    let resolved = hooks.manage_str(resolved_segments.join("/"));
    if let Some(module) = self.cache.get(&resolved) {
      return Ok(*module);
    }

    // generate a new import object
    let import = Import::new(
      resolved_segments
        .iter()
        .map(|segment| hooks.manage_str(segment.to_string()))
        .collect(),
    );

    // match by package then resolve inside the package
    match import.package() {
      Some(pkg) => match self.packages.get(&pkg) {
        Some(package) => package.import(hooks, import),
        None => Err(hooks.make_error(format!("Package {} does not exist", pkg))),
      },
      None => Err(hooks.make_error("Import needs to be prepended with a package name".to_string())),
    }
  }

  /// Add a package to the dependency manager
  pub fn add_package(&mut self, package: Managed<Package>) {
    self.packages.insert(package.name(), package);
  }
}

impl<I: Io> fmt::Debug for DepManager<I> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("DepManager")
      .field("io", &self.io)
      .field("src_directory", &self.src_dir)
      .field("packages", &"LyHashMap { ... })")
      .field("cache", &"LyHashMap { ... }")
      .finish()
  }
}

impl<I: Io> Trace for DepManager<I> {
  fn trace(&self) -> bool {
    self.src_dir.trace();
    self.packages.iter().for_each(|(key, value)| {
      key.trace();
      value.trace();
    });

    true
  }

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    self.src_dir.trace_debug(stdio);
    self.packages.iter().for_each(|(key, value)| {
      key.trace_debug(stdio);
      value.trace_debug(stdio);
    });

    true
  }
}

impl<I: Io> Manage for DepManager<I> {
  fn alloc_type(&self) -> &str {
    "Dependency Manager"
  }

  fn debug(&self) -> String {
    format!("{:?}", self)
  }

  fn debug_free(&self) -> String {
    "DepManager: {{ io: {{ ... }}, src_directory: {{ ... }}, packages: {{ ... }}, cache: {{ ... }} }}".to_string()
  }

  fn size(&self) -> usize {
    mem::size_of::<Self>()
      + self.cache.capacity() * 2 * mem::size_of::<Managed<String>>()
      + self.packages.capacity() * 2 * mem::size_of::<Managed<String>>()
  }
}
