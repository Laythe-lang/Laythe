use crate::constants::IMPORT_SEPARATOR;
use spacelox_core::{
  hooks::Hooks,
  module::Module,
  object::SlHashMap,
  package::{Import, Package},
  ModuleResult,
};
use spacelox_env::{
  fs::FsIo,
  io::Io,
  managed::{Manage, Managed, Trace},
};
use std::{fmt, mem, path::PathBuf};

pub struct DepManager<I: Io> {
  /// interface to the current environments io
  io: I,

  /// The directory of the entry point
  src_directory: Managed<PathBuf>,

  /// A collection of packages that have already been loaded
  packages: SlHashMap<Managed<String>, Managed<Package>>,

  /// A cache for full filepath to individual modules
  cache: SlHashMap<Managed<String>, Managed<Module>>,
}

impl<I: Io> DepManager<I> {
  /// Create a new dependency manager
  pub fn new(io: I, src_directory: Managed<PathBuf>) -> Self {
    Self {
      io,
      src_directory,
      packages: SlHashMap::default(),
      cache: SlHashMap::default(),
    }
  }

  /// Import a module using the given path
  pub fn import(
    &mut self,
    hooks: &Hooks,
    module: Managed<Module>,
    path: Managed<String>,
  ) -> ModuleResult<Managed<Module>> {
    // determine relative position of module relative to the src directory
    let relative = match self
      .io
      .fsio()
      .normalized_import(&*self.src_directory, &*module.path)
    {
      Ok(relative) => relative,
      Err(err) => return Err(hooks.make_error(err.message)),
    };

    // split path into segments
    let relative: Vec<String> = relative
      .ancestors()
      .map(|p| p.display().to_string())
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
      .field("src_directory", &self.src_directory)
      .field("packages", &"SlHashMap { ... })")
      .field("cache", &"SlHashMap { ... }")
      .finish()
  }
}

impl<I: Io> Trace for DepManager<I> {
  fn trace(&self) -> bool {
    self.packages.iter().for_each(|(key, value)| {
      key.trace();
      value.trace();
    });

    true
  }

  fn trace_debug(&self, stdio: &dyn spacelox_env::stdio::StdIo) -> bool {
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
