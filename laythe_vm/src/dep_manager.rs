use laythe_core::{
  constants::IMPORT_SEPARATOR,
  hooks::{GcHooks, Hooks},
  module::Module,
  object::Map,
  package::{Import, Package},
  val,
  value::Value,
  Call, LyResult,
};
use laythe_env::{
  io::Io,
  managed::{DebugHeap, DebugWrap, Gc, Manage, Trace},
};
use laythe_lib::{create_error, BuiltIn, BuiltInDependencies, BuiltInErrors, BuiltInPrimitives};
use smol_str::SmolStr;
use std::{fmt, io::Write, mem, path::PathBuf};

pub struct DepManager {
  /// The directory of the entry point
  pub src_dir: Gc<PathBuf>,

  /// interface to the current environments io
  io: Io,

  /// A collection of builtin in classes, values and functions
  builtin: BuiltIn,

  /// A collection of packages that have already been loaded
  packages: Map<Gc<SmolStr>, Gc<Package>>,

  /// A cache for full filepath to individual modules
  cache: Map<Gc<SmolStr>, Gc<Module>>,

  /// The error class for import errors
  import_error: Value,
}

impl DepManager {
  /// Create a new dependency manager
  pub fn new(io: Io, builtin: BuiltIn, src_dir: Gc<PathBuf>) -> Self {
    let import_error = val!(builtin.errors.import);

    Self {
      io,
      src_dir,
      builtin,
      packages: Map::default(),
      cache: Map::default(),
      import_error,
    }
  }

  /// Get the class for primitives in laythe
  pub fn primitive_classes(&self) -> &BuiltInPrimitives {
    &self.builtin.primitives
  }

  /// Get the classes for dependency entities in laythe
  pub fn dependency_classes(&self) -> &BuiltInDependencies {
    &self.builtin.dependencies
  }

  /// Get the classes for dependency entities in laythe
  pub fn error_classes(&self) -> &BuiltInErrors {
    &self.builtin.errors
  }

  /// Import a module using the given path
  pub fn import(
    &mut self,
    hooks: &mut Hooks,
    module: Gc<Module>,
    path: Gc<SmolStr>,
  ) -> LyResult<Gc<Module>> {
    let mut module_dir = (*module.path).clone();
    module_dir.pop();

    // determine relative position of module relative to the src directory
    let relative = match self.io.fs().relative_path(&*self.src_dir, &module_dir) {
      Ok(relative) => relative,
      Err(err) => {
        return create_error!(self.import_error, hooks, err.to_string())
          .and_then(|err| LyResult::Err(err.to_instance()))
      }
    };

    // split path into segments
    let relative: Vec<String> = relative
      .ancestors()
      .map(|p| p.display().to_string())
      .filter(|p| !p.is_empty())
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

    // check if fully resolved module has already been loaded
    let resolved = hooks.manage_str(resolved_segments.join("/"));
    if let Some(module) = self.cache.get(&resolved) {
      return LyResult::Ok(*module);
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
        Some(package) => match package.import(&hooks.as_gc(), import) {
          Ok(module) => LyResult::Ok(module),
          Err(err) => create_error!(self.import_error, hooks, err.to_string())
            .and_then(|err| LyResult::Err(err.to_instance())),
        },
        None => create_error!(
          self.import_error,
          hooks,
          format!("Package {} does not exist", pkg)
        )
        .and_then(|err| LyResult::Err(err.to_instance())),
      },
      None => create_error!(
        self.import_error,
        hooks,
        "Import needs to be prepended with a package name"
      )
      .and_then(|err| LyResult::Err(err.to_instance())),
    }
  }

  /// Add a package to the dependency manager
  pub fn add_package(&mut self, hooks: &GcHooks, package: Gc<Package>) {
    hooks.grow(&mut self.packages, |packages| {
      packages.insert(package.name(), package)
    });
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
  fn trace(&self) {
    self.src_dir.trace();
    self.packages.iter().for_each(|(key, value)| {
      key.trace();
      value.trace();
    });
    self.builtin.trace();
  }

  fn trace_debug(&self, stdout: &mut dyn Write) {
    self.src_dir.trace_debug(stdout);
    self.packages.iter().for_each(|(key, value)| {
      key.trace_debug(stdout);
      value.trace_debug(stdout);
    });
    self.builtin.trace_debug(stdout);
  }
}

impl DebugHeap for DepManager {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    let depth = depth.saturating_sub(1);

    f.debug_struct("DepManager")
      .field("io", &self.io)
      .field("src_directory", &DebugWrap(&self.src_dir, depth))
      .field("packages", &DebugWrap(&self.packages, depth))
      .field("cache", &DebugWrap(&self.cache, depth))
      .finish()
  }
}

impl Manage for DepManager {
  fn size(&self) -> usize {
    mem::size_of::<Self>()
      + self.cache.capacity() * 2 * mem::size_of::<Gc<SmolStr>>()
      + self.packages.capacity() * 2 * mem::size_of::<Gc<SmolStr>>()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}
