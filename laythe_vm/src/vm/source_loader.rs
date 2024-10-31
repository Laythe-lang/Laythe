use super::{Vm, VmFileId};
use crate::{
  cache::InlineCache,
  compiler::{Compiler, Parser, Resolver},
  source::Source,
  FeResult,
};
use bumpalo::Bump;
use codespan_reporting::term::{self, Config};
use laythe_core::{
  hooks::GcHooks,
  managed::{Allocator, Gc, GcObj, GcStr},
  module::{Import, ImportError, Module, ModuleInsertError, Package},
  object::{Class, Fun},
};
use std::path::PathBuf;

/// What was the outcome of the attempted import
pub enum ImportResult {
  /// The file was already loaded and the module is available
  Loaded(Gc<Module>),

  /// The file was found be not yet executed
  Compiled(GcObj<Fun>),

  /// The file was not present
  NotFound,

  /// The file was present but contained compiler errors
  CompileError,
}

impl Vm {
  /// Compile the provided laythe source into the virtual machine's bytecode
  pub(super) fn compile(
    &mut self,
    repl: bool,
    module: Gc<Module>,
    source: &Source,
    file_id: VmFileId,
  ) -> FeResult<GcObj<Fun>> {
    let (ast, line_offsets) = Parser::new(source, file_id).parse();
    self
      .files
      .update_line_offsets(file_id, line_offsets.clone())
      .expect("File id not set for line offsets");

    let mut ast = ast?;
    Resolver::new(self.global_module, &self.gc.borrow(), source, file_id, repl)
      .resolve(&mut ast)?;

    let gc = self.gc.replace(Allocator::default());
    let alloc = Bump::new();
    let compiler = Compiler::new(module, &alloc, &line_offsets, file_id, repl, self, gc);

    #[cfg(feature = "debug")]
    let compiler = compiler.with_io(self.io.clone());

    let (result, gc, cache_id_emitter) = compiler.compile(&ast);
    self.gc.replace(gc);

    result.map(|fun| {
      let cache = InlineCache::new(
        cache_id_emitter.property_count(),
        cache_id_emitter.invoke_count(),
      );

      if module.id() < self.inline_cache.len() {
        self.inline_cache[module.id()] = cache;
      } else {
        self.inline_cache.push(cache);
      }
      self.manage_obj(fun)
    })
  }

  /// Create a new module
  pub(super) fn module(&mut self, name: &str, path: &str) -> Gc<Module> {
    let id = self.emitter.emit();
    let hooks = GcHooks::new(self);

    // create a new module class that subclass
    // the base module class
    let name = hooks.manage_str(name);
    let module_class = Class::with_inheritance(&hooks, name, self.builtin.dependencies.module);
    hooks.push_root(module_class);

    let path = hooks.manage_str(path);
    hooks.push_root(path);

    let mut module = hooks.manage(Module::new(module_class, path, id));
    hooks.pop_roots(2);
    hooks.push_root(module);

    // transfer the symbols from the global module into this module
    self.global_module.transfer_exported(&hooks, &mut module);

    hooks.pop_roots(1);
    let package = hooks.manage(Package::new(name, module));

    self.packages.insert(name, package);
    module
  }

  /// Import a module into the current fiber. If the
  /// module already exists return that module otherwise attempt
  /// to load the missing module from the file system.
  pub(super) fn import_module(&mut self, import: Gc<Import>) -> ImportResult {
    let package = self.packages.get(&import.package()).cloned();

    match package {
      Some(existing_package) => match existing_package.import(import) {
        Ok(module) => ImportResult::Loaded(module),
        Err(err) => match err {
          ImportError::ModuleDoesNotExist => self.load_missing_module(existing_package, import),
          ImportError::PackageDoesNotMatch => panic!("Unexpected package mismatch"),
          _ => unreachable!(),
        },
      },
      None => ImportResult::NotFound,
    }
  }

  /// Attempt to load a missing module from the filesystem into Laythe.
  /// This can error from a missing file or from the file
  /// failing to compile. If successful attach the new module
  /// onto it's parent
  fn load_missing_module(
    &mut self,
    existing_package: Gc<Package>,
    import: Gc<Import>,
  ) -> ImportResult {
    let (mut parent_module, (found_path, remaining_path)) =
      find_missing_module(existing_package.root_module(), import.path(), 0);

    let fs = self.io.fs();
    let mut resolved_path = self.root_dir.clone();

    for path_segment in found_path {
      resolved_path.push(PathBuf::from(path_segment));
    }

    let module_name = *remaining_path.first().unwrap();
    resolved_path.push(PathBuf::from(&format!("{module_name}.lay")));

    match fs.read_file(&resolved_path) {
      Ok(file_contents) => {
        let source_content = self.manage_str(file_contents);

        match resolved_path.into_os_string().into_string() {
          Ok(name) => {
            let path = self.manage_str(name);
            let source = Source::new(source_content);
            let file_id = self.files.upsert(path, source_content);

            let module = self.module(&module_name, &path);
            if let Err(err) = parent_module.insert_module(module) {
              match err {
                ModuleInsertError::ModuleAlreadyExists => todo!(),
              }
            }

            match self.compile(false, module, &source, file_id) {
              Ok(fun) => ImportResult::Compiled(fun),
              Err(errors) => {
                let mut stdio = self.io.stdio();
                let stderr_color = stdio.stderr_color();
                for error in errors.iter() {
                  term::emit(stderr_color, &Config::default(), &self.files, error)
                    .expect("Unable to write to stderr");
                }
                ImportResult::CompileError
              },
            }
          },
          Err(_) => ImportResult::NotFound,
        }
      },
      Err(_) => ImportResult::NotFound,
    }
  }
}

fn find_missing_module(
  module: Gc<Module>,
  path: &[GcStr],
  index: usize,
) -> (Gc<Module>, (&[GcStr], &[GcStr])) {
  if path.is_empty() {
    return (module, (&[], &[]));
  }

  match module.get_module(path[0]) {
    Some(module) => find_missing_module(module, path, index + 1),
    None => (module, path.split_at(index)),
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod find_missing_module {
    use super::*;
    use laythe_core::{
      hooks::{GcHooks, NoContext},
      module::module_class,
      support::{test_class, test_module},
    };

    #[test]
    fn empty_path() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let root_module = test_module(&hooks, "root");

      let (found_module, (beginning, end)) = find_missing_module(root_module, &[], 0);

      assert_eq!(found_module, root_module);

      assert!(beginning.is_empty());
      assert!(end.is_empty());
    }

    #[test]
    fn no_nested_modules() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let root_module = test_module(&hooks, "root");
      let path = &[hooks.manage_str("first"), hooks.manage_str("second")];

      let (found_module, (beginning, end)) = find_missing_module(root_module, path, 0);

      assert_eq!(found_module, root_module);

      assert!(beginning.is_empty());
      assert_eq!(end.len(), 2);
      assert_eq!(end[0], hooks.manage_str("first"));
      assert_eq!(end[1], hooks.manage_str("second"));
    }

    #[test]
    fn partial_match() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let class = test_class(&hooks, "Module");
      let module_path = hooks.manage_str("example");

      let mut root_module = hooks.manage(Module::new(
        module_class(&hooks, "root", class),
        module_path,
        0,
      ));
      let nested_module = hooks.manage(Module::new(
        module_class(&hooks, "first", class),
        module_path,
        1,
      ));

      assert!(root_module.insert_module(nested_module).is_ok());

      let path = &[hooks.manage_str("first"), hooks.manage_str("second")];

      let (found_module, (beginning, end)) = find_missing_module(root_module, path, 0);

      assert_eq!(found_module, nested_module);

      assert_eq!(beginning.len(), 1);
      assert_eq!(beginning[0], hooks.manage_str("first"));
      assert_eq!(end.len(), 1);
      assert_eq!(end[0], hooks.manage_str("second"));
    }
  }
}
