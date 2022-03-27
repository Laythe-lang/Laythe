#![deny(clippy::all)]
mod builtin;
mod env;
pub mod global;
mod io;
mod math;
mod regexp;
mod support;

use env::env_module;
use global::create_std_core;
use io::add_io_package;
use laythe_core::{
  hooks::GcHooks,
  managed::Gc,
  module::{ImportError, ModuleInsertError, Package, SymbolExportError, SymbolInsertError},
  utils::IdEmitter,
};
use math::add_math_module;
use regexp::regexp_module;

pub use builtin::{
  builtin_from_module, BuiltIn, BuiltInDependencies, BuiltInErrors, BuiltInPrimitives,
};

type StdResult<T> = Result<T, StdError>;

#[macro_export]
macro_rules! native {
  ( $st:ident, $meta:ident ) => {
    #[derive(Debug)]
    pub struct $st {}

    impl $st {
      pub fn native(hooks: &GcHooks) -> GcObj<Native> {
        let native = Box::new(Self {}) as Box<dyn LyNative>;
        hooks.manage_obj(Native::new($meta.to_meta(hooks), native))
      }
    }

    impl Trace for $st {
      fn trace(&self) {}

      fn trace_debug(&self, _stdio: &mut dyn Write) {}
    }
  };
}

#[macro_export]
macro_rules! create_error {
  ( $error:expr, $hooks:ident, $message:expr ) => {
    match $hooks.call($error, &[Value::from($hooks.manage_str($message))]) {
      Call::Ok(err) => {
        if err.is_obj_kind(ObjectKind::Instance) {
          Call::Err(LyError::Err(err.to_obj().to_instance()))
        } else {
          panic!(
            "Standard library failed to instantiate error instance\nFound value {:?}",
            err.kind()
          )
        }
      },
      Call::Err(err) => Call::Err(err),
    }
  };
}

#[macro_export]
macro_rules! native_with_error {
  ( $st:ident, $meta:ident ) => {
    #[derive(Debug)]
    pub struct $st {
      error: Value,
    }

    impl $st {
      fn native(hooks: &GcHooks, error: Value) -> GcObj<Native> {
        debug_assert!(error.is_obj_kind(ObjectKind::Class));
        let native = Box::new(Self { error }) as Box<dyn LyNative>;

        hooks.manage_obj(Native::new($meta.to_meta(hooks), native))
      }

      fn call_error<T: Into<String> + AsRef<str>>(&self, hooks: &mut Hooks, message: T) -> Call {
        match hooks.call(self.error, &[val!(hooks.manage_str(message))]) {
          Call::Ok(err) => {
            if err.is_obj_kind(ObjectKind::Instance) {
              Call::Err(LyError::Err(err.to_obj().to_instance()))
            } else {
              panic!(
                "Standard library failed to instantiate error instance\nFound value {:?}",
                err.kind()
              )
            }
          },
          Call::Err(err) => Call::Err(err),
        }
      }
    }

    impl Trace for $st {
      fn trace(&self) {
        self.error.trace();
      }

      fn trace_debug(&self, stdio: &mut dyn Write) {
        self.error.trace_debug(stdio);
      }
    }
  };
}

#[derive(Debug)]
pub enum StdError {
  ImportError(ImportError),
  ModuleInsertError(ModuleInsertError),
  SymbolInsertError(SymbolInsertError),
  SymbolExportError(SymbolExportError),
  SymbolNotFound,
  SymbolNotClass,
  SymbolNotInstance,
}

impl From<ImportError> for StdError {
  fn from(err: ImportError) -> Self {
    StdError::ImportError(err)
  }
}

impl From<SymbolInsertError> for StdError {
  fn from(err: SymbolInsertError) -> Self {
    StdError::SymbolInsertError(err)
  }
}

impl From<SymbolExportError> for StdError {
  fn from(err: SymbolExportError) -> Self {
    StdError::SymbolExportError(err)
  }
}

impl From<ModuleInsertError> for StdError {
  fn from(err: ModuleInsertError) -> Self {
    StdError::ModuleInsertError(err)
  }
}

pub const STD: &str = "std";
pub const GLOBAL: &str = "global";

pub fn create_std_lib(hooks: &GcHooks, emitter: &mut IdEmitter) -> StdResult<Gc<Package>> {
  let std = create_std_core(hooks, emitter)?;

  add_math_module(hooks, std, emitter)?;
  add_io_package(hooks, std, emitter)?;
  let env = env_module(hooks, std, emitter)?;
  let regexp = regexp_module(hooks, std, emitter)?;

  let mut root_module = std.root_module();

  root_module.insert_module(env)?;
  root_module.insert_module(regexp)?;

  Ok(std)
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::support::{load_class_from_module, MockedContext};
  use global::CLASS_CLASS_NAME;
  use laythe_core::{
    managed::{GcObj, GcObject},
    match_obj,
    module::Module,
    object::{Class, ObjectKind},
    signature::Arity,
    to_obj_kind,
  };

  fn new_inner(module: Gc<Module>) {
    module.symbols().for_each(|(_key, symbol)| {
      let option = if symbol.is_obj() {
        match_obj!((&symbol.to_obj()) {
          ObjectKind::Native(native) => Some(native.meta().clone()),
          _ => None,
        })
      } else {
        None
      };

      if let Some(fun_meta) = option {
        match fun_meta.signature.arity {
          Arity::Default(_, total_count) => {
            assert_eq!(fun_meta.signature.parameters.len(), total_count as usize);
          },
          Arity::Fixed(count) => {
            assert_eq!(fun_meta.signature.parameters.len(), count as usize);
          },
          Arity::Variadic(min_count) => {
            assert_eq!(fun_meta.signature.parameters.len(), min_count as usize + 1);
          },
        }
      }
    });

    module
      .modules()
      .for_each(|(_name, module)| new_inner(*module))
  }

  fn class_setup_inner(module: Gc<Module>, class_class: GcObj<Class>) {
    module.symbols().for_each(|(_key, symbol)| {
      let option = if symbol.is_obj() {
        match_obj!((&symbol.to_obj()) {
          ObjectKind::Class(class) => Some(class),
          _ => None,
        })
      } else {
        None
      };

      if let Some(class) = option {
        if class.name() != "Object" {
          assert!(class.super_class().is_some(), "{}", class.name());
        }

        assert!(class.meta_class().is_some(), "{}", class.name());

        if class != class {
          let meta_super = class
            .meta_class()
            .and_then(|cls| *cls.super_class())
            .unwrap();

          let meta_meta = class
            .meta_class()
            .and_then(|cls| *cls.meta_class())
            .unwrap();

          assert_eq!(meta_super, class_class);
          assert_eq!(meta_meta, class_class);
        }
      }
    });

    module
      .modules()
      .for_each(|(_name, module)| class_setup_inner(*module, class_class))
  }

  #[test]
  fn new() {
    let mut context = MockedContext::default();
    let hooks = GcHooks::new(&mut context);
    let mut emitter = IdEmitter::default();

    let std_lib = create_std_lib(&hooks, &mut emitter);
    assert!(std_lib.is_ok());

    let std_lib = std_lib.unwrap();
    let root_module = std_lib.root_module();
    new_inner(root_module);
  }

  #[test]
  fn class_setup() {
    let mut context = MockedContext::default();
    let hooks = GcHooks::new(&mut context);
    let mut emitter = IdEmitter::default();

    let std_lib = create_std_lib(&hooks, &mut emitter);
    assert!(std_lib.is_ok());

    let std_lib = std_lib.unwrap();
    let root_module = std_lib.root_module();

    let class_class = load_class_from_module(&hooks, root_module, CLASS_CLASS_NAME).unwrap();

    class_setup_inner(root_module, class_class);
  }
}
