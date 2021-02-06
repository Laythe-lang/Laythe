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
  module::{ModuleError, Package},
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
      pub fn native(hooks: &GcHooks) -> Gc<Native> {
        let native = Box::new(Self {}) as Box<dyn LyNative>;
        hooks.manage(Native::new($meta.to_meta(hooks), native))
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
        if err.is_instance() {
          Call::Err(err.to_instance())
        } else {
          panic!(
            "Standard library failed to instantiate error instance\nFound value {:?}",
            err.kind()
          )
        }
      }
      Call::Err(err) => Call::Err(err),
      Call::Exit(exit) => Call::Exit(exit),
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
      fn native(hooks: &GcHooks, error: Value) -> Gc<Native> {
        debug_assert!(error.is_class());
        let native = Box::new(Self { error }) as Box<dyn LyNative>;

        hooks.manage(Native::new($meta.to_meta(hooks), native))
      }

      fn call_error<T: Into<String> + AsRef<str>>(&self, hooks: &mut Hooks, message: T) -> Call {
        match hooks.call(self.error, &[val!(hooks.manage_str(message))]) {
          Call::Ok(err) => {
            if err.is_instance() {
              Call::Err(err.to_instance())
            } else {
              panic!(
                "Standard library failed to instantiate error instance\nFound value {:?}",
                err.kind()
              )
            }
          }
          Call::Err(err) => Call::Err(err),
          Call::Exit(err) => Call::Exit(err),
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
  ModuleError(ModuleError),
  SymbolNotClass,
  SymbolNotInstance,
}

impl From<ModuleError> for StdError {
  fn from(err: ModuleError) -> Self {
    StdError::ModuleError(err)
  }
}

pub const STD: &str = "std";
pub const GLOBAL: &str = "global";

pub fn create_std_lib(hooks: &GcHooks, emitter: &mut IdEmitter) -> StdResult<Gc<Package>> {
  // let mut std = hooks.manage(Package::new(hooks.manage_str(STD.to_string())));

  let mut std = create_std_core(hooks, emitter)?;

  add_math_module(hooks, &mut std, emitter)?;
  add_io_package(hooks, &mut std, emitter)?;
  let env = env_module(hooks, &std, emitter)?;
  let regexp = regexp_module(hooks, &std, emitter)?;

  let mut root_module = std.root_module();

  root_module.insert_module(hooks, env)?;
  root_module.insert_module(hooks, regexp)?;

  Ok(std)
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::support::MockedContext;
  use laythe_core::{module::Module, signature::Arity, value::ValueKind};

  fn check_inner(module: Gc<Module>) {
    module.symbols().for_each(|(_key, symbol)| {
      let option = match symbol.kind() {
        ValueKind::Native => Some(symbol.to_native().meta().clone()),
        _ => None,
      };

      if let Some(fun_meta) = option {
        match fun_meta.signature.arity {
          Arity::Default(_, total_count) => {
            assert_eq!(fun_meta.signature.parameters.len(), total_count as usize);
          }
          Arity::Fixed(count) => {
            assert_eq!(fun_meta.signature.parameters.len(), count as usize);
          }
          Arity::Variadic(min_count) => {
            assert_eq!(fun_meta.signature.parameters.len(), min_count as usize + 1);
          }
        }
      }
    });

    module
      .modules()
      .for_each(|(_name, module)| check_inner(*module))
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
    check_inner(root_module);
  }
}
