#![deny(clippy::all)]
mod env;
pub mod global;
mod io;
mod math;
mod regexp;
mod support;

use env::env_module;
use global::add_global_module;
use io::io_package;
use laythe_core::{hooks::GcHooks, package::Package, LyResult};
use laythe_env::managed::Managed;
use math::math_module;
use regexp::regexp_module;

#[macro_export]
macro_rules! native {
  ( $x:ident, $y:ident ) => {
    #[derive(Debug)]
    pub struct $x {
      meta: NativeMeta,
    }

    impl<'a> From<&GcHooks<'a>> for $x {
      fn from(hooks: &GcHooks<'a>) -> Self {
        Self {
          meta: $y.to_meta(hooks),
        }
      }
    }

    impl<'a> From<&Hooks<'a>> for $x {
      fn from(hooks: &Hooks<'a>) -> Self {
        Self {
          meta: $y.to_meta(&hooks.as_gc()),
        }
      }
    }

    impl MetaData for $x {
      fn meta(&self) -> &NativeMeta {
        &self.meta
      }
    }

    impl Trace for $x {
      fn trace(&self) -> bool {
        self.meta.trace()
      }

      fn trace_debug(&self, stdio: &mut dyn Write) -> bool {
        self.meta.trace_debug(stdio)
      }
    }
  };
}

pub const STD: &str = "std";
pub const GLOBAL: &str = "global";
pub const GLOBAL_PATH: &str = "std/global.ly";

pub fn create_std_lib(hooks: &GcHooks) -> LyResult<Managed<Package>> {
  let mut std = hooks.manage(Package::new(hooks.manage_str(STD.to_string())));

  add_global_module(hooks, std)?;

  let math = math_module(hooks, std)?;
  let io = io_package(hooks, std)?;
  let env = env_module(hooks, std)?;
  let regexp = regexp_module(hooks, std)?;

  std.add_module(hooks, math)?;
  std.add_package(hooks, io)?;
  std.add_module(hooks, env)?;
  std.add_module(hooks, regexp)?;

  Ok(std)
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::support::MockedContext;
  use laythe_core::{package::PackageEntity, signature::Arity, value::ValueKind};

  fn check_inner(hooks: &GcHooks, package: Managed<Package>) {
    package.entities().for_each(|(_key, entity)| match entity {
      PackageEntity::Module(module) => {
        let import = module.import(hooks);
        import.fields().iter().for_each(|symbol| {
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
      }
      PackageEntity::Package(package) => {
        check_inner(hooks, *package);
      }
    })
  }

  #[test]
  fn new() {
    let mut context = MockedContext::default();
    let hooks = GcHooks::new(&mut context);

    let std_lib = create_std_lib(&hooks);
    assert!(std_lib.is_ok());

    let std_lib = std_lib.unwrap();
    check_inner(&hooks, std_lib);
  }
}
