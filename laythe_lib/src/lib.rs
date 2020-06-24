#![deny(clippy::all)]
pub mod global;
mod math;
mod support;

use global::create_global;
use math::create_math;
use laythe_core::{hooks::GcHooks, package::Package, PackageResult};
use laythe_env::managed::Managed;

pub const STD: &str = "std";
pub const GLOBAL: &str = "global";
pub const GLOBAL_PATH: &str = "std/global.lox";

pub fn create_std_lib(hooks: &GcHooks) -> PackageResult<Managed<Package>> {
  let mut std = hooks.manage(Package::new(hooks.manage_str(STD.to_string())));

  let global = create_global(hooks, std)?;
  let math = create_math(hooks, std)?;

  std.add_module(hooks, global)?;
  std.add_module(hooks, math)?;

  Ok(std)
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::support::{test_native_dependencies, TestContext};
  use laythe_core::{package::PackageEntity, signature::Arity, value::ValueVariant};

  fn check_inner(hooks: &GcHooks, package: Managed<Package>) {
    package.entities().for_each(|(_key, entity)| match entity {
      PackageEntity::Module(module) => {
        let import = module.import(hooks);
        import.fields().for_each(|(_key, symbol)| {
          let option = match symbol.clone().kind() {
            ValueVariant::NativeFun => Some(symbol.to_native_fun().meta().clone()),
            ValueVariant::NativeMethod => Some(symbol.to_native_method().meta().clone()),
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
    let gc = test_native_dependencies();
    let mut context = TestContext::new(&gc, &[]);
    let hooks = GcHooks::new(&mut context);

    let std_lib = create_std_lib(&hooks);
    assert!(std_lib.is_ok());

    let std_lib = std_lib.unwrap();
    check_inner(&hooks, std_lib);
  }
}
