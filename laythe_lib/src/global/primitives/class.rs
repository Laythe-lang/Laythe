use crate::support::{export_and_insert, load_class_from_module, to_dyn_method};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{NativeMeta, NativeMethod},
  object::Class,
  package::Package,
  signature::Arity,
  value::{Value, VALUE_NIL},
  CallResult, LyResult,
};
use laythe_env::{managed::Trace, stdio::Stdio};

pub const CLASS_CLASS_NAME: &'static str = "Class";

const CLASS_SUPER_CLASS: NativeMeta = NativeMeta::new("superClass", Arity::Fixed(0), &[]);

pub fn declare_class_class(hooks: &GcHooks, module: &mut Module) -> LyResult<()> {
  let name = hooks.manage_str(String::from(CLASS_CLASS_NAME));
  let class = hooks.manage(Class::bare(name));

  export_and_insert(hooks, module, name, Value::from(class))
}

pub fn define_class_class(hooks: &GcHooks, module: &Module, _: &Package) -> LyResult<()> {
  let mut class_class = load_class_from_module(hooks, module, CLASS_CLASS_NAME)?;

  class_class.add_method(
    &hooks,
    hooks.manage_str(String::from(CLASS_SUPER_CLASS.name)),
    Value::from(to_dyn_method(hooks, ClassSuperClass())),
  );

  Ok(())
}

#[derive(Clone, Debug, Trace)]
struct ClassSuperClass();

impl NativeMethod for ClassSuperClass {
  fn meta(&self) -> &NativeMeta {
    &CLASS_SUPER_CLASS
  }

  fn call(&self, _hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    let super_class = this
      .to_class()
      .super_class()
      .map(|super_class| Value::from(super_class))
      .unwrap_or(VALUE_NIL);

    Ok(super_class)
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod super_class {
    use super::*;
    use crate::support::{test_native_dependencies, MockedContext};

    #[test]
    fn new() {
      let class_super_class = ClassSuperClass();

      assert_eq!(class_super_class.meta().name, "superClass");
      assert_eq!(class_super_class.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let class_super_class = ClassSuperClass();
      let gc = test_native_dependencies();
      let mut context = MockedContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let mut class = hooks.manage(Class::bare(hooks.manage_str("someClass".to_string())));

      let super_class = hooks.manage(Class::bare(hooks.manage_str("someSuperClass".to_string())));

      class.inherit(&hooks.to_gc(), super_class);

      let class_value = Value::from(class);
      let super_class_value = Value::from(super_class);

      let result1 = class_super_class.call(&mut hooks, class_value, &[]);
      let result2 = class_super_class.call(&mut hooks, super_class_value, &[]);

      match result1 {
        Ok(r) => assert_eq!(r, super_class_value),
        Err(_) => assert!(false),
      }
      match result2 {
        Ok(r) => assert_eq!(r, VALUE_NIL),
        Err(_) => assert!(false),
      }
    }
  }
}
