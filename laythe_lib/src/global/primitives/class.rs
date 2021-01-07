use crate::{
  native,
  support::{export_and_insert, load_class_from_module, to_dyn_native},
  InitResult,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{MetaData, Native, NativeMeta, NativeMetaBuilder},
  object::Class,
  package::Package,
  signature::Arity,
  val,
  value::{Value, VALUE_NIL},
  Call,
};
use laythe_env::managed::Trace;
use std::io::Write;

pub const CLASS_CLASS_NAME: &str = "Class";

const CLASS_SUPER_CLS: NativeMetaBuilder = NativeMetaBuilder::method("superCls", Arity::Fixed(0));

const CLASS_STR: NativeMetaBuilder = NativeMetaBuilder::method("str", Arity::Fixed(0));

pub fn declare_class_class(hooks: &GcHooks, module: &mut Module) -> InitResult<()> {
  let name = hooks.manage_str(CLASS_CLASS_NAME);
  let class = hooks.manage(Class::bare(name));

  export_and_insert(hooks, module, name, val!(class))
}

pub fn define_class_class(hooks: &GcHooks, module: &Module, _: &Package) -> InitResult<()> {
  let mut class_class = load_class_from_module(hooks, module, CLASS_CLASS_NAME)?;

  class_class.add_method(
    &hooks,
    hooks.manage_str(CLASS_SUPER_CLS.name),
    val!(to_dyn_native(hooks, ClassSuperCls::from(hooks))),
  );

  class_class.add_method(
    &hooks,
    hooks.manage_str(CLASS_STR.name),
    val!(to_dyn_native(hooks, ClassStr::from(hooks))),
  );

  Ok(())
}

native!(ClassSuperCls, CLASS_SUPER_CLS);

impl Native for ClassSuperCls {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    let super_class = this
      .unwrap()
      .to_class()
      .super_class()
      .map(Value::from)
      .unwrap_or(VALUE_NIL);

    Call::Ok(super_class)
  }
}

native!(ClassStr, CLASS_STR);

impl Native for ClassStr {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    let class = this.unwrap().to_class();

    Call::Ok(val!(
      hooks.manage_str(&format!("<class {} {:p}>", class.name, &*class))
    ))
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::support::MockedContext;

  mod super_cls {
    use super::*;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let class_super_class = ClassSuperCls::from(&hooks);

      assert_eq!(class_super_class.meta().name, "superCls");
      assert_eq!(class_super_class.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let class_super_class = ClassSuperCls::from(&hooks);

      let mut class = hooks.manage(Class::bare(hooks.manage_str("someClass".to_string())));

      let super_class = hooks.manage(Class::bare(hooks.manage_str("someSuperClass".to_string())));

      class.inherit(&hooks.as_gc(), super_class);

      let class_value = val!(class);
      let super_class_value = val!(super_class);

      let result1 = class_super_class.call(&mut hooks, Some(class_value), &[]);
      let result2 = class_super_class.call(&mut hooks, Some(super_class_value), &[]);

      match result1 {
        Call::Ok(r) => assert_eq!(r, super_class_value),
        _ => assert!(false),
      }
      match result2 {
        Call::Ok(r) => assert_eq!(r, VALUE_NIL),
        _ => assert!(false),
      }
    }
  }

  mod str {
    use super::*;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let class_str = ClassStr::from(&hooks);

      assert_eq!(class_str.meta().name, "str");
      assert_eq!(class_str.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let class_str = ClassStr::from(&hooks);

      let class = hooks.manage(Class::bare(hooks.manage_str("someClass".to_string())));

      let class_value = val!(class);

      let result = class_str.call(&mut hooks, Some(class_value), &[]).unwrap();
      assert!(result.to_str().contains("<class someClass"));
    }
  }
}
