use crate::{native, support::to_dyn_native};
use laythe_core::{
  constants::OBJECT,
  hooks::{GcHooks, Hooks},
  managed::{Gc, Trace},
  object::{Class, MetaData, Native, NativeMeta, NativeMetaBuilder},
  signature::{Arity, ParameterBuilder, ParameterKind},
  val,
  value::Value,
  Call,
};
use std::io::Write;

pub const OBJECT_CLASS_NAME: &str = OBJECT;

const OBJECT_EQUALS: NativeMetaBuilder = NativeMetaBuilder::method("equals", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("other", ParameterKind::Any)]);

const OBJECT_CLASS: NativeMetaBuilder = NativeMetaBuilder::method("cls", Arity::Fixed(0));

const OBJECT_STR: NativeMetaBuilder = NativeMetaBuilder::method("str", Arity::Fixed(0));

pub fn create_object_class(hooks: &GcHooks) -> Gc<Class> {
  let name = hooks.manage_str(OBJECT_CLASS_NAME);
  let mut object = hooks.manage(Class::bare(name));

  object.add_method(
    &hooks,
    hooks.manage_str(OBJECT_EQUALS.name),
    val!(to_dyn_native(hooks, ObjectEquals::from(hooks))),
  );

  object.add_method(
    &hooks,
    hooks.manage_str(OBJECT_CLASS.name),
    val!(to_dyn_native(hooks, ObjectCls::from(hooks))),
  );

  object.add_method(
    &hooks,
    hooks.manage_str(OBJECT_STR.name),
    val!(to_dyn_native(hooks, ObjectStr::from(hooks))),
  );

  object
}

native!(ObjectEquals, OBJECT_EQUALS);

impl Native for ObjectEquals {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    Call::Ok(val!(this.unwrap() == args[0]))
  }
}

native!(ObjectCls, OBJECT_CLASS);

impl Native for ObjectCls {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    Call::Ok(hooks.get_class(this.unwrap()))
  }
}

native!(ObjectStr, OBJECT_STR);

impl Native for ObjectStr {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    let this = this.unwrap();
    let class = hooks.get_class(this).to_class();

    let string = match this.kind() {
      laythe_core::value::ValueKind::Bool => format!("<{} {}>", class.name(), this.to_bool()),
      laythe_core::value::ValueKind::Nil => format!("<{} nil>", class.name()),
      laythe_core::value::ValueKind::Number => format!("<{} {}>", class.name(), this.to_num()),
      laythe_core::value::ValueKind::String => format!("<{} {}>", class.name(), this.to_str()),
      laythe_core::value::ValueKind::List => format!("<{} {:p}>", class.name(), &*this.to_list()),
      laythe_core::value::ValueKind::Map => format!("<{} {:p}>", class.name(), &*this.to_map()),
      laythe_core::value::ValueKind::Fun => format!("<{} {:p}>", class.name(), &*this.to_fun()),
      laythe_core::value::ValueKind::Closure => {
        format!("<{} {:p}>", class.name(), &*this.to_closure())
      }
      laythe_core::value::ValueKind::Class => format!("<{} {:p}>", class.name(), &this.to_class()),
      laythe_core::value::ValueKind::Instance => {
        format!("<{} {:p}>", class.name(), &*this.to_instance())
      }
      laythe_core::value::ValueKind::Iter => format!("<{} {:p}>", class.name(), &*this.to_iter()),
      laythe_core::value::ValueKind::Method => {
        format!("<{} {:p}>", class.name(), &*this.to_method())
      }
      laythe_core::value::ValueKind::Native => {
        format!("<{} {:p}>", class.name(), &*this.to_native())
      }
      laythe_core::value::ValueKind::Upvalue => {
        format!("<{} {:p}>", class.name(), &*this.to_upvalue())
      }
    };

    Call::Ok(val!(hooks.manage_str(&string)))
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod equals {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let object_equals = ObjectEquals::from(&hooks);

      assert_eq!(object_equals.meta().name, "equals");
      assert_eq!(object_equals.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        object_equals.meta().signature.parameters[0].kind,
        ParameterKind::Any
      )
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let bool_str = ObjectEquals::from(&hooks);

      let ten_1 = val!(10.0);
      let b_false = val!(false);
      let ten_2 = val!(10.0);

      let result1 = bool_str.call(&mut hooks, Some(ten_1), &[ten_2]);
      let result2 = bool_str.call(&mut hooks, Some(ten_2), &[b_false]);

      match result1 {
        Call::Ok(r) => assert_eq!(r.to_bool(), true),
        _ => assert!(false),
      }
      match result2 {
        Call::Ok(r) => assert_eq!(r.to_bool(), false),
        _ => assert!(false),
      }
    }
  }

  mod cls {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let object_cls = ObjectCls::from(&hooks);

      assert_eq!(object_cls.meta().name, "cls");
      assert_eq!(object_cls.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::with_std(&[]).unwrap();
      let mut hooks = Hooks::new(&mut context);
      let object_cls = ObjectCls::from(&hooks);

      let ten = val!(10.0);

      let result = object_cls.call(&mut hooks, Some(ten), &[]).unwrap();
      assert_eq!(result.to_class().name(), hooks.manage_str("Number"));
    }
  }

  mod str {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let object_equals = ObjectStr::from(&hooks);

      assert_eq!(object_equals.meta().name, "str");
      assert_eq!(object_equals.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::with_std(&[]).unwrap();
      let mut hooks = Hooks::new(&mut context);
      let object_str = ObjectStr::from(&hooks);

      let ten = val!(10.0);

      let result = object_str.call(&mut hooks, Some(ten), &[]);

      match result {
        Call::Ok(r) => assert_eq!(r.to_str(), "<Number 10>"),
        _ => assert!(false),
      }
    }
  }
}
