use crate::native;
use laythe_core::{
  constants::OBJECT,
  hooks::{GcHooks, Hooks},
  managed::{GcObj, GcObject, Trace},
  match_obj,
  object::{Class, LyNative, Native, NativeMetaBuilder, ObjectKind},
  signature::{Arity, ParameterBuilder, ParameterKind},
  to_obj_kind, val,
  value::{Value, ValueKind},
  Call,
};
use std::io::Write;

pub const OBJECT_CLASS_NAME: &str = OBJECT;

const OBJECT_EQUALS: NativeMetaBuilder = NativeMetaBuilder::method("equals", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("other", ParameterKind::Any)]);

const OBJECT_CLASS: NativeMetaBuilder = NativeMetaBuilder::method("cls", Arity::Fixed(0));

const OBJECT_STR: NativeMetaBuilder = NativeMetaBuilder::method("str", Arity::Fixed(0));

const OBJECT_IS_A: NativeMetaBuilder = NativeMetaBuilder::method("isA?", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("class", ParameterKind::Class)]);

pub fn create_object_class(hooks: &GcHooks) -> GcObj<Class> {
  let name = hooks.manage_str(OBJECT_CLASS_NAME);
  let mut object = hooks.manage_obj(Class::bare(name));

  object.add_method(
    hooks.manage_str(OBJECT_EQUALS.name),
    val!(ObjectEquals::native(hooks)),
  );

  object.add_method(
    hooks.manage_str(OBJECT_CLASS.name),
    val!(ObjectCls::native(hooks)),
  );

  object.add_method(
    hooks.manage_str(OBJECT_STR.name),
    val!(ObjectStr::native(hooks)),
  );

  object.add_method(
    hooks.manage_str(OBJECT_IS_A.name),
    val!(ObjectIsA::native(hooks)),
  );

  object
}

native!(ObjectEquals, OBJECT_EQUALS);

impl LyNative for ObjectEquals {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    Call::Ok(val!(this.unwrap() == args[0]))
  }
}

native!(ObjectCls, OBJECT_CLASS);

impl LyNative for ObjectCls {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    Call::Ok(val!(hooks.get_class(this.unwrap())))
  }
}

native!(ObjectStr, OBJECT_STR);

impl LyNative for ObjectStr {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    let this = this.unwrap();
    let class = hooks.get_class(this);

    let string = match this.kind() {
      ValueKind::Bool => format!("<{} {}>", &*class.name(), this.to_bool()),
      ValueKind::Nil => format!("<{} nil>", &*class.name()),
      ValueKind::Number => format!("<{} {}>", &*class.name(), this.to_num()),
      ValueKind::Obj => match_obj!((&this.to_obj()) {
        ObjectKind::Channel(channel) => {
          format!("<{} {:p}>", &*class.name(), &*channel)
        },
        ObjectKind::Class(cls) => {
          format!("<{} {:p}>", &*class.name(), &*cls)
        },
        ObjectKind::Closure(closure) => {
          format!("<{} {:p}>", &*class.name(), &*closure)
        },
        ObjectKind::Enumerator(enumerator) => {
          format!("<{} {:p}>", &*class.name(), &*enumerator)
        },
        ObjectKind::Fun(fun) => {
          format!("<{} {:p}>", &*class.name(), &*fun)
        },
        ObjectKind::Fiber(fiber) => {
          format!("<{} {:p}>", &*class.name(), &*fiber)
        },
        ObjectKind::Instance(instance) => {
          format!("<{} {:p}>", &*class.name(), &*instance)
        },
        ObjectKind::List(list) => {
          format!("<{} {:p}>", &*class.name(), &*list)
        },
        ObjectKind::Map(map) => {
          format!("<{} {:p}>", &*class.name(), &*map)
        },
        ObjectKind::Method(method) => {
          format!("<{} {:p}>", &*class.name(), &*method)
        },
        ObjectKind::Native(native) => {
          format!("<{} {:p}>", &*class.name(), &*native)
        },
        ObjectKind::String(string) => {
          format!("<{} {}>", &*class.name(), string)
        },
        ObjectKind::Tuple(tuple) => {
          format!("<{} {:p}>", &*class.name(), &tuple)
        },
        ObjectKind::LyBox(ly_box) => {
          format!("<{} {:p}>", &*class.name(), &ly_box)
        },
      }),
    };

    Call::Ok(val!(hooks.manage_str(string)))
  }
}

native!(ObjectIsA, OBJECT_IS_A);

impl LyNative for ObjectIsA {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let self_class = hooks.get_class(this.unwrap());
    let class = args[0].to_obj().to_class();

    Call::Ok(val!(self_class.is_subclass(class)))
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

      let object_equals = ObjectEquals::native(&hooks);

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
      let object_equals = ObjectEquals::native(&hooks.as_gc());

      let ten_1 = val!(10.0);
      let b_false = val!(false);
      let ten_2 = val!(10.0);

      let result1 = object_equals.call(&mut hooks, Some(ten_1), &[ten_2]);
      let result2 = object_equals.call(&mut hooks, Some(ten_2), &[b_false]);

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

      let object_cls = ObjectCls::native(&hooks);

      assert_eq!(object_cls.meta().name, "cls");
      assert_eq!(object_cls.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::with_std(&[]).unwrap();
      let mut hooks = Hooks::new(&mut context);
      let object_cls = ObjectCls::native(&hooks.as_gc());

      let ten = val!(10.0);

      let result = object_cls.call(&mut hooks, Some(ten), &[]).unwrap();
      assert_eq!(
        result.to_obj().to_class().name(),
        hooks.manage_str("Number")
      );
    }
  }

  mod str {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let object_str = ObjectStr::native(&hooks);

      assert_eq!(object_str.meta().name, "str");
      assert_eq!(object_str.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::with_std(&[]).unwrap();
      let mut hooks = Hooks::new(&mut context);
      let object_str = ObjectStr::native(&hooks.as_gc());

      let ten = val!(10.0);

      let result = object_str.call(&mut hooks, Some(ten), &[]);

      match result {
        Call::Ok(r) => assert_eq!(r.to_obj().to_str(), "<Number 10>"),
        _ => assert!(false),
      }
    }
  }

  mod is_a {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let object_is_a = ObjectIsA::native(&hooks);

      assert_eq!(object_is_a.meta().name, "isA?");
      assert_eq!(object_is_a.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        object_is_a.meta().signature.parameters[0].kind,
        ParameterKind::Class
      )
    }

    #[test]
    fn call() {
      let mut context = MockedContext::with_std(&[]).unwrap();
      let builtin = context.builtin.as_ref().unwrap();

      let ten = val!(10.0);
      let error = val!(builtin.errors.error);
      let object = val!(builtin.primitives.object);

      let mut hooks = Hooks::new(&mut context);
      let object_is_a = ObjectIsA::native(&hooks.as_gc());

      let result1 = object_is_a.call(&mut hooks, Some(ten), &[error]);
      let result2 = object_is_a.call(&mut hooks, Some(ten), &[object]);
      let result3 = object_is_a.call(&mut hooks, Some(error), &[object]);
      let result4 = object_is_a.call(&mut hooks, Some(error), &[error]);
      let result5 = object_is_a.call(&mut hooks, Some(object), &[object]);
      let result6 = object_is_a.call(&mut hooks, Some(object), &[error]);

      match result1 {
        Call::Ok(r) => assert_eq!(r.to_bool(), false),
        _ => assert!(false),
      }
      match result2 {
        Call::Ok(r) => assert_eq!(r.to_bool(), true),
        _ => assert!(false),
      }

      match result3 {
        Call::Ok(r) => assert_eq!(r.to_bool(), true),
        _ => assert!(false),
      }
      match result4 {
        Call::Ok(r) => assert_eq!(r.to_bool(), false),
        _ => assert!(false),
      }

      match result5 {
        Call::Ok(r) => assert_eq!(r.to_bool(), true),
        _ => assert!(false),
      }
      match result6 {
        Call::Ok(r) => assert_eq!(r.to_bool(), false),
        _ => assert!(false),
      }
    }
  }
}
