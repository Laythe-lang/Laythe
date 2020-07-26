use crate::{
  native,
  support::{default_class_inheritance, export_and_insert, load_class_from_module, to_dyn_native},
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  iterator::{LyIter, LyIterator},
  module::Module,
  native::{MetaData, Native, NativeMeta, NativeMetaBuilder},
  package::Package,
  signature::{Arity, ParameterBuilder, ParameterKind},
  val,
  value::{Value, VALUE_NIL},
  CallResult, LyResult,
};
use laythe_env::{
  managed::{Managed, Trace},
  stdio::Stdio,
};
use smol_str::SmolStr;
use std::{mem, str::Chars};

pub const STRING_CLASS_NAME: &str = "String";
const STRING_STR: NativeMetaBuilder = NativeMetaBuilder::method("str", Arity::Fixed(0));

const STRING_HAS: NativeMetaBuilder = NativeMetaBuilder::method("has", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("string", ParameterKind::String)]);

const STRING_ITER: NativeMetaBuilder = NativeMetaBuilder::method("iter", Arity::Fixed(0));

pub fn declare_string_class(
  hooks: &GcHooks,
  module: &mut Module,
  package: &Package,
) -> LyResult<()> {
  let class = default_class_inheritance(hooks, package, STRING_CLASS_NAME)?;
  export_and_insert(hooks, module, class.name, val!(class))
}

pub fn define_string_class(hooks: &GcHooks, module: &Module, _: &Package) -> LyResult<()> {
  let mut class = load_class_from_module(hooks, module, STRING_CLASS_NAME)?;

  class.add_method(
    hooks,
    hooks.manage_str(STRING_STR.name),
    val!(to_dyn_native(hooks, StringStr::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(STRING_HAS.name),
    val!(to_dyn_native(hooks, StringHas::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(STRING_ITER.name),
    val!(to_dyn_native(hooks, StringIter::from(hooks))),
  );

  Ok(())
}

native!(StringStr, STRING_STR);

impl Native for StringStr {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> CallResult {
    Ok(this.unwrap())
  }
}

native!(StringHas, STRING_HAS);

impl Native for StringHas {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> CallResult {
    let str = this.unwrap().to_str();
    Ok(val!(str.contains(args[0].to_str().as_str())))
  }
}

native!(StringIter, STRING_ITER);

impl Native for StringIter {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> CallResult {
    let str = this.unwrap().to_str();

    let inner_iter: Box<dyn LyIter> = Box::new(StringIterator::new(str));
    let iter = LyIterator::new(inner_iter);

    Ok(val!(hooks.manage(iter)))
  }
}

#[derive(Debug)]
struct StringIterator {
  string: Managed<SmolStr>,
  iter: Chars<'static>,
  current: Value,
}

impl StringIterator {
  fn new(string: Managed<SmolStr>) -> Self {
    let iter = unsafe { string.deref_static().chars() };

    Self {
      string,
      iter,
      current: VALUE_NIL,
    }
  }
}

impl LyIter for StringIterator {
  fn name(&self) -> &str {
    "String Iterator"
  }

  fn current(&self) -> Value {
    self.current
  }

  fn next(&mut self, hooks: &mut Hooks) -> CallResult {
    let s = &mut [0; 4];

    match self.iter.next() {
      Some(next) => {
        self.current = val!(hooks.manage_str(next.encode_utf8(s)));
        Ok(val!(true))
      }
      None => {
        self.current = VALUE_NIL;
        Ok(val!(false))
      }
    }
  }

  fn size_hint(&self) -> Option<usize> {
    None
  }

  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }
}

impl Trace for StringIterator {
  fn trace(&self) -> bool {
    self.string.trace()
  }

  fn trace_debug(&self, stdio: &mut Stdio) -> bool {
    self.string.trace_debug(stdio)
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod str {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let string_str = StringStr::from(&hooks);

      assert_eq!(string_str.meta().name, "str");
      assert_eq!(string_str.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let string_str = StringStr::from(&hooks);

      let this = val!(hooks.manage_str("test".to_string()));
      let result = string_str.call(&mut hooks, Some(this), &[]);
      match result {
        Ok(r) => assert_eq!(*r.to_str(), "test".to_string()),
        Err(_) => assert!(false),
      }
    }
  }

  mod has {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let string_str = StringHas::from(&hooks);

      assert_eq!(string_str.meta().name, "has");
      assert_eq!(string_str.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        string_str.meta().signature.parameters[0].kind,
        ParameterKind::String
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let string_iter = StringIter::from(&hooks);
      let this = val!(hooks.manage_str("abc"));

      let result = string_iter.call(&mut hooks, Some(this), &[]);

      match result {
        Ok(r) => {
          let mut string_iter = r.to_iter();
          assert_eq!(string_iter.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(string_iter.current(), val!(hooks.manage_str("a")));

          assert_eq!(string_iter.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(string_iter.current(), val!(hooks.manage_str("b")));

          assert_eq!(string_iter.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(string_iter.current(), val!(hooks.manage_str("c")));

          assert_eq!(string_iter.next(&mut hooks).unwrap(), val!(false));
        }
        Err(_) => assert!(false),
      }
    }
  }

  mod iter {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let string_iter = StringIter::from(&hooks);

      assert_eq!(string_iter.meta().name, "iter");
      assert_eq!(string_iter.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let string_str = StringHas::from(&hooks);

      let this = val!(hooks.manage_str("some string".to_string()));
      let contained = val!(hooks.manage_str("ome".to_string()));
      let not_contained = val!(hooks.manage_str("other".to_string()));

      let result = string_str.call(&mut hooks, Some(this), &[contained]);
      match result {
        Ok(r) => assert!(r.to_bool()),
        Err(_) => assert!(false),
      }

      let result = string_str.call(&mut hooks, Some(this), &[not_contained]);
      match result {
        Ok(r) => assert!(!r.to_bool()),
        Err(_) => assert!(false),
      }
    }
  }
}
