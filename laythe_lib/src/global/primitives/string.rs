use crate::{
  native,
  support::{default_class_inheritance, export_and_insert, load_class_from_module, to_dyn_native},
};
use laythe_core::{
  constants::INDEX_GET,
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
use laythe_env::managed::{Managed, Trace};
use smol_str::SmolStr;
use std::io::Write;
use std::{mem, str::Chars};

pub const STRING_CLASS_NAME: &str = "String";

const STRING_INDEX_GET: NativeMetaBuilder = NativeMetaBuilder::method(INDEX_GET, Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("index", ParameterKind::Number)]);

const STRING_STR: NativeMetaBuilder = NativeMetaBuilder::method("str", Arity::Fixed(0));

const STRING_LEN: NativeMetaBuilder = NativeMetaBuilder::method("len", Arity::Fixed(0));

const STRING_HAS: NativeMetaBuilder = NativeMetaBuilder::method("has", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("string", ParameterKind::String)]);

const STRING_SLICE: NativeMetaBuilder = NativeMetaBuilder::method("slice", Arity::Default(1, 2))
  .with_params(&[
    ParameterBuilder::new("start", ParameterKind::Number),
    ParameterBuilder::new("end", ParameterKind::Number),
  ]);

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
    hooks.manage_str(STRING_INDEX_GET.name),
    val!(to_dyn_native(hooks, StringIndexGet::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(STRING_STR.name),
    val!(to_dyn_native(hooks, StringStr::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(STRING_LEN.name),
    val!(to_dyn_native(hooks, StringLen::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(STRING_HAS.name),
    val!(to_dyn_native(hooks, StringHas::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(STRING_SLICE.name),
    val!(to_dyn_native(hooks, StringSlice::from(hooks))),
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

native!(StringLen, STRING_LEN);

impl Native for StringLen {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> CallResult {
    Ok(val!(this.unwrap().to_str().chars().count() as f64))
  }
}

native!(StringIndexGet, STRING_INDEX_GET);

impl Native for StringIndexGet {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> CallResult {
    let this = this.unwrap().to_str();
    let index = args[0].to_num();

    // eliminate non integers
    if index.fract() != 0.0 {
      return hooks.error("slice methods takes integer parameters");
    }

    // may want to look back at this for ascii special case
    let mut chars = this.chars();
    let c = if index >= 0.0 {
      chars.nth(index as usize)
    } else {
      chars.rev().nth((-index) as usize - 1)
    };

    let mut buffer: [u8; 4] = [0, 0, 0, 0];
    match c {
      Some(c) => Ok(val!(hooks.manage_str(c.encode_utf8(&mut buffer)))),
      None => hooks.error(format!(
        "Index out of bounds. string was length {} but attempted to index with {}.",
        this.chars().count(),
        index
      )),
    }
  }
}

native!(StringHas, STRING_HAS);

impl Native for StringHas {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> CallResult {
    let str = this.unwrap().to_str();
    Ok(val!(str.contains(args[0].to_str().as_str())))
  }
}

native!(StringSlice, STRING_SLICE);

impl Native for StringSlice {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> CallResult {
    // get underlying string slice
    let string = this.unwrap().to_str();
    let string = string.as_str();

    let start = args[0].to_num();
    let end = if args.len() > 1 {
      Some(args[1].to_num())
    } else {
      None
    };

    // get start and end indices
    let start_index = string_index(hooks, string, start)?;
    let end_index = if let Some(end) = end {
      string_index(hooks, string, end)?
    } else {
      string.len()
    };

    if start_index <= end_index {
      // TODO investigate special case where slice is full string
      Ok(val!(hooks.manage_str(&string[start_index..end_index])))
    } else {
      Ok(val!(hooks.manage_str("")))
    }
  }
}

fn string_index(hooks: &Hooks, string: &str, index: f64) -> LyResult<usize> {
  // eliminate non integers
  if index.fract() != 0.0 {
    return hooks.error("slice methods takes integer parameters");
  }

  // may want to look back at this for ascii special case
  let mut char_indices = string.char_indices();
  let index = if index >= 0.0 {
    char_indices
      .nth(index as usize)
      .map(|(index, _)| index)
      .unwrap_or_else(|| string.len())
  } else {
    char_indices
      .rev()
      .nth((-index) as usize - 1)
      .map(|(index, _)| index)
      .unwrap_or(0)
  };

  Ok(index)
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

  fn trace_debug(&self, stdout: &mut dyn Write) -> bool {
    self.string.trace_debug(stdout)
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

  mod index_get {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let index_get = StringIndexGet::from(&hooks);

      assert_eq!(index_get.meta().name, "[]");
      assert_eq!(index_get.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        index_get.meta().signature.parameters[0].kind,
        ParameterKind::Number
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let string_index_get = StringIndexGet::from(&hooks);

      let this = val!(hooks.manage_str("test".to_string()));
      let result = string_index_get.call(&mut hooks, Some(this), &[val!(0.0)]);
      match result {
        Ok(r) => assert_eq!(*r.to_str(), "t".to_string()),
        Err(_) => assert!(false),
      }
    }
  }

  mod len {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let string_len = StringLen::from(&hooks);

      assert_eq!(string_len.meta().name, "len");
      assert_eq!(string_len.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let string_len = StringLen::from(&hooks);
      let this = val!(hooks.manage_str("abc"));

      let result = string_len.call(&mut hooks, Some(this), &[]);

      match result {
        Ok(r) => {
          assert_eq!(r.to_num(), 3.0);
        }
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

  mod slice {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let string_slice = StringSlice::from(&hooks);

      assert_eq!(string_slice.meta().name, "slice");
      assert_eq!(string_slice.meta().signature.arity, Arity::Default(1, 2));
      assert_eq!(
        string_slice.meta().signature.parameters[0].kind,
        ParameterKind::Number
      );
      assert_eq!(
        string_slice.meta().signature.parameters[1].kind,
        ParameterKind::Number
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let string_slice = StringSlice::from(&hooks);
      let this = val!(hooks.manage_str("abc123"));

      let result = string_slice.call(&mut hooks, Some(this), &[val!(-5.0), val!(3.0)]);

      match result {
        Ok(r) => {
          assert!(r.is_str());
          assert_eq!(r.to_str(), "bc");
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
