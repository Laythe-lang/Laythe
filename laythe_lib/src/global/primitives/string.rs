use crate::{
  native, native_with_error,
  support::{export_and_insert, load_class_from_module},
  StdResult,
};
use laythe_core::{
  constants::INDEX_GET,
  hooks::{GcHooks, Hooks},
  managed::GcObj,
  managed::{DebugHeap, DebugWrap, Gc, GcStr, Manage, Trace},
  module::Module,
  object::{Enumerate, Enumerator, LyNative, Native, NativeMetaBuilder, ObjectKind},
  signature::{Arity, ParameterBuilder, ParameterKind},
  val,
  value::{Value, VALUE_NIL},
  Call, LyError, LyResult,
};
use std::{io::Write, str::Split};
use std::{mem, str::Chars};

use super::{class_inheritance, error::INDEX_ERROR_NAME};

pub const STRING_CLASS_NAME: &str = "String";

const STRING_INDEX_GET: NativeMetaBuilder = NativeMetaBuilder::method(INDEX_GET, Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("index", ParameterKind::Number)])
  .with_stack();

const STRING_STR: NativeMetaBuilder = NativeMetaBuilder::method("str", Arity::Fixed(0));

const STRING_LEN: NativeMetaBuilder = NativeMetaBuilder::method("len", Arity::Fixed(0));

const STRING_HAS: NativeMetaBuilder = NativeMetaBuilder::method("has", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("string", ParameterKind::String)]);

const STRING_SPLIT: NativeMetaBuilder = NativeMetaBuilder::method("split", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("separator", ParameterKind::String)]);

const STRING_SLICE: NativeMetaBuilder = NativeMetaBuilder::method("slice", Arity::Default(0, 2))
  .with_params(&[
    ParameterBuilder::new("start", ParameterKind::Number),
    ParameterBuilder::new("end", ParameterKind::Number),
  ])
  .with_stack();

const STRING_ITER: NativeMetaBuilder = NativeMetaBuilder::method("iter", Arity::Fixed(0));

pub fn declare_string_class(hooks: &GcHooks, module: Gc<Module>) -> StdResult<()> {
  let class = class_inheritance(hooks, module, STRING_CLASS_NAME)?;
  export_and_insert(hooks, module, class.name(), val!(class))
}

pub fn define_string_class(hooks: &GcHooks, module: Gc<Module>) -> StdResult<()> {
  let mut class = load_class_from_module(hooks, module, STRING_CLASS_NAME)?;
  let index_error = val!(load_class_from_module(hooks, module, INDEX_ERROR_NAME)?);

  class.add_method(
    hooks,
    hooks.manage_str(STRING_INDEX_GET.name),
    val!(StringIndexGet::native(hooks, index_error)),
  );

  class.add_method(
    hooks,
    hooks.manage_str(STRING_STR.name),
    val!(StringStr::native(hooks)),
  );

  class.add_method(
    hooks,
    hooks.manage_str(STRING_LEN.name),
    val!(StringLen::native(hooks)),
  );

  class.add_method(
    hooks,
    hooks.manage_str(STRING_HAS.name),
    val!(StringHas::native(hooks)),
  );

  class.add_method(
    hooks,
    hooks.manage_str(STRING_SLICE.name),
    val!(StringSlice::native(hooks, index_error)),
  );

  class.add_method(
    hooks,
    hooks.manage_str(STRING_SPLIT.name),
    val!(StringSplit::native(hooks)),
  );

  class.add_method(
    hooks,
    hooks.manage_str(STRING_ITER.name),
    val!(StringIter::native(hooks)),
  );

  Ok(())
}

native!(StringStr, STRING_STR);

impl LyNative for StringStr {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    Call::Ok(this.unwrap())
  }
}

native!(StringLen, STRING_LEN);

impl LyNative for StringLen {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    Call::Ok(val!(this.unwrap().to_obj().to_str().chars().count() as f64))
  }
}

native_with_error!(StringIndexGet, STRING_INDEX_GET);

impl LyNative for StringIndexGet {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let this = this.unwrap().to_obj().to_str();
    let index = args[0].to_num();

    // eliminate non integers
    if index.fract() != 0.0 {
      return self.call_error(hooks, "slice methods takes integer parameters");
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
      Some(c) => Call::Ok(val!(hooks.manage_str(c.encode_utf8(&mut buffer)))),
      None => self.call_error(
        hooks,
        format!(
          "Index out of bounds. string was length {} but attempted to index with {}.",
          this.chars().count(),
          index
        ),
      ),
    }
  }
}

native!(StringHas, STRING_HAS);

impl LyNative for StringHas {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let str = this.unwrap().to_obj().to_str();
    Call::Ok(val!(str.contains(&*args[0].to_obj().to_str())))
  }
}

native!(StringSplit, STRING_SPLIT);

impl LyNative for StringSplit {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let separator = args[0].to_obj().to_str();
    let str = this.unwrap().to_obj().to_str();

    let inner_iter: Box<dyn Enumerate> = Box::new(SplitIterator::new(str, separator));
    let iter = Enumerator::new(inner_iter);

    Call::Ok(val!(hooks.manage_obj(iter)))
  }
}

#[derive(Debug)]
struct SplitIterator {
  string: GcStr,
  separator: GcStr,
  iter: Split<'static, &'static str>,
  current: Value,
}

impl SplitIterator {
  fn new(string: GcStr, separator: GcStr) -> Self {
    let iter = unsafe { string.deref_static().split(separator.deref_static()) };

    Self {
      string,
      separator,
      iter,
      current: VALUE_NIL,
    }
  }
}

impl Enumerate for SplitIterator {
  fn name(&self) -> &str {
    "Split"
  }

  fn current(&self) -> Value {
    self.current
  }

  fn next(&mut self, hooks: &mut Hooks) -> Call {
    match self.iter.next() {
      Some(next) => {
        self.current = val!(hooks.manage_str(next));
        Call::Ok(val!(true))
      }
      None => {
        self.current = VALUE_NIL;
        Call::Ok(val!(false))
      }
    }
  }

  fn size_hint(&self) -> Option<usize> {
    None
  }
}

impl Trace for SplitIterator {
  fn trace(&self) {
    self.string.trace();
    self.separator.trace();
    self.current.trace();
  }

  fn trace_debug(&self, stdout: &mut dyn Write) {
    self.string.trace_debug(stdout);
    self.separator.trace_debug(stdout);
    self.current.trace_debug(stdout);
  }
}

impl DebugHeap for SplitIterator {
  fn fmt_heap(&self, f: &mut std::fmt::Formatter, depth: usize) -> std::fmt::Result {
    f.debug_struct("SplitIterator")
      .field("string", &DebugWrap(&self.string, depth))
      .field("separator", &DebugWrap(&self.separator, depth))
      .field("iter", &"*")
      .field("current", &DebugWrap(&self.current, depth))
      .finish()
  }
}

impl Manage for SplitIterator {
  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

native_with_error!(StringSlice, STRING_SLICE);

impl LyNative for StringSlice {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    // get underlying string slice
    let string = this.unwrap().to_obj().to_str();

    let (start, end) = match args.len() {
      0 => (0.0, string.len() as f64),
      1 => (args[0].to_num(), string.len() as f64),
      2 => (args[0].to_num(), args[1].to_num()),
      _ => panic!("string slice should only been passed 0, 1 or 2 parameters"),
    };

    // get start and end indices
    let start_index = self.string_index(hooks, &string, start)?;
    let end_index = self.string_index(hooks, &string, end)?;

    if start_index <= end_index {
      // TODO investigate special case where slice is full string
      Call::Ok(val!(hooks.manage_str(&string[start_index..end_index])))
    } else {
      Call::Ok(val!(hooks.manage_str("")))
    }
  }
}

impl StringSlice {
  fn string_index(&self, hooks: &mut Hooks, string: &str, index: f64) -> LyResult<usize> {
    // eliminate non integers
    if index.fract() != 0.0 {
      return LyResult::Err(
        self
          .call_error(hooks, "slice methods takes integer parameters")
          .expect_err("TODO"),
      );
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

    LyResult::Ok(index)
  }
}

native!(StringIter, STRING_ITER);

impl LyNative for StringIter {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    let str = this.unwrap().to_obj().to_str();

    let inner_iter: Box<dyn Enumerate> = Box::new(StringIterator::new(str));
    let iter = Enumerator::new(inner_iter);

    Call::Ok(val!(hooks.manage_obj(iter)))
  }
}

#[derive(Debug)]
struct StringIterator {
  string: GcStr,
  iter: Chars<'static>,
  current: Value,
}

impl StringIterator {
  fn new(string: GcStr) -> Self {
    let iter = unsafe { string.deref_static().chars() };

    Self {
      string,
      iter,
      current: VALUE_NIL,
    }
  }
}

impl Enumerate for StringIterator {
  fn name(&self) -> &str {
    "String"
  }

  fn current(&self) -> Value {
    self.current
  }

  fn next(&mut self, hooks: &mut Hooks) -> Call {
    let s = &mut [0; 4];

    match self.iter.next() {
      Some(next) => {
        self.current = val!(hooks.manage_str(next.encode_utf8(s)));
        Call::Ok(val!(true))
      }
      None => {
        self.current = VALUE_NIL;
        Call::Ok(val!(false))
      }
    }
  }

  fn size_hint(&self) -> Option<usize> {
    None
  }
}

impl Trace for StringIterator {
  fn trace(&self) {
    self.string.trace();
    self.current.trace();
  }

  fn trace_debug(&self, stdout: &mut dyn Write) {
    self.string.trace_debug(stdout);
    self.current.trace_debug(stdout);
  }
}

impl DebugHeap for StringIterator {
  fn fmt_heap(&self, f: &mut std::fmt::Formatter, depth: usize) -> std::fmt::Result {
    f.debug_struct("StringIterator")
      .field("string", &DebugWrap(&self.string, depth))
      .field("iter", &"*")
      .field("current", &DebugWrap(&self.current, depth))
      .finish()
  }
}

impl Manage for StringIterator {
  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
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

      let string_str = StringStr::native(&hooks);

      assert_eq!(string_str.meta().name, "str");
      assert_eq!(string_str.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let string_str = StringStr::native(&hooks.as_gc());

      let this = val!(hooks.manage_str("test".to_string()));
      let result = string_str.call(&mut hooks, Some(this), &[]);
      match result {
        Call::Ok(r) => assert_eq!(*r.to_obj().to_str(), "test".to_string()),
        _ => assert!(false),
      }
    }
  }

  mod index_get {
    use super::*;
    use crate::support::{test_error_class, MockedContext};

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let error = val!(test_error_class(&hooks));
      let index_get = StringIndexGet::native(&hooks, error);

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

      let error = val!(test_error_class(&hooks.as_gc()));
      let string_index_get = StringIndexGet::native(&hooks.as_gc(), error);

      let this = val!(hooks.manage_str("test".to_string()));
      let result = string_index_get.call(&mut hooks, Some(this), &[val!(0.0)]);
      match result {
        Call::Ok(r) => assert_eq!(*r.to_obj().to_str(), "t".to_string()),
        _ => assert!(false),
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

      let string_len = StringLen::native(&hooks);

      assert_eq!(string_len.meta().name, "len");
      assert_eq!(string_len.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let string_len = StringLen::native(&hooks.as_gc());
      let this = val!(hooks.manage_str("abc"));

      let result = string_len.call(&mut hooks, Some(this), &[]);

      match result {
        Call::Ok(r) => {
          assert_eq!(r.to_num(), 3.0);
        }
        _ => assert!(false),
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

      let string_str = StringHas::native(&hooks);

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

      let string_iter = StringIter::native(&hooks.as_gc());
      let this = val!(hooks.manage_str("abc"));

      let result = string_iter.call(&mut hooks, Some(this), &[]);

      match result {
        Call::Ok(r) => {
          let mut string_iter = r.to_obj().to_enumerator();
          assert_eq!(string_iter.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(string_iter.current(), val!(hooks.manage_str("a")));

          assert_eq!(string_iter.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(string_iter.current(), val!(hooks.manage_str("b")));

          assert_eq!(string_iter.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(string_iter.current(), val!(hooks.manage_str("c")));

          assert_eq!(string_iter.next(&mut hooks).unwrap(), val!(false));
        }
        _ => assert!(false),
      }
    }
  }

  mod slice {
    use super::*;
    use crate::support::{test_error_class, MockedContext};

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let error = val!(test_error_class(&hooks));
      let string_slice = StringSlice::native(&hooks, error);

      assert_eq!(string_slice.meta().name, "slice");
      assert_eq!(string_slice.meta().signature.arity, Arity::Default(0, 2));
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

      let error = val!(test_error_class(&hooks.as_gc()));
      let string_slice = StringSlice::native(&hooks.as_gc(), error);
      let this = val!(hooks.manage_str("abc123"));

      let result = string_slice.call(&mut hooks, Some(this), &[val!(-5.0), val!(3.0)]);

      match result {
        Call::Ok(r) => {
          assert!(r.is_obj_kind(ObjectKind::String));
          assert_eq!(r.to_obj().to_str(), "bc");
        }
        _ => assert!(false),
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

      let string_iter = StringIter::native(&hooks);

      assert_eq!(string_iter.meta().name, "iter");
      assert_eq!(string_iter.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let string_str = StringHas::native(&hooks.as_gc());

      let this = val!(hooks.manage_str("some string".to_string()));
      let contained = val!(hooks.manage_str("ome".to_string()));
      let not_contained = val!(hooks.manage_str("other".to_string()));

      let result = string_str.call(&mut hooks, Some(this), &[contained]);
      match result {
        Call::Ok(r) => assert!(r.to_bool()),
        _ => assert!(false),
      }

      let result = string_str.call(&mut hooks, Some(this), &[not_contained]);
      match result {
        Call::Ok(r) => assert!(!r.to_bool()),
        _ => assert!(false),
      }
    }
  }

  mod split {
    use laythe_core::value::{VALUE_FALSE, VALUE_TRUE};

    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let string_split = StringSplit::native(&hooks);

      assert_eq!(string_split.meta().name, "split");
      assert_eq!(string_split.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        string_split.meta().signature.parameters[0].kind,
        ParameterKind::String
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let string_split = StringSplit::native(&hooks.as_gc());

      let this = val!(hooks.manage_str("some string here"));
      let separator = val!(hooks.manage_str(" "));

      let result = string_split
        .call(&mut hooks, Some(this), &[separator])
        .unwrap();
      assert!(result.is_obj_kind(ObjectKind::Enumerator));

      let mut iter = result.to_obj().to_enumerator();
      assert_eq!(iter.current(), VALUE_NIL);
      assert_eq!(iter.next(&mut hooks).unwrap(), VALUE_TRUE);

      assert_eq!(iter.current(), val!(hooks.manage_str("some")));
      assert_eq!(iter.next(&mut hooks).unwrap(), VALUE_TRUE);

      assert_eq!(iter.current(), val!(hooks.manage_str("string")));
      assert_eq!(iter.next(&mut hooks).unwrap(), VALUE_TRUE);

      assert_eq!(iter.current(), val!(hooks.manage_str("here")));
      assert_eq!(iter.next(&mut hooks).unwrap(), VALUE_FALSE);
    }
  }
}
