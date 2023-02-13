use crate::{
  native, native_with_error,
  support::{export_and_insert, load_class_from_module},
  StdResult,
};
use laythe_core::{
  constants::INDEX_GET,
  hooks::{GcHooks, Hooks},
  if_let_obj,
  managed::{DebugHeap, DebugWrap, Gc, GcObj, GcStr, Trace, Tuple},
  module::Module,
  object::{Enumerate, Enumerator, List, LyNative, Native, NativeMetaBuilder, ObjectKind},
  signature::{Arity, ParameterBuilder, ParameterKind},
  to_obj_kind,
  utils::is_falsey,
  val,
  value::{Value, VALUE_NIL},
  Call, LyError, LyResult,
};
use std::io::Write;
use std::slice::Iter;

use super::{
  class_inheritance,
  error::{INDEX_ERROR_NAME, TYPE_ERROR_NAME},
};

pub const TUPLE_CLASS_NAME: &str = "Tuple";

const TUPLE_INDEX_GET: NativeMetaBuilder = NativeMetaBuilder::method(INDEX_GET, Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("index", ParameterKind::Number)])
  .with_stack();

const TUPLE_HAS: NativeMetaBuilder = NativeMetaBuilder::method("has", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("val", ParameterKind::Any)]);

const TUPLE_ITER: NativeMetaBuilder = NativeMetaBuilder::method("iter", Arity::Fixed(0));

const TUPLE_INDEX: NativeMetaBuilder = NativeMetaBuilder::method("index", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("value", ParameterKind::Any)]);

const TUPLE_LEN: NativeMetaBuilder = NativeMetaBuilder::method("len", Arity::Fixed(0));
const TUPLE_STR: NativeMetaBuilder = NativeMetaBuilder::method("str", Arity::Fixed(0)).with_stack();

const TUPLE_SLICE: NativeMetaBuilder = NativeMetaBuilder::method("slice", Arity::Default(0, 2))
  .with_params(&[
    ParameterBuilder::new("start", ParameterKind::Number),
    ParameterBuilder::new("end", ParameterKind::Number),
  ])
  .with_stack();

const TUPLE_COLLECT: NativeMetaBuilder = NativeMetaBuilder::fun("collect", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("iter", ParameterKind::Enumerator)]);

pub fn declare_tuple_class(hooks: &GcHooks, module: Gc<Module>) -> StdResult<()> {
  let class = class_inheritance(hooks, module, TUPLE_CLASS_NAME)?;

  export_and_insert(module, class.name(), val!(class))
}

pub fn define_tuple_class(hooks: &GcHooks, module: Gc<Module>) -> StdResult<()> {
  let mut class = load_class_from_module(hooks, module, TUPLE_CLASS_NAME)?;
  let index_error = val!(load_class_from_module(hooks, module, INDEX_ERROR_NAME)?);
  let type_error = val!(load_class_from_module(hooks, module, TYPE_ERROR_NAME)?);

  class.add_method(
    hooks.manage_str(TUPLE_INDEX_GET.name),
    val!(TupleIndexGet::native(hooks, index_error)),
  );

  class.add_method(
    hooks.manage_str(TUPLE_LEN.name),
    val!(TupleLen::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(TUPLE_INDEX.name),
    val!(TupleIndex::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(TUPLE_STR.name),
    val!(TupleStr::native(
      hooks,
      hooks.manage_str(TUPLE_STR.name),
      type_error,
    )),
  );

  class.add_method(
    hooks.manage_str(TUPLE_SLICE.name),
    val!(TupleSlice::native(hooks, index_error)),
  );

  class.add_method(
    hooks.manage_str(TUPLE_HAS.name),
    val!(TupleHas::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(TUPLE_ITER.name),
    val!(TupleIter::native(hooks)),
  );

  class.meta_class().expect("Meta class not set.").add_method(
    hooks.manage_str(TUPLE_COLLECT.name),
    val!(TupleCollect::native(hooks)),
  );

  Ok(())
}

#[derive(Debug)]
struct TupleStr {
  method_name: GcStr,
  error: Value,
}

impl TupleStr {
  fn native(hooks: &GcHooks, method_name: GcStr, error: Value) -> GcObj<Native> {
    debug_assert!(error.is_obj_kind(ObjectKind::Class));
    let native = Box::new(Self { method_name, error }) as Box<dyn LyNative>;

    hooks.manage_obj(Native::new(TUPLE_STR.to_meta(hooks), native))
  }
}

impl Trace for TupleStr {
  fn trace(&self) {
    self.method_name.trace();
  }

  fn trace_debug(&self, log: &mut dyn Write) {
    self.method_name.trace_debug(log);
  }
}

fn quote_string(buf: &mut String, string: &str) {
  buf.push('\'');
  buf.push_str(string);
  buf.push('\'');
}

impl LyNative for TupleStr {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    let tuple = this.unwrap().to_obj().to_tuple();

    // buffer for temporary strings
    let mut buf = String::new();
    buf.push('(');

    if let Some((last, rest)) = tuple.split_last() {
      for item in rest.iter() {
        // if already string quote and add to temps
        let item = *item;
        if_let_obj!(ObjectKind::String(string) = (item) {
          quote_string(&mut buf, &string);
          buf.push_str(", ");
          continue;
        });

        // call '.str' method on each value
        let result = hooks
          .get_method(item, self.method_name)
          .and_then(|method| hooks.call_method(item, method, &[]))?;

        if_let_obj!(ObjectKind::String(string) = (result) {
          buf.push_str(&string);
          buf.push_str(", ");
        } else {
          // if error throw away temporary strings
          return hooks.call(
            self.error,
            &[val!(hooks.manage_str(format!(
              "Expected type str from {item}.str()"
            )))],
          );
        });
      }

      if_let_obj!(ObjectKind::String(string) = (*last) {
        quote_string(&mut buf, &string);
      } else {
        // call '.str' method on each value
        let result = hooks
          .get_method(*last, self.method_name)
          .and_then(|method| hooks.call_method(*last, method, &[]))?;

        if_let_obj!(ObjectKind::String(string) = (result) {
          buf.push_str(&string);
        } else {
          // if error throw away temporary strings
          return hooks.call(
            self.error,
            &[val!(hooks.manage_str(format!(
              "Expected type str from {}.str()",
              *last
            )))],
          );
        });
      })
    }

    buf.push(')');
    Call::Ok(val!(hooks.manage_str(buf)))
  }
}

native_with_error!(TupleSlice, TUPLE_SLICE);

impl LyNative for TupleSlice {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    // get underlying string slice
    let tuple = this.unwrap().to_obj().to_tuple();

    let (start, end) = match args.len() {
      0 => (0.0, tuple.len() as f64),
      1 => (args[0].to_num(), tuple.len() as f64),
      2 => (args[0].to_num(), args[1].to_num()),
      _ => panic!("list slice should only been passed 0, 1 or 2 parameters"),
    };

    // get start and end indices
    let start_index = self.index(hooks, &tuple, start)?;
    let end_index = self.index(hooks, &tuple, end)?;

    let start_index = start_index.max(0);
    let end_index = end_index.min(tuple.len());

    if start_index <= end_index {
      Call::Ok(val!(
        hooks.manage_obj(List::from(&tuple[start_index..end_index]))
      ))
    } else {
      Call::Ok(val!(hooks.manage_obj(List::new())))
    }
  }
}

impl TupleSlice {
  fn index(&self, hooks: &mut Hooks, list: &[Value], index: f64) -> LyResult<usize> {
    // eliminate non integers
    if index.fract() != 0.0 {
      return LyResult::Err(
        self
          .call_error(hooks, "Method slice takes integer parameters")
          .expect_err("Expected Err"),
      );
    }

    if index >= 0.0 {
      LyResult::Ok(index as usize)
    } else {
      LyResult::Ok(list.len().saturating_sub(-index as usize))
    }
  }
}

fn determine_index(tuple: &[Value], index: f64) -> Result<usize, String> {
  if index.fract() != 0.0 {
    return Err("Index must be an integer.".to_string());
  }

  if index < 0.0 {
    let negated_index = (-index) as usize;

    if negated_index > tuple.len() {
      return Err(format!(
        "Index out of bounds. list was length {} but attempted to index with -{}.",
        tuple.len(),
        negated_index
      ));
    }

    Ok(tuple.len() - negated_index)
  } else {
    let index = index as usize;

    if index >= tuple.len() {
      return Err(format!(
        "Index out of bounds. list was length {} but attempted to index with {}.",
        tuple.len(),
        index
      ));
    }

    Ok(index)
  }
}

native_with_error!(TupleIndexGet, TUPLE_INDEX_GET);

impl LyNative for TupleIndexGet {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let index = args[0].to_num();
    let tuple = this.unwrap().to_obj().to_tuple();

    match determine_index(&tuple, index) {
      Ok(index) => Ok(tuple[index]),
      Err(message) => self.call_error(hooks, message),
    }
  }
}

native!(TupleLen, TUPLE_LEN);

impl LyNative for TupleLen {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    Call::Ok(val!(this.unwrap().to_obj().to_tuple().len() as f64))
  }
}

native!(TupleIndex, TUPLE_INDEX);

impl LyNative for TupleIndex {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let item = args[0];
    let index = this
      .unwrap()
      .to_obj()
      .to_tuple()
      .iter()
      .position(|x| *x == item);

    Call::Ok(index.map(|i| val!(i as f64)).unwrap_or(VALUE_NIL))
  }
}

native!(TupleHas, TUPLE_HAS);

impl LyNative for TupleHas {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    Call::Ok(val!(this.unwrap().to_obj().to_tuple().contains(&args[0])))
  }
}

native!(TupleIter, TUPLE_ITER);

impl LyNative for TupleIter {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    let inner_iter: Box<dyn Enumerate> =
      Box::new(TupleIterator::new(this.unwrap().to_obj().to_tuple()));
    let iter = Enumerator::new(inner_iter);
    let iter = hooks.manage_obj(iter);

    Call::Ok(val!(iter))
  }
}

native!(TupleCollect, TUPLE_COLLECT);

impl LyNative for TupleCollect {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> Call {
    let mut iter = args[0].to_obj().to_enumerator();
    let mut list = hooks.manage_obj(match iter.size_hint() {
      Some(size) => List::with_capacity(size),
      None => List::new(),
    });

    hooks.push_root(list);

    while !is_falsey(iter.next(hooks)?) {
      let current = iter.current();
      list.push(current);
    }

    hooks.pop_roots(1);
    Call::Ok(val!(hooks.manage_tuple(&list)))
  }
}

#[derive(Debug)]
struct TupleIterator {
  tuple: Tuple,
  current: Value,
  iter: Iter<'static, Value>,
}

impl TupleIterator {
  fn new(tuple: Tuple) -> Self {
    let iter = unsafe { tuple.deref_static().iter() };

    Self {
      iter,
      current: VALUE_NIL,
      tuple,
    }
  }
}

impl Enumerate for TupleIterator {
  fn name(&self) -> &str {
    "Tuple"
  }

  fn current(&self) -> Value {
    self.current
  }

  fn next(&mut self, _hooks: &mut Hooks) -> Call {
    match self.iter.next() {
      Some(value) => {
        self.current = *value;
        Call::Ok(val!(true))
      },
      None => {
        self.current = VALUE_NIL;
        Call::Ok(val!(false))
      },
    }
  }

  fn size_hint(&self) -> Option<usize> {
    Some(self.tuple.len())
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

impl Trace for TupleIterator {
  fn trace(&self) {
    self.tuple.trace();
  }

  fn trace_debug(&self, stdout: &mut dyn Write) {
    self.tuple.trace_debug(stdout);
  }
}

impl DebugHeap for TupleIterator {
  fn fmt_heap(&self, f: &mut std::fmt::Formatter, depth: usize) -> std::fmt::Result {
    f.debug_struct("TupleIterator")
      .field("tuple", &DebugWrap(&self.tuple, depth))
      .field("current", &DebugWrap(&self.current, depth))
      .field("iter", &"*")
      .finish()
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod index_get {
    use super::*;
    use crate::support::{test_error_class, MockedContext};

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let error = val!(test_error_class(&hooks));
      let tuple_index_get = TupleIndexGet::native(&hooks, error);

      assert_eq!(tuple_index_get.meta().name, "[]");
      assert_eq!(tuple_index_get.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        tuple_index_get.meta().signature.parameters[0].kind,
        ParameterKind::Number
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let error = val!(test_error_class(&hooks.as_gc()));
      let tuple_index_get = TupleIndexGet::native(&hooks.as_gc(), error);

      let values = &[val!(0.0)];

      let this = hooks.manage_tuple(&[VALUE_NIL, val!(10.0)]);

      let result = tuple_index_get.call(&mut hooks, Some(val!(this)), values);
      match result {
        Call::Ok(r) => assert!(r.is_nil()),
        _ => assert!(false),
      }

      assert_eq!(this[0], VALUE_NIL)
    }
  }

  mod str {
    use laythe_core::memory::NO_GC;

    use super::*;
    use crate::support::{test_error_class, test_native_dependencies, MockedContext};

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);
      let error = val!(test_error_class(&hooks));

      let list_str = TupleStr::native(&hooks, hooks.manage_str("str".to_string()), error);

      assert_eq!(list_str.meta().name, "str");
      assert_eq!(list_str.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut gc = test_native_dependencies();
      let mut context = MockedContext::with_std(&[
        val!(gc.manage_str("nil".to_string(), &NO_GC)),
        val!(gc.manage_str("10".to_string(), &NO_GC)),
        val!(gc.manage_str("[5]".to_string(), &NO_GC)),
      ])
      .unwrap();
      let mut hooks = Hooks::new(&mut context);
      let error = val!(test_error_class(&hooks.as_gc()));
      let list_str = TupleStr::native(&hooks.as_gc(), hooks.manage_str("str".to_string()), error);

      let values = &[];

      let this = hooks.manage_tuple(&[
        VALUE_NIL,
        val!(10.0),
        val!(hooks.manage_obj(List::from(vec![val!(5.0)]))),
      ]);

      let result = list_str.call(&mut hooks, Some(val!(this)), values);
      match result {
        Call::Ok(r) => assert_eq!(&*r.to_obj().to_str(), "(nil, 10, [5])"),
        _ => assert!(false),
      }
    }
  }

  mod slice {
    use super::*;
    use crate::support::{test_error_class, MockedContext};
    use laythe_core::hooks::Hooks;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = Hooks::new(&mut context);
      let error = val!(test_error_class(&hooks.as_gc()));

      let tuple_slice = TupleSlice::native(&hooks.as_gc(), error);

      assert_eq!(tuple_slice.meta().name, "slice");
      assert_eq!(tuple_slice.meta().signature.arity, Arity::Default(0, 2));
      assert_eq!(
        tuple_slice.meta().signature.parameters[0].kind,
        ParameterKind::Number
      );
      assert_eq!(
        tuple_slice.meta().signature.parameters[1].kind,
        ParameterKind::Number
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let error = val!(test_error_class(&hooks.as_gc()));

      let tuple_slice = TupleSlice::native(&hooks.as_gc(), error);

      let this = hooks.manage_tuple(&[val!(1.0), val!(2.0), val!(3.0)]);

      let result = tuple_slice
        .call(&mut hooks, Some(val!(this)), &[val!(0.0), val!(2.0)])
        .unwrap();
      assert!(result.is_obj_kind(ObjectKind::List));

      assert_eq!(result.to_obj().to_list().len(), 2);
      assert_eq!(result.to_obj().to_list()[0], val!(1.0));
      assert_eq!(result.to_obj().to_list()[1], val!(2.0));
    }
  }

  mod len {
    use super::*;
    use crate::support::MockedContext;
    use laythe_core::hooks::Hooks;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let list_size = TupleLen::native(&hooks);

      assert_eq!(list_size.meta().name, "len");
      assert_eq!(list_size.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let list_size = TupleLen::native(&hooks.as_gc());

      let values = &[];

      let this = hooks.manage_tuple(&[VALUE_NIL, val!(10.0)]);

      let result = list_size.call(&mut hooks, Some(val!(this)), values);
      match result {
        Call::Ok(r) => assert_eq!(r.to_num(), 2.0),
        _ => assert!(false),
      }
    }
  }

  mod index {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let tuple_index = TupleIndex::native(&hooks);

      assert_eq!(tuple_index.meta().name, "index");
      assert_eq!(tuple_index.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        tuple_index.meta().signature.parameters[0].kind,
        ParameterKind::Any
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let tuple_index = TupleIndex::native(&hooks.as_gc());

      let this = hooks.manage_tuple(&[VALUE_NIL, val!(10.0), val!(true)]);
      let tuple_value = Some(val!(this));

      let result = tuple_index.call(&mut hooks, tuple_value, &[val!(10.0)]);
      match result {
        Call::Ok(r) => {
          assert_eq!(r.to_num(), 1.0);
        },
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

      let tuple_has = TupleHas::native(&hooks);

      assert_eq!(tuple_has.meta().name, "has");
      assert_eq!(tuple_has.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        tuple_has.meta().signature.parameters[0].kind,
        ParameterKind::Any
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let tuple_hash = TupleHas::native(&hooks.as_gc());

      let this = hooks.manage_tuple(&[VALUE_NIL, val!(10.0), val!(true)]);
      let tuple_value = Some(val!(this));

      let result = tuple_hash.call(&mut hooks, tuple_value, &[val!(10.0)]);
      match result {
        Call::Ok(r) => {
          assert!(r.to_bool());
        },
        _ => assert!(false),
      }

      let result = tuple_hash.call(&mut hooks, tuple_value, &[val!(false)]);
      match result {
        Call::Ok(r) => {
          assert!(!r.to_bool());
        },
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

      let tuple_iter = TupleIter::native(&hooks);

      assert_eq!(tuple_iter.meta().name, "iter");
      assert_eq!(tuple_iter.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let tuple_iter = TupleIter::native(&hooks.as_gc());

      let this = hooks.manage_tuple(&[VALUE_NIL, val!(10.0), val!(true)]);
      let tuple_value = Some(val!(this));

      let result = tuple_iter.call(&mut hooks, tuple_value, &[]);
      match result {
        Call::Ok(r) => {
          let mut iter = r.to_obj().to_enumerator();
          assert_eq!(iter.current(), VALUE_NIL);
          assert_eq!(iter.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(iter.current(), VALUE_NIL);
        },
        _ => assert!(false),
      }
    }
  }

  mod collect {
    use super::*;
    use crate::support::{test_iter, MockedContext};

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let tuple_collect = TupleCollect::native(&hooks);

      assert_eq!(tuple_collect.meta().name, "collect");
      assert_eq!(tuple_collect.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        tuple_collect.meta().signature.parameters[0].kind,
        ParameterKind::Enumerator
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let tuple_collect = TupleCollect::native(&hooks.as_gc());

      let iter = test_iter();
      let iter_value = val!(hooks.manage_obj(Enumerator::new(iter)));

      let result = tuple_collect.call(&mut hooks, None, &[iter_value]);
      match result {
        Call::Ok(r) => {
          let list = r.to_obj().to_tuple();
          assert_eq!(list.len(), 4);
        },
        _ => assert!(false),
      }
    }
  }
}
