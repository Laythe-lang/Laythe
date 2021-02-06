use crate::{
  native, native_with_error,
  support::{export_and_insert, load_class_from_module, to_dyn_native},
  StdResult,
};
use laythe_core::{
  constants::{INDEX_GET, INDEX_SET},
  get,
  hooks::{GcHooks, Hooks},
  iterator::{LyIter, LyIterator},
  managed::{Gc, GcStr, Trace},
  module::Module,
  native::{MetaData, Native, NativeMeta, NativeMetaBuilder},
  object::List,
  signature::{Arity, ParameterBuilder, ParameterKind},
  utils::is_falsey,
  val,
  value::{Value, VALUE_NIL},
  Call, LyResult,
};
use std::{cmp::Ordering, io::Write};
use std::{mem, slice::Iter};

use super::{
  class_inheritance,
  error::{INDEX_ERROR_NAME, TYPE_ERROR_NAME},
};

pub const LIST_CLASS_NAME: &str = "List";

const LIST_INDEX_GET: NativeMetaBuilder = NativeMetaBuilder::method(INDEX_GET, Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("index", ParameterKind::Number)]);

const LIST_INDEX_SET: NativeMetaBuilder = NativeMetaBuilder::method(INDEX_SET, Arity::Fixed(2))
  .with_params(&[
    ParameterBuilder::new("val", ParameterKind::Any),
    ParameterBuilder::new("index", ParameterKind::Number),
  ]);

const LIST_CLEAR: NativeMetaBuilder = NativeMetaBuilder::method("clear", Arity::Fixed(0));

const LIST_HAS: NativeMetaBuilder = NativeMetaBuilder::method("has", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("val", ParameterKind::Any)]);

const LIST_INSERT: NativeMetaBuilder = NativeMetaBuilder::method("insert", Arity::Fixed(2))
  .with_params(&[
    ParameterBuilder::new("index", ParameterKind::Number),
    ParameterBuilder::new("val", ParameterKind::Any),
  ]);

const LIST_ITER: NativeMetaBuilder = NativeMetaBuilder::method("iter", Arity::Fixed(0));
const LIST_POP: NativeMetaBuilder = NativeMetaBuilder::method("pop", Arity::Fixed(0));

const LIST_PUSH: NativeMetaBuilder = NativeMetaBuilder::method("push", Arity::Variadic(0))
  .with_params(&[ParameterBuilder::new("values", ParameterKind::Any)]);

const LIST_REMOVE: NativeMetaBuilder = NativeMetaBuilder::method("remove", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("index", ParameterKind::Number)]);

const LIST_INDEX: NativeMetaBuilder = NativeMetaBuilder::method("index", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("value", ParameterKind::Any)]);

const LIST_LEN: NativeMetaBuilder = NativeMetaBuilder::method("len", Arity::Fixed(0));
const LIST_STR: NativeMetaBuilder = NativeMetaBuilder::method("str", Arity::Fixed(0));

const LIST_SLICE: NativeMetaBuilder = NativeMetaBuilder::method("slice", Arity::Default(0, 2))
  .with_params(&[
    ParameterBuilder::new("start", ParameterKind::Number),
    ParameterBuilder::new("end", ParameterKind::Number),
  ]);

const LIST_REV: NativeMetaBuilder = NativeMetaBuilder::method("rev", Arity::Fixed(0));

const LIST_SORT: NativeMetaBuilder = NativeMetaBuilder::method("sort", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("comparator", ParameterKind::Fun)])
  .with_stack();

// this may need a stack
const LIST_COLLECT: NativeMetaBuilder = NativeMetaBuilder::fun("collect", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("iter", ParameterKind::Iter)]);

pub fn declare_list_class(hooks: &GcHooks, module: &mut Module) -> StdResult<()> {
  let class = class_inheritance(hooks, module, LIST_CLASS_NAME)?;

  export_and_insert(hooks, module, class.name(), val!(class))
}

pub fn define_list_class(hooks: &GcHooks, module: &Module) -> StdResult<()> {
  let mut class = load_class_from_module(hooks, module, LIST_CLASS_NAME)?;
  let index_error = val!(load_class_from_module(hooks, module, INDEX_ERROR_NAME)?);
  let type_error = val!(load_class_from_module(hooks, module, TYPE_ERROR_NAME)?);

  class.add_method(
    hooks,
    hooks.manage_str(LIST_INDEX_GET.name),
    val!(to_dyn_native(hooks, ListIndexGet::new(hooks, index_error))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(LIST_INDEX_SET.name),
    val!(to_dyn_native(hooks, ListIndexSet::new(hooks, index_error))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(LIST_LEN.name),
    val!(to_dyn_native(hooks, ListLen::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(LIST_PUSH.name),
    val!(to_dyn_native(hooks, ListPush::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(LIST_POP.name),
    val!(to_dyn_native(hooks, ListPop::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(LIST_REMOVE.name),
    val!(to_dyn_native(hooks, ListRemove::new(hooks, index_error))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(LIST_INDEX.name),
    val!(to_dyn_native(hooks, ListIndex::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(LIST_INSERT.name),
    val!(to_dyn_native(hooks, ListInsert::new(hooks, index_error))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(LIST_STR.name),
    val!(to_dyn_native(
      hooks,
      ListStr::new(
        LIST_STR.to_meta(&hooks),
        hooks.manage_str(LIST_STR.name),
        type_error,
      ),
    )),
  );

  class.add_method(
    hooks,
    hooks.manage_str(LIST_SLICE.name),
    val!(to_dyn_native(hooks, ListSlice::new(hooks, index_error))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(LIST_CLEAR.name),
    val!(to_dyn_native(hooks, ListClear::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(LIST_HAS.name),
    val!(to_dyn_native(hooks, ListHas::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(LIST_ITER.name),
    val!(to_dyn_native(hooks, ListIter::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(LIST_REV.name),
    val!(to_dyn_native(hooks, ListRev::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(LIST_SORT.name),
    val!(to_dyn_native(hooks, ListSort::new(hooks, type_error))),
  );

  class.meta_class().expect("Meta class not set.").add_method(
    hooks,
    hooks.manage_str(LIST_COLLECT.name),
    val!(to_dyn_native(hooks, ListCollect::from(hooks))),
  );

  Ok(())
}

#[derive(Debug)]
struct ListStr {
  meta: NativeMeta,
  method_name: GcStr,
  error: Value,
}

impl ListStr {
  fn new(meta: NativeMeta, method_name: GcStr, error: Value) -> Self {
    Self {
      meta,
      method_name,
      error,
    }
  }
}

impl Trace for ListStr {
  fn trace(&self) {
    self.method_name.trace();
  }

  fn trace_debug(&self, log: &mut dyn Write) {
    self.method_name.trace_debug(log);
  }
}

impl MetaData for ListStr {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }
}

impl Native for ListStr {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    let list = this.unwrap().to_list();

    // buffer for temporary strings
    let mut strings: Vec<String> = Vec::with_capacity(list.len());
    let mut count: usize = 0;

    for item in list.iter() {
      // if already string quote and add to temps
      if item.is_str() {
        strings.push(format!("'{}'", item.to_str()));
        continue;
      }

      // call '.str' method on each value
      let str_result = hooks
        .get_method(*item, self.method_name)
        .and_then(|method| hooks.call_method(*item, method, &[]));

      match str_result {
        Call::Ok(result) => {
          if result.is_str() {
            let string = result.to_str();
            count += 1;
            hooks.push_root(string);
            strings.push(string.to_string());
          } else {
            // if error throw away temporary strings
            hooks.pop_roots(count);
            return hooks.call(
              self.error,
              &[val!(hooks.manage_str(format!(
                "Expected type str from {}.str()",
                item
              )))],
            );
          }
        }
        Call::Err(err) => {
          hooks.pop_roots(count);
          return Call::Err(err);
        }
        Call::Exit(exit) => {
          hooks.pop_roots(count);
          return Call::Exit(exit);
        }
      }
    }

    // format and join strings
    let formatted = format!("[{}]", strings.join(", "));

    // pop temporary roots and return joined string
    hooks.pop_roots(count);
    Call::Ok(val!(hooks.manage_str(formatted)))
  }
}

native_with_error!(ListSlice, LIST_SLICE);

impl Native for ListSlice {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    // get underlying string slice
    let list = this.unwrap().to_list();

    let (start, end) = match args.len() {
      0 => (0.0, list.len() as f64),
      1 => (args[0].to_num(), list.len() as f64),
      2 => (args[0].to_num(), args[1].to_num()),
      _ => panic!("list slice should only been passed 0, 1 or 2 parameters"),
    };

    // get start and end indices
    let start_index = match self.index(hooks, &list, start) {
      LyResult::Ok(index) => index,
      LyResult::Err(err) => return LyResult::Err(err),
      LyResult::Exit(exit) => return LyResult::Exit(exit),
    };

    let end_index = match self.index(hooks, &list, end) {
      LyResult::Ok(index) => index,
      LyResult::Err(err) => return LyResult::Err(err),
      LyResult::Exit(exit) => return LyResult::Exit(exit),
    };

    let start_index = start_index.max(0);
    let end_index = end_index.min(list.len());

    if start_index <= end_index {
      Call::Ok(val!(hooks.manage(List::from(&list[start_index..end_index]))))
    } else {
      Call::Ok(val!(hooks.manage(List::new())))
    }
  }
}

impl ListSlice {
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

native_with_error!(ListIndexGet, LIST_INDEX_GET);

impl Native for ListIndexGet {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let index = args[0].to_num();
    let list = this.unwrap().to_list();

    if index.fract() != 0.0 {
      return LyResult::Err(
        self
          .call_error(hooks, "Index must be an integer.")
          .expect_err("Expected error"),
      );
    }

    let index = index as usize;
    if index >= list.len() {
      return self.call_error(
        hooks,
        format!(
          "Index out of bounds. list was length {} but attempted to index with {}.",
          list.len(),
          index
        ),
      );
    }

    Call::Ok(list[index])
  }
}

native_with_error!(ListIndexSet, LIST_INDEX_SET);

impl Native for ListIndexSet {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let index = args[1].to_num();
    let mut list = this.unwrap().to_list();

    if index.fract() != 0.0 {
      return LyResult::Err(
        self
          .call_error(hooks, "Index must be an integer.")
          .expect_err("Expected error"),
      );
    }

    let index = index as usize;
    if index >= list.len() {
      return self.call_error(
        hooks,
        &format!(
          "Index out of bounds. list was length {} but attempted to index with {}.",
          list.len(),
          index
        ),
      );
    }

    list[index] = args[0];
    Call::Ok(VALUE_NIL)
  }
}

native!(ListLen, LIST_LEN);

impl Native for ListLen {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    Call::Ok(val!(this.unwrap().to_list().len() as f64))
  }
}

native!(ListPush, LIST_PUSH);

impl Native for ListPush {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    hooks.grow(&mut this.unwrap().to_list(), |list| {
      list.extend_from_slice(args)
    });
    Call::Ok(VALUE_NIL)
  }
}

native!(ListPop, LIST_POP);

impl Native for ListPop {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    match this.unwrap().to_list().pop() {
      Some(value) => Call::Ok(value),
      None => Call::Ok(VALUE_NIL),
    }
  }
}

native_with_error!(ListRemove, LIST_REMOVE);

impl Native for ListRemove {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let index = args[0].to_num();
    let mut list = this.unwrap().to_list();

    if index < 0.0 {
      return self.call_error(
        hooks,
        &format!("Cannot remove at negative index {}.", index),
      );
    }

    if index as usize >= list.len() {
      return self.call_error(
        hooks,
        &format!(
          "Cannot remove at index {}, list has size {}",
          index,
          list.len()
        ),
      );
    }

    let result = list.remove(index as usize);
    Call::Ok(result)
  }
}

native!(ListIndex, LIST_INDEX);

impl Native for ListIndex {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let item = args[0];
    let index = this.unwrap().to_list().iter().position(|x| *x == item);

    Call::Ok(index.map(|i| val!(i as f64)).unwrap_or(VALUE_NIL))
  }
}

native_with_error!(ListInsert, LIST_INSERT);

impl Native for ListInsert {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let index = args[0].to_num();
    let mut list = this.unwrap().to_list();

    if index < 0.0 {
      return self.call_error(hooks, format!("Cannot insert at index {}", index));
    }

    if index as usize > list.len() {
      return self.call_error(
        hooks,
        format!(
          "Cannot insert at index {}, list has size {}",
          index,
          list.len()
        ),
      );
    }

    hooks.grow(&mut list, |list| list.insert(index as usize, args[1]));
    Call::Ok(VALUE_NIL)
  }
}

native!(ListClear, LIST_CLEAR);

impl Native for ListClear {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    hooks.shrink(&mut this.unwrap().to_list(), |list| list.clear());
    Call::Ok(VALUE_NIL)
  }
}

native!(ListHas, LIST_HAS);

impl Native for ListHas {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    Call::Ok(val!(this.unwrap().to_list().contains(&args[0])))
  }
}

native!(ListIter, LIST_ITER);

impl Native for ListIter {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    let inner_iter: Box<dyn LyIter> = Box::new(ListIterator::new(this.unwrap().to_list()));
    let iter = LyIterator::new(inner_iter);
    let iter = hooks.manage(iter);

    Call::Ok(val!(iter))
  }
}

native!(ListRev, LIST_REV);

impl Native for ListRev {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    let list = this.unwrap().to_list();
    let rev: List<Value> = list.iter().cloned().rev().collect();
    Call::Ok(val!(hooks.manage(rev)))
  }
}

native_with_error!(ListSort, LIST_SORT);

impl Native for ListSort {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let comparator = args[0];
    let mut list = hooks.manage(this.unwrap().to_list().to_list());
    hooks.push_root(list);

    let mut failure: Option<Call> = None;
    list.sort_by(|a, b| {
      if failure.is_some() {
        return Ordering::Equal;
      }

      match hooks.call(comparator, &[*a, *b]) {
        laythe_core::LyResult::Ok(result) => {
          if result.is_num() {
            match result.to_num().partial_cmp(&0.0) {
              Some(ord) => ord,
              None => {
                failure.get_or_insert_with(|| {
                  self.call_error(hooks, "comparator must return a valid number.")
                });
                Ordering::Equal
              }
            }
          } else {
            failure
              .get_or_insert_with(|| self.call_error(hooks, "comparator must return a number."));
            Ordering::Equal
          }
        }
        laythe_core::LyResult::Err(err) => {
          failure.get_or_insert(Call::Err(err));
          Ordering::Equal
        }
        laythe_core::LyResult::Exit(code) => {
          failure.get_or_insert(Call::Exit(code));
          Ordering::Equal
        }
      }
    });

    hooks.pop_roots(1);
    Call::Ok(val!(list))
  }
}

native!(ListCollect, LIST_COLLECT);

impl Native for ListCollect {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> Call {
    let mut iter = args[0].to_iter();
    let mut list = hooks.manage(match iter.size_hint() {
      Some(size) => List::with_capacity(size),
      None => List::new(),
    });

    hooks.push_root(list);

    while !is_falsey(get!(iter.next(hooks))) {
      let current = iter.current();
      hooks.grow(&mut *list, |list| list.push(current));
    }

    hooks.pop_roots(1);
    Call::Ok(val!(list))
  }
}

#[derive(Debug)]
struct ListIterator {
  list: Gc<List<Value>>,
  current: Value,
  iter: Iter<'static, Value>,
}

impl ListIterator {
  fn new(list: Gc<List<Value>>) -> Self {
    let iter = unsafe { list.deref_static().iter() };

    Self {
      iter,
      current: VALUE_NIL,
      list,
    }
  }
}

impl LyIter for ListIterator {
  fn name(&self) -> &str {
    "List Iterator"
  }

  fn current(&self) -> Value {
    self.current
  }

  fn next(&mut self, _hooks: &mut Hooks) -> Call {
    match self.iter.next() {
      Some(value) => {
        self.current = *value;
        Call::Ok(val!(true))
      }
      None => {
        self.current = VALUE_NIL;
        Call::Ok(val!(false))
      }
    }
  }

  fn size_hint(&self) -> Option<usize> {
    Some(self.list.len())
  }

  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }
}

impl Trace for ListIterator {
  fn trace(&self) {
    self.list.trace();
  }

  fn trace_debug(&self, stdout: &mut dyn Write) {
    self.list.trace_debug(stdout);
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
      let list_index_get = ListIndexGet::new(&hooks, error);

      assert_eq!(list_index_get.meta().name, "[]");
      assert_eq!(list_index_get.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        list_index_get.meta().signature.parameters[0].kind,
        ParameterKind::Number
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let error = val!(test_error_class(&hooks.as_gc()));
      let list_index_get = ListIndexGet::new(&hooks.as_gc(), error);

      let values = &[val!(0.0)];

      let list = List::from(vec![VALUE_NIL, val!(10.0)]);
      let this = hooks.manage(list);

      let result = list_index_get.call(&mut hooks, Some(val!(this)), values);
      match result {
        Call::Ok(r) => assert!(r.is_nil()),
        _ => assert!(false),
      }

      assert_eq!(this[0], VALUE_NIL)
    }
  }

  mod index_set {
    use super::*;
    use crate::support::{test_error_class, MockedContext};

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let error = val!(test_error_class(&hooks));
      let list_index_set = ListIndexSet::new(&hooks, error);

      assert_eq!(list_index_set.meta().name, "[]=");
      assert_eq!(list_index_set.meta().signature.arity, Arity::Fixed(2));
      assert_eq!(
        list_index_set.meta().signature.parameters[0].kind,
        ParameterKind::Any
      );
      assert_eq!(
        list_index_set.meta().signature.parameters[1].kind,
        ParameterKind::Number
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let error = val!(test_error_class(&hooks.as_gc()));
      let list_index_set = ListIndexSet::new(&hooks.as_gc(), error);

      let values = &[val!(false), val!(1.0)];

      let list = List::from(vec![VALUE_NIL, val!(10.0)]);
      let this = hooks.manage(list);

      let result = list_index_set
        .call(&mut hooks, Some(val!(this)), values)
        .unwrap();
      assert_eq!(result, VALUE_NIL);
      assert_eq!(this[1], val!(false))
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

      let list_str = ListStr::new(
        LIST_STR.to_meta(&hooks),
        hooks.manage_str("str".to_string()),
        error,
      );

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
      let list_str = ListStr::new(
        LIST_STR.to_meta(&hooks.as_gc()),
        hooks.manage_str("str".to_string()),
        error,
      );

      let values = &[];

      let list = List::from(vec![
        VALUE_NIL,
        val!(10.0),
        val!(hooks.manage(List::from(vec![val!(5.0)]))),
      ]);
      let this = hooks.manage(list);

      let result = list_str.call(&mut hooks, Some(val!(this)), values);
      match result {
        Call::Ok(r) => assert_eq!(&*r.to_str(), "[nil, 10, [5]]"),
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

      let list_slice = ListSlice::new(&hooks.as_gc(), error);

      assert_eq!(list_slice.meta().name, "slice");
      assert_eq!(list_slice.meta().signature.arity, Arity::Default(0, 2));
      assert_eq!(
        list_slice.meta().signature.parameters[0].kind,
        ParameterKind::Number
      );
      assert_eq!(
        list_slice.meta().signature.parameters[1].kind,
        ParameterKind::Number
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let error = val!(test_error_class(&hooks.as_gc()));

      let list_slice = ListSlice::new(&hooks.as_gc(), error);

      let list = List::from(vec![val!(1.0), val!(2.0), val!(3.0)]);
      let this = hooks.manage(list);

      let result = list_slice
        .call(&mut hooks, Some(val!(this)), &[val!(0.0), val!(2.0)])
        .unwrap();
      assert!(result.is_list());

      assert_eq!(result.to_list().len(), 2);
      assert_eq!(result.to_list()[0], val!(1.0));
      assert_eq!(result.to_list()[1], val!(2.0));
    }
  }

  mod len {
    use super::*;
    use crate::support::MockedContext;
    use laythe_core::hooks::Hooks;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = Hooks::new(&mut context);

      let list_size = ListLen::from(&hooks);

      assert_eq!(list_size.meta().name, "len");
      assert_eq!(list_size.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let list_size = ListLen::from(&hooks);

      let values = &[];

      let list = List::from(vec![VALUE_NIL, val!(10.0)]);
      let this = hooks.manage(list);

      let result = list_size.call(&mut hooks, Some(val!(this)), values);
      match result {
        Call::Ok(r) => assert_eq!(r.to_num(), 2.0),
        _ => assert!(false),
      }
    }
  }

  mod push {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = Hooks::new(&mut context);

      let list_push = ListPush::from(&hooks);

      assert_eq!(list_push.meta().name, "push");
      assert_eq!(list_push.meta().signature.arity, Arity::Variadic(0));
      assert_eq!(
        list_push.meta().signature.parameters[0].kind,
        ParameterKind::Any
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let list_push = ListPush::from(&hooks);

      let list = List::from(vec![VALUE_NIL, val!(10.0)]);
      let this = hooks.manage(list);
      let list_value = Some(val!(this));

      let result = list_push.call(&mut hooks, list_value, &[val!(false)]);
      match result {
        Call::Ok(r) => {
          assert_eq!(r, VALUE_NIL);
          assert_eq!(list_value.unwrap().to_list().len(), 3);
          assert_eq!(list_value.unwrap().to_list()[2], val!(false));
        }
        _ => assert!(false),
      }

      let result = list_push.call(&mut hooks, Some(val!(this)), &[val!(10.3), VALUE_NIL]);
      match result {
        Call::Ok(r) => {
          assert_eq!(r, VALUE_NIL);
          assert_eq!(list_value.unwrap().to_list().len(), 5);
          assert_eq!(list_value.unwrap().to_list()[3], val!(10.3));
          assert_eq!(list_value.unwrap().to_list()[4], VALUE_NIL);
        }
        _ => assert!(false),
      }
    }
  }

  mod pop {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let list_pop = ListPop::from(&hooks);

      assert_eq!(list_pop.meta().name, "pop");
      assert_eq!(list_pop.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let list_pop = ListPop::from(&hooks);

      let list = List::from(vec![val!(true)]);
      let this = hooks.manage(list);
      let list_value = Some(val!(this));

      let result = list_pop.call(&mut hooks, list_value, &[]);
      match result {
        Call::Ok(r) => {
          assert_eq!(r.to_bool(), true);
          assert_eq!(this.len(), 0);
        }
        _ => assert!(false),
      }

      let result = list_pop.call(&mut hooks, list_value, &[]);
      match result {
        Call::Ok(r) => {
          assert!(r.is_nil());
          assert_eq!(this.len(), 0);
        }
        _ => assert!(false),
      }
    }
  }

  mod remove {
    use super::*;
    use crate::support::{test_error_class, MockedContext};

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);
      let error = val!(test_error_class(&hooks));

      let list_remove = ListRemove::new(&hooks, error);

      assert_eq!(list_remove.meta().name, "remove");
      assert_eq!(list_remove.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        list_remove.meta().signature.parameters[0].kind,
        ParameterKind::Number
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let error = val!(test_error_class(&hooks.as_gc()));
      let list_remove = ListRemove::new(&hooks.as_gc(), error);

      let list = List::from(&[VALUE_NIL, val!(10.0), val!(true)] as &[Value]);
      let this = hooks.manage(list);
      let list_value = Some(val!(this));

      let result = list_remove.call(&mut hooks, list_value, &[val!(1.0)]);
      match result {
        Call::Ok(r) => {
          assert_eq!(r.to_num(), 10.0);
          assert_eq!(this.len(), 2);
        }
        _ => assert!(false),
      }

      let result = list_remove.call(&mut hooks, list_value, &[val!(-1.0)]);
      match result {
        Call::Ok(_) => assert!(false),
        _ => assert!(true),
      }

      let result = list_remove.call(&mut hooks, list_value, &[val!(10.0)]);
      match result {
        Call::Ok(_) => assert!(false),
        _ => assert!(true),
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

      let list_index = ListIndex::from(&hooks);

      assert_eq!(list_index.meta().name, "index");
      assert_eq!(list_index.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        list_index.meta().signature.parameters[0].kind,
        ParameterKind::Any
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let list_index = ListIndex::from(&hooks);

      let list = List::from(&[VALUE_NIL, val!(10.0), val!(true)] as &[Value]);
      let this = hooks.manage(list);
      let list_value = Some(val!(this));

      let result = list_index.call(&mut hooks, list_value, &[val!(10.0)]);
      match result {
        Call::Ok(r) => {
          assert_eq!(r.to_num(), 1.0);
        }
        _ => assert!(false),
      }
    }
  }

  mod insert {
    use super::*;
    use crate::support::{test_error_class, MockedContext};

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);
      let error = val!(test_error_class(&hooks));

      let list_insert = ListInsert::new(&hooks, error);

      assert_eq!(list_insert.meta().name, "insert");
      assert_eq!(list_insert.meta().signature.arity, Arity::Fixed(2));
      assert_eq!(
        list_insert.meta().signature.parameters[0].kind,
        ParameterKind::Number
      );
      assert_eq!(
        list_insert.meta().signature.parameters[1].kind,
        ParameterKind::Any
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let error = val!(test_error_class(&hooks.as_gc()));

      let list_insert = ListInsert::new(&hooks.as_gc(), error);

      let list = List::from(&[VALUE_NIL, val!(10.0), val!(true)] as &[Value]);
      let this = hooks.manage(list);
      let list_value = Some(val!(this));

      let result = list_insert.call(&mut hooks, list_value, &[val!(1.0), val!(false)]);
      match result {
        Call::Ok(r) => {
          assert!(r.is_nil());
          assert_eq!(this[1], val!(false));
          assert_eq!(this.len(), 4);
        }
        _ => assert!(false),
      }

      let result = list_insert.call(&mut hooks, list_value, &[val!(-1.0)]);
      match result {
        Call::Ok(_) => assert!(false),
        _ => assert!(true),
      }

      let result = list_insert.call(&mut hooks, list_value, &[val!(10.0)]);
      match result {
        Call::Ok(_) => assert!(false),
        _ => assert!(true),
      }
    }
  }

  mod clear {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let list_clear = ListClear::from(&hooks);

      assert_eq!(list_clear.meta().name, "clear");
      assert_eq!(list_clear.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let list_clear = ListClear::from(&hooks);

      let list = List::from(&[VALUE_NIL, val!(10.0), val!(true)] as &[Value]);
      let this = hooks.manage(list);
      let list_value = Some(val!(this));

      let result = list_clear.call(&mut hooks, list_value, &[]);
      match result {
        Call::Ok(r) => {
          assert!(r.is_nil());
          assert_eq!(this.len(), 0);
        }
        _ => assert!(false),
      }

      let result = list_clear.call(&mut hooks, list_value, &[]);
      match result {
        Call::Ok(r) => {
          assert!(r.is_nil());
          assert_eq!(this.len(), 0);
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

      let list_has = ListHas::from(&hooks);

      assert_eq!(list_has.meta().name, "has");
      assert_eq!(list_has.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        list_has.meta().signature.parameters[0].kind,
        ParameterKind::Any
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let list_has = ListHas::from(&hooks);

      let list = List::from(&[VALUE_NIL, val!(10.0), val!(true)] as &[Value]);
      let this = hooks.manage(list);
      let list_value = Some(val!(this));

      let result = list_has.call(&mut hooks, list_value, &[val!(10.0)]);
      match result {
        Call::Ok(r) => {
          assert!(r.to_bool());
        }
        _ => assert!(false),
      }

      let result = list_has.call(&mut hooks, list_value, &[val!(false)]);
      match result {
        Call::Ok(r) => {
          assert!(!r.to_bool());
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

      let list_iter = ListIter::from(&hooks);

      assert_eq!(list_iter.meta().name, "iter");
      assert_eq!(list_iter.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let list_iter = ListIter::from(&hooks);

      let list = List::from(&[VALUE_NIL, val!(10.0), val!(true)] as &[Value]);
      let this = hooks.manage(list);
      let list_value = Some(val!(this));

      let result = list_iter.call(&mut hooks, list_value, &[]);
      match result {
        Call::Ok(r) => {
          let mut iter = r.to_iter();
          assert_eq!(iter.current(), VALUE_NIL);
          assert_eq!(iter.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(iter.current(), VALUE_NIL);
        }
        _ => assert!(false),
      }
    }
  }

  mod sort {
    use super::*;
    use crate::support::{test_error_class, MockedContext};

    const LIST_TEST: NativeMetaBuilder = NativeMetaBuilder::method(INDEX_SET, Arity::Fixed(2))
      .with_params(&[
        ParameterBuilder::new("a", ParameterKind::Number),
        ParameterBuilder::new("b", ParameterKind::Number),
      ]);

    native!(ListTest, LIST_TEST);

    impl Native for ListTest {
      fn call(&self, _hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> Call {
        Call::Ok(args[0])
      }
    }

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);
      let error = val!(test_error_class(&hooks));

      let list_sort = ListSort::new(&hooks, error);

      assert_eq!(list_sort.meta().name, "sort");
      assert_eq!(list_sort.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        list_sort.meta().signature.parameters[0].kind,
        ParameterKind::Fun
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(-2.0)]);
      let mut hooks = Hooks::new(&mut context);
      let error = val!(test_error_class(&hooks.as_gc()));

      let list_sort = ListSort::new(&hooks.as_gc(), error);
      let num_cmp = val!(to_dyn_native(&hooks.as_gc(), ListTest::from(&hooks)));

      let list = List::from(&[val!(3.0), val!(5.0)] as &[Value]);
      let this = hooks.manage(list);
      let list_value = Some(val!(this));

      let result = list_sort.call(&mut hooks, list_value, &[num_cmp]).unwrap();
      assert!(result.is_list());
      assert_eq!(result.to_list()[0], val!(5.0));
      assert_eq!(result.to_list()[1], val!(3.0));
    }
  }

  mod rev {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let list_sort = ListRev::from(&hooks);

      assert_eq!(list_sort.meta().name, "rev");
      assert_eq!(list_sort.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(-2.0)]);
      let mut hooks = Hooks::new(&mut context);

      let list_sort = ListRev::from(&hooks.as_gc());

      let list = List::from(vec![val!(3.0), val!(5.0)]);
      let this = hooks.manage(list);
      let list_value = Some(val!(this));

      let result = list_sort.call(&mut hooks, list_value, &[]).unwrap();
      assert!(result.is_list());
      assert_eq!(result.to_list()[0], val!(5.0));
      assert_eq!(result.to_list()[1], val!(3.0));
    }
  }

  mod collect {
    use super::*;
    use crate::support::{test_iter, MockedContext};

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let list_iter = ListCollect::from(&hooks);

      assert_eq!(list_iter.meta().name, "collect");
      assert_eq!(list_iter.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        list_iter.meta().signature.parameters[0].kind,
        ParameterKind::Iter
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let list_iter = ListCollect::from(&hooks);

      let iter = test_iter();
      let iter_value = val!(hooks.manage(LyIterator::new(iter)));

      let result = list_iter.call(&mut hooks, None, &[iter_value]);
      match result {
        Call::Ok(r) => {
          let list = r.to_list();
          assert_eq!(list.len(), 4);
        }
        _ => assert!(false),
      }
    }
  }
}
