use crate::{
  native,
  support::{default_class_inheritance, export_and_insert, load_class_from_module, to_dyn_native},
};
use laythe_core::{
  constants::INDEX_GET,
  constants::INDEX_SET,
  hooks::{GcHooks, Hooks},
  iterator::{LyIter, LyIterator},
  module::Module,
  native::{MetaData, Native, NativeMeta, NativeMetaBuilder},
  object::List,
  package::Package,
  signature::{Arity, ParameterBuilder, ParameterKind},
  utils::is_falsey,
  val,
  value::{Value, VALUE_NIL},
  CallResult, LyResult,
};
use laythe_env::managed::{Managed, Trace};
use smol_str::SmolStr;
use std::io::Write;
use std::{mem, slice::Iter};

pub const LIST_CLASS_NAME: &str = "List";

const LIST_INDEX_GET: NativeMetaBuilder = NativeMetaBuilder::method(INDEX_GET, Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("index", ParameterKind::Number)]);

const LIST_INDEX_SET: NativeMetaBuilder = NativeMetaBuilder::method(INDEX_SET, Arity::Fixed(2))
  .with_params(&[
    ParameterBuilder::new("index", ParameterKind::Number),
    ParameterBuilder::new("val", ParameterKind::Any),
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

// this may need a stack
const LIST_COLLECT: NativeMetaBuilder = NativeMetaBuilder::fun("collect", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("iter", ParameterKind::Iter)]);

pub fn declare_list_class(hooks: &GcHooks, module: &mut Module, package: &Package) -> LyResult<()> {
  let class = default_class_inheritance(hooks, package, LIST_CLASS_NAME)?;
  export_and_insert(hooks, module, class.name, val!(class))
}

pub fn define_list_class(hooks: &GcHooks, module: &Module, _: &Package) -> LyResult<()> {
  let mut class = load_class_from_module(hooks, module, LIST_CLASS_NAME)?;

  class.add_method(
    hooks,
    hooks.manage_str(LIST_INDEX_GET.name),
    val!(to_dyn_native(hooks, ListIndexGet::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(LIST_INDEX_SET.name),
    val!(to_dyn_native(hooks, ListIndexSet::from(hooks))),
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
    val!(to_dyn_native(hooks, ListRemove::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(LIST_INDEX.name),
    val!(to_dyn_native(hooks, ListIndex::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(LIST_INSERT.name),
    val!(to_dyn_native(hooks, ListInsert::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(LIST_STR.name),
    val!(to_dyn_native(
      hooks,
      ListStr::new(LIST_STR.to_meta(&hooks), hooks.manage_str(LIST_STR.name)),
    )),
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

  class.meta().expect("Meta class not set.").add_method(
    hooks,
    hooks.manage_str(LIST_COLLECT.name),
    val!(to_dyn_native(hooks, ListCollect::from(hooks))),
  );

  Ok(())
}

#[derive(Debug, Trace)]
struct ListStr {
  meta: NativeMeta,
  method_name: Managed<SmolStr>,
}

impl ListStr {
  fn new(meta: NativeMeta, method_name: Managed<SmolStr>) -> Self {
    Self { meta, method_name }
  }
}

impl MetaData for ListStr {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }
}

impl Native for ListStr {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> CallResult {
    let list = this.unwrap().to_list();

    // buffer for temporary strings
    let mut strings: Vec<String> = Vec::with_capacity(list.len());
    let mut count: usize = 0;

    for item in list.iter() {
      // if already string quote and add to temps
      if item.is_str() {
        strings.push(format!("'{}'", item.to_str().as_str()));
        continue;
      }

      // call '.str' method on each value
      let str_result = hooks
        .get_method(*item, self.method_name)
        .and_then(|method| hooks.call_method(*item, method, &[]));

      match str_result {
        Ok(result) => {
          if result.is_str() {
            let string = result.to_str();
            count += 1;
            hooks.push_root(string);
            strings.push(string.to_string());
          } else {
            // if error throw away temporary strings
            hooks.pop_roots(count);
            return hooks.error(format!("No method str on {}", item));
          }
        }
        Err(err) => {
          hooks.pop_roots(count);
          return Err(err);
        }
      }
    }

    // format and join strings
    let formatted = format!("[{}]", strings.join(", "));

    // pop temporary roots and return joined string
    hooks.pop_roots(count);
    Ok(val!(hooks.manage_str(formatted)))
  }
}

native!(ListIndexGet, LIST_INDEX_GET);

impl Native for ListIndexGet {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> CallResult {
    let rounded = args[0].to_num() as usize;
    let list = this.unwrap().to_list();

    if rounded >= list.len() {
      return hooks.error(format!(
        "Index out of bounds. list was length {} but attempted to index with {}.",
        list.len(),
        rounded
      ));
    }

    Ok(list[rounded])
  }
}

native!(ListIndexSet, LIST_INDEX_SET);

impl Native for ListIndexSet {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> CallResult {
    let rounded = args[0].to_num() as usize;
    let mut list = this.unwrap().to_list();

    if rounded >= list.len() {
      return hooks.error(format!(
        "Index out of bounds. list was length {} but attempted to index with {}.",
        list.len(),
        rounded
      ));
    }

    list[rounded] = args[1];
    Ok(VALUE_NIL)
  }
}

native!(ListLen, LIST_LEN);

impl Native for ListLen {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> CallResult {
    Ok(val!(this.unwrap().to_list().len() as f64))
  }
}

native!(ListPush, LIST_PUSH);

impl Native for ListPush {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> CallResult {
    hooks.grow(&mut this.unwrap().to_list(), |list| {
      list.extend_from_slice(args)
    });
    Ok(VALUE_NIL)
  }
}

native!(ListPop, LIST_POP);

impl Native for ListPop {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> CallResult {
    match this.unwrap().to_list().pop() {
      Some(value) => Ok(value),
      None => Ok(VALUE_NIL),
    }
  }
}

native!(ListRemove, LIST_REMOVE);

impl Native for ListRemove {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> CallResult {
    let index = args[0].to_num();
    let mut list = this.unwrap().to_list();

    if index < 0.0 {
      return hooks.error(format!("Cannot remove at negative index {}.", index));
    }

    if index as usize >= list.len() {
      return hooks.error(format!(
        "Cannot remove at index {}, list has size {}",
        index,
        list.len()
      ));
    }

    let result = list.remove(index as usize);
    Ok(result)
  }
}

native!(ListIndex, LIST_INDEX);

impl Native for ListIndex {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> CallResult {
    let item = args[0];
    let index = this.unwrap().to_list().iter().position(|x| *x == item);

    Ok(index.map(|i| val!(i as f64)).unwrap_or(VALUE_NIL))
  }
}

native!(ListInsert, LIST_INSERT);

impl Native for ListInsert {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> CallResult {
    let index = args[0].to_num();
    let mut list = this.unwrap().to_list();

    if index < 0.0 {
      return hooks.error(format!("Cannot insert at index {}", index));
    }

    if index as usize > list.len() {
      return hooks.error(format!(
        "Cannot insert at index {}, list has size {}",
        index,
        list.len()
      ));
    }

    hooks.grow(&mut list, |list| list.insert(index as usize, args[1]));
    Ok(VALUE_NIL)
  }
}

native!(ListClear, LIST_CLEAR);

impl Native for ListClear {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> CallResult {
    hooks.shrink(&mut this.unwrap().to_list(), |list| list.clear());
    Ok(VALUE_NIL)
  }
}

native!(ListHas, LIST_HAS);

impl Native for ListHas {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> CallResult {
    Ok(val!(this.unwrap().to_list().contains(&args[0])))
  }
}

native!(ListIter, LIST_ITER);

impl Native for ListIter {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> CallResult {
    let inner_iter: Box<dyn LyIter> = Box::new(ListIterator::new(this.unwrap().to_list()));
    let iter = LyIterator::new(inner_iter);
    let iter = hooks.manage(iter);

    Ok(val!(iter))
  }
}

native!(ListCollect, LIST_COLLECT);

impl Native for ListCollect {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> CallResult {
    let mut iter = args[0].to_iter();
    let mut list = match iter.size_hint() {
      Some(size) => List::with_capacity(size),
      None => List::new(),
    };

    while !is_falsey(iter.next(hooks)?) {
      let current = iter.current();
      list.push(current);
    }

    Ok(val!(hooks.manage(list)))
  }
}

#[derive(Debug)]
struct ListIterator {
  list: Managed<List<Value>>,
  current: Value,
  iter: Iter<'static, Value>,
}

impl ListIterator {
  fn new(list: Managed<List<Value>>) -> Self {
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

  fn next(&mut self, _hooks: &mut Hooks) -> CallResult {
    match self.iter.next() {
      Some(value) => {
        self.current = *value;
        Ok(val!(true))
      }
      None => {
        self.current = VALUE_NIL;
        Ok(val!(false))
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
  fn trace(&self) -> bool {
    self.list.trace()
  }

  fn trace_debug(&self, stdout: &mut dyn Write) -> bool {
    self.list.trace_debug(stdout)
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod index_get {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let list_index_get = ListIndexGet::from(&hooks);

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
      let list_index_get = ListIndexGet::from(&hooks);

      let values = &[val!(0.0)];

      let list = List::from(vec![VALUE_NIL, val!(10.0)]);
      let this = hooks.manage(list);

      let this2 = this;
      let this3 = this;

      let result = list_index_get.call(&mut hooks, Some(val!(this)), values);
      match result {
        Ok(r) => assert!(r.is_nil()),
        Err(_) => assert!(false),
      }

      assert_eq!(this[0], VALUE_NIL)
    }
  }

  mod index_set {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let list_index_set = ListIndexSet::from(&hooks);

      assert_eq!(list_index_set.meta().name, "[]=");
      assert_eq!(list_index_set.meta().signature.arity, Arity::Fixed(2));
      assert_eq!(
        list_index_set.meta().signature.parameters[0].kind,
        ParameterKind::Number
      );
      assert_eq!(
        list_index_set.meta().signature.parameters[1].kind,
        ParameterKind::Any
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let list_index_set = ListIndexSet::from(&hooks);

      let values = &[val!(1.0), val!(false)];

      let list = List::from(vec![VALUE_NIL, val!(10.0)]);
      let this = hooks.manage(list);

      let result = list_index_set.call(&mut hooks, Some(val!(this)), values);
      match result {
        Ok(r) => assert!(r.is_nil()),
        Err(_) => assert!(false),
      }

      assert_eq!(this[1], val!(false))
    }
  }

  mod str {
    use super::*;
    use crate::support::{test_native_dependencies, MockedContext};
    use laythe_env::memory::NO_GC;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let list_str = ListStr::new(
        LIST_STR.to_meta(&hooks),
        hooks.manage_str("str".to_string()),
      );

      assert_eq!(list_str.meta().name, "str");
      assert_eq!(list_str.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let gc = test_native_dependencies();
      let mut context = MockedContext::with_std(&[
        val!(gc.manage_str("nil".to_string(), &NO_GC)),
        val!(gc.manage_str("10".to_string(), &NO_GC)),
        val!(gc.manage_str("[5]".to_string(), &NO_GC)),
      ]);
      let mut hooks = Hooks::new(&mut context);
      let list_str = ListStr::new(
        LIST_STR.to_meta(&hooks.as_gc()),
        hooks.manage_str("str".to_string()),
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
        Ok(r) => assert_eq!(&*r.to_str(), "[nil, 10, [5]]"),
        Err(_) => assert!(false),
      }
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
        Ok(r) => assert_eq!(r.to_num(), 2.0),
        Err(_) => assert!(false),
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
        Ok(r) => {
          assert_eq!(r, VALUE_NIL);
          assert_eq!(list_value.unwrap().to_list().len(), 3);
          assert_eq!(list_value.unwrap().to_list()[2], val!(false));
        }
        Err(_) => assert!(false),
      }

      let result = list_push.call(&mut hooks, Some(val!(this)), &[val!(10.3), VALUE_NIL]);
      match result {
        Ok(r) => {
          assert_eq!(r, VALUE_NIL);
          assert_eq!(list_value.unwrap().to_list().len(), 5);
          assert_eq!(list_value.unwrap().to_list()[3], val!(10.3));
          assert_eq!(list_value.unwrap().to_list()[4], VALUE_NIL);
        }
        Err(_) => assert!(false),
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
        Ok(r) => {
          assert_eq!(r.to_bool(), true);
          assert_eq!(this.len(), 0);
        }
        Err(_) => assert!(false),
      }

      let result = list_pop.call(&mut hooks, list_value, &[]);
      match result {
        Ok(r) => {
          assert!(r.is_nil());
          assert_eq!(this.len(), 0);
        }
        Err(_) => assert!(false),
      }
    }
  }

  mod remove {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let list_remove = ListRemove::from(&hooks);

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

      let list_remove = ListRemove::from(&hooks);

      let list = List::from(&[VALUE_NIL, val!(10.0), val!(true)] as &[Value]);
      let this = hooks.manage(list);
      let list_value = Some(val!(this));

      let result = list_remove.call(&mut hooks, list_value, &[val!(1.0)]);
      match result {
        Ok(r) => {
          assert_eq!(r.to_num(), 10.0);
          assert_eq!(this.len(), 2);
        }
        Err(_) => assert!(false),
      }

      let result = list_remove.call(&mut hooks, list_value, &[val!(-1.0)]);
      match result {
        Ok(_) => assert!(false),
        Err(_) => assert!(true),
      }

      let result = list_remove.call(&mut hooks, list_value, &[val!(10.0)]);
      match result {
        Ok(_) => assert!(false),
        Err(_) => assert!(true),
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
        Ok(r) => {
          assert_eq!(r.to_num(), 1.0);
        }
        Err(_) => assert!(false),
      }
    }
  }

  mod insert {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let list_insert = ListInsert::from(&hooks);

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

      let list_insert = ListInsert::from(&hooks);

      let list = List::from(&[VALUE_NIL, val!(10.0), val!(true)] as &[Value]);
      let this = hooks.manage(list);
      let list_value = Some(val!(this));

      let result = list_insert.call(&mut hooks, list_value, &[val!(1.0), val!(false)]);
      match result {
        Ok(r) => {
          assert!(r.is_nil());
          assert_eq!(this[1], val!(false));
          assert_eq!(this.len(), 4);
        }
        Err(_) => assert!(false),
      }

      let result = list_insert.call(&mut hooks, list_value, &[val!(-1.0)]);
      match result {
        Ok(_) => assert!(false),
        Err(_) => assert!(true),
      }

      let result = list_insert.call(&mut hooks, list_value, &[val!(10.0)]);
      match result {
        Ok(_) => assert!(false),
        Err(_) => assert!(true),
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
        Ok(r) => {
          assert!(r.is_nil());
          assert_eq!(this.len(), 0);
        }
        Err(_) => assert!(false),
      }

      let result = list_clear.call(&mut hooks, list_value, &[]);
      match result {
        Ok(r) => {
          assert!(r.is_nil());
          assert_eq!(this.len(), 0);
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
        Ok(r) => {
          assert!(r.to_bool());
        }
        Err(_) => assert!(false),
      }

      let result = list_has.call(&mut hooks, list_value, &[val!(false)]);
      match result {
        Ok(r) => {
          assert!(!r.to_bool());
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
        Ok(r) => {
          let mut iter = r.to_iter();
          assert_eq!(iter.current(), VALUE_NIL);
          assert_eq!(iter.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(iter.current(), VALUE_NIL);
        }
        Err(_) => assert!(false),
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
        Ok(r) => {
          let list = r.to_list();
          assert_eq!(list.len(), 5);
        }
        Err(_) => assert!(false),
      }
    }
  }
}
