use crate::support::{
  default_class_inheritance, export_and_insert, load_class_from_module, to_dyn_fun, to_dyn_method,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  iterator::{LyIter, LyIterator},
  module::Module,
  native::{NativeFun, NativeMeta, NativeMethod},
  object::LyVec,
  package::Package,
  signature::{Arity, Parameter, ParameterKind},
  utils::is_falsey,
  value::{Value, VALUE_NIL},
  CallResult, LyResult,
};
use laythe_env::{
  managed::{Managed, Trace},
  stdio::Stdio,
};
use std::{mem, slice::Iter};

pub const LIST_CLASS_NAME: &'static str = "List";

const LIST_CLEAR: NativeMeta = NativeMeta::new("clear", Arity::Fixed(0), &[]);
const LIST_HAS: NativeMeta = NativeMeta::new(
  "has",
  Arity::Fixed(1),
  &[Parameter::new("val", ParameterKind::Any)],
);
const LIST_INSERT: NativeMeta = NativeMeta::new(
  "insert",
  Arity::Fixed(2),
  &[
    Parameter::new("index", ParameterKind::Number),
    Parameter::new("val", ParameterKind::Any),
  ],
);
const LIST_ITER: NativeMeta = NativeMeta::new("iter", Arity::Fixed(0), &[]);
const LIST_POP: NativeMeta = NativeMeta::new("pop", Arity::Fixed(0), &[]);
const LIST_PUSH: NativeMeta = NativeMeta::new(
  "push",
  Arity::Variadic(0),
  &[Parameter::new("values", ParameterKind::Any)],
);
const LIST_REMOVE: NativeMeta = NativeMeta::new(
  "remove",
  Arity::Fixed(1),
  &[Parameter::new("index", ParameterKind::Number)],
);
const LIST_INDEX: NativeMeta = NativeMeta::new(
  "index",
  Arity::Fixed(1),
  &[Parameter::new("value", ParameterKind::Any)],
);
const LIST_SIZE: NativeMeta = NativeMeta::new("size", Arity::Fixed(0), &[]);
const LIST_STR: NativeMeta = NativeMeta::new("str", Arity::Fixed(0), &[]);

const LIST_COLLECT: NativeMeta = NativeMeta::new(
  "collect",
  Arity::Fixed(1),
  &[Parameter::new("iter", ParameterKind::Iter)],
);

pub fn declare_list_class(hooks: &GcHooks, module: &mut Module, package: &Package) -> LyResult<()> {
  let class = default_class_inheritance(hooks, package, LIST_CLASS_NAME)?;
  export_and_insert(hooks, module, class.name, Value::from(class))
}

pub fn define_list_class(hooks: &GcHooks, module: &Module, _: &Package) -> LyResult<()> {
  let mut class = load_class_from_module(hooks, module, LIST_CLASS_NAME)?;

  class.add_method(
    hooks,
    hooks.manage_str(String::from(LIST_SIZE.name)),
    Value::from(to_dyn_method(hooks, ListSize())),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(LIST_PUSH.name)),
    Value::from(to_dyn_method(hooks, ListPush())),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(LIST_POP.name)),
    Value::from(to_dyn_method(hooks, ListPop())),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(LIST_REMOVE.name)),
    Value::from(to_dyn_method(hooks, ListRemove())),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(LIST_INDEX.name)),
    Value::from(to_dyn_method(hooks, ListIndex())),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(LIST_INSERT.name)),
    Value::from(to_dyn_method(hooks, ListInsert())),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(LIST_STR.name)),
    Value::from(to_dyn_method(
      hooks,
      ListStr::new(hooks.manage_str(String::from(LIST_STR.name))),
    )),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(LIST_CLEAR.name)),
    Value::from(to_dyn_method(hooks, ListClear())),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(LIST_HAS.name)),
    Value::from(to_dyn_method(hooks, ListHas())),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(LIST_ITER.name)),
    Value::from(to_dyn_method(hooks, ListIter())),
  );

  class.meta().expect("Meta class not set.").add_method(
    hooks,
    hooks.manage_str(String::from(LIST_COLLECT.name)),
    Value::from(to_dyn_fun(hooks, ListCollect())),
  );

  Ok(())
}

#[derive(Clone, Debug, Trace)]
struct ListStr {
  method_name: Managed<String>,
}

impl ListStr {
  fn new(method_name: Managed<String>) -> Self {
    Self { method_name }
  }
}

impl NativeMethod for ListStr {
  fn meta(&self) -> &NativeMeta {
    &LIST_STR
  }

  fn call(&self, hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    let list = this.to_list();

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
      match hooks.call_method_by_name(*item, self.method_name, &[]) {
        Ok(result) => {
          if result.is_str() {
            let string = result.to_str();
            count += 1;
            hooks.push_root(string);
            strings.push(string.to_string());
          } else {
            // if error throw away temporary strings
            hooks.pop_roots(count);
            return Err(hooks.make_error(format!("No method str on {}", item)));
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
    Ok(Value::from(hooks.manage_str(formatted)))
  }
}

#[derive(Clone, Debug, Trace)]
struct ListSize();

impl NativeMethod for ListSize {
  fn meta(&self) -> &NativeMeta {
    &LIST_SIZE
  }

  fn call(&self, _hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    Ok(Value::from(this.to_list().len() as f64))
  }
}

#[derive(Clone, Debug, Trace)]
struct ListPush();

impl NativeMethod for ListPush {
  fn meta(&self) -> &NativeMeta {
    &LIST_PUSH
  }

  fn call(&self, hooks: &mut Hooks, this: Value, args: &[Value]) -> CallResult {
    hooks.grow(&mut this.to_list(), |list| list.extend_from_slice(args));
    Ok(VALUE_NIL)
  }
}

#[derive(Clone, Debug, Trace)]
struct ListPop();

impl NativeMethod for ListPop {
  fn meta(&self) -> &NativeMeta {
    &LIST_POP
  }

  fn call(&self, _hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    match this.to_list().pop() {
      Some(value) => Ok(value),
      None => Ok(VALUE_NIL),
    }
  }
}

#[derive(Clone, Debug, Trace)]
struct ListRemove();

impl NativeMethod for ListRemove {
  fn meta(&self) -> &NativeMeta {
    &LIST_REMOVE
  }

  fn call(&self, hooks: &mut Hooks, this: Value, args: &[Value]) -> CallResult {
    let index = args[0].to_num();
    let mut list = this.to_list();

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

#[derive(Clone, Debug, Trace)]
struct ListIndex();

impl NativeMethod for ListIndex {
  fn meta(&self) -> &NativeMeta {
    &LIST_INDEX
  }

  fn call(&self, _hooks: &mut Hooks, this: Value, args: &[Value]) -> CallResult {
    let item = args[0];
    let index = this.to_list().iter().position(|x| *x == item);

    Ok(index.map(|i| Value::from(i as f64)).unwrap_or(VALUE_NIL))
  }
}

#[derive(Clone, Debug, Trace)]
struct ListInsert();

impl NativeMethod for ListInsert {
  fn meta(&self) -> &NativeMeta {
    &LIST_INSERT
  }

  fn call(&self, hooks: &mut Hooks, this: Value, args: &[Value]) -> CallResult {
    let index = args[0].to_num();
    let mut list = this.to_list();

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

#[derive(Clone, Debug, Trace)]
struct ListClear();

impl NativeMethod for ListClear {
  fn meta(&self) -> &NativeMeta {
    &LIST_CLEAR
  }

  fn call(&self, hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    hooks.shrink(&mut this.to_list(), |list| list.clear());
    Ok(VALUE_NIL)
  }
}

#[derive(Clone, Debug, Trace)]
struct ListHas();

impl NativeMethod for ListHas {
  fn meta(&self) -> &NativeMeta {
    &LIST_HAS
  }

  fn call(&self, _hooks: &mut Hooks, this: Value, args: &[Value]) -> CallResult {
    Ok(Value::from(this.to_list().contains(&args[0])))
  }
}

#[derive(Clone, Debug, Trace)]
struct ListIter();

impl NativeMethod for ListIter {
  fn meta(&self) -> &NativeMeta {
    &LIST_ITER
  }

  fn call(&self, hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    let inner_iter: Box<dyn LyIter> = Box::new(ListIterator::new(this.to_list()));
    let iter = LyIterator::new(inner_iter);
    let iter = hooks.manage(iter);

    Ok(Value::from(iter))
  }
}

#[derive(Clone, Debug, Trace)]
struct ListCollect();

impl NativeFun for ListCollect {
  fn meta(&self) -> &NativeMeta {
    &LIST_COLLECT
  }

  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> CallResult {
    let mut iter = args[0].to_iter();
    let mut list = match iter.size_hint() {
      Some(size) => LyVec::with_capacity(size),
      None => LyVec::new(),
    };

    while !is_falsey(iter.next(hooks)?) {
      let current = iter.current();
      list.push(current);
    }

    Ok(Value::from(hooks.manage(list)))
  }
}

#[derive(Debug)]
struct ListIterator {
  list: Managed<LyVec<Value>>,
  current: Value,
  iter: Iter<'static, Value>,
}

impl ListIterator {
  fn new(list: Managed<LyVec<Value>>) -> Self {
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
        Ok(Value::from(true))
      }
      None => {
        self.current = VALUE_NIL;
        Ok(Value::from(false))
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

  fn trace_debug(&self, stdio: &mut Stdio) -> bool {
    self.list.trace_debug(stdio)
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod str {
    use super::*;
    use crate::support::{test_native_dependencies, MockedContext};
    use laythe_env::memory::NO_GC;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = Hooks::new(&mut context);

      let list_str = ListStr::new(hooks.manage_str("str".to_string()));

      assert_eq!(list_str.meta().name, "str");
      assert_eq!(list_str.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let gc = test_native_dependencies();
      let mut context = MockedContext::new(&[
        Value::from(gc.manage_str("nil".to_string(), &NO_GC)),
        Value::from(gc.manage_str("10".to_string(), &NO_GC)),
        Value::from(gc.manage_str("[5]".to_string(), &NO_GC)),
      ]);
      let mut hooks = Hooks::new(&mut context);
      let list_str = ListStr::new(hooks.manage_str("str".to_string()));

      let values = &[];

      let list = LyVec::from(vec![
        VALUE_NIL,
        Value::from(10.0),
        Value::from(hooks.manage(LyVec::from(vec![Value::from(5.0)]))),
      ]);
      let this = hooks.manage(list);

      let result = list_str.call(&mut hooks, Value::from(this), values);
      match result {
        Ok(r) => assert_eq!(&*r.to_str(), "[nil, 10, [5]]"),
        Err(_) => assert!(false),
      }
    }
  }

  mod size {
    use super::*;
    use crate::support::MockedContext;
    use laythe_core::hooks::Hooks;

    #[test]
    fn new() {
      let list_size = ListSize();

      assert_eq!(list_size.meta().name, "size");
      assert_eq!(list_size.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let list_size = ListSize();
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let values = &[];

      let list = LyVec::from(vec![VALUE_NIL, Value::from(10.0)]);
      let this = hooks.manage(list);

      let result = list_size.call(&mut hooks, Value::from(this), values);
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
      let list_push = ListPush();

      assert_eq!(list_push.meta().name, "push");
      assert_eq!(list_push.meta().signature.arity, Arity::Variadic(0));
      assert_eq!(
        list_push.meta().signature.parameters[0].kind,
        ParameterKind::Any
      );
    }

    #[test]
    fn call() {
      let list_push = ListPush();
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let list = LyVec::from(vec![VALUE_NIL, Value::from(10.0)]);
      let this = hooks.manage(list);
      let list_value = Value::from(this);

      let result = list_push.call(&mut hooks, list_value, &[Value::from(false)]);
      match result {
        Ok(r) => {
          assert_eq!(r, VALUE_NIL);
          assert_eq!(list_value.to_list().len(), 3);
          assert_eq!(list_value.to_list()[2], Value::from(false));
        }
        Err(_) => assert!(false),
      }

      let result = list_push.call(
        &mut hooks,
        Value::from(this),
        &[Value::from(10.3), VALUE_NIL],
      );
      match result {
        Ok(r) => {
          assert_eq!(r, VALUE_NIL);
          assert_eq!(list_value.to_list().len(), 5);
          assert_eq!(list_value.to_list()[3], Value::from(10.3));
          assert_eq!(list_value.to_list()[4], VALUE_NIL);
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
      let list_pop = ListPop();

      assert_eq!(list_pop.meta().name, "pop");
      assert_eq!(list_pop.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let list_pop = ListPop();
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let list = LyVec::from(vec![Value::from(true)]);
      let this = hooks.manage(list);
      let list_value = Value::from(this);

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
      let list_remove = ListRemove();

      assert_eq!(list_remove.meta().name, "remove");
      assert_eq!(list_remove.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        list_remove.meta().signature.parameters[0].kind,
        ParameterKind::Number
      );
    }

    #[test]
    fn call() {
      let list_remove = ListRemove();
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let list = LyVec::from(&[VALUE_NIL, Value::from(10.0), Value::from(true)] as &[Value]);
      let this = hooks.manage(list);
      let list_value = Value::from(this);

      let result = list_remove.call(&mut hooks, list_value, &[Value::from(1.0)]);
      match result {
        Ok(r) => {
          assert_eq!(r.to_num(), 10.0);
          assert_eq!(this.len(), 2);
        }
        Err(_) => assert!(false),
      }

      let result = list_remove.call(&mut hooks, list_value, &[Value::from(-1.0)]);
      match result {
        Ok(_) => assert!(false),
        Err(_) => assert!(true),
      }

      let result = list_remove.call(&mut hooks, list_value, &[Value::from(10.0)]);
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
      let list_index = ListIndex();

      assert_eq!(list_index.meta().name, "index");
      assert_eq!(list_index.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        list_index.meta().signature.parameters[0].kind,
        ParameterKind::Any
      );
    }

    #[test]
    fn call() {
      let list_index = ListIndex();
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let list = LyVec::from(&[VALUE_NIL, Value::from(10.0), Value::from(true)] as &[Value]);
      let this = hooks.manage(list);
      let list_value = Value::from(this);

      let result = list_index.call(&mut hooks, list_value, &[Value::from(10.0)]);
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
      let list_insert = ListInsert();

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
      let list_insert = ListInsert();
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let list = LyVec::from(&[VALUE_NIL, Value::from(10.0), Value::from(true)] as &[Value]);
      let this = hooks.manage(list);
      let list_value = Value::from(this);

      let result = list_insert.call(
        &mut hooks,
        list_value,
        &[Value::from(1.0), Value::from(false)],
      );
      match result {
        Ok(r) => {
          assert!(r.is_nil());
          assert_eq!(this[1], Value::from(false));
          assert_eq!(this.len(), 4);
        }
        Err(_) => assert!(false),
      }

      let result = list_insert.call(&mut hooks, list_value, &[Value::from(-1.0)]);
      match result {
        Ok(_) => assert!(false),
        Err(_) => assert!(true),
      }

      let result = list_insert.call(&mut hooks, list_value, &[Value::from(10.0)]);
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
      let list_clear = ListClear();

      assert_eq!(list_clear.meta().name, "clear");
      assert_eq!(list_clear.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let list_clear = ListClear();
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let list = LyVec::from(&[VALUE_NIL, Value::from(10.0), Value::from(true)] as &[Value]);
      let this = hooks.manage(list);
      let list_value = Value::from(this);

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
      let list_has = ListHas();

      assert_eq!(list_has.meta().name, "has");
      assert_eq!(list_has.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        list_has.meta().signature.parameters[0].kind,
        ParameterKind::Any
      );
    }

    #[test]
    fn call() {
      let list_has = ListHas();
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let list = LyVec::from(&[VALUE_NIL, Value::from(10.0), Value::from(true)] as &[Value]);
      let this = hooks.manage(list);
      let list_value = Value::from(this);

      let result = list_has.call(&mut hooks, list_value, &[Value::from(10.0)]);
      match result {
        Ok(r) => {
          assert!(r.to_bool());
        }
        Err(_) => assert!(false),
      }

      let result = list_has.call(&mut hooks, list_value, &[Value::from(false)]);
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
      let list_iter = ListIter();

      assert_eq!(list_iter.meta().name, "iter");
      assert_eq!(list_iter.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let list_iter = ListIter();

      let list = LyVec::from(&[VALUE_NIL, Value::from(10.0), Value::from(true)] as &[Value]);
      let this = hooks.manage(list);
      let list_value = Value::from(this);

      let result = list_iter.call(&mut hooks, list_value, &[]);
      match result {
        Ok(r) => {
          let mut iter = r.to_iter();
          assert_eq!(iter.current(), VALUE_NIL);
          assert_eq!(iter.next(&mut hooks).unwrap(), Value::from(true));
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
      let list_iter = ListCollect();

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
      let list_iter = ListCollect();

      let iter = test_iter();
      let iter_value = Value::from(hooks.manage(LyIterator::new(iter)));

      let result = list_iter.call(&mut hooks, &[iter_value]);
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
