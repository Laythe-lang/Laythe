use super::iter::ITER_CLASS_NAME;
use spacelox_core::{
  arity::ArityKind,
  hooks::Hooks,
  io::StdIo,
  iterator::{SlIter, SlIterator},
  managed::{Managed, Trace},
  module::Module,
  native::{NativeMeta, NativeMethod},
  package::Package,
  value::{Class, Value},
  CallResult, ModuleResult,
};
use std::fmt;
use std::mem;

pub const LIST_CLASS_NAME: &'static str = "List";

const LIST_STR: NativeMeta = NativeMeta::new("str", ArityKind::Fixed(0));
const LIST_SIZE: NativeMeta = NativeMeta::new("size", ArityKind::Fixed(0));
const LIST_PUSH: NativeMeta = NativeMeta::new("push", ArityKind::Variadic(1));
const LIST_POP: NativeMeta = NativeMeta::new("pop", ArityKind::Fixed(0));
const LIST_REMOVE: NativeMeta = NativeMeta::new("remove", ArityKind::Fixed(1));
const LIST_INSERT: NativeMeta = NativeMeta::new("insert", ArityKind::Fixed(2));
const LIST_CLEAR: NativeMeta = NativeMeta::new("clear", ArityKind::Fixed(0));
const LIST_HAS: NativeMeta = NativeMeta::new("has", ArityKind::Fixed(1));
const LIST_ITER: NativeMeta = NativeMeta::new("iter", ArityKind::Fixed(0));

pub fn declare_list_class(hooks: &Hooks, self_module: &mut Module) -> ModuleResult<()> {
  let name = hooks.manage_str(String::from(LIST_CLASS_NAME));
  let class = hooks.manage(Class::new(name));

  self_module.add_export(hooks, name, Value::Class(class))
}

pub fn define_list_class(hooks: &Hooks, self_module: &Module, _: &Package) {
  let name = hooks.manage_str(String::from(LIST_CLASS_NAME));
  let mut class = self_module.get_symbol(hooks, name).unwrap().to_class();

  let list_iter_name = hooks.manage_str(String::from(ITER_CLASS_NAME));
  let list_iter_class = self_module
    .get_symbol(hooks, list_iter_name)
    .unwrap()
    .to_class();

  class.add_method(
    hooks,
    hooks.manage_str(String::from(LIST_SIZE.name)),
    Value::NativeMethod(hooks.manage(Box::new(ListSize::new()))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(LIST_PUSH.name)),
    Value::NativeMethod(hooks.manage(Box::new(ListPush::new()))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(LIST_POP.name)),
    Value::NativeMethod(hooks.manage(Box::new(ListPop::new()))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(LIST_REMOVE.name)),
    Value::NativeMethod(hooks.manage(Box::new(ListRemove::new()))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(LIST_INSERT.name)),
    Value::NativeMethod(hooks.manage(Box::new(ListInsert::new()))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(LIST_STR.name)),
    Value::NativeMethod(hooks.manage(Box::new(ListStr::new()))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(LIST_CLEAR.name)),
    Value::NativeMethod(hooks.manage(Box::new(ListClear::new()))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(LIST_HAS.name)),
    Value::NativeMethod(hooks.manage(Box::new(ListHas::new()))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(LIST_ITER.name)),
    Value::NativeMethod(hooks.manage(Box::new(ListIter::new(list_iter_class)))),
  )
}

#[derive(Clone, Debug, Trace)]
struct ListStr {
  meta: Box<NativeMeta>,
}

impl ListStr {
  fn new() -> Self {
    Self {
      meta: Box::new(LIST_STR),
    }
  }
}

impl NativeMethod for ListStr {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    Ok(Value::String(hooks.manage_str(this.to_string())))
  }
}

#[derive(Clone, Debug, Trace)]
struct ListSize {
  meta: Box<NativeMeta>,
}

impl ListSize {
  fn new() -> Self {
    Self {
      meta: Box::new(LIST_SIZE),
    }
  }
}

impl NativeMethod for ListSize {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, _hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    Ok(Value::Number(this.to_list().len() as f64))
  }
}

#[derive(Clone, Debug, Trace)]
struct ListPush {
  meta: Box<NativeMeta>,
}

impl ListPush {
  fn new() -> Self {
    Self {
      meta: Box::new(LIST_PUSH),
    }
  }
}

impl NativeMethod for ListPush {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, hooks: &mut Hooks, this: Value, args: &[Value]) -> CallResult {
    hooks.resize(&mut this.to_list(), |list| list.extend(args));
    Ok(Value::Nil)
  }
}

#[derive(Clone, Debug, Trace)]
struct ListPop {
  meta: Box<NativeMeta>,
}

impl ListPop {
  fn new() -> Self {
    Self {
      meta: Box::new(LIST_POP),
    }
  }
}

impl NativeMethod for ListPop {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, _hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    match this.to_list().pop() {
      Some(value) => Ok(value),
      None => Ok(Value::Nil),
    }
  }
}

#[derive(Clone, Debug, Trace)]
struct ListRemove {
  meta: Box<NativeMeta>,
}

impl ListRemove {
  fn new() -> Self {
    Self {
      meta: Box::new(LIST_REMOVE),
    }
  }
}

impl NativeMethod for ListRemove {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, hooks: &mut Hooks, this: Value, args: &[Value]) -> CallResult {
    let index = args[0].to_num();
    let mut list = this.to_list();

    if index < 0.0 {
      return hooks.error(format!("Cannot remove at index {}", index));
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
struct ListInsert {
  meta: Box<NativeMeta>,
}

impl ListInsert {
  fn new() -> Self {
    Self {
      meta: Box::new(LIST_INSERT),
    }
  }
}

impl NativeMethod for ListInsert {
  fn meta(&self) -> &NativeMeta {
    &self.meta
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

    list.insert(index as usize, args[1]);
    Ok(Value::Nil)
  }
}

#[derive(Clone, Debug, Trace)]
struct ListClear {
  meta: Box<NativeMeta>,
}

impl ListClear {
  fn new() -> Self {
    Self {
      meta: Box::new(LIST_CLEAR),
    }
  }
}

impl NativeMethod for ListClear {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, _hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    this.to_list().clear();
    Ok(Value::Nil)
  }
}

#[derive(Clone, Debug, Trace)]
struct ListHas {
  meta: Box<NativeMeta>,
}

impl ListHas {
  fn new() -> Self {
    Self {
      meta: Box::new(LIST_HAS),
    }
  }
}

impl NativeMethod for ListHas {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, _hooks: &mut Hooks, this: Value, args: &[Value]) -> CallResult {
    Ok(Value::Bool(this.to_list().contains(&args[0])))
  }
}

#[derive(Clone, Debug)]
struct ListIter {
  meta: Box<NativeMeta>,
  iter_class: Managed<Class>,
}

impl ListIter {
  pub fn new(iter_class: Managed<Class>) -> Self {
    Self {
      meta: Box::new(LIST_ITER),
      iter_class,
    }
  }
}

impl Trace for ListIter {
  fn trace(&self) -> bool {
    self.iter_class.trace();
    true
  }

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    self.iter_class.trace_debug(stdio);
    true
  }
}

impl NativeMethod for ListIter {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    let inner_iter: Box<dyn SlIter> = Box::new(ListIterator::new(this.to_list()));
    let iter = SlIterator::new(inner_iter, self.iter_class);
    let iter = hooks.manage(iter);

    Ok(Value::Iter(iter))
  }
}

struct ListIterator {
  list: Managed<Vec<Value>>,
  index: isize,
}

impl ListIterator {
  fn new(list: Managed<Vec<Value>>) -> Self {
    Self { list, index: -1 }
  }
}

impl SlIter for ListIterator {
  fn current(&self) -> Value {
    if self.index >= 0 && (self.index as usize) < self.list.len() {
      return self.list[self.index as usize];
    }

    Value::Nil
  }

  fn next(&mut self) -> Value {
    self.index += 1;
    Value::Bool((self.index as usize) < self.list.len())
  }

  fn size(&self) -> usize {
    mem::size_of::<Self>() + self.list.capacity() * mem::size_of::<Value>()
  }
}

impl Trace for ListIterator {
  fn trace(&self) -> bool {
    self.list.trace()
  }

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    self.list.trace_debug(stdio)
  }
}

impl fmt::Debug for ListIterator {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("ListIterator")
      .field("list", &self.list)
      .field("index", &self.index)
      .finish()
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod str {
    use super::*;
    use crate::support::{test_native_dependencies, TestContext};

    #[test]
    fn new() {
      let list_str = ListStr::new();

      assert_eq!(list_str.meta.name, "str");
      assert_eq!(list_str.meta.arity, ArityKind::Fixed(0));
    }

    #[test]
    fn call() {
      let list_str = ListStr::new();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let values = &[];

      let list = vec![Value::Nil, Value::Number(10.0)];
      let this = hooks.manage(list);

      let result = list_str.call(&mut hooks, Value::List(this), values);
      match result {
        Ok(r) => assert_eq!(&*r.to_str(), "[nil, 10]"),
        Err(_) => assert!(false),
      }
    }
  }

  mod size {
    use super::*;
    use crate::support::{test_native_dependencies, TestContext};
    use spacelox_core::hooks::Hooks;

    #[test]
    fn new() {
      let list_size = ListSize::new();

      assert_eq!(list_size.meta.name, "size");
      assert_eq!(list_size.meta.arity, ArityKind::Fixed(0));
    }

    #[test]
    fn call() {
      let list_size = ListSize::new();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let values = &[];

      let list = vec![Value::Nil, Value::Number(10.0)];
      let this = hooks.manage(list);

      let result = list_size.call(&mut hooks, Value::List(this), values);
      match result {
        Ok(r) => assert_eq!(r.to_num(), 2.0),
        Err(_) => assert!(false),
      }
    }
  }

  mod push {
    use super::*;
    use crate::support::{test_native_dependencies, TestContext};

    #[test]
    fn new() {
      let list_push = ListPush::new();

      assert_eq!(list_push.meta.name, "push");
      assert_eq!(list_push.meta.arity, ArityKind::Variadic(1));
    }

    #[test]
    fn call() {
      let list_push = ListPush::new();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let list = vec![Value::Nil, Value::Number(10.0)];
      let this = hooks.manage(list);
      let list_value = Value::List(this);

      let result = list_push.call(&mut hooks, list_value, &[Value::Bool(false)]);
      match result {
        Ok(r) => {
          assert_eq!(r, Value::Nil);
          assert_eq!(list_value.to_list().len(), 3);
          assert_eq!(list_value.to_list()[2], Value::Bool(false));
        }
        Err(_) => assert!(false),
      }

      let result = list_push.call(
        &mut hooks,
        Value::List(this),
        &[Value::Number(10.3), Value::Nil],
      );
      match result {
        Ok(r) => {
          assert_eq!(r, Value::Nil);
          assert_eq!(list_value.to_list().len(), 5);
          assert_eq!(list_value.to_list()[3], Value::Number(10.3));
          assert_eq!(list_value.to_list()[4], Value::Nil);
        }
        Err(_) => assert!(false),
      }
    }
  }

  mod pop {
    use super::*;
    use crate::support::{test_native_dependencies, TestContext};

    #[test]
    fn new() {
      let list_pop = ListPop::new();

      assert_eq!(list_pop.meta.name, "pop");
      assert_eq!(list_pop.meta.arity, ArityKind::Fixed(0));
    }

    #[test]
    fn call() {
      let list_pop = ListPop::new();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let list = vec![Value::Bool(true)];
      let this = hooks.manage(list);
      let list_value = Value::List(this);

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
    use crate::support::{test_native_dependencies, TestContext};

    #[test]
    fn new() {
      let list_remove = ListRemove::new();

      assert_eq!(list_remove.meta.name, "remove");
      assert_eq!(list_remove.meta.arity, ArityKind::Fixed(1));
    }

    #[test]
    fn call() {
      let list_remove = ListRemove::new();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let list = vec![Value::Nil, Value::Number(10.0), Value::Bool(true)];
      let this = hooks.manage(list);
      let list_value = Value::List(this);

      let result = list_remove.call(&mut hooks, list_value, &[Value::Number(1.0)]);
      match result {
        Ok(r) => {
          assert_eq!(r.to_num(), 10.0);
          assert_eq!(this.len(), 2);
        }
        Err(_) => assert!(false),
      }

      let result = list_remove.call(&mut hooks, list_value, &[Value::Number(-1.0)]);
      match result {
        Ok(_) => assert!(false),
        Err(_) => assert!(true),
      }

      let result = list_remove.call(&mut hooks, list_value, &[Value::Number(10.0)]);
      match result {
        Ok(_) => assert!(false),
        Err(_) => assert!(true),
      }
    }
  }

  mod insert {
    use super::*;
    use crate::support::{test_native_dependencies, TestContext};

    #[test]
    fn new() {
      let list_insert = ListInsert::new();

      assert_eq!(list_insert.meta.name, "insert");
      assert_eq!(list_insert.meta.arity, ArityKind::Fixed(2));
    }

    #[test]
    fn call() {
      let list_insert = ListInsert::new();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let list = vec![Value::Nil, Value::Number(10.0), Value::Bool(true)];
      let this = hooks.manage(list);
      let list_value = Value::List(this);

      let result = list_insert.call(
        &mut hooks,
        list_value,
        &[Value::Number(1.0), Value::Bool(false)],
      );
      match result {
        Ok(r) => {
          assert!(r.is_nil());
          assert_eq!(this[1], Value::Bool(false));
          assert_eq!(this.len(), 4);
        }
        Err(_) => assert!(false),
      }

      let result = list_insert.call(&mut hooks, list_value, &[Value::Number(-1.0)]);
      match result {
        Ok(_) => assert!(false),
        Err(_) => assert!(true),
      }

      let result = list_insert.call(&mut hooks, list_value, &[Value::Number(10.0)]);
      match result {
        Ok(_) => assert!(false),
        Err(_) => assert!(true),
      }
    }
  }

  mod clear {
    use super::*;
    use crate::support::{test_native_dependencies, TestContext};

    #[test]
    fn new() {
      let list_clear = ListClear::new();

      assert_eq!(list_clear.meta.name, "clear");
      assert_eq!(list_clear.meta.arity, ArityKind::Fixed(0));
    }

    #[test]
    fn call() {
      let list_clear = ListClear::new();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let list = vec![Value::Nil, Value::Number(10.0), Value::Bool(true)];
      let this = hooks.manage(list);
      let list_value = Value::List(this);

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
    use crate::support::{test_native_dependencies, TestContext};

    #[test]
    fn new() {
      let list_has = ListHas::new();

      assert_eq!(list_has.meta.name, "has");
      assert_eq!(list_has.meta.arity, ArityKind::Fixed(1));
    }

    #[test]
    fn call() {
      let list_has = ListHas::new();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let list = vec![Value::Nil, Value::Number(10.0), Value::Bool(true)];
      let this = hooks.manage(list);
      let list_value = Value::List(this);

      let result = list_has.call(&mut hooks, list_value, &[Value::Number(10.0)]);
      match result {
        Ok(r) => {
          assert!(r.to_bool());
        }
        Err(_) => assert!(false),
      }

      let result = list_has.call(&mut hooks, list_value, &[Value::Bool(false)]);
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
    use crate::support::{test_native_dependencies, TestContext};

    #[test]
    fn new() {
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let hooks = Hooks::new(&mut context);

      let list_iter =
        ListIter::new(hooks.manage(Class::new(hooks.manage_str(String::from("something")))));

      assert_eq!(list_iter.meta.name, "iter");
      assert_eq!(list_iter.meta.arity, ArityKind::Fixed(0));
    }

    #[test]
    fn call() {
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);
      let list_iter =
        ListIter::new(hooks.manage(Class::new(hooks.manage_str(String::from("something")))));

      let list = vec![Value::Nil, Value::Number(10.0), Value::Bool(true)];
      let this = hooks.manage(list);
      let list_value = Value::List(this);

      let result = list_iter.call(&mut hooks, list_value, &[]);
      match result {
        Ok(r) => {
          let mut iter = r.to_iter();
          assert_eq!(iter.current(), Value::Nil);
          assert_eq!(iter.next(), Value::Bool(true));
          assert_eq!(iter.current(), Value::Nil);
        }
        Err(_) => assert!(false),
      }
    }
  }
}
