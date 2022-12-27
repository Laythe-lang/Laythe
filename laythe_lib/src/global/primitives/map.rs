use super::{class_inheritance, error::TYPE_ERROR_NAME, error_inheritance};
use crate::{
  native, native_with_error,
  support::{export_and_insert, load_class_from_module},
  StdResult,
};
use hashbrown::hash_map::Iter;
use laythe_core::{
  constants::INDEX_GET,
  constants::INDEX_SET,
  hooks::{GcHooks, Hooks},
  if_let_obj,
  managed::{DebugHeap, DebugWrap, Gc, GcObj, GcStr, Trace},
  module::Module,
  object::{Enumerate, Enumerator, List, LyNative, Map, Native, NativeMetaBuilder, ObjectKind},
  signature::{Arity, ParameterBuilder, ParameterKind},
  to_obj_kind,
  utils::use_sentinel_nan,
  val,
  value::{Value, VALUE_NIL},
  Call, LyError,
};
use std::io::Write;

pub const MAP_CLASS_NAME: &str = "Map";
pub const KEY_ERROR_NAME: &str = "KeyError";

const MAP_INDEX_GET: NativeMetaBuilder = NativeMetaBuilder::method(INDEX_GET, Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("key", ParameterKind::Any)])
  .with_stack();

const MAP_INDEX_SET: NativeMetaBuilder = NativeMetaBuilder::method(INDEX_SET, Arity::Fixed(2))
  .with_params(&[
    ParameterBuilder::new("key", ParameterKind::Any),
    ParameterBuilder::new("val", ParameterKind::Any),
  ]);

const MAP_GET: NativeMetaBuilder = NativeMetaBuilder::method("get", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("key", ParameterKind::Any)]);

const MAP_SET: NativeMetaBuilder =
  NativeMetaBuilder::method("set", Arity::Fixed(2)).with_params(&[
    ParameterBuilder::new("key", ParameterKind::Any),
    ParameterBuilder::new("value", ParameterKind::Any),
  ]);

const MAP_HAS: NativeMetaBuilder = NativeMetaBuilder::method("has", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("key", ParameterKind::Any)]);

const MAP_INSERT: NativeMetaBuilder = NativeMetaBuilder::method("insert", Arity::Fixed(2))
  .with_params(&[
    ParameterBuilder::new("key", ParameterKind::Any),
    ParameterBuilder::new("val", ParameterKind::Any),
  ]);

const MAP_REMOVE: NativeMetaBuilder = NativeMetaBuilder::method("remove", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("key", ParameterKind::Any)])
  .with_stack();

const MAP_LEN: NativeMetaBuilder = NativeMetaBuilder::method("len", Arity::Fixed(0));
const MAP_STR: NativeMetaBuilder = NativeMetaBuilder::method("str", Arity::Fixed(0)).with_stack();
const MAP_ITER: NativeMetaBuilder = NativeMetaBuilder::method("iter", Arity::Fixed(0));

pub fn declare_map_class(hooks: &GcHooks, module: Gc<Module>) -> StdResult<()> {
  let class = class_inheritance(hooks, module, MAP_CLASS_NAME)?;
  let key_error = error_inheritance(hooks, module, KEY_ERROR_NAME)?;

  export_and_insert(module, class.name(), val!(class))?;
  export_and_insert(module, key_error.name(), val!(key_error))
}

pub fn define_map_class(hooks: &GcHooks, module: Gc<Module>) -> StdResult<()> {
  let mut class = load_class_from_module(hooks, module, MAP_CLASS_NAME)?;
  let key_error = val!(load_class_from_module(hooks, module, KEY_ERROR_NAME)?);
  let type_error = val!(load_class_from_module(hooks, module, TYPE_ERROR_NAME)?);

  class.add_method(
    hooks.manage_str(MAP_LEN.name),
    val!(MapLen::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(MAP_INDEX_GET.name),
    val!(MapIndexGet::native(hooks, key_error)),
  );

  class.add_method(
    hooks.manage_str(MAP_INDEX_SET.name),
    val!(MapIndexSet::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(MAP_STR.name),
    val!(MapStr::native(
      hooks,
      hooks.manage_str(MAP_STR.name),
      type_error
    )),
  );

  class.add_method(
    hooks.manage_str(MAP_HAS.name),
    val!(MapHas::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(MAP_GET.name),
    val!(MapGet::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(MAP_SET.name),
    val!(MapSet::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(MAP_REMOVE.name),
    val!(MapRemove::native(hooks, key_error)),
  );

  class.add_method(
    hooks.manage_str(MAP_INSERT.name),
    val!(MapInsert::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(MAP_ITER.name),
    val!(MapIter::native(hooks)),
  );

  Ok(())
}

#[derive(Debug)]
struct MapStr {
  method_name: GcStr,
  error: Value,
}

impl MapStr {
  fn native(hooks: &GcHooks, method_name: GcStr, error: Value) -> GcObj<Native> {
    debug_assert!(error.is_obj_kind(ObjectKind::Class));
    let native = Box::new(Self { method_name, error }) as Box<dyn LyNative>;

    hooks.manage_obj(Native::new(MAP_STR.to_meta(hooks), native))
  }
}

impl LyNative for MapStr {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    let map = this.unwrap().to_obj().to_map();

    if map.len() == 0 {
      return Call::Ok(val!(hooks.manage_str("{}")));
    }

    // buffer for temporary strings
    let mut strings: Vec<String> = Vec::with_capacity(map.len());

    for (key, value) in map.iter() {
      let mut kvp_string = String::new();

      format_map_entry(*key, self.method_name, self.error, &mut kvp_string, hooks)?;
      kvp_string.push_str(": ");
      format_map_entry(*value, self.method_name, self.error, &mut kvp_string, hooks)?;

      strings.push(kvp_string)
    }

    // format and join strings
    let formatted = format!("{{ {} }}", strings.join(", "));
    Call::Ok(val!(hooks.manage_str(formatted)))
  }
}

fn format_map_entry(
  item: Value,
  method_name: GcStr,
  error: Value,
  buffer: &mut String,
  hooks: &mut Hooks,
) -> Call {
  // if already string quote and add to temps
  if_let_obj!(ObjectKind::String(string) = (item) {
    buffer.push('\'');
    buffer.push_str(&string);
    buffer.push('\'');
    return Call::Ok(VALUE_NIL);
  });

  // call '.str' method on each value
  let result = hooks
    .get_method(item, method_name)
    .and_then(|method| hooks.call_method(item, method, &[]))?;

  if_let_obj!(ObjectKind::String(string) = (result) {
    buffer.push_str(&string);
    Call::Ok(VALUE_NIL)
  } else {
    // if error throw away temporary strings
    hooks.call(
      error,
      &[val!(hooks.manage_str(format!(
        "Expected type str from {}.str()",
        item
      )))],
    )
  })
}

impl Trace for MapStr {
  fn trace(&self) {
    self.method_name.trace();
  }

  fn trace_debug(&self, stdout: &mut dyn Write) {
    self.method_name.trace_debug(stdout);
  }
}

native!(MapLen, MAP_LEN);

impl LyNative for MapLen {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    Call::Ok(val!(this.unwrap().to_obj().to_map().len() as f64))
  }
}

native!(MapHas, MAP_HAS);

impl LyNative for MapHas {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    Call::Ok(val!(this.unwrap().to_obj().to_map().contains_key(&args[0])))
  }
}

native_with_error!(MapIndexGet, MAP_INDEX_GET);

impl LyNative for MapIndexGet {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let index = args[0];
    let key = if index.is_num() {
      val!(use_sentinel_nan(index.to_num()))
    } else {
      index
    };

    match this.unwrap().to_obj().to_map().get(&key) {
      Some(value) => Call::Ok(*value),
      None => self.call_error(hooks, format!("Key not found. {} is not present", key)),
    }
  }
}

native!(MapIndexSet, MAP_INDEX_SET);

impl LyNative for MapIndexSet {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let index = args[1];
    let key = if index.is_num() {
      val!(use_sentinel_nan(index.to_num()))
    } else {
      index
    };

    let mut map = this.unwrap().to_obj().to_map();
    map.insert(key, args[0]);
    Call::Ok(args[0])
  }
}

native!(MapGet, MAP_GET);

impl LyNative for MapGet {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let index = args[0];
    let key = if index.is_num() {
      val!(use_sentinel_nan(index.to_num()))
    } else {
      index
    };

    match this.unwrap().to_obj().to_map().get(&key) {
      Some(value) => Call::Ok(*value),
      None => Call::Ok(VALUE_NIL),
    }
  }
}

native!(MapSet, MAP_SET);

impl LyNative for MapSet {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let index = args[0];
    let key = if index.is_num() {
      val!(use_sentinel_nan(index.to_num()))
    } else {
      index
    };

    let mut map = this.unwrap().to_obj().to_map();
    let result = map.insert(key, args[1]);
    Call::Ok(result.unwrap_or(VALUE_NIL))
  }
}

native!(MapInsert, MAP_INSERT);

impl LyNative for MapInsert {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let mut map = this.unwrap().to_obj().to_map();
    match map.insert(args[0], args[1]) {
      Some(value) => Call::Ok(value),
      None => Call::Ok(VALUE_NIL),
    }
  }
}

native_with_error!(MapRemove, MAP_REMOVE);

impl LyNative for MapRemove {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let mut map = this.unwrap().to_obj().to_map();
    match map.remove(&args[0]) {
      Some(removed) => Call::Ok(removed),
      None => self.call_error(hooks, "Key not found in map."),
    }
  }
}

native!(MapIter, MAP_ITER);

impl LyNative for MapIter {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    let inner_iter: Box<dyn Enumerate> =
      Box::new(MapIterator::new(this.unwrap().to_obj().to_map()));
    let iter = Enumerator::new(inner_iter);
    let iter = hooks.manage_obj(iter);

    Call::Ok(val!(iter))
  }
}

#[derive(Debug)]
struct MapIterator {
  map: GcObj<Map<Value, Value>>,
  iter: Iter<'static, Value, Value>,
  current: Value,
}

impl MapIterator {
  fn new(map: GcObj<Map<Value, Value>>) -> Self {
    let iter = unsafe { map.data_static().iter() };

    Self {
      map,
      iter,
      current: VALUE_NIL,
    }
  }
}

impl Enumerate for MapIterator {
  fn name(&self) -> &str {
    "Map"
  }

  fn current(&self) -> Value {
    self.current
  }

  fn next(&mut self, hooks: &mut Hooks) -> Call {
    match self.iter.next() {
      Some(next) => {
        self.current = val!(hooks.manage_obj(List::from(&[*next.0, *next.1] as &[Value])));
        Call::Ok(val!(true))
      }
      None => {
        self.current = VALUE_NIL;
        Call::Ok(val!(false))
      }
    }
  }

  fn size_hint(&self) -> Option<usize> {
    Some(self.map.len())
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

impl Trace for MapIterator {
  fn trace(&self) {
    self.map.trace();
  }

  fn trace_debug(&self, stdout: &mut dyn Write) {
    self.map.trace_debug(stdout);
  }
}

impl DebugHeap for MapIterator {
  fn fmt_heap(&self, f: &mut std::fmt::Formatter, depth: usize) -> std::fmt::Result {
    f.debug_struct("MapIterator")
      .field("map", &DebugWrap(&self.map, depth))
      .field("iter", &"*")
      .field("current", &DebugWrap(&self.current, depth))
      .finish()
  }
}
#[cfg(test)]
mod test {
  use super::*;
  use laythe_core::object::Map;

  #[cfg(test)]
  mod str {
    use laythe_core::memory::NO_GC;

    use super::*;
    use crate::support::{test_error_class, MockedContext};

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let error = val!(test_error_class(&hooks));
      let map_str = MapStr::native(&hooks, hooks.manage_str("str".to_string()), error);

      assert_eq!(map_str.meta().name, "str");
      assert_eq!(map_str.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::with_std(&[]).unwrap();
      let nil = val!(context
        .gc
        .borrow_mut()
        .manage_str("nil".to_string(), &NO_GC));
      let response = &[nil, nil];
      context.responses.extend_from_slice(response);

      let mut hooks = Hooks::new(&mut context);

      let error = val!(test_error_class(&hooks.as_gc()));
      let map_str = MapStr::native(&hooks.as_gc(), hooks.manage_str("str".to_string()), error);

      let values = &[];

      let mut map = Map::default();
      map.insert(VALUE_NIL, VALUE_NIL);
      let this = hooks.manage_obj(map);

      let result = map_str.call(&mut hooks, Some(val!(this)), values);
      match result {
        Call::Ok(r) => assert_eq!(&*r.to_obj().to_str(), "{ nil: nil }"),
        _ => assert!(false),
      }
    }
  }

  #[cfg(test)]
  mod len {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let map_str = MapLen::native(&hooks);

      assert_eq!(map_str.meta().name, "len");
      assert_eq!(map_str.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let map_str = MapLen::native(&hooks.as_gc());

      let values = &[];

      let mut map = Map::default();
      map.insert(VALUE_NIL, VALUE_NIL);
      let this = hooks.manage_obj(map);

      let result = map_str.call(&mut hooks, Some(val!(this)), values);
      match result {
        Call::Ok(r) => assert_eq!(r.to_num(), 1.0),
        _ => assert!(false),
      }
    }
  }

  #[cfg(test)]
  mod has {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);
      let map_has = MapHas::native(&hooks);

      assert_eq!(map_has.meta().name, "has");
      assert_eq!(map_has.meta().signature.arity, Arity::Fixed(1));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let map_has = MapHas::native(&hooks.as_gc());

      let mut map = Map::default();
      map.insert(VALUE_NIL, VALUE_NIL);
      let this = hooks.manage_obj(map);

      let result = map_has.call(&mut hooks, Some(val!(this)), &[VALUE_NIL]);
      match result {
        Call::Ok(r) => assert_eq!(r.to_bool(), true),
        _ => assert!(false),
      }

      let result = map_has.call(&mut hooks, Some(val!(this)), &[val!(false)]);
      match result {
        Call::Ok(r) => assert_eq!(r.to_bool(), false),
        _ => assert!(false),
      }
    }
  }

  #[cfg(test)]
  mod get_index {
    use super::*;
    use crate::support::{test_error_class, MockedContext};

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);
      let error = val!(test_error_class(&hooks));

      let map_index_get = MapIndexGet::native(&hooks, error);

      assert_eq!(map_index_get.meta().name, "[]");
      assert_eq!(map_index_get.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        map_index_get.meta().signature.parameters[0].kind,
        ParameterKind::Any
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let error = val!(test_error_class(&hooks.as_gc()));

      let map_index_get = MapIndexGet::native(&hooks.as_gc(), error);

      let mut map = Map::default();
      map.insert(VALUE_NIL, val!(false));
      let this = hooks.manage_obj(map);

      let result = map_index_get.call(&mut hooks, Some(val!(this)), &[VALUE_NIL]);
      match result {
        Call::Ok(r) => assert_eq!(r.to_bool(), false),
        _ => assert!(false),
      }
    }
  }

  #[cfg(test)]
  mod set_index {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let map_set = MapIndexSet::native(&hooks);

      assert_eq!(map_set.meta().name, "[]=");
      assert_eq!(map_set.meta().signature.arity, Arity::Fixed(2));
      assert_eq!(
        map_set.meta().signature.parameters[0].kind,
        ParameterKind::Any
      );
      assert_eq!(
        map_set.meta().signature.parameters[1].kind,
        ParameterKind::Any
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let map_set = MapIndexSet::native(&hooks.as_gc());

      let map = Map::default();
      let this = hooks.manage_obj(map);

      let result = map_set
        .call(&mut hooks, Some(val!(this)), &[val!(10.0), val!(true)])
        .unwrap();
      assert_eq!(result, val!(10.0));

      assert_eq!(this.len(), 1);
      assert_eq!(*this.get(&val!(true)).unwrap(), val!(10.0));

      let result = map_set
        .call(&mut hooks, Some(val!(this)), &[val!(false), val!(true)])
        .unwrap();
      assert_eq!(result, val!(false));

      assert_eq!(this.len(), 1);
      assert_eq!(*this.get(&val!(true)).unwrap(), val!(false));
    }
  }

  #[cfg(test)]
  mod get {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let map_get = MapGet::native(&hooks);

      assert_eq!(map_get.meta().name, "get");
      assert_eq!(map_get.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        map_get.meta().signature.parameters[0].kind,
        ParameterKind::Any
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let map_get = MapGet::native(&hooks.as_gc());

      let mut map = Map::default();
      map.insert(VALUE_NIL, val!(false));
      let this = hooks.manage_obj(map);

      let result = map_get.call(&mut hooks, Some(val!(this)), &[VALUE_NIL]);
      match result {
        Call::Ok(r) => assert_eq!(r.to_bool(), false),
        _ => assert!(false),
      }

      let result = map_get.call(&mut hooks, Some(val!(this)), &[val!(true)]);
      match result {
        Call::Ok(r) => assert!(r.is_nil()),
        _ => assert!(false),
      }
    }
  }

  #[cfg(test)]
  mod set {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = Hooks::new(&mut context);

      let map_set = MapSet::native(&hooks.as_gc());

      assert_eq!(map_set.meta().name, "set");
      assert_eq!(map_set.meta().signature.arity, Arity::Fixed(2));
      assert_eq!(
        map_set.meta().signature.parameters[0].kind,
        ParameterKind::Any
      );
      assert_eq!(
        map_set.meta().signature.parameters[0].kind,
        ParameterKind::Any
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let map_set = MapSet::native(&hooks.as_gc());

      let map = Map::default();
      let this = hooks.manage_obj(map);

      let result = map_set.call(&mut hooks, Some(val!(this)), &[val!(true), val!(10.0)]);
      match result {
        Call::Ok(r) => assert!(r.is_nil()),
        _ => assert!(false),
      }

      assert_eq!(this.len(), 1);
      assert_eq!(*this.get(&val!(true)).unwrap(), val!(10.0));

      let result = map_set.call(&mut hooks, Some(val!(this)), &[val!(true), val!(false)]);
      match result {
        Call::Ok(r) => assert_eq!(r.to_num(), 10.0),
        _ => assert!(false),
      }

      assert_eq!(this.len(), 1);
      assert_eq!(*this.get(&val!(true)).unwrap(), val!(false));
    }
  }

  #[cfg(test)]
  mod insert {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let map_insert = MapInsert::native(&hooks);

      assert_eq!(map_insert.meta().name, "insert");
      assert_eq!(map_insert.meta().signature.arity, Arity::Fixed(2));
      assert_eq!(
        map_insert.meta().signature.parameters[0].kind,
        ParameterKind::Any
      );
      assert_eq!(
        map_insert.meta().signature.parameters[1].kind,
        ParameterKind::Any
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let map_insert = MapInsert::native(&hooks.as_gc());

      let mut map = Map::default();
      map.insert(VALUE_NIL, val!(false));
      let this = hooks.manage_obj(map);

      let result = map_insert.call(&mut hooks, Some(val!(this)), &[VALUE_NIL, val!(true)]);
      match result {
        Call::Ok(r) => assert_eq!(r.to_bool(), false),
        _ => assert!(false),
      }

      let result = map_insert.call(&mut hooks, Some(val!(this)), &[val!(15.0), val!(true)]);
      match result {
        Call::Ok(r) => assert!(r.is_nil()),
        _ => assert!(false),
      }
    }
  }

  #[cfg(test)]
  mod remove {
    use super::*;
    use crate::support::{test_error_class, MockedContext};

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let error = val!(test_error_class(&hooks));
      let map_remove = MapRemove::native(&hooks, error);

      assert_eq!(map_remove.meta().name, "remove");
      assert_eq!(map_remove.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        map_remove.meta().signature.parameters[0].kind,
        ParameterKind::Any
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let error = val!(test_error_class(&hooks.as_gc()));
      let map_remove = MapRemove::native(&hooks.as_gc(), error);

      let mut map = Map::default();
      map.insert(VALUE_NIL, val!(false));
      let this = hooks.manage_obj(map);

      let result = map_remove.call(&mut hooks, Some(val!(this)), &[val!(10.5)]);
      match result {
        Call::Ok(_) => assert!(false),
        _ => assert!(true),
      }

      let result = map_remove.call(&mut hooks, Some(val!(this)), &[VALUE_NIL]);
      match result {
        Call::Ok(r) => assert_eq!(r.to_bool(), false),
        _ => assert!(false),
      }
    }
  }
}
