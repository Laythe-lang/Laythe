use super::{class_inheritance, error::TYPE_ERROR_NAME, error_inheritance};
use crate::{
  native, native_with_error,
  support::{export_and_insert, load_class_from_module},
  StdResult,
};
use hashbrown::hash_map::Iter;
use laythe_core::{
  constants::{INDEX_GET, INDEX_SET}, hooks::{GcHooks, Hooks}, if_let_obj, list, managed::{DebugHeap, DebugWrap, Trace}, module::Module, object::{Enumerate, Enumerator, LyNative, LyStr, Map, Native, NativeMetaBuilder, ObjectKind}, signature::{Arity, ParameterBuilder, ParameterKind}, to_obj_kind, utils::use_sentinel_nan, val, value::{Value, VALUE_NIL}, Call, LyError, ObjRef, Ref
};
use std::io::Write;

pub const MAP_CLASS_NAME: &str = "Map";
pub const KEY_ERROR_NAME: &str = "KeyError";

const MAP_INDEX_GET: NativeMetaBuilder = NativeMetaBuilder::method(INDEX_GET, Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("key", ParameterKind::Object)])
  .with_stack();

const MAP_INDEX_SET: NativeMetaBuilder = NativeMetaBuilder::method(INDEX_SET, Arity::Fixed(2))
  .with_params(&[
    ParameterBuilder::new("key", ParameterKind::Object),
    ParameterBuilder::new("val", ParameterKind::Object),
  ]);

const MAP_GET: NativeMetaBuilder = NativeMetaBuilder::method("get", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("key", ParameterKind::Object)]);

const MAP_SET: NativeMetaBuilder =
  NativeMetaBuilder::method("set", Arity::Fixed(2)).with_params(&[
    ParameterBuilder::new("key", ParameterKind::Object),
    ParameterBuilder::new("value", ParameterKind::Object),
  ]);

const MAP_HAS: NativeMetaBuilder = NativeMetaBuilder::method("has", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("key", ParameterKind::Object)]);

const MAP_INSERT: NativeMetaBuilder = NativeMetaBuilder::method("insert", Arity::Fixed(2))
  .with_params(&[
    ParameterBuilder::new("key", ParameterKind::Object),
    ParameterBuilder::new("val", ParameterKind::Object),
  ]);

const MAP_REMOVE: NativeMetaBuilder = NativeMetaBuilder::method("remove", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("key", ParameterKind::Object)])
  .with_stack();

const MAP_LEN: NativeMetaBuilder = NativeMetaBuilder::method("len", Arity::Fixed(0));
const MAP_STR: NativeMetaBuilder = NativeMetaBuilder::method("str", Arity::Fixed(0)).with_stack();
const MAP_ITER: NativeMetaBuilder = NativeMetaBuilder::method("iter", Arity::Fixed(0));

pub fn declare_map_class(hooks: &GcHooks, module: Ref<Module>) -> StdResult<()> {
  let class = class_inheritance(hooks, module, MAP_CLASS_NAME)?;
  let key_error = error_inheritance(hooks, module, KEY_ERROR_NAME)?;

  export_and_insert(hooks, module, class.name(), val!(class))?;
  export_and_insert(hooks, module, key_error.name(), val!(key_error))
}

pub fn define_map_class(hooks: &GcHooks, module: Ref<Module>) -> StdResult<()> {
  let mut class = load_class_from_module(hooks, module, MAP_CLASS_NAME)?;
  let key_error = val!(load_class_from_module(hooks, module, KEY_ERROR_NAME)?);
  let type_error = val!(load_class_from_module(hooks, module, TYPE_ERROR_NAME)?);

  class.add_method(hooks.manage_str(MAP_LEN.name), val!(MapLen::native(hooks)));

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

  class.add_method(hooks.manage_str(MAP_HAS.name), val!(MapHas::native(hooks)));

  class.add_method(hooks.manage_str(MAP_GET.name), val!(MapGet::native(hooks)));

  class.add_method(hooks.manage_str(MAP_SET.name), val!(MapSet::native(hooks)));

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
  method_name: LyStr,
  error: Value,
}

impl MapStr {
  fn native(hooks: &GcHooks, method_name: LyStr, error: Value) -> ObjRef<Native> {
    debug_assert!(error.is_obj_kind(ObjectKind::Class));
    let native = Box::new(Self { method_name, error }) as Box<dyn LyNative>;

    hooks.manage_obj(Native::new(MAP_STR.build(hooks), native))
  }
}

impl LyNative for MapStr {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let map = args[0].to_obj().to_map();

    if map.is_empty() {
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
  method_name: LyStr,
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
        "Expected type str from {item}.str()"
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
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
    Call::Ok(val!(args[0].to_obj().to_map().len() as f64))
  }
}

native!(MapHas, MAP_HAS);

impl LyNative for MapHas {
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
    Call::Ok(val!(args[0].to_obj().to_map().contains_key(&args[1])))
  }
}

native_with_error!(MapIndexGet, MAP_INDEX_GET);

impl LyNative for MapIndexGet {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let index = args[1];
    let key = if index.is_num() {
      val!(use_sentinel_nan(index.to_num()))
    } else {
      index
    };

    match args[0].to_obj().to_map().get(&key) {
      Some(value) => Call::Ok(*value),
      None => self.call_error(hooks, format!("Key not found. {key} is not present")),
    }
  }
}

native!(MapIndexSet, MAP_INDEX_SET);

impl LyNative for MapIndexSet {
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
    let index = args[2];
    let key = if index.is_num() {
      val!(use_sentinel_nan(index.to_num()))
    } else {
      index
    };

    let mut map = args[0].to_obj().to_map();
    map.insert(key, args[1]);
    Call::Ok(args[1])
  }
}

native!(MapGet, MAP_GET);

impl LyNative for MapGet {
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
    let index = args[1];
    let key = if index.is_num() {
      val!(use_sentinel_nan(index.to_num()))
    } else {
      index
    };

    match args[0].to_obj().to_map().get(&key) {
      Some(value) => Call::Ok(*value),
      None => Call::Ok(VALUE_NIL),
    }
  }
}

native!(MapSet, MAP_SET);

impl LyNative for MapSet {
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
    let index = args[1];
    let key = if index.is_num() {
      val!(use_sentinel_nan(index.to_num()))
    } else {
      index
    };

    let mut map = args[0].to_obj().to_map();
    let result = map.insert(key, args[2]);
    Call::Ok(result.unwrap_or(VALUE_NIL))
  }
}

native!(MapInsert, MAP_INSERT);

impl LyNative for MapInsert {
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
    let mut map = args[0].to_obj().to_map();
    match map.insert(args[1], args[2]) {
      Some(value) => Call::Ok(value),
      None => Call::Ok(VALUE_NIL),
    }
  }
}

native_with_error!(MapRemove, MAP_REMOVE);

impl LyNative for MapRemove {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let mut map = args[0].to_obj().to_map();
    match map.remove(&args[1]) {
      Some(removed) => Call::Ok(removed),
      None => self.call_error(hooks, "Key not found in map."),
    }
  }
}

native!(MapIter, MAP_ITER);

impl LyNative for MapIter {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let inner_iter: Box<dyn Enumerate> = Box::new(MapIterator::new(args[0].to_obj().to_map()));
    let iter = Enumerator::new(inner_iter);
    let iter = hooks.manage_obj(iter);

    Call::Ok(val!(iter))
  }
}

#[derive(Debug)]
struct MapIterator {
  map: ObjRef<Map<Value, Value>>,
  iter: Iter<'static, Value, Value>,
  current: Value,
}

impl MapIterator {
  fn new(map: ObjRef<Map<Value, Value>>) -> Self {
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
        let dawg = &[*next.0, *next.1];
        let bro = list!(dawg);
        self.current = val!(hooks.manage_obj(bro));
        Call::Ok(val!(true))
      },
      None => {
        self.current = VALUE_NIL;
        Call::Ok(val!(false))
      },
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
    use laythe_core::NO_GC;

    use super::*;
    use crate::support::{test_error_class, MockedContext};

    #[test]
    fn call() {
      let mut context = MockedContext::with_std(&[]).unwrap();
      let nil = val!(context.gc.borrow_mut().manage_str("nil", &NO_GC));
      let response = &[nil, nil];
      context.responses.extend_from_slice(response);

      let mut hooks = Hooks::new(&mut context);

      let error = val!(test_error_class(&hooks.as_gc()));
      let map_str = MapStr::native(&hooks.as_gc(), hooks.manage_str("str"), error);

      let mut map = Map::default();
      map.insert(VALUE_NIL, VALUE_NIL);
      let map = hooks.manage_obj(map);
      let values = &[val!(map)];

      let result = map_str.call(&mut hooks, values);
      match result {
        Call::Ok(r) => assert_eq!(&*r.to_obj().to_str(), "{ nil: nil }"),
        _ => panic!(),
      }
    }
  }

  #[cfg(test)]
  mod len {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let map_str = MapLen::native(&hooks.as_gc());

      let mut map = Map::default();
      map.insert(VALUE_NIL, VALUE_NIL);
      let map = hooks.manage_obj(map);
      let values = &[val!(map)];

      let result = map_str.call(&mut hooks, values);
      match result {
        Call::Ok(r) => assert_eq!(r.to_num(), 1.0),
        _ => panic!(),
      }
    }
  }

  #[cfg(test)]
  mod has {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let map_has = MapHas::native(&hooks.as_gc());

      let mut map = Map::default();
      map.insert(VALUE_NIL, VALUE_NIL);
      let map = hooks.manage_obj(map);
      let this = val!(map);

      let result = map_has.call(&mut hooks, &[this, VALUE_NIL]);
      match result {
        Call::Ok(r) => assert!(r.to_bool()),
        _ => panic!(),
      }

      let result = map_has.call(&mut hooks, &[this, val!(false)]);
      match result {
        Call::Ok(r) => assert!(!r.to_bool()),
        _ => panic!(),
      }
    }
  }

  #[cfg(test)]
  mod get_index {
    use super::*;
    use crate::support::{test_error_class, MockedContext};

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let error = val!(test_error_class(&hooks.as_gc()));

      let map_index_get = MapIndexGet::native(&hooks.as_gc(), error);

      let mut map = Map::default();
      map.insert(VALUE_NIL, val!(false));
      let map = hooks.manage_obj(map);
      let this = val!(map);

      let result = map_index_get.call(&mut hooks, &[this, VALUE_NIL]);
      match result {
        Call::Ok(r) => assert!(!r.to_bool()),
        _ => panic!(),
      }
    }
  }

  #[cfg(test)]
  mod set_index {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let map_set = MapIndexSet::native(&hooks.as_gc());

      let map = Map::default();
      let map = hooks.manage_obj(map);
      let this = val!(map);

      let result = map_set
        .call(&mut hooks, &[this, val!(10.0), val!(true)])
        .unwrap();
      assert_eq!(result, val!(10.0));

      assert_eq!(map.len(), 1);
      assert_eq!(*map.get(&val!(true)).unwrap(), val!(10.0));

      let result = map_set
        .call(&mut hooks, &[this, val!(false), val!(true)])
        .unwrap();
      assert_eq!(result, val!(false));

      assert_eq!(map.len(), 1);
      assert_eq!(*map.get(&val!(true)).unwrap(), val!(false));
    }
  }

  #[cfg(test)]
  mod get {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let map_get = MapGet::native(&hooks.as_gc());

      let mut map = Map::default();
      map.insert(VALUE_NIL, val!(false));
      let map = hooks.manage_obj(map);
      let this = val!(map);

      let result = map_get.call(&mut hooks, &[this, VALUE_NIL]);
      match result {
        Call::Ok(r) => assert!(!r.to_bool()),
        _ => panic!(),
      }

      let result = map_get.call(&mut hooks, &[this, val!(true)]);
      match result {
        Call::Ok(r) => assert!(r.is_nil()),
        _ => panic!(),
      }
    }
  }

  #[cfg(test)]
  mod set {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let map_set = MapSet::native(&hooks.as_gc());

      let map = Map::default();
      let map = hooks.manage_obj(map);
      let this = val!(map);

      let result = map_set.call(&mut hooks, &[this, val!(true), val!(10.0)]);
      match result {
        Call::Ok(r) => assert!(r.is_nil()),
        _ => panic!(),
      }

      assert_eq!(map.len(), 1);
      assert_eq!(*map.get(&val!(true)).unwrap(), val!(10.0));

      let result = map_set.call(&mut hooks, &[this, val!(true), val!(false)]);
      match result {
        Call::Ok(r) => assert_eq!(r.to_num(), 10.0),
        _ => panic!(),
      }

      assert_eq!(map.len(), 1);
      assert_eq!(*map.get(&val!(true)).unwrap(), val!(false));
    }
  }

  #[cfg(test)]
  mod insert {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let map_insert = MapInsert::native(&hooks.as_gc());

      let mut map = Map::default();
      map.insert(VALUE_NIL, val!(false));
      let map = hooks.manage_obj(map);
      let this = val!(map);

      let result = map_insert.call(&mut hooks, &[this, VALUE_NIL, val!(true)]);
      match result {
        Call::Ok(r) => assert!(!r.to_bool()),
        _ => panic!(),
      }

      let result = map_insert.call(&mut hooks, &[this, val!(15.0), val!(true)]);
      match result {
        Call::Ok(r) => assert!(r.is_nil()),
        _ => panic!(),
      }
    }
  }

  #[cfg(test)]
  mod remove {
    use super::*;
    use crate::support::{test_error_class, MockedContext};

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let error = val!(test_error_class(&hooks.as_gc()));
      let map_remove = MapRemove::native(&hooks.as_gc(), error);

      let mut map = Map::default();
      map.insert(VALUE_NIL, val!(false));
      let map = hooks.manage_obj(map);
      let this = val!(map);

      let result = map_remove.call(&mut hooks, &[this, val!(10.5)]);
      if result.is_ok() {
        panic!()
      }

      let result = map_remove.call(&mut hooks, &[this, VALUE_NIL]);
      if let Call::Ok(r) = result {
        assert!(!r.to_bool())
      }
    }
  }
}
