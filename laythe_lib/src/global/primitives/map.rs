use crate::{
  native,
  support::{default_class_inheritance, export_and_insert, load_class_from_module, to_dyn_native},
};
use hashbrown::hash_map::Iter;
use laythe_core::{
  constants::INDEX_GET,
  constants::INDEX_SET,
  hooks::{GcHooks, Hooks},
  iterator::{LyIter, LyIterator},
  module::Module,
  native::{MetaData, Native, NativeMeta, NativeMetaBuilder},
  object::{List, Map},
  package::Package,
  signature::{Arity, ParameterBuilder, ParameterKind},
  utils::use_sentinel_nan,
  val,
  value::{Value, VALUE_NIL},
  CallResult, LyResult,
};
use laythe_env::managed::{Managed, Trace};
use smol_str::SmolStr;
use std::io::Write;
use std::mem;

pub const MAP_CLASS_NAME: &str = "Map";

const MAP_INDEX_GET: NativeMetaBuilder = NativeMetaBuilder::method(INDEX_GET, Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("key", ParameterKind::Any)]);

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
  .with_params(&[ParameterBuilder::new("key", ParameterKind::Any)]);

const MAP_LEN: NativeMetaBuilder = NativeMetaBuilder::method("len", Arity::Fixed(0));
const MAP_STR: NativeMetaBuilder = NativeMetaBuilder::method("str", Arity::Fixed(0));
const MAP_ITER: NativeMetaBuilder = NativeMetaBuilder::method("iter", Arity::Fixed(0));

pub fn declare_map_class(hooks: &GcHooks, module: &mut Module, package: &Package) -> LyResult<()> {
  let class = default_class_inheritance(hooks, package, MAP_CLASS_NAME)?;
  export_and_insert(hooks, module, class.name, val!(class))
}

pub fn define_map_class(hooks: &GcHooks, module: &Module, _: &Package) -> LyResult<()> {
  let mut class = load_class_from_module(hooks, module, MAP_CLASS_NAME)?;

  class.add_method(
    hooks,
    hooks.manage_str(MAP_LEN.name),
    val!(to_dyn_native(hooks, MapLen::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(MAP_INDEX_GET.name),
    val!(to_dyn_native(hooks, MapIndexGet::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(MAP_INDEX_SET.name),
    val!(to_dyn_native(hooks, MapIndexSet::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(MAP_STR.name),
    val!(to_dyn_native(
      hooks,
      MapStr::new(MAP_STR.to_meta(hooks), hooks.manage_str(MAP_STR.name)),
    )),
  );

  class.add_method(
    hooks,
    hooks.manage_str(MAP_HAS.name),
    val!(to_dyn_native(hooks, MapHas::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(MAP_GET.name),
    val!(to_dyn_native(hooks, MapGet::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(MAP_SET.name),
    val!(to_dyn_native(hooks, MapSet::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(MAP_REMOVE.name),
    val!(to_dyn_native(hooks, MapRemove::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(MAP_INSERT.name),
    val!(to_dyn_native(hooks, MapInsert::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(MAP_ITER.name),
    val!(to_dyn_native(hooks, MapIter::from(hooks))),
  );

  Ok(())
}

#[derive(Debug)]
struct MapStr {
  method_name: Managed<SmolStr>,
  meta: NativeMeta,
}

impl MapStr {
  fn new(meta: NativeMeta, method_name: Managed<SmolStr>) -> Self {
    Self { meta, method_name }
  }
}

impl MetaData for MapStr {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }
}

impl Native for MapStr {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> CallResult {
    let map = this.unwrap().to_map();

    if map.len() == 0 {
      return Ok(val!(hooks.manage_str("{}")));
    }

    // buffer for temporary strings
    let mut strings: Vec<String> = Vec::with_capacity(map.len());

    for (key, value) in map.iter() {
      let mut kvp_string = String::new();

      format_map_entry(key, self.method_name, &mut kvp_string, hooks)?;
      kvp_string.push_str(": ");
      format_map_entry(value, self.method_name, &mut kvp_string, hooks)?;

      strings.push(kvp_string)
    }

    // format and join strings
    let formatted = format!("{{ {} }}", strings.join(", "));
    Ok(val!(hooks.manage_str(formatted)))
  }
}

fn format_map_entry(
  item: &Value,
  method_name: Managed<SmolStr>,
  buffer: &mut String,
  hooks: &mut Hooks,
) -> LyResult<()> {
  // if already string quote and add to temps
  if item.is_str() {
    buffer.push_str(&format!("'{}'", item.to_str()));
    return Ok(());
  }

  // call '.str' method on each value
  let result = hooks
    .get_method(*item, method_name)
    .and_then(|method| hooks.call_method(*item, method, &[]))?;

  if result.is_str() {
    buffer.push_str(&*result.to_str());
    Ok(())
  } else {
    // if error throw away temporary strings
    hooks.error(format!("No method str on {}", item))
  }
}

impl Trace for MapStr {
  fn trace(&self) -> bool {
    self.method_name.trace()
  }

  fn trace_debug(&self, stdout: &mut dyn Write) -> bool {
    self.method_name.trace_debug(stdout)
  }
}

native!(MapLen, MAP_LEN);

impl Native for MapLen {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> CallResult {
    Ok(val!(this.unwrap().to_map().len() as f64))
  }
}

native!(MapHas, MAP_HAS);

impl Native for MapHas {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> CallResult {
    Ok(val!(this.unwrap().to_map().contains_key(&args[0])))
  }
}

native!(MapIndexGet, MAP_INDEX_GET);

impl Native for MapIndexGet {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> CallResult {
    let index = args[0];
    let key = if index.is_num() {
      val!(use_sentinel_nan(index.to_num()))
    } else {
      index
    };

    match this.unwrap().to_map().get(&key) {
      Some(value) => Ok(*value),
      None => hooks.error(format!("Key not found. {} is not present", key)),
    }
  }
}

native!(MapIndexSet, MAP_INDEX_SET);

impl Native for MapIndexSet {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> CallResult {
    let index = args[0];
    let key = if index.is_num() {
      val!(use_sentinel_nan(index.to_num()))
    } else {
      index
    };

    Ok(
      this
        .unwrap()
        .to_map()
        .insert(key, args[1])
        .unwrap_or(VALUE_NIL),
    )
  }
}

native!(MapGet, MAP_GET);

impl Native for MapGet {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> CallResult {
    let index = args[0];
    let key = if index.is_num() {
      val!(use_sentinel_nan(index.to_num()))
    } else {
      index
    };

    match this.unwrap().to_map().get(&key) {
      Some(value) => Ok(*value),
      None => Ok(VALUE_NIL),
    }
  }
}

native!(MapSet, MAP_SET);

impl Native for MapSet {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> CallResult {
    let index = args[0];
    let key = if index.is_num() {
      val!(use_sentinel_nan(index.to_num()))
    } else {
      index
    };

    Ok(
      this
        .unwrap()
        .to_map()
        .insert(key, args[1])
        .unwrap_or(VALUE_NIL),
    )
  }
}

native!(MapInsert, MAP_INSERT);

impl Native for MapInsert {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> CallResult {
    match hooks.grow(&mut this.unwrap().to_map(), |map| {
      map.insert(args[0], args[1])
    }) {
      Some(value) => Ok(value),
      None => Ok(VALUE_NIL),
    }
  }
}

native!(MapRemove, MAP_REMOVE);

impl Native for MapRemove {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> CallResult {
    match hooks.shrink(&mut this.unwrap().to_map(), |map| map.remove(&args[0])) {
      Some(removed) => Ok(removed),
      None => hooks.error("Key not found in map.".to_string()),
    }
  }
}

native!(MapIter, MAP_ITER);

impl Native for MapIter {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> CallResult {
    let inner_iter: Box<dyn LyIter> = Box::new(MapIterator::new(this.unwrap().to_map()));
    let iter = LyIterator::new(inner_iter);
    let iter = hooks.manage(iter);

    Ok(val!(iter))
  }
}

#[derive(Debug)]
struct MapIterator {
  map: Managed<Map<Value, Value>>,
  iter: Iter<'static, Value, Value>,
  current: Value,
}

impl MapIterator {
  fn new(map: Managed<Map<Value, Value>>) -> Self {
    let iter = unsafe { map.deref_static().iter() };

    Self {
      map,
      iter,
      current: VALUE_NIL,
    }
  }
}

impl LyIter for MapIterator {
  fn name(&self) -> &str {
    "Map Iterator"
  }

  fn current(&self) -> Value {
    self.current
  }

  fn next(&mut self, hooks: &mut Hooks) -> CallResult {
    match self.iter.next() {
      Some(next) => {
        self.current = val!(hooks.manage(List::from(&[*next.0, *next.1] as &[Value])));
        Ok(val!(true))
      }
      None => {
        self.current = VALUE_NIL;
        Ok(val!(false))
      }
    }
  }

  fn size_hint(&self) -> Option<usize> {
    Some(self.map.len())
  }

  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }
}

impl Trace for MapIterator {
  fn trace(&self) -> bool {
    self.map.trace()
  }

  fn trace_debug(&self, stdout: &mut dyn Write) -> bool {
    self.map.trace_debug(stdout)
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use laythe_core::object::Map;
  use laythe_env::memory::NO_GC;

  #[cfg(test)]
  mod str {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);
      let map_str = MapStr::new(MAP_STR.to_meta(&hooks), hooks.manage_str("str".to_string()));

      assert_eq!(map_str.meta().name, "str");
      assert_eq!(map_str.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::with_std(&[]);
      let response = &[
        val!(context.gc.manage_str("nil".to_string(), &NO_GC)),
        val!(context.gc.manage_str("nil".to_string(), &NO_GC)),
      ];
      context.responses.extend_from_slice(response);

      let mut hooks = Hooks::new(&mut context);
      let map_str = MapStr::new(
        MAP_STR.to_meta(&hooks.as_gc()),
        hooks.manage_str("str".to_string()),
      );

      let values = &[];

      let mut map = Map::default();
      map.insert(VALUE_NIL, VALUE_NIL);
      let this = hooks.manage(map);

      let result = map_str.call(&mut hooks, Some(val!(this)), values);
      match result {
        Ok(r) => assert_eq!(&*r.to_str(), "{ nil: nil }"),
        Err(_) => assert!(false),
      }
    }
  }

  #[cfg(test)]
  mod size {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = Hooks::new(&mut context);

      let map_str = MapLen::from(&hooks);

      assert_eq!(map_str.meta().name, "len");
      assert_eq!(map_str.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let map_str = MapLen::from(&hooks);

      let values = &[];

      let mut map = Map::default();
      map.insert(VALUE_NIL, VALUE_NIL);
      let this = hooks.manage(map);

      let result = map_str.call(&mut hooks, Some(val!(this)), values);
      match result {
        Ok(r) => assert_eq!(r.to_num(), 1.0),
        Err(_) => assert!(false),
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
      let hooks = Hooks::new(&mut context);
      let map_has = MapHas::from(&hooks);

      assert_eq!(map_has.meta().name, "has");
      assert_eq!(map_has.meta().signature.arity, Arity::Fixed(1));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let map_has = MapHas::from(&hooks);

      let mut map = Map::default();
      map.insert(VALUE_NIL, VALUE_NIL);
      let this = hooks.manage(map);

      let result = map_has.call(&mut hooks, Some(val!(this)), &[VALUE_NIL]);
      match result {
        Ok(r) => assert_eq!(r.to_bool(), true),
        Err(_) => assert!(false),
      }

      let result = map_has.call(&mut hooks, Some(val!(this)), &[val!(false)]);
      match result {
        Ok(r) => assert_eq!(r.to_bool(), false),
        Err(_) => assert!(false),
      }
    }
  }

  #[cfg(test)]
  mod get_index {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = Hooks::new(&mut context);

      let map_get = MapGet::from(&hooks);

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
      let map_get = MapGet::from(&hooks);

      let mut map = Map::default();
      map.insert(VALUE_NIL, val!(false));
      let this = hooks.manage(map);

      let result = map_get.call(&mut hooks, Some(val!(this)), &[VALUE_NIL]);
      match result {
        Ok(r) => assert_eq!(r.to_bool(), false),
        Err(_) => assert!(false),
      }

      let result = map_get.call(&mut hooks, Some(val!(this)), &[val!(true)]);
      match result {
        Ok(r) => assert!(r.is_nil()),
        Err(_) => assert!(false),
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
      let hooks = Hooks::new(&mut context);

      let map_set = MapSet::from(&hooks);

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
      let map_set = MapSet::from(&hooks);

      let map = Map::default();
      let this = hooks.manage(map);

      let result = map_set.call(&mut hooks, Some(val!(this)), &[val!(true), val!(10.0)]);
      match result {
        Ok(r) => assert!(r.is_nil()),
        Err(_) => assert!(false),
      }

      assert_eq!(this.len(), 1);
      assert_eq!(*this.get(&val!(true)).unwrap(), val!(10.0));

      let result = map_set.call(&mut hooks, Some(val!(this)), &[val!(true), val!(false)]);
      match result {
        Ok(r) => assert_eq!(r.to_num(), 10.0),
        Err(_) => assert!(false),
      }

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
      let hooks = Hooks::new(&mut context);

      let map_get = MapGet::from(&hooks);

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
      let map_get = MapGet::from(&hooks);

      let mut map = Map::default();
      map.insert(VALUE_NIL, val!(false));
      let this = hooks.manage(map);

      let result = map_get.call(&mut hooks, Some(val!(this)), &[VALUE_NIL]);
      match result {
        Ok(r) => assert_eq!(r.to_bool(), false),
        Err(_) => assert!(false),
      }

      let result = map_get.call(&mut hooks, Some(val!(this)), &[val!(true)]);
      match result {
        Ok(r) => assert!(r.is_nil()),
        Err(_) => assert!(false),
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

      let map_set = MapSet::from(&hooks);

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
      let map_set = MapSet::from(&hooks);

      let map = Map::default();
      let this = hooks.manage(map);

      let result = map_set.call(&mut hooks, Some(val!(this)), &[val!(true), val!(10.0)]);
      match result {
        Ok(r) => assert!(r.is_nil()),
        Err(_) => assert!(false),
      }

      assert_eq!(this.len(), 1);
      assert_eq!(*this.get(&val!(true)).unwrap(), val!(10.0));

      let result = map_set.call(&mut hooks, Some(val!(this)), &[val!(true), val!(false)]);
      match result {
        Ok(r) => assert_eq!(r.to_num(), 10.0),
        Err(_) => assert!(false),
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
      let hooks = Hooks::new(&mut context);

      let map_insert = MapInsert::from(&hooks);

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
      let map_insert = MapInsert::from(&hooks);

      let mut map = Map::default();
      map.insert(VALUE_NIL, val!(false));
      let this = hooks.manage(map);

      let result = map_insert.call(&mut hooks, Some(val!(this)), &[VALUE_NIL, val!(true)]);
      match result {
        Ok(r) => assert_eq!(r.to_bool(), false),
        Err(_) => assert!(false),
      }

      let result = map_insert.call(&mut hooks, Some(val!(this)), &[val!(15.0), val!(true)]);
      match result {
        Ok(r) => assert!(r.is_nil()),
        Err(_) => assert!(false),
      }
    }
  }

  #[cfg(test)]
  mod remove {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = Hooks::new(&mut context);

      let map_remove = MapRemove::from(&hooks);

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
      let map_remove = MapRemove::from(&hooks);

      let mut map = Map::default();
      map.insert(VALUE_NIL, val!(false));
      let this = hooks.manage(map);

      let result = map_remove.call(&mut hooks, Some(val!(this)), &[val!(10.5)]);
      match result {
        Ok(_) => assert!(false),
        Err(_) => assert!(true),
      }

      let result = map_remove.call(&mut hooks, Some(val!(this)), &[VALUE_NIL]);
      match result {
        Ok(r) => assert_eq!(r.to_bool(), false),
        Err(_) => assert!(false),
      }
    }
  }
}
