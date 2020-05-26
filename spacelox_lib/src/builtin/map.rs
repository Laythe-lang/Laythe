use super::iter::ITER_CLASS_NAME;
use crate::support::to_dyn_method;
use hashbrown::hash_map::Iter;
use spacelox_core::{
  arity::ArityKind,
  hooks::Hooks,
  io::StdIo,
  iterator::{SlIter, SlIterator},
  managed::{Managed, Trace},
  module::Module,
  native::{NativeMeta, NativeMethod},
  object::Class,
  package::Package,
  value::{Value, VALUE_NIL},
  CallResult, ModuleResult, SlError, SlHashMap,
};
use std::fmt;
use std::mem;

pub const MAP_CLASS_NAME: &'static str = "Map";

const MAP_GET: NativeMeta = NativeMeta::new("get", ArityKind::Fixed(1));
const MAP_HAS: NativeMeta = NativeMeta::new("has", ArityKind::Fixed(1));
const MAP_INSERT: NativeMeta = NativeMeta::new("insert", ArityKind::Fixed(2));
const MAP_REMOVE: NativeMeta = NativeMeta::new("remove", ArityKind::Fixed(1));
const MAP_SIZE: NativeMeta = NativeMeta::new("size", ArityKind::Fixed(0));
const MAP_STR: NativeMeta = NativeMeta::new("str", ArityKind::Fixed(0));
const MAP_ITER: NativeMeta = NativeMeta::new("iter", ArityKind::Fixed(0));

pub fn declare_map_class(hooks: &Hooks, self_module: &mut Module) -> ModuleResult<()> {
  let name = hooks.manage_str(String::from(MAP_CLASS_NAME));
  let class = hooks.manage(Class::new(name));

  self_module.export_symbol(hooks, name, Value::from(class))
}

pub fn define_map_class(hooks: &Hooks, self_module: &Module, _: &Package) {
  let name = hooks.manage_str(String::from(MAP_CLASS_NAME));
  let mut class = self_module.import().get(&name).unwrap().to_class();

  let map_iter_name = hooks.manage_str(String::from(ITER_CLASS_NAME));
  let map_iter_class = self_module.import().get(&map_iter_name).unwrap().to_class();

  class.add_method(
    hooks,
    hooks.manage_str(String::from(MAP_SIZE.name)),
    Value::from(to_dyn_method(hooks, MapSize::new())),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(MAP_STR.name)),
    Value::from(to_dyn_method(
      hooks,
      MapStr::new(hooks.manage_str(String::from(MAP_STR.name))),
    )),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(MAP_HAS.name)),
    Value::from(to_dyn_method(hooks, MapHas::new())),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(MAP_GET.name)),
    Value::from(to_dyn_method(hooks, MapGet::new())),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(MAP_REMOVE.name)),
    Value::from(to_dyn_method(hooks, MapRemove::new())),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(MAP_INSERT.name)),
    Value::from(to_dyn_method(hooks, MapInsert::new())),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(MAP_ITER.name)),
    Value::from(to_dyn_method(hooks, MapIter::new(map_iter_class))),
  );
}

#[derive(Clone, Debug)]
struct MapStr {
  meta: &'static NativeMeta,
  method_name: Managed<String>,
}

impl MapStr {
  fn new(method_name: Managed<String>) -> Self {
    Self {
      meta: &MAP_STR,
      method_name,
    }
  }
}

impl NativeMethod for MapStr {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    let map = this.to_map();

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
    Ok(Value::from(hooks.manage_str(formatted)))
  }
}

fn format_map_entry(
  item: &Value,
  method_name: Managed<String>,
  buffer: &mut String,
  hooks: &mut Hooks,
) -> Result<(), SlError> {
  // if already string quote and add to temps
  if item.is_str() {
    buffer.push_str(&format!("'{}'", item.to_str()));
    return Ok(());
  }

  // call '.str' method on each value
  let result = hooks.call_method_by_name(*item, method_name, &[])?;

  if result.is_str() {
    buffer.push_str(&*result.to_str());
    Ok(())
  } else {
    // if error throw away temporary strings
    Err(hooks.make_error(format!("No method str on {}", item)))
  }
}

impl Trace for MapStr {
  fn trace(&self) -> bool {
    self.method_name.trace()
  }

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    self.method_name.trace_debug(stdio)
  }
}

#[derive(Clone, Debug, Trace)]
struct MapSize {
  meta: &'static NativeMeta,
}

impl MapSize {
  fn new() -> Self {
    Self { meta: &MAP_SIZE }
  }
}

impl NativeMethod for MapSize {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, _hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    Ok(Value::from(this.to_map().len() as f64))
  }
}

#[derive(Clone, Debug, Trace)]
struct MapHas {
  meta: &'static NativeMeta,
}

impl MapHas {
  fn new() -> Self {
    Self { meta: &MAP_HAS }
  }
}

impl NativeMethod for MapHas {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, _hooks: &mut Hooks, this: Value, args: &[Value]) -> CallResult {
    Ok(Value::from(this.to_map().contains_key(&args[0])))
  }
}

#[derive(Clone, Debug, Trace)]
struct MapGet {
  meta: &'static NativeMeta,
}

impl MapGet {
  fn new() -> Self {
    Self { meta: &MAP_GET }
  }
}

impl NativeMethod for MapGet {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, _hooks: &mut Hooks, this: Value, args: &[Value]) -> CallResult {
    match this.to_map().get(&args[0]) {
      Some(value) => Ok(*value),
      None => Ok(VALUE_NIL),
    }
  }
}

#[derive(Clone, Debug, Trace)]
struct MapInsert {
  meta: &'static NativeMeta,
}

impl MapInsert {
  fn new() -> Self {
    Self { meta: &MAP_INSERT }
  }
}

impl NativeMethod for MapInsert {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, hooks: &mut Hooks, this: Value, args: &[Value]) -> CallResult {
    match hooks.grow(&mut this.to_map(), |map| map.insert(args[0], args[1])) {
      Some(value) => Ok(value),
      None => Ok(VALUE_NIL),
    }
  }
}

#[derive(Clone, Debug, Trace)]
struct MapRemove {
  meta: &'static NativeMeta,
}

impl MapRemove {
  fn new() -> Self {
    Self { meta: &MAP_REMOVE }
  }
}

impl NativeMethod for MapRemove {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, hooks: &mut Hooks, this: Value, args: &[Value]) -> CallResult {
    match hooks.shrink(&mut this.to_map(), |map| map.remove(&args[0])) {
      Some(removed) => Ok(removed),
      None => Err(hooks.make_error("Key not found in map.".to_string())),
    }
  }
}

struct MapIter {
  meta: &'static NativeMeta,
  iter_class: Managed<Class>,
}

impl MapIter {
  pub fn new(iter_class: Managed<Class>) -> Self {
    Self {
      meta: &MAP_ITER,
      iter_class,
    }
  }
}

impl Trace for MapIter {
  fn trace(&self) -> bool {
    self.iter_class.trace();
    true
  }

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    self.iter_class.trace_debug(stdio);
    true
  }
}

impl NativeMethod for MapIter {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    let inner_iter: Box<dyn SlIter> = Box::new(MapIterator::new(this.to_map()));
    let iter = SlIterator::new(inner_iter, self.iter_class);
    let iter = hooks.manage(iter);

    Ok(Value::from(iter))
  }
}

struct MapIterator {
  map: Managed<SlHashMap<Value, Value>>,
  iter: Iter<'static, Value, Value>,
  current: Value,
}

impl MapIterator {
  fn new(map: Managed<SlHashMap<Value, Value>>) -> Self {
    let iter = unsafe { map.deref_static().iter() };

    Self {
      map,
      iter,
      current: VALUE_NIL,
    }
  }
}

impl SlIter for MapIterator {
  fn name(&self) -> &str {
    "Map Iterator"
  }

  fn current(&self) -> Value {
    self.current
  }

  fn next(&mut self, hooks: &mut Hooks) -> CallResult {
    match self.iter.next() {
      Some(next) => {
        self.current = Value::from(hooks.manage(vec![*next.0, *next.1]));
        Ok(Value::from(true))
      }
      None => {
        self.current = VALUE_NIL;
        Ok(Value::from(false))
      }
    }
  }

  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }
}

impl Trace for MapIterator {
  fn trace(&self) -> bool {
    self.map.trace()
  }

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    self.map.trace_debug(stdio)
  }
}

impl fmt::Debug for MapIterator {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("MapIterator")
      .field("map", &self.map)
      .field("iter", &self.iter)
      .field("current", &self.current)
      .finish()
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[cfg(test)]
  mod str {
    use super::*;
    use crate::support::{test_native_dependencies, TestContext};
    use spacelox_core::{memory::NO_GC, SlHashMap};

    #[test]
    fn new() {
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let hooks = Hooks::new(&mut context);
      let map_str = MapStr::new(hooks.manage_str("str".to_string()));

      assert_eq!(map_str.meta.name, "str");
      assert_eq!(map_str.meta.arity, ArityKind::Fixed(0));
    }

    #[test]
    fn call() {
      let gc = test_native_dependencies();
      let mut context = TestContext::new(
        &gc,
        &[
          Value::from(gc.manage_str("nil".to_string(), &NO_GC)),
          Value::from(gc.manage_str("nil".to_string(), &NO_GC)),
        ],
      );
      let mut hooks = Hooks::new(&mut context);
      let map_str = MapStr::new(hooks.manage_str("str".to_string()));

      let values = &[];

      let mut map = SlHashMap::default();
      map.insert(VALUE_NIL, VALUE_NIL);
      let this = hooks.manage(map);

      let result = map_str.call(&mut hooks, Value::from(this), values);
      match result {
        Ok(r) => assert_eq!(&*r.to_str(), "{ nil: nil }"),
        Err(_) => assert!(false),
      }
    }
  }

  #[cfg(test)]
  mod size {
    use super::*;
    use crate::support::{test_native_dependencies, TestContext};
    use spacelox_core::SlHashMap;

    #[test]
    fn new() {
      let map_str = MapSize::new();

      assert_eq!(map_str.meta.name, "size");
      assert_eq!(map_str.meta.arity, ArityKind::Fixed(0));
    }

    #[test]
    fn call() {
      let map_str = MapSize::new();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let values = &[];

      let mut map = SlHashMap::default();
      map.insert(VALUE_NIL, VALUE_NIL);
      let this = hooks.manage(map);

      let result = map_str.call(&mut hooks, Value::from(this), values);
      match result {
        Ok(r) => assert_eq!(r.to_num(), 1.0),
        Err(_) => assert!(false),
      }
    }
  }

  #[cfg(test)]
  mod has {
    use super::*;
    use crate::support::{test_native_dependencies, TestContext};
    use spacelox_core::SlHashMap;

    #[test]
    fn new() {
      let map_has = MapHas::new();

      assert_eq!(map_has.meta.name, "has");
      assert_eq!(map_has.meta.arity, ArityKind::Fixed(1));
    }

    #[test]
    fn call() {
      let map_has = MapHas::new();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let mut map = SlHashMap::default();
      map.insert(VALUE_NIL, VALUE_NIL);
      let this = hooks.manage(map);

      let result = map_has.call(&mut hooks, Value::from(this), &[VALUE_NIL]);
      match result {
        Ok(r) => assert_eq!(r.to_bool(), true),
        Err(_) => assert!(false),
      }

      let result = map_has.call(&mut hooks, Value::from(this), &[Value::from(false)]);
      match result {
        Ok(r) => assert_eq!(r.to_bool(), false),
        Err(_) => assert!(false),
      }
    }
  }

  #[cfg(test)]
  mod get {
    use super::*;
    use crate::support::{test_native_dependencies, TestContext};
    use spacelox_core::SlHashMap;

    #[test]
    fn new() {
      let map_get = MapGet::new();

      assert_eq!(map_get.meta.name, "get");
      assert_eq!(map_get.meta.arity, ArityKind::Fixed(1));
    }

    #[test]
    fn call() {
      let map_get = MapGet::new();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let mut map = SlHashMap::default();
      map.insert(VALUE_NIL, Value::from(false));
      let this = hooks.manage(map);

      let result = map_get.call(&mut hooks, Value::from(this), &[VALUE_NIL]);
      match result {
        Ok(r) => assert_eq!(r.to_bool(), false),
        Err(_) => assert!(false),
      }

      let result = map_get.call(&mut hooks, Value::from(this), &[Value::from(true)]);
      match result {
        Ok(r) => assert!(r.is_nil()),
        Err(_) => assert!(false),
      }
    }
  }

  #[cfg(test)]
  mod insert {
    use super::*;
    use crate::support::{test_native_dependencies, TestContext};
    use spacelox_core::SlHashMap;

    #[test]
    fn new() {
      let map_insert = MapInsert::new();

      assert_eq!(map_insert.meta.name, "insert");
      assert_eq!(map_insert.meta.arity, ArityKind::Fixed(2));
    }

    #[test]
    fn call() {
      let map_insert = MapInsert::new();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let mut map = SlHashMap::default();
      map.insert(VALUE_NIL, Value::from(false));
      let this = hooks.manage(map);

      let result = map_insert.call(
        &mut hooks,
        Value::from(this),
        &[VALUE_NIL, Value::from(true)],
      );
      match result {
        Ok(r) => assert_eq!(r.to_bool(), false),
        Err(_) => assert!(false),
      }

      let result = map_insert.call(
        &mut hooks,
        Value::from(this),
        &[Value::from(15.0), Value::from(true)],
      );
      match result {
        Ok(r) => assert!(r.is_nil()),
        Err(_) => assert!(false),
      }
    }
  }

  #[cfg(test)]
  mod remove {
    use super::*;
    use crate::support::{test_native_dependencies, TestContext};
    use spacelox_core::SlHashMap;

    #[test]
    fn new() {
      let map_remove = MapRemove::new();

      assert_eq!(map_remove.meta.name, "remove");
      assert_eq!(map_remove.meta.arity, ArityKind::Fixed(1));
    }

    #[test]
    fn call() {
      let map_remove = MapRemove::new();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let mut map = SlHashMap::default();
      map.insert(VALUE_NIL, Value::from(false));
      let this = hooks.manage(map);

      let result = map_remove.call(&mut hooks, Value::from(this), &[Value::from(10.5)]);
      match result {
        Ok(_) => assert!(false),
        Err(_) => assert!(true),
      }

      let result = map_remove.call(&mut hooks, Value::from(this), &[VALUE_NIL]);
      match result {
        Ok(r) => assert_eq!(r.to_bool(), false),
        Err(_) => assert!(false),
      }
    }
  }
}
