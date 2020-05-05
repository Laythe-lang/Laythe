use spacelox_core::{
  arity::ArityKind,
  hooks::Hooks,
  io::StdIo,
  managed::Trace,
  module::Module,
  native::{NativeMeta, NativeMethod},
  package::Package,
  value::{Class, Value},
  CallResult, ModuleResult,
};

pub const MAP_CLASS_NAME: &'static str = "Map";

const MAP_STR: NativeMeta = NativeMeta::new("str", ArityKind::Fixed(0));
const MAP_SIZE: NativeMeta = NativeMeta::new("size", ArityKind::Fixed(0));
const MAP_HAS: NativeMeta = NativeMeta::new("has", ArityKind::Fixed(1));
const MAP_GET: NativeMeta = NativeMeta::new("get", ArityKind::Fixed(1));
const MAP_SET: NativeMeta = NativeMeta::new("set", ArityKind::Fixed(2));

pub fn declare_map_class(hooks: &Hooks, self_module: &mut Module) -> ModuleResult<()> {
  let name = hooks.manage_str(String::from(MAP_CLASS_NAME));
  let class = hooks.manage(Class::new(name));

  self_module.add_export(hooks, name, Value::Class(class))
}

pub fn define_map_class(hooks: &Hooks, self_module: &Module, _: &Package) {
  let name = hooks.manage_str(String::from(MAP_CLASS_NAME));
  let mut class = self_module.get_symbol(hooks, name).unwrap().to_class();

  class.add_method(
    hooks,
    hooks.manage_str(String::from(MAP_SIZE.name)),
    Value::NativeMethod(hooks.manage(Box::new(MapSize::new()))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(MAP_STR.name)),
    Value::NativeMethod(hooks.manage(Box::new(MapStr::new()))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(MAP_HAS.name)),
    Value::NativeMethod(hooks.manage(Box::new(MapHas::new()))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(MAP_GET.name)),
    Value::NativeMethod(hooks.manage(Box::new(MapGet::new()))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(MAP_SET.name)),
    Value::NativeMethod(hooks.manage(Box::new(MapSet::new()))),
  );
}

#[derive(Clone, Debug, Trace)]
struct MapStr {
  meta: Box<NativeMeta>,
}

impl MapStr {
  fn new() -> Self {
    Self {
      meta: Box::new(MAP_STR),
    }
  }
}

impl NativeMethod for MapStr {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    Ok(Value::String(hooks.manage_str(this.to_string())))
  }
}

#[derive(Clone, Debug, Trace)]
struct MapSize {
  meta: Box<NativeMeta>,
}

impl MapSize {
  fn new() -> Self {
    Self {
      meta: Box::new(MAP_SIZE),
    }
  }
}

impl NativeMethod for MapSize {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, _hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    Ok(Value::Number(this.to_map().len() as f64))
  }
}

#[derive(Clone, Debug, Trace)]
struct MapHas {
  meta: Box<NativeMeta>,
}

impl MapHas {
  fn new() -> Self {
    Self {
      meta: Box::new(MAP_HAS),
    }
  }
}

impl NativeMethod for MapHas {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, _hooks: &mut Hooks, this: Value, args: &[Value]) -> CallResult {
    Ok(Value::Bool(this.to_map().contains_key(&args[0])))
  }
}

#[derive(Clone, Debug, Trace)]
struct MapGet {
  meta: Box<NativeMeta>,
}

impl MapGet {
  fn new() -> Self {
    Self {
      meta: Box::new(MAP_GET),
    }
  }
}

impl NativeMethod for MapGet {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, _hooks: &mut Hooks, this: Value, args: &[Value]) -> CallResult {
    match this.to_map().get(&args[0]) {
      Some(value) => Ok(*value),
      None => Ok(Value::Nil),
    }
  }
}

#[derive(Clone, Debug, Trace)]
struct MapSet {
  meta: Box<NativeMeta>,
}

impl MapSet {
  fn new() -> Self {
    Self {
      meta: Box::new(MAP_SET),
    }
  }
}

impl NativeMethod for MapSet {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, _hooks: &mut Hooks, this: Value, args: &[Value]) -> CallResult {
    match this.to_map().insert(args[0], args[1]) {
      Some(value) => Ok(value),
      None => Ok(Value::Nil),
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[cfg(test)]
  mod str {
    use super::*;
    use crate::support::{test_native_dependencies, TestContext};
    use spacelox_core::SlHashMap;

    #[test]
    fn new() {
      let map_str = MapStr::new();

      assert_eq!(map_str.meta.name, "str");
      assert_eq!(map_str.meta.arity, ArityKind::Fixed(0));
    }

    #[test]
    fn call() {
      let map_str = MapStr::new();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let values = &[];

      let mut map = SlHashMap::default();
      map.insert(Value::Nil, Value::Nil);
      let this = hooks.manage(map);

      let result = map_str.call(&mut hooks, Value::Map(this), values);
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
      map.insert(Value::Nil, Value::Nil);
      let this = hooks.manage(map);

      let result = map_str.call(&mut hooks, Value::Map(this), values);
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
      map.insert(Value::Nil, Value::Nil);
      let this = hooks.manage(map);

      let result = map_has.call(&mut hooks, Value::Map(this), &[Value::Nil]);
      match result {
        Ok(r) => assert_eq!(r.to_bool(), true),
        Err(_) => assert!(false),
      }

      let result = map_has.call(&mut hooks, Value::Map(this), &[Value::Bool(false)]);
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
      map.insert(Value::Nil, Value::Bool(false));
      let this = hooks.manage(map);

      let result = map_get.call(&mut hooks, Value::Map(this), &[Value::Nil]);
      match result {
        Ok(r) => assert_eq!(r.to_bool(), false),
        Err(_) => assert!(false),
      }

      let result = map_get.call(&mut hooks, Value::Map(this), &[Value::Bool(true)]);
      match result {
        Ok(r) => assert!(r.is_nil()),
        Err(_) => assert!(false),
      }
    }
  }

  #[cfg(test)]
  mod set {
    use super::*;
    use crate::support::{test_native_dependencies, TestContext};
    use spacelox_core::SlHashMap;

    #[test]
    fn new() {
      let map_get = MapSet::new();

      assert_eq!(map_get.meta.name, "set");
      assert_eq!(map_get.meta.arity, ArityKind::Fixed(2));
    }

    #[test]
    fn call() {
      let map_set = MapSet::new();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let mut map = SlHashMap::default();
      map.insert(Value::Nil, Value::Bool(false));
      let this = hooks.manage(map);

      let result = map_set.call(
        &mut hooks,
        Value::Map(this),
        &[Value::Nil, Value::Bool(true)],
      );
      match result {
        Ok(r) => assert_eq!(r.to_bool(), false),
        Err(_) => assert!(false),
      }

      let result = map_set.call(
        &mut hooks,
        Value::Map(this),
        &[Value::Number(15.0), Value::Bool(true)],
      );
      match result {
        Ok(r) => assert!(r.is_nil()),
        Err(_) => assert!(false),
      }
    }
  }
}
