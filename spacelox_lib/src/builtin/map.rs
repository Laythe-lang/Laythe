use spacelox_core::managed::{Managed, Trace};
use spacelox_core::memory::Gc;
use spacelox_core::native::{NativeMeta, NativeMethod, NativeResult};
use spacelox_core::value::{ArityKind, Class, Value};

pub const MAP_CLASS_NAME: &'static str = "Map";

const MAP_STR: NativeMeta = NativeMeta::new("str", ArityKind::Fixed(0));
const MAP_LEN: NativeMeta = NativeMeta::new("len", ArityKind::Fixed(0));

pub fn create_map_class<C: Trace>(gc: &Gc, context: &C) -> Managed<Class> {
  let name = gc.manage_str(String::from(MAP_CLASS_NAME), context);
  let mut class = gc.manage(Class::new(name), context);

  class.methods.insert(
    gc.manage_str(String::from(MAP_LEN.name), context),
    Value::NativeMethod(gc.manage(Box::new(MapLen::new()), context)),
  );

  class.methods.insert(
    gc.manage_str(String::from(MAP_STR.name), context),
    Value::NativeMethod(gc.manage(Box::new(MapStr::new()), context)),
  );

  class
}

#[derive(Clone, Debug)]
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

  fn call(&self, gc: &Gc, context: &dyn Trace, this: Value, _args: &[Value]) -> NativeResult {
    NativeResult::Success(Value::String(gc.manage_str(this.to_string(), context)))
  }
}

#[derive(Clone, Debug)]
struct MapLen {
  meta: Box<NativeMeta>,
}

impl MapLen {
  fn new() -> Self {
    Self {
      meta: Box::new(MAP_LEN),
    }
  }
}

impl NativeMethod for MapLen {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, _gc: &Gc, _context: &dyn Trace, this: Value, _args: &[Value]) -> NativeResult {
    NativeResult::Success(Value::Number(this.to_map().len() as f64))
  }
}
