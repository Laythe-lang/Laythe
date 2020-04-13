use spacelox_core::managed::{Managed, Trace};
use spacelox_core::memory::Gc;
use spacelox_core::native::{NativeMeta, NativeMethod, NativeResult};
use spacelox_core::value::{ArityKind, Class, Value};

pub const BOOL_CLASS_NAME: &'static str = "Bool";
const BOOL_STR: NativeMeta = NativeMeta::new("str", ArityKind::Fixed(0));

pub fn create_bool_class<C: Trace>(gc: &Gc, context: &C) -> Managed<Class> {
  let name = gc.manage_str(String::from(BOOL_CLASS_NAME), context);
  let mut class = gc.manage(Class::new(name), context);

  class.methods.insert(
    gc.manage_str(String::from(BOOL_STR.name), context),
    Value::NativeMethod(gc.manage(Box::new(BoolStr::new()), context)),
  );

  class
}

#[derive(Clone, Debug)]
struct BoolStr {
  meta: Box<NativeMeta>,
}

impl BoolStr {
  fn new() -> Self {
    Self {
      meta: Box::new(BOOL_STR),
    }
  }
}

impl NativeMethod for BoolStr {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, gc: &Gc, context: &dyn Trace, this: Value, _args: &[Value]) -> NativeResult {
    NativeResult::Success(Value::String(gc.manage_str(this.to_string(), context)))
  }
}
