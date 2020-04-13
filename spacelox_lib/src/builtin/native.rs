use spacelox_core::managed::{Managed, Trace};
use spacelox_core::memory::Gc;
use spacelox_core::native::{NativeMeta, NativeMethod, NativeResult};
use spacelox_core::value::{ArityKind, Class, Value};

pub const NATIVE_CLASS_NAME: &'static str = "Native";

const NATIVE_NAME: NativeMeta = NativeMeta::new("name", ArityKind::Fixed(0));

pub fn create_native_class<C: Trace>(gc: &Gc, context: &C) -> Managed<Class> {
  let name = gc.manage_str(String::from(NATIVE_CLASS_NAME), context);
  let mut class = gc.manage(Class::new(name), context);

  class.methods.insert(
    gc.manage_str(String::from(NATIVE_NAME.name), context),
    Value::NativeMethod(gc.manage(Box::new(NativeName::new()), context)),
  );

  class
}

#[derive(Clone, Debug)]
struct NativeName {
  meta: Box<NativeMeta>,
}

impl NativeName {
  fn new() -> Self {
    Self {
      meta: Box::new(NATIVE_NAME),
    }
  }
}

impl NativeMethod for NativeName {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, gc: &Gc, context: &dyn Trace, this: Value, _args: &[Value]) -> NativeResult {
    NativeResult::Success(Value::String(
      gc.manage_str(String::from(this.to_native_fun().meta().name), context),
    ))
  }
}
