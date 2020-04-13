use spacelox_core::managed::{Managed, Trace};
use spacelox_core::memory::Gc;
use spacelox_core::native::{NativeMeta, NativeMethod, NativeResult};
use spacelox_core::value::{ArityKind, Class, Value};

pub const NUMBER_CLASS_NAME: &'static str = "Nil";
const NUMBER_STR: NativeMeta = NativeMeta::new("str", ArityKind::Fixed(0));

pub fn create_number_class<C: Trace>(gc: &Gc, context: &C) -> Managed<Class> {
  let name = gc.manage_str(String::from(NUMBER_CLASS_NAME), context);
  let mut class = gc.manage(Class::new(name), context);

  class.methods.insert(
    gc.manage_str(String::from(NUMBER_STR.name), context),
    Value::NativeMethod(gc.manage(Box::new(NumberStr::new()), context)),
  );

  class
}

#[derive(Clone, Debug)]
struct NumberStr {
  meta: Box<NativeMeta>,
}

impl NumberStr {
  fn new() -> Self {
    Self {
      meta: Box::new(NUMBER_STR),
    }
  }
}

impl NativeMethod for NumberStr {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, gc: &Gc, context: &dyn Trace, this: Value, _args: &[Value]) -> NativeResult {
    NativeResult::Success(Value::String(gc.manage_str(this.to_string(), context)))
  }
}
