use spacelox_core::managed::{Managed, Trace};
use spacelox_core::memory::Gc;
use spacelox_core::native::{NativeMeta, NativeMethod, NativeResult};
use spacelox_core::value::{ArityKind, Class, Value};

pub const STRING_CLASS_NAME: &'static str = "String";
const STRING_STR: NativeMeta = NativeMeta::new("str", ArityKind::Fixed(0));

pub fn create_string_class<C: Trace>(gc: &Gc, context: &C) -> Managed<Class> {
  let name = gc.manage_str(String::from(STRING_CLASS_NAME), context);
  let mut class = gc.manage(Class::new(name), context);

  class.methods.insert(
    gc.manage_str(String::from(STRING_STR.name), context),
    Value::NativeMethod(gc.manage(Box::new(StringStr::new()), context)),
  );

  class
}

#[derive(Clone, Debug)]
pub struct StringStr {
  meta: Box<NativeMeta>,
}

impl StringStr {
  pub fn new() -> Self {
    Self {
      meta: Box::new(STRING_STR),
    }
  }
}

impl NativeMethod for StringStr {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, _gc: &Gc, _context: &dyn Trace, this: Value, _args: &[Value]) -> NativeResult {
    NativeResult::Success(Value::String(this.to_str()))
  }
}
