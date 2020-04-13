use spacelox_core::managed::{Managed, Trace};
use spacelox_core::memory::Gc;
use spacelox_core::native::{NativeMeta, NativeMethod, NativeResult};
use spacelox_core::value::{ArityKind, Class, Value};

pub const FUN_CLASS_NAME: &'static str = "Fun";

const FUN_NAME: NativeMeta = NativeMeta::new("name", ArityKind::Fixed(0));

pub fn create_fun_class<C: Trace>(gc: &Gc, context: &C) -> Managed<Class> {
  let name = gc.manage_str(String::from(FUN_CLASS_NAME), context);
  let mut class = gc.manage(Class::new(name), context);

  class.methods.insert(
    gc.manage_str(String::from(FUN_NAME.name), context),
    Value::NativeMethod(gc.manage(Box::new(FunName::new()), context)),
  );

  class
}

#[derive(Clone, Debug)]
struct FunName {
  meta: Box<NativeMeta>,
}

impl FunName {
  fn new() -> Self {
    Self {
      meta: Box::new(FUN_NAME),
    }
  }
}

impl NativeMethod for FunName {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, _gc: &Gc, _context: &dyn Trace, this: Value, _args: &[Value]) -> NativeResult {
    NativeResult::Success(Value::String(this.to_fun().name))
  }
}
