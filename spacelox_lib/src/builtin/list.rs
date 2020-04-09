use spacelox_core::managed::{Managed, Trace};
use spacelox_core::memory::Gc;
use spacelox_core::native::{NativeMeta, NativeMethod, NativeResult, NativeFun};
use spacelox_core::value::{ArityKind, Class, Value};

pub const LIST_CLASS_NAME: &'static str = "List";

const LIST_LEN: NativeMeta = NativeMeta::new("len", ArityKind::Fixed(0));
const LIST_INIT: NativeMeta = NativeMeta::new("init", ArityKind::Variadic(0));

pub fn create_list_class<C: Trace>(gc: &Gc, context: &C) -> Managed<Class> {
  let name = gc.manage_str(String::from(LIST_CLASS_NAME), context);
  let mut class = gc.manage(Class::new(name), context);

  let init = Value::NativeFun(gc.manage(Box::new(ListInit::new()), context));

  class.init = Some(init);

  class.methods.insert(
    gc.manage_str(String::from(LIST_INIT.name), context),
    init,
  );

  class.methods.insert(
    gc.manage_str(String::from(LIST_LEN.name), context),
    Value::NativeMethod(gc.manage(Box::new(ListStr::new()), context)),
  );

  class
}


#[derive(Clone, Debug)]
pub struct ListInit {
  meta: Box<NativeMeta>,
}

impl ListInit {
  pub fn new() -> Self {
    Self {
      meta: Box::new(LIST_INIT),
    }
  }
}

impl NativeFun for ListInit {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, gc: &Gc, context: &dyn Trace, args: &[Value]) -> NativeResult {
    let vec = args.to_vec();
    let list = gc.manage(vec, context);
    NativeResult::Success(Value::List(list))
  }
}


#[derive(Clone, Debug)]
pub struct ListStr {
  meta: Box<NativeMeta>,
}

impl ListStr {
  pub fn new() -> Self {
    Self {
      meta: Box::new(LIST_LEN),
    }
  }
}

impl NativeMethod for ListStr {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, _gc: &Gc, _context: &dyn Trace, this: Value, _args: &[Value]) -> NativeResult {
    NativeResult::Success(Value::Number(this.to_list().len() as f64))
  }
}
