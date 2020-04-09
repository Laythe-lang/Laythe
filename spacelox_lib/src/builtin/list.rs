use spacelox_core::managed::{Managed, Trace};
use spacelox_core::memory::Gc;
use spacelox_core::native::{NativeMeta, NativeMethod, NativeResult};
use spacelox_core::value::{ArityKind, Class, Value};

pub const LIST_CLASS_NAME: &'static str = "List";

const LIST_LEN: NativeMeta = NativeMeta::new("len", ArityKind::Fixed(0));
const LIST_PUSH: NativeMeta = NativeMeta::new("push", ArityKind::Variadic(0));
const LIST_POP: NativeMeta = NativeMeta::new("pop", ArityKind::Fixed(0));

pub fn create_list_class<C: Trace>(gc: &Gc, context: &C) -> Managed<Class> {
  let name = gc.manage_str(String::from(LIST_CLASS_NAME), context);
  let mut class = gc.manage(Class::new(name), context);

  class.methods.insert(
    gc.manage_str(String::from(LIST_LEN.name), context),
    Value::NativeMethod(gc.manage(Box::new(ListLen::new()), context)),
  );

  class.methods.insert(
    gc.manage_str(String::from(LIST_PUSH.name), context),
    Value::NativeMethod(gc.manage(Box::new(ListPush::new()), context)),
  );

  class.methods.insert(
    gc.manage_str(String::from(LIST_POP.name), context),
    Value::NativeMethod(gc.manage(Box::new(ListPop::new()), context)),
  );

  class
}

#[derive(Clone, Debug)]
pub struct ListLen {
  meta: Box<NativeMeta>,
}

impl ListLen {
  pub fn new() -> Self {
    Self {
      meta: Box::new(LIST_LEN),
    }
  }
}

impl NativeMethod for ListLen {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, _gc: &Gc, _context: &dyn Trace, this: Value, _args: &[Value]) -> NativeResult {
    NativeResult::Success(Value::Number(this.to_list().len() as f64))
  }
}

#[derive(Clone, Debug)]
pub struct ListPush {
  meta: Box<NativeMeta>,
}

impl ListPush {
  pub fn new() -> Self {
    Self {
      meta: Box::new(LIST_PUSH),
    }
  }
}

impl NativeMethod for ListPush {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, _gc: &Gc, _context: &dyn Trace, this: Value, args: &[Value]) -> NativeResult {
    this.to_list().extend(args);
    NativeResult::Success(Value::Nil)
  }
}

#[derive(Clone, Debug)]
pub struct ListPop {
  meta: Box<NativeMeta>,
}

impl ListPop {
  pub fn new() -> Self {
    Self {
      meta: Box::new(LIST_POP),
    }
  }
}

impl NativeMethod for ListPop {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, _gc: &Gc, _context: &dyn Trace, this: Value, _args: &[Value]) -> NativeResult {
    match this.to_list().pop() {
      Some(value) => NativeResult::Success(value),
      None => NativeResult::Success(Value::Nil),
    }
  }
}
