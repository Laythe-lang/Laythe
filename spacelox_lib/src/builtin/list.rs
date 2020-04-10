use spacelox_core::managed::{Managed, Trace};
use spacelox_core::memory::Gc;
use spacelox_core::native::{NativeMeta, NativeMethod, NativeResult};
use spacelox_core::value::{ArityKind, Class, Value};

pub const LIST_CLASS_NAME: &'static str = "List";

const LIST_STR: NativeMeta = NativeMeta::new("str", ArityKind::Fixed(0));
const LIST_LEN: NativeMeta = NativeMeta::new("len", ArityKind::Fixed(0));
const LIST_PUSH: NativeMeta = NativeMeta::new("push", ArityKind::Variadic(0));
const LIST_POP: NativeMeta = NativeMeta::new("pop", ArityKind::Fixed(0));
const LIST_INSERT: NativeMeta = NativeMeta::new("insert", ArityKind::Fixed(2));
const LIST_CONTAINS: NativeMeta = NativeMeta::new("contains", ArityKind::Fixed(1));

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

  class.methods.insert(
    gc.manage_str(String::from(LIST_INSERT.name), context),
    Value::NativeMethod(gc.manage(Box::new(ListInsert::new()), context)),
  );

  class.methods.insert(
    gc.manage_str(String::from(LIST_STR.name), context),
    Value::NativeMethod(gc.manage(Box::new(ListStr::new()), context)),
  );

  class.methods.insert(
    gc.manage_str(String::from(LIST_CONTAINS.name), context),
    Value::NativeMethod(gc.manage(Box::new(ListContain::new()), context)),
  );

  class
}


#[derive(Clone, Debug)]
pub struct ListStr {
  meta: Box<NativeMeta>,
}

impl ListStr {
  pub fn new() -> Self {
    Self {
      meta: Box::new(LIST_STR),
    }
  }
}

impl NativeMethod for ListStr {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, gc: &Gc, context: &dyn Trace, this: Value, _args: &[Value]) -> NativeResult {
    NativeResult::Success(Value::String(gc.manage_str(this.to_string(), context)))
  }
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

#[derive(Clone, Debug)]
pub struct ListInsert {
  meta: Box<NativeMeta>,
}

impl ListInsert {
  pub fn new() -> Self {
    Self {
      meta: Box::new(LIST_INSERT),
    }
  }
}

impl NativeMethod for ListInsert {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, _gc: &Gc, _context: &dyn Trace, this: Value, args: &[Value]) -> NativeResult {
    let rounded = args[0].to_num() as usize;
    this.to_list().insert(rounded, args[1]);
    NativeResult::Success(Value::Nil)
  }
}


#[derive(Clone, Debug)]
pub struct ListContain {
  meta: Box<NativeMeta>,
}

impl ListContain {
  pub fn new() -> Self {
    Self {
      meta: Box::new(LIST_CONTAINS),
    }
  }
}

impl NativeMethod for ListContain {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, _gc: &Gc, _context: &dyn Trace, this: Value, args: &[Value]) -> NativeResult {
    NativeResult::Success(Value::Bool(this.to_list().contains(&args[0])))
  }
}
