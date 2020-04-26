use spacelox_core::{
  CallResult,
  SpaceloxError,
  managed::Trace,
  io::{NativeStdIo, StdIo},
  hooks::HookContext,
  memory::{Gc, NoGc, NO_GC},
  value::Value,
};

pub struct TestContext<'a> {
  gc: &'a Gc,
  no_gc: NoGc,
  responses: Vec<Value>,
  response_count: usize,
}

impl<'a> TestContext<'a> {
  pub fn new(gc: &'a Gc, responses: &[Value]) -> Self {
    Self {
      gc,
      no_gc: NoGc(),
      responses: Vec::from(responses),
      response_count: 0,
    }
  }
}

#[cfg(test)]
impl<'a> HookContext for TestContext<'a> {
  fn gc(&self) -> &Gc {
    self.gc
  }

  fn call(&mut self, callable: Value, args: &[Value]) -> CallResult {
    let arity = match callable {
      Value::Closure(closure) => closure.fun.arity,
      Value::Method(method) => method.method.to_closure().fun.arity,
      Value::NativeFun(native) => native.meta().arity,
      Value::NativeMethod(method) => method.meta().arity,
      _ => {
        return Err(SpaceloxError::new(self.gc.manage_str(String::from("Not callable"), &NO_GC)));
      }
    };

    match arity.check(args.len() as u8) {
      Ok(_) => (),
      Err(_) => {
        return Err(SpaceloxError::new(self.gc.manage_str(String::from("Not correct arity"), &NO_GC)))
      }
    }

    if self.response_count < self.responses.len() {
      let response = self.responses[self.response_count];
      self.response_count += 1;
      return Ok(response);
    }

    Err(SpaceloxError::new(self.gc.manage_str(String::from("No mocked results"), &NO_GC)))
  }
}

#[cfg(test)]
impl<'a> Trace for TestContext<'a> {
  fn trace(&self) -> bool {
    self.no_gc.trace()
  }

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    self.no_gc.trace_debug(stdio)
  }
}

#[cfg(test)]
pub fn test_native_dependencies() -> Box<Gc> {
  Box::new(Gc::new(Box::new(NativeStdIo())))
}
