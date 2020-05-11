use spacelox_core::{
  arity::ArityKind,
  hooks::HookContext,
  io::{NativeStdIo, StdIo},
  managed::{Managed, Trace},
  memory::{Gc, NoGc, NO_GC},
  value::Value,
  CallResult, SlError,
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
        return Err(SlError::new(
          self.gc.manage_str(String::from("Not callable"), &NO_GC),
        ));
      }
    };

    match arity.check(args.len() as u8) {
      Ok(_) => (),
      Err(_) => {
        return Err(SlError::new(
          self
            .gc
            .manage_str(String::from("Incorrect function arity"), &NO_GC),
        ))
      }
    }

    if self.response_count < self.responses.len() {
      let response = self.responses[self.response_count];
      self.response_count += 1;
      return Ok(response);
    }

    Err(SlError::new(
      self
        .gc
        .manage_str(String::from("No mocked results"), &NO_GC),
    ))
  }

  fn call_method(&mut self, _this: Value, method: Value, args: &[Value]) -> CallResult {
    let arity = match method {
      Value::Closure(closure) => closure.fun.arity,
      Value::Method(method) => method.method.to_closure().fun.arity,
      Value::NativeFun(native) => native.meta().arity,
      Value::NativeMethod(method) => method.meta().arity,
      _ => {
        return Err(SlError::new(
          self.gc.manage_str(String::from("Not callable"), &NO_GC),
        ));
      }
    };

    match arity.check(args.len() as u8) {
      Ok(_) => (),
      Err(_) => {
        return Err(SlError::new(
          self
            .gc
            .manage_str(String::from("Incorrect function arity"), &NO_GC),
        ))
      }
    }

    if self.response_count < self.responses.len() {
      let response = self.responses[self.response_count];
      self.response_count += 1;
      return Ok(response);
    }

    Err(SlError::new(
      self
        .gc
        .manage_str(String::from("No mocked results"), &NO_GC),
    ))
  }

  fn call_method_by_name(
    &mut self,
    this: Value,
    method_name: Managed<String>,
    args: &[Value],
  ) -> CallResult {
    let arity = match this {
      Value::Instance(instance) => match instance.class.get_method(&method_name) {
        Some(method) => match method {
          Value::Closure(closure) => closure.fun.arity,
          Value::NativeMethod(method) => method.meta().arity,
          _ => panic!("Only closures and native methods should be methods on an instance"),
        },
        None => {
          return Err(SlError::new(self.gc.manage_str(
            format!("No method {} exists on {:?}.", method_name, instance),
            &NO_GC,
          )));
        }
      },
      _ => ArityKind::Variadic(0),
    };

    match arity.check(args.len() as u8) {
      Ok(_) => (),
      Err(_) => {
        return Err(SlError::new(
          self
            .gc
            .manage_str(String::from("Incorrect method arity"), &NO_GC),
        ))
      }
    }

    if self.response_count < self.responses.len() {
      let response = self.responses[self.response_count];
      self.response_count += 1;
      return Ok(response);
    }

    Err(SlError::new(
      self
        .gc
        .manage_str(String::from("No mocked results"), &NO_GC),
    ))
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
