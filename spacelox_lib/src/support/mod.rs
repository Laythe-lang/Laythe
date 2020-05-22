use spacelox_core::{
  hooks::Hooks,
  managed::Managed,
  native::NativeMethod,
};

pub fn to_dyn_method<T: 'static + NativeMethod>(hooks: &Hooks, method: T) -> Managed<Box<dyn NativeMethod>> {
  hooks.manage(Box::new(method) as Box<dyn NativeMethod>)
}

#[cfg(test)]
pub use self::test::*;

#[cfg(test)]
mod test {
  use spacelox_core::{
    hooks::{HookContext},
    managed::{Trace, Managed},
    memory::{Gc, NoGc, NO_GC},
    value::{ValueVariant, Value},
    arity::ArityKind, SlError, CallResult, io::{StdIo, NativeStdIo},
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
  
  impl<'a> HookContext for TestContext<'a> {
    fn gc(&self) -> &Gc {
      self.gc
    }
  
    fn call(&mut self, callable: Value, args: &[Value]) -> CallResult {
      let arity = match callable.kind() {
        ValueVariant::Closure => callable.to_closure().fun.arity,
        ValueVariant::Method => callable.to_method().method.to_closure().fun.arity,
        ValueVariant::NativeFun => callable.to_native_fun().meta().arity,
        ValueVariant::NativeMethod => callable.to_native_method().meta().arity,
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
      let arity = match method.kind() {
        ValueVariant::Closure => method.to_closure().fun.arity,
        ValueVariant::Method => method.to_method().method.to_closure().fun.arity,
        ValueVariant::NativeFun => method.to_native_fun().meta().arity,
        ValueVariant::NativeMethod => method.to_native_method().meta().arity,
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
      let arity = if this.is_instance() {
        let instance = this.to_instance();
        match instance.class.get_method(&method_name) {
          Some(method) => {
            if method.is_closure() {
              method.to_closure().fun.arity
            } else if method.is_native_method() {
              method.to_native_fun().meta().arity
            } else {
              panic!("Only closures and native methods should be methods on an instance")
            }
          }
          None => {
            return Err(SlError::new(self.gc.manage_str(
              format!("No method {} exists on {:?}.", method_name, instance),
              &NO_GC,
            )));
          }
        }
      } else {
        ArityKind::Variadic(0)
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
  
  impl<'a> Trace for TestContext<'a> {
    fn trace(&self) -> bool {
      self.no_gc.trace()
    }
  
    fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
      self.no_gc.trace_debug(stdio)
    }
  }
  
  pub fn test_native_dependencies() -> Box<Gc> {
    Box::new(Gc::new(Box::new(NativeStdIo())))
  }
}

