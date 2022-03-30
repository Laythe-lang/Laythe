use laythe_core::managed::Trace;
use laythe_core::{
  hooks::{GcContext, HookContext, ValueContext},
  managed::{GcStr, TraceRoot},
  memory::Allocator,
  val,
  value::Value,
  Call,
};
use laythe_env::io::Io;
use std::io::Write;

use super::Vm;

impl TraceRoot for Vm {
  fn trace(&self) {
    self.fiber.trace();
    self.main_fiber.trace();
    for fiber in &self.fiber_queue {
      fiber.trace();
    }
    self.files.trace();
    self.packages.trace();
    self.module_cache.trace();
    self.capture_stub.trace();

    for stub in &self.native_fun_stubs {
      stub.trace();
    }
  }

  fn trace_debug(&self, log: &mut dyn Write) {
    self.fiber.trace_debug(log);
    self.main_fiber.trace_debug(log);
    for fiber in &self.fiber_queue {
      fiber.trace_debug(log);
    }
    self.files.trace_debug(log);
    self.packages.trace_debug(log);
    self.module_cache.trace_debug(log);
    self.capture_stub.trace_debug(log);

    for stub in &self.native_fun_stubs {
      stub.trace_debug(log);
    }
  }

  fn can_collect(&self) -> bool {
    true
  }
}

impl HookContext for Vm {
  fn gc_context(&self) -> &dyn GcContext {
    self
  }

  fn value_context(&mut self) -> &mut dyn ValueContext {
    self
  }

  fn io(&mut self) -> Io {
    self.io.clone()
  }
}

impl GcContext for Vm {
  fn gc(&self) -> std::cell::RefMut<'_, Allocator> {
    self.gc.borrow_mut()
  }
}

impl ValueContext for Vm {
  fn call(&mut self, callable: Value, args: &[Value]) -> Call {
    unsafe { self.run_fun(callable, args) }
  }

  fn call_method(&mut self, this: Value, method: Value, args: &[Value]) -> Call {
    unsafe { self.run_method(this, method, args) }
  }

  fn get_method(&mut self, this: Value, method_name: GcStr) -> Call {
    unsafe { self.get_method(this, method_name) }
  }

  fn get_class(&mut self, this: Value) -> Value {
    val!(self.value_class(this))
  }
}
