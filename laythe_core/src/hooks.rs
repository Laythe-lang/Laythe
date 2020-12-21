use std::io::Write;

use crate::{
  value::{Value, VALUE_NIL},
  Call,
};
use laythe_env::{
  io::Io,
  managed::{Manage, Managed, Trace},
  memory::Gc,
};
use smol_str::SmolStr;

/// A set of commands that a native function to request from it's surrounding
/// context
pub struct Hooks<'a> {
  /// The hooks context typically the vm in most cases or a testing harness
  context: &'a mut dyn HookContext,
}

impl<'a> Hooks<'a> {
  /// Create a new Hooks instance. Uses the provided uses the surrounding
  /// context to allocate objects and call functions
  ///
  /// # Examples
  /// ```
  /// use laythe_core::hooks::{Hooks, NoContext};
  /// use laythe_core::value::Value;
  /// use laythe_env::memory::Gc;
  ///
  /// let gc = Gc::default();
  /// let mut context = NoContext::new(&gc);
  /// let hooks = Hooks::new(&mut context);
  ///
  /// let allocated = hooks.manage_str("example");
  /// assert_eq!(*allocated, "example".to_string());
  /// ```
  pub fn new(context: &'a mut dyn HookContext) -> Hooks<'a> {
    Hooks { context }
  }

  /// Get a GcHooks from this hooks struct
  #[inline]
  pub fn as_gc(&self) -> GcHooks {
    GcHooks::new(self.context.gc_context())
  }

  /// Get a CallHooks from this Hooks struct
  #[inline]
  pub fn as_value(&mut self) -> ValueHooks {
    ValueHooks::new(self.context.value_context())
  }

  /// Get a handle to the current io
  pub fn as_io(&mut self) -> Io {
    self.context.io()
  }

  /// Provide a function for the surround context to execute
  pub fn call(&mut self, callable: Value, args: &[Value]) -> Call {
    self.context.value_context().call(callable, args)
  }

  /// Provide a value and a method for the surround context to execute
  pub fn call_method(&mut self, this: Value, method: Value, args: &[Value]) -> Call {
    self.context.value_context().call_method(this, method, args)
  }

  /// Provide a value and a method name for the surrounding context to execute
  pub fn get_method(&mut self, this: Value, method_name: Managed<SmolStr>) -> Call {
    self.context.value_context().get_method(this, method_name)
  }

  /// Provide a value and a method name for the surrounding context to execute
  pub fn get_class(&mut self, this: Value) -> Value {
    self.context.value_context().get_class(this)
  }

  /// Request an object be managed by the context's garbage collector
  pub fn manage<T: 'static + Manage>(&self, data: T) -> Managed<T> {
    self.as_gc().manage(data)
  }

  /// Request a string be managed by the context's garbage collector
  pub fn manage_str<S: Into<String> + AsRef<str>>(&self, string: S) -> Managed<SmolStr> {
    self.as_gc().manage_str(string)
  }

  /// Tell the context's gc that the provided managed object may grow during this operation
  pub fn grow<T: 'static + Manage, R, F: Fn(&mut T) -> R>(&self, managed: &mut T, action: F) -> R {
    self.as_gc().grow(managed, action)
  }

  /// Tell the context's gc that the provided managed object may shrink during this operation
  pub fn shrink<T: 'static + Manage, R, F: Fn(&mut T) -> R>(
    &self,
    managed: &mut T,
    action: F,
  ) -> R {
    self.as_gc().shrink(managed, action)
  }

  /// Push a new root onto the gc
  pub fn push_root<T: 'static + Manage>(&self, managed: T) {
    self.as_gc().push_root(managed)
  }

  /// Pop a root off the gc
  pub fn pop_roots(&self, count: usize) {
    self.as_gc().pop_roots(count)
  }
}

/// A set of hooks that provide a gc and tracing roots
pub struct GcHooks<'a> {
  context: &'a dyn GcContext,
}

impl<'a> GcHooks<'a> {
  /// Create a new GcHooks instance. Uses the provided surrounding
  /// context to allocate objects and track resizing
  ///
  /// # Examples
  /// ```
  /// use laythe_core::hooks::{GcHooks, NoContext};
  /// use laythe_core::value::Value;
  /// use laythe_env::memory::Gc;
  ///
  /// let gc = Gc::default();
  /// let mut context = NoContext::new(&gc);
  /// let hooks = GcHooks::new(&mut context);
  ///
  /// let allocated = hooks.manage_str("example");
  /// assert_eq!(allocated, "example");
  /// ```
  pub fn new(context: &'a dyn GcContext) -> GcHooks<'a> {
    GcHooks { context }
  }

  /// Request an object be managed by the context's garbage collector
  #[inline]
  pub fn manage<T: 'static + Manage>(&self, data: T) -> Managed<T> {
    self.context.gc().manage(data, self.context)
  }

  /// Request a string be managed by the context's garbage collector
  #[inline]
  pub fn manage_str<S: Into<String> + AsRef<str>>(&self, string: S) -> Managed<SmolStr> {
    self.context.gc().manage_str(string, self.context)
  }

  /// Tell the context's gc that the provided managed object may grow during this operation
  #[inline]
  pub fn grow<T: 'static + Manage, R, F: Fn(&mut T) -> R>(&self, managed: &mut T, action: F) -> R {
    self.context.gc().grow(managed, self.context, action)
  }

  /// Tell the context's gc that the provided managed object may shrink during this operation
  #[inline]
  pub fn shrink<T: 'static + Manage, R, F: Fn(&mut T) -> R>(
    &self,
    managed: &mut T,
    action: F,
  ) -> R {
    self.context.gc().shrink(managed, action)
  }

  /// Push a new root onto the gc
  #[inline]
  pub fn push_root<T: 'static + Manage>(&self, managed: T) {
    self.context.gc().push_root(managed);
  }

  /// Pop a root off the gc
  #[inline]
  pub fn pop_roots(&self, count: usize) {
    self.context.gc().pop_roots(count);
  }
}

pub struct ValueHooks<'a> {
  context: &'a mut dyn ValueContext,
}

impl<'a> ValueHooks<'a> {
  /// Create a new CallHooks instance. Uses the provided surrounding
  /// context to allocate objects and track resizing
  ///
  /// # Examples
  /// ```
  /// use laythe_core::hooks::{ValueHooks, NoContext};
  /// use laythe_core::value::Value;
  /// use laythe_env::memory::Gc;
  ///
  /// let gc = Gc::default();
  /// let mut context = NoContext::new(&gc);
  /// let hooks = ValueHooks::new(&mut context);
  /// ```
  pub fn new(context: &'a mut dyn ValueContext) -> ValueHooks<'a> {
    ValueHooks { context }
  }

  /// Provide a function for the surround context to execute
  pub fn call(&mut self, callable: Value, args: &[Value]) -> Call {
    self.context.call(callable, args)
  }

  /// Provide a value and a method for the surround context to execute
  pub fn call_method(&mut self, this: Value, method: Value, args: &[Value]) -> Call {
    self.context.call_method(this, method, args)
  }

  /// Provide a value and a method name for the surrounding context to execute
  pub fn get_method(&mut self, this: Value, method_name: Managed<SmolStr>) -> Call {
    self.context.get_method(this, method_name)
  }
}

pub trait HookContext {
  fn gc_context(&self) -> &dyn GcContext;
  fn value_context(&mut self) -> &mut dyn ValueContext;
  fn io(&mut self) -> Io;
}

/// A set of functions related to calling laythe values
pub trait ValueContext {
  /// Execute a laythe value in the surround context
  fn call(&mut self, callable: Value, args: &[Value]) -> Call;

  /// Execute a method on a laythe object with a given method name
  fn call_method(&mut self, this: Value, method: Value, args: &[Value]) -> Call;

  /// Retrieve a method for this value with a given method name
  fn get_method(&mut self, this: Value, method_name: Managed<SmolStr>) -> Call;

  /// Retrieve the class for this value
  fn get_class(&mut self, this: Value) -> Value;
}

/// A set of functionality required by the hooks objects in order to operate
pub trait GcContext: Trace {
  /// Get a reference to the context garbage collector
  fn gc(&self) -> &Gc;
}

pub struct NoContext<'a> {
  /// A reference to a gc just to allocate
  gc: &'a Gc,
}

impl<'a> NoContext<'a> {
  /// Create a new instance of no context
  pub fn new(gc: &'a Gc) -> Self {
    Self { gc }
  }
}

impl<'a> Trace for NoContext<'a> {
  fn trace(&self) -> bool {
    false
  }

  fn trace_debug(&self, _stdout: &mut dyn Write) -> bool {
    false
  }
}

impl<'a> HookContext for NoContext<'a> {
  fn gc_context(&self) -> &dyn GcContext {
    self
  }

  fn value_context(&mut self) -> &mut dyn ValueContext {
    self
  }

  fn io(&mut self) -> Io {
    Io::default()
  }
}

impl<'a> GcContext for NoContext<'a> {
  fn gc(&self) -> &Gc {
    &self.gc
  }
}

impl<'a> ValueContext for NoContext<'a> {
  fn call(&mut self, _callable: Value, _args: &[Value]) -> Call {
    Call::Ok(VALUE_NIL)
  }

  fn call_method(&mut self, _this: Value, _method: Value, _args: &[Value]) -> Call {
    Call::Ok(VALUE_NIL)
  }

  fn get_method(&mut self, _this: Value, _method_name: Managed<SmolStr>) -> Call {
    Call::Ok(VALUE_NIL)
  }

  fn get_class(&mut self, _this: Value) -> Value {
    VALUE_NIL
  }
}

pub mod support {
  use super::*;

  /// A placeholder context that does not gc and does not call functions
  #[derive(Default)]
  pub struct TestContext {
    /// A reference to a gc just to allocate
    gc: Gc,
  }

  impl TestContext {
    /// Create a new instance of no context
    pub fn new(gc: Gc) -> Self {
      Self { gc }
    }
  }

  impl Trace for TestContext {
    fn trace(&self) -> bool {
      false
    }

    fn trace_debug(&self, _stdout: &mut dyn Write) -> bool {
      false
    }
  }

  impl HookContext for TestContext {
    fn gc_context(&self) -> &dyn GcContext {
      self
    }

    fn value_context(&mut self) -> &mut dyn ValueContext {
      self
    }

    fn io(&mut self) -> Io {
      Io::default()
    }
  }

  impl GcContext for TestContext {
    fn gc(&self) -> &Gc {
      &self.gc
    }
  }

  impl ValueContext for TestContext {
    fn call(&mut self, _callable: Value, _args: &[Value]) -> Call {
      Call::Ok(VALUE_NIL)
    }

    fn call_method(&mut self, _this: Value, _method: Value, _args: &[Value]) -> Call {
      Call::Ok(VALUE_NIL)
    }

    fn get_method(&mut self, _this: Value, _method_name: Managed<SmolStr>) -> Call {
      Call::Ok(VALUE_NIL)
    }

    fn get_class(&mut self, _this: Value) -> Value {
      VALUE_NIL
    }
  }
}
