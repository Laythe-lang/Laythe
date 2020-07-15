use crate::{
  value::{Value, VALUE_NIL},
  CallResult, LyError,
};
use laythe_env::{
  io::Io,
  managed::{Manage, Managed, Trace},
  memory::Gc,
  stdio::Stdio,
};

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
  /// let allocated = hooks.manage_str("example".to_string());
  /// assert_eq!(*allocated, "example".to_string());
  /// ```
  pub fn new(context: &'a mut dyn HookContext) -> Hooks<'a> {
    Hooks { context }
  }

  /// Get a GcHooks from this hooks struct
  #[inline]
  pub fn to_gc(&self) -> GcHooks {
    GcHooks::new(self.context.gc_context())
  }

  /// Get a CallHooks from this Hooks struct
  #[inline]
  pub fn to_call(&mut self) -> CallHooks {
    CallHooks::new(self.context.call_context())
  }

  /// Get a handle to the current io
  pub fn to_io(&mut self) -> Io {
    self.context.io()
  }

  /// Provide a function for the surround context to execute
  pub fn call(&mut self, callable: Value, args: &[Value]) -> CallResult {
    self.context.call_context().call(callable, args)
  }

  /// Provide a value and a method for the surround context to execute
  pub fn call_method(&mut self, this: Value, method: Value, args: &[Value]) -> CallResult {
    self.context.call_context().call_method(this, method, args)
  }

  /// Provide a value and a method name for the surrounding context to execute
  pub fn call_method_by_name(
    &mut self,
    this: Value,
    method_name: Managed<String>,
    args: &[Value],
  ) -> CallResult {
    self
      .context
      .call_context()
      .call_method_by_name(this, method_name, args)
  }

  /// Request a laythe error object be generated with the provided message
  pub fn error(&self, message: String) -> CallResult {
    Err(LyError::new(self.manage_str(message)))
  }

  /// Request a laythe error object be generated with the provided message
  pub fn make_error(&self, message: String) -> LyError {
    LyError::new(self.manage_str(message))
  }

  /// Request an object be managed by the context's garbage collector
  pub fn manage<T: 'static + Manage>(&self, data: T) -> Managed<T> {
    self.to_gc().manage(data)
  }

  /// Request a string be managed by the context's garbage collector
  pub fn manage_str(&self, string: String) -> Managed<String> {
    self.to_gc().manage_str(string)
  }

  /// Request an object by cloned then managed by the context's garbage collector
  pub fn clone_managed<T: 'static + Manage + Clone>(&self, managed: Managed<T>) -> Managed<T> {
    self.to_gc().clone_managed(managed)
  }

  /// Tell the context's gc that the provided managed object may grow during this operation
  pub fn grow<T: 'static + Manage, R, F: Fn(&mut T) -> R>(&self, managed: &mut T, action: F) -> R {
    self.to_gc().grow(managed, action)
  }

  /// Tell the context's gc that the provided managed object may shrink during this operation
  pub fn shrink<T: 'static + Manage, R, F: Fn(&mut T) -> R>(
    &self,
    managed: &mut T,
    action: F,
  ) -> R {
    self.to_gc().shrink(managed, action)
  }

  /// Push a new root onto the gc
  pub fn push_root<T: 'static + Manage>(&self, managed: T) {
    self.to_gc().push_root(managed)
  }

  /// Pop a root off the gc
  pub fn pop_roots(&self, count: usize) {
    self.to_gc().pop_roots(count)
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
  /// let allocated = hooks.manage_str("example".to_string());
  /// assert_eq!(*allocated, "example".to_string());
  /// ```
  pub fn new(context: &'a dyn GcContext) -> GcHooks<'a> {
    GcHooks { context }
  }

  /// Request a laythe error object be generated with the provided message
  pub fn error(&self, message: String) -> CallResult {
    Err(LyError::new(self.manage_str(message)))
  }

  /// Request a laythe error object be generated with the provided message
  pub fn make_error(&self, message: String) -> LyError {
    LyError::new(self.manage_str(message))
  }

  /// Request an object be managed by the context's garbage collector
  #[inline]
  pub fn manage<T: 'static + Manage>(&self, data: T) -> Managed<T> {
    self.context.gc().manage(data, self.context)
  }

  /// Request a string be managed by the context's garbage collector
  #[inline]
  pub fn manage_str(&self, string: String) -> Managed<String> {
    self.context.gc().manage_str(string, self.context)
  }

  /// Request an object by cloned then managed by the context's garbage collector
  #[inline]
  pub fn clone_managed<T: 'static + Manage + Clone>(&self, managed: Managed<T>) -> Managed<T> {
    self.context.gc().clone_managed(managed, self.context)
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

pub struct CallHooks<'a> {
  context: &'a mut dyn CallContext,
}

impl<'a> CallHooks<'a> {
  /// Create a new CallHooks instance. Uses the provided surrounding
  /// context to allocate objects and track resizing
  ///
  /// # Examples
  /// ```
  /// use laythe_core::hooks::{CallHooks, NoContext};
  /// use laythe_core::value::Value;
  /// use laythe_env::memory::Gc;
  ///
  /// let gc = Gc::default();
  /// let mut context = NoContext::new(&gc);
  /// let hooks = CallHooks::new(&mut context);
  /// ```
  pub fn new(context: &'a mut dyn CallContext) -> CallHooks<'a> {
    CallHooks { context }
  }

  /// Provide a function for the surround context to execute
  pub fn call(&mut self, callable: Value, args: &[Value]) -> CallResult {
    self.context.call(callable, args)
  }

  /// Provide a value and a method for the surround context to execute
  pub fn call_method(&mut self, this: Value, method: Value, args: &[Value]) -> CallResult {
    self.context.call_method(this, method, args)
  }

  /// Provide a value and a method name for the surrounding context to execute
  pub fn call_method_by_name(
    &mut self,
    this: Value,
    method_name: Managed<String>,
    args: &[Value],
  ) -> CallResult {
    self.context.call_method_by_name(this, method_name, args)
  }
}

pub trait HookContext {
  fn gc_context(&self) -> &dyn GcContext;
  fn call_context(&mut self) -> &mut dyn CallContext;
  fn io(&mut self) -> Io;
}

/// A set of functions related to calling laythe values
pub trait CallContext {
  /// Execute a laythe value in the surround context
  fn call(&mut self, callable: Value, args: &[Value]) -> CallResult;

  /// Execute a method on a laythe object with a given method name
  fn call_method(&mut self, this: Value, method: Value, args: &[Value]) -> CallResult;

  /// Execute a method on a laythe object with a given method name
  fn call_method_by_name(
    &mut self,
    this: Value,
    method_name: Managed<String>,
    args: &[Value],
  ) -> CallResult;
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

  fn trace_debug(&self, _stdio: &mut Stdio) -> bool {
    false
  }
}

impl<'a> HookContext for NoContext<'a> {
  fn gc_context(&self) -> &dyn GcContext {
    self
  }

  fn call_context(&mut self) -> &mut dyn CallContext {
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

impl<'a> CallContext for NoContext<'a> {
  fn call(&mut self, _callable: Value, _args: &[Value]) -> CallResult {
    Ok(VALUE_NIL)
  }

  fn call_method(&mut self, _this: Value, _method: Value, _args: &[Value]) -> CallResult {
    Ok(VALUE_NIL)
  }

  fn call_method_by_name(
    &mut self,
    _this: Value,
    _method_name: Managed<String>,
    _args: &[Value],
  ) -> CallResult {
    Ok(VALUE_NIL)
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

    fn trace_debug(&self, _stdio: &mut Stdio) -> bool {
      false
    }
  }

  impl HookContext for TestContext {
    fn gc_context(&self) -> &dyn GcContext {
      self
    }

    fn call_context(&mut self) -> &mut dyn CallContext {
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

  impl CallContext for TestContext {
    fn call(&mut self, _callable: Value, _args: &[Value]) -> CallResult {
      Ok(VALUE_NIL)
    }

    fn call_method(&mut self, _this: Value, _method: Value, _args: &[Value]) -> CallResult {
      Ok(VALUE_NIL)
    }

    fn call_method_by_name(
      &mut self,
      _this: Value,
      _method_name: Managed<String>,
      _args: &[Value],
    ) -> CallResult {
      Ok(VALUE_NIL)
    }
  }
}
