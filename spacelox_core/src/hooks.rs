use crate::{
  value::{Value, VALUE_NIL},
  CallResult, SlError,
};
use spacelox_env::{
  managed::{Manage, Managed, Trace},
  memory::Gc,
  stdio::StdIo,
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
  /// use spacelox_core::hooks::{Hooks, NoContext};
  /// use spacelox_core::value::Value;
  /// use spacelox_env::memory::Gc;
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

  /// Stub for creating a module
  pub fn create_module() {}

  /// Stub for loading a module
  pub fn load_module() {}

  /// Request a spacelox error object be generated with the provided message
  pub fn error(&self, message: String) -> CallResult {
    Err(SlError::new(self.manage_str(message)))
  }

  /// Request a spacelox error object be generated with the provided message
  pub fn make_error(&self, message: String) -> SlError {
    SlError::new(self.manage_str(message))
  }

  /// Request an object be managed by the context's garbage collector
  pub fn manage<T: 'static + Manage>(&self, data: T) -> Managed<T> {
    self.context.gc().manage(data, self.context)
  }

  /// Request a string be managed by the context's garbage collector
  pub fn manage_str(&self, string: String) -> Managed<String> {
    self.context.gc().manage_str(string, self.context)
  }

  /// Request an object by cloned then managed by the context's garbage collector
  pub fn clone_managed<T: 'static + Manage + Clone>(&self, managed: Managed<T>) -> Managed<T> {
    self.context.gc().clone_managed(managed, self.context)
  }

  /// Tell the context's gc that the provided managed object may grow during this operation
  pub fn grow<T: 'static + Manage, R, F: Fn(&mut T) -> R>(&self, managed: &mut T, action: F) -> R {
    self.context.gc().grow(managed, self.context, action)
  }

  /// Tell the context's gc that the provided managed object may shrink during this operation
  pub fn shrink<T: 'static + Manage, R, F: Fn(&mut T) -> R>(
    &self,
    managed: &mut T,
    action: F,
  ) -> R {
    self.context.gc().shrink(managed, action)
  }

  /// Push a new root onto the gc
  pub fn push_root<T: 'static + Manage>(&self, managed: T) {
    self.context.gc().push_root(managed);
  }

  /// Pop a root off the gc
  pub fn pop_roots(&self, count: usize) {
    self.context.gc().pop_roots(count);
  }
}

/// A set of functionality required by the hooks objects in order to operate
pub trait HookContext: Trace {
  /// Execute a spacelox value in the surround context
  fn call(&mut self, callable: Value, args: &[Value]) -> CallResult;

  /// Execute a method on a spacelox object with a given method name
  fn call_method(&mut self, this: Value, method: Value, args: &[Value]) -> CallResult;

  /// Execute a method on a spacelox object with a given method name
  fn call_method_by_name(
    &mut self,
    this: Value,
    method_name: Managed<String>,
    args: &[Value],
  ) -> CallResult;

  /// Get a reference to the context garbage collector
  fn gc(&self) -> &Gc;
}

/// A placeholder context that does not gc and does not call functions
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
  fn trace_debug(&self, _stdio: &dyn StdIo) -> bool {
    false
  }
}

impl<'a> HookContext for NoContext<'a> {
  fn gc(&self) -> &Gc {
    self.gc
  }

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
