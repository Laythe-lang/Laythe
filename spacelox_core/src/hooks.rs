use crate::{
  CallResult,
  managed::{Manage, Managed, Trace},
  memory::Gc,
  value::Value,
  SpaceloxError,
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
  /// use spacelox_core::memory::Gc;
  /// 
  /// let gc = Gc::default();
  /// let mut context = NoContext::new(&gc);
  /// let hooks = Hooks::new(&mut context);
  /// 
  /// let allocated = hooks.manage_str(String::from("example"));
  /// assert_eq!(*allocated, String::from("example"));
  /// ```
  pub fn new(context: &'a mut dyn HookContext) -> Hooks<'a> {
    Hooks { context }
  }

  /// Provide a function for the surround context to execute
  pub fn call(&mut self, callable: Value, args: &[Value]) -> CallResult {
    self.context.call(callable, args)
  }

  /// Request a spacelox error object be generated with the provided message
  pub fn error(&self, message: String) -> CallResult {
    Err(SpaceloxError::new(self.manage_str(message)))
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

  /// Tell the context's gc that the provided managed object may resize during this operation
  pub fn resize<T: 'static + Manage, R, F: Fn(&mut T) -> R>(
    &self,
    managed: &mut T,
    action: F,
  ) -> R {
    self.context.gc().resize(managed, self.context, action)
  }
}

/// A set of functionality required by the hooks objects in order to operate
pub trait HookContext: Trace {
  /// Execute a spacelox value in the surround context
  fn call(&mut self, callable: Value, args: &[Value]) -> CallResult;

  /// Get a reference to the context garbage collector
  fn gc(&self) -> &Gc;
}

pub struct NoContext<'a> {
  gc: &'a Gc,
}

impl<'a> NoContext<'a> {
  pub fn new(gc: &'a Gc) -> Self {
    Self { gc }
  }
}

impl<'a> Trace for NoContext<'a> {
  fn trace(&self) -> bool {
    false
  }
  fn trace_debug(&self, _stdio: &dyn crate::io::StdIo) -> bool {
    false
  }
}

impl<'a> HookContext for NoContext<'a> {
  fn gc(&self) -> &Gc {
    self.gc
  }

  fn call(&mut self, _callable: Value, _args: &[Value]) -> CallResult {
    Ok(Value::Nil)
  }
}
