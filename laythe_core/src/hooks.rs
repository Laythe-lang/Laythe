use std::{
  cell::{RefCell, RefMut},
  io::Write,
};

use crate::{
  managed::{Gc, GcStr, Manage, Trace, TraceRoot},
  memory::Allocator,
  value::{Value, VALUE_NIL},
  Call,
};
use laythe_env::io::Io;

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
  ///
  /// let mut context = NoContext::default();
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
  pub fn get_method(&mut self, this: Value, method_name: GcStr) -> Call {
    self.context.value_context().get_method(this, method_name)
  }

  /// Provide a value and a method name for the surrounding context to execute
  pub fn get_class(&mut self, this: Value) -> Value {
    self.context.value_context().get_class(this)
  }

  /// Request an object be managed by the context's garbage collector
  pub fn manage<T: 'static + Manage>(&self, data: T) -> Gc<T> {
    self.as_gc().manage(data)
  }

  /// Request a string be managed by the context's garbage collector
  pub fn manage_str<S: AsRef<str>>(&self, string: S) -> GcStr {
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
  pub fn push_root<T: 'static + Trace>(&self, managed: T) {
    self.as_gc().push_root(managed)
  }

  /// Pop a root off the gc
  pub fn pop_roots(&self, count: usize) {
    self.as_gc().pop_roots(count)
  }
}

pub trait HookContext {
  fn gc_context(&self) -> &dyn GcContext;
  fn value_context(&mut self) -> &mut dyn ValueContext;
  fn io(&mut self) -> Io;
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
  /// use laythe_core::memory::Allocator;
  ///
  /// let mut context = NoContext::default();
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
  pub fn manage<T: 'static + Manage>(&self, data: T) -> Gc<T> {
    self.context.gc().manage(data, self.context)
  }

  /// Request a string be managed by the context's garbage collector
  #[inline]
  pub fn manage_str<S: AsRef<str>>(&self, string: S) -> GcStr {
    self.context.gc().manage_str(string, self.context)
  }

  /// Tell the context's gc that the provided managed object may grow during this operation
  #[inline]
  pub fn grow<T: 'static + Manage, R, F: FnOnce(&mut T) -> R>(
    &self,
    managed: &mut T,
    action: F,
  ) -> R {
    self.context.gc().grow(managed, self.context, action)
  }

  /// Tell the context's gc that the provided managed object may shrink during this operation
  #[inline]
  pub fn shrink<T: 'static + Manage, R, F: FnOnce(&mut T) -> R>(
    &self,
    managed: &mut T,
    action: F,
  ) -> R {
    self.context.gc().shrink(managed, action)
  }

  /// Push a new root onto the gc
  #[inline]
  pub fn push_root<T: 'static + Trace>(&self, managed: T) {
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
  ///
  /// let mut context = NoContext::default();
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
  pub fn get_method(&mut self, this: Value, method_name: GcStr) -> Call {
    self.context.get_method(this, method_name)
  }
}

/// A set of functions related to calling laythe values
pub trait ValueContext {
  /// Execute a laythe value in the surround context
  fn call(&mut self, callable: Value, args: &[Value]) -> Call;

  /// Execute a method on a laythe object with a given method name
  fn call_method(&mut self, this: Value, method: Value, args: &[Value]) -> Call;

  /// Retrieve a method for this value with a given method name
  fn get_method(&mut self, this: Value, method_name: GcStr) -> Call;

  /// Retrieve the class for this value
  fn get_class(&mut self, this: Value) -> Value;
}

/// A set of functionality required by the hooks objects in order to operate
pub trait GcContext: TraceRoot {
  /// Get a reference to the context garbage collector
  fn gc(&self) -> RefMut<'_, Allocator>;
}

#[derive(Default)]
pub struct NoContext {
  /// A reference to a gc just to allocate
  pub gc: RefCell<Allocator>,
}

impl NoContext {
  /// Create a new instance of no context
  pub fn new(gc: Allocator) -> Self {
    Self {
      gc: RefCell::new(gc),
    }
  }

  pub fn done(self) -> Allocator {
    self.gc.replace(Allocator::default())
  }
}

impl TraceRoot for NoContext {
  fn trace(&self) {}

  fn trace_debug(&self, _stdout: &mut dyn Write) {}

  fn can_collect(&self) -> bool {
    false
  }
}

impl HookContext for NoContext {
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

impl GcContext for NoContext {
  fn gc(&self) -> RefMut<'_, Allocator> {
    self.gc.borrow_mut()
  }
}

impl ValueContext for NoContext {
  fn call(&mut self, _callable: Value, _args: &[Value]) -> Call {
    Call::Ok(VALUE_NIL)
  }

  fn call_method(&mut self, _this: Value, _method: Value, _args: &[Value]) -> Call {
    Call::Ok(VALUE_NIL)
  }

  fn get_method(&mut self, _this: Value, _method_name: GcStr) -> Call {
    Call::Ok(VALUE_NIL)
  }

  fn get_class(&mut self, _this: Value) -> Value {
    VALUE_NIL
  }
}
