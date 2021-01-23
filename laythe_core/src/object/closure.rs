use super::{Fun, Upvalue};
use crate::value::Value;
use laythe_env::managed::{DebugHeap, DebugWrap, Gc, Manage, Trace};
use std::{fmt, io::Write, mem};

#[derive(PartialEq, Clone)]
pub struct Closure {
  fun: Gc<Fun>,
  upvalues: Box<[Gc<Upvalue>]>,
}

impl Closure {
  /// Create a new closure using a pointer to an underlying Fun
  ///
  /// # Example
  /// ```
  /// use laythe_core::object::{Closure, Class, Fun};
  /// use laythe_core::signature::Arity;
  /// use laythe_core::module::Module;
  /// use laythe_core::chunk::Chunk;
  /// use laythe_core::hooks::{NoContext, Hooks, HookContext};
  /// use std::path::PathBuf;
  ///
  /// let mut context = NoContext::default();
  /// let hooks = Hooks::new(&mut context);
  ///
  /// let module = hooks.manage(Module::new(
  ///   hooks.manage(Class::bare(hooks.manage_str("module"))),
  ///   PathBuf::from("self/module.ly"),
  /// ));
  /// let mut fun = Fun::new(hooks.manage_str("example"), module);
  /// let managed_fun = hooks.manage(fun);
  ///
  /// let closure = Closure::new(managed_fun, vec![].into_boxed_slice());
  /// assert_eq!(&*closure.fun().name, "example");
  /// ```
  pub fn new(fun: Gc<Fun>, upvalues: Box<[Gc<Upvalue>]>) -> Self {
    Closure { upvalues, fun }
  }

  pub fn without_upvalues(fun: Gc<Fun>) -> Self {
    assert!(fun.upvalue_count == 0);
    Closure {
      upvalues: vec![].into_boxed_slice(),
      fun,
    }
  }

  #[inline]
  pub fn fun(&self) -> Gc<Fun> {
    self.fun
  }

  #[inline]
  pub fn upvalues(&self) -> usize {
    self.upvalues.len()
  }

  #[inline]
  pub fn get_upvalue(&self, index: usize) -> Gc<Upvalue> {
    self.upvalues[index]
  }

  #[inline]
  pub fn get_value(&self, index: usize) -> Value {
    self.upvalues[index].value()
  }

  #[inline]
  pub fn set_value(&mut self, index: usize, value: Value) {
    self.upvalues[index].set_value(value);
  }
}

impl fmt::Debug for Closure {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.fmt_heap(f, 1)
  }
}

impl Trace for Closure {
  fn trace(&self) {
    self.upvalues.iter().for_each(|upvalue| {
      upvalue.trace();
    });

    self.fun.trace();
  }

  fn trace_debug(&self, stdio: &mut dyn Write) {
    self.upvalues.iter().for_each(|upvalue| {
      upvalue.trace_debug(stdio);
    });

    self.fun.trace_debug(stdio);
  }
}

impl DebugHeap for Closure {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    let depth = depth.saturating_sub(1);

    f.debug_struct("Closure")
      .field("fun", &DebugWrap(&self.fun, depth))
      .field("upvalues", &DebugWrap(&&*self.upvalues, depth))
      .finish()
  }
}

impl Manage for Closure {
  fn size(&self) -> usize {
    mem::size_of::<Self>() + mem::size_of::<Value>() * self.upvalues.len()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}
