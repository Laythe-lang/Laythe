use super::{Fun, ObjectKind};
use crate::{
  captures::Captures,
  managed::{DebugHeap, DebugWrap, GcObj, Object, Trace},
};
use std::{fmt, io::Write};

#[derive(PartialEq, Eq, Clone)]
pub struct Closure {
  fun: GcObj<Fun>,
  captures: Captures,
}

impl Closure {
  /// Create a new closure using a pointer to an underlying Fun
  ///
  /// # Example
  /// ```
  /// use laythe_core::object::{Closure, Class, FunBuilder};
  /// use laythe_core::signature::Arity;
  /// use laythe_core::module::Module;
  /// use laythe_core::captures::Captures;
  /// use laythe_core::hooks::{NoContext, GcHooks};
  /// use laythe_core::chunk::Chunk;
  /// use std::path::PathBuf;
  ///
  /// let mut context = NoContext::default();
  /// let hooks = GcHooks::new(&mut context);
  ///
  /// let module = hooks.manage(Module::new(
  ///   hooks.manage_obj(Class::bare(hooks.manage_str("module"))),
  ///   0,
  /// ));
  /// let mut builder = FunBuilder::new(hooks.manage_str("example"), module, Arity::default());
  /// let chunk = Chunk::stub(&hooks);
  /// let managed_fun = hooks.manage_obj(builder.build(chunk));
  ///
  /// let captures = Captures::new(&hooks, &[]);
  /// let closure = Closure::new(managed_fun, captures);
  /// assert_eq!(&*closure.fun().name(), "example");
  /// ```
  pub fn new(fun: GcObj<Fun>, captures: Captures) -> Self {
    Closure { fun, captures }
  }

  #[inline]
  pub fn fun(&self) -> GcObj<Fun> {
    self.fun
  }

  #[inline]
  pub fn captures(&self) -> Captures {
    self.captures
  }
}

impl fmt::Display for Closure {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.fun())
  }
}

impl fmt::Debug for Closure {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.fmt_heap(f, 2)
  }
}

impl Trace for Closure {
  fn trace(&self) {
    self.fun.trace();
    self.captures.trace();
  }

  fn trace_debug(&self, log: &mut dyn Write) {
    self.fun.trace_debug(log);
    self.captures.trace_debug(log);
  }
}

impl DebugHeap for Closure {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    f.debug_struct("Closure")
      .field("fun", &DebugWrap(&self.fun, depth))
      .field("captures", &DebugWrap(&self.captures, depth))
      .finish()
  }
}

impl Object for Closure {
  fn kind(&self) -> ObjectKind {
    ObjectKind::Closure
  }
}
