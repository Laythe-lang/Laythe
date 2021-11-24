use super::{Fun, LyBox, ObjectKind};
use crate::{
  managed::{DebugHeap, DebugWrap, GcObj, Manage, Object, Trace},
  value::Value,
};
use std::{fmt, io::Write, mem};

#[derive(PartialEq, Clone)]
pub struct Closure {
  fun: GcObj<Fun>,
  captures: Box<[GcObj<LyBox>]>,
}

impl Closure {
  /// Create a new closure using a pointer to an underlying Fun
  ///
  /// # Example
  /// ```
  /// use laythe_core::object::{Closure, Class, FunBuilder};
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
  ///   hooks.manage_obj(Class::bare(hooks.manage_str("module"))),
  ///   PathBuf::from("self/module.ly"),
  ///   0,
  /// ));
  /// let mut builder = FunBuilder::new(hooks.manage_str("example"), module, Arity::default());
  /// let managed_fun = hooks.manage_obj(builder.build());
  ///
  /// let closure = Closure::without_captures(managed_fun);
  /// assert_eq!(&*closure.fun().name(), "example");
  /// ```
  pub fn new(fun: GcObj<Fun>, captures: Box<[GcObj<LyBox>]>) -> Self {
    Closure { fun, captures }
  }

  pub fn without_captures(fun: GcObj<Fun>) -> Self {
    Closure {
      captures: vec![].into_boxed_slice(),
      fun,
    }
  }

  #[inline]
  pub fn fun(&self) -> GcObj<Fun> {
    self.fun
  }

  #[inline]
  pub fn captures(&self) -> usize {
    self.captures.len()
  }

  #[inline]
  pub fn get_capture(&self, index: usize) -> GcObj<LyBox> {
    self.captures[index]
  }

  #[inline]
  pub fn get_capture_value(&self, index: usize) -> Value {
    self.captures[index].value
  }

  #[inline]
  pub fn set_capture_value(&mut self, index: usize, value: Value) {
    self.captures[index].value = value;
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
    self.captures.iter().for_each(|capture| {
      capture.trace();
    });

    self.fun.trace();
  }

  fn trace_debug(&self, stdio: &mut dyn Write) {
    self.captures.iter().for_each(|capture| {
      capture.trace_debug(stdio);
    });

    self.fun.trace_debug(stdio);
  }
}

impl DebugHeap for Closure {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    f.debug_struct("Closure")
      .field("fun", &DebugWrap(&self.fun, depth))
      .field("captures", &DebugWrap(&&*self.captures, depth))
      .finish()
  }
}

impl Manage for Closure {
  fn size(&self) -> usize {
    mem::size_of::<Self>() + mem::size_of::<Value>() * self.captures.len()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

impl Object for Closure {
  fn kind(&self) -> ObjectKind {
    ObjectKind::Closure
  }
}
