use fmt::Display;

use crate::{
  managed::{DebugHeap, DebugWrap, Manage, Object, Trace},
  value::Value,
};
use std::{fmt, io::Write, mem, ptr};

use super::ObjectKind;

#[derive(PartialEq, Clone, Debug)]
pub enum Upvalue {
  Open(usize),
  Closed(Value),
}

impl Upvalue {
  /// Close over the upvalue by moving it onto the stack to the heap
  ///
  /// # Examples
  /// ```
  /// use laythe_core::val;
  /// use laythe_core::value::Value;
  /// use laythe_core::object::Upvalue;
  /// use std::rc::Rc;
  /// use std::ptr::NonNull;
  ///
  /// let stack = &[val!(10.0)];
  ///
  /// let mut upvalue = Upvalue::Open(0);
  /// upvalue.hoist(stack);
  ///
  /// match upvalue {
  ///   Upvalue::Closed(store) => assert_eq!(store, val!(10.0)),
  ///   Upvalue::Open(_) => assert!(false),
  /// };
  /// ```
  #[inline]
  pub fn hoist(&mut self, stack: &[Value]) {
    match self {
      Upvalue::Open(index) => {
        *self = Upvalue::Closed(stack[*index]);
      },
      Upvalue::Closed(_) => panic!("Attempted to hoist already hoisted upvalue."),
    }
  }

  /// Is this upvalue currently open
  ///
  /// # Examples
  /// ```
  /// use laythe_core::object::Upvalue;
  /// use std::ptr::NonNull;
  ///
  /// let upvalue = Upvalue::Open(0);
  /// assert_eq!(upvalue.is_open(), true);
  /// ```
  #[inline]
  pub fn is_open(&self) -> bool {
    match self {
      Upvalue::Open(_) => true,
      Upvalue::Closed(_) => false,
    }
  }

  #[inline]
  pub fn value(&self, stack: &[Value]) -> Value {
    match self {
      Upvalue::Open(index) => stack[*index],
      Upvalue::Closed(store) => *store,
    }
  }

  #[inline]
  pub fn set_value(&mut self, stack: &mut [Value], value: Value) {
    let storage = match self {
      Upvalue::Open(index) => &mut stack[*index],
      Upvalue::Closed(store) => store,
    };

    unsafe { ptr::write(storage, value) }
  }
}

impl Display for Upvalue {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    // TODO which we indicate this is an upvalue somehow
    match self {
      Upvalue::Open(index) => write!(f, "{}", index),
      Upvalue::Closed(store) => write!(f, "{}", store),
    }
  }
}

impl Trace for Upvalue {
  fn trace(&self) {
    if let Upvalue::Closed(upvalue) = self {
      upvalue.trace();
    }
  }

  fn trace_debug(&self, stdio: &mut dyn Write) {
    if let Upvalue::Closed(upvalue) = self {
      upvalue.trace_debug(stdio);
    }
  }
}

impl DebugHeap for Upvalue {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    match self {
      Self::Open(index) => f.write_fmt(format_args!("Upvalue::Open(*{:?})", index)),
      Self::Closed(v) => f.write_fmt(format_args!("Upvalue::Closed({:?})", &DebugWrap(v, depth))),
    }
  }
}

impl Manage for Upvalue {
  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

unsafe impl Send for Upvalue {}

impl Object for Upvalue {
  fn kind(&self) -> ObjectKind {
    ObjectKind::Upvalue
  }
}
