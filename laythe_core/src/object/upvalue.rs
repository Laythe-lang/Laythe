use crate::{
  managed::{DebugHeap, DebugWrap, Manage, Trace},
  value::Value,
};
use std::{
  fmt,
  io::Write,
  mem,
  ptr::{self, NonNull},
};

#[derive(PartialEq, Clone, Debug)]
pub enum Upvalue {
  Open(NonNull<Value>),
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
  /// let value = val!(10.0);
  ///
  /// let mut upvalue = Upvalue::Open(NonNull::from(&value));
  /// upvalue.hoist();
  ///
  /// match upvalue {
  ///   Upvalue::Closed(store) => assert_eq!(store, val!(10.0)),
  ///   Upvalue::Open(_) => assert!(false),
  /// };
  /// ```
  pub fn hoist(&mut self) {
    match self {
      Upvalue::Open(stack_ptr) => {
        let value = *unsafe { stack_ptr.as_ref() };
        *self = Upvalue::Closed(value);
      }
      Upvalue::Closed(_) => panic!("Attempted to hoist already hoisted upvalue."),
    }
  }

  /// Is this upvalue currently open
  ///
  /// # Examples
  /// ```
  /// use laythe_core::value::{Value, VALUE_NIL};
  /// use laythe_core::object::Upvalue;
  /// use std::ptr::NonNull;
  ///
  /// let value = VALUE_NIL;
  ///
  /// let upvalue = Upvalue::Open(NonNull::from(&value));
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
  pub fn value(&self) -> Value {
    match self {
      Upvalue::Open(stack_ptr) => *unsafe { stack_ptr.as_ref() },
      Upvalue::Closed(store) => *store,
    }
  }

  #[inline]
  pub fn set_value(&self, value: Value) {
    let storage = match self {
      Upvalue::Open(stack_ptr) => *stack_ptr,
      Upvalue::Closed(store) => NonNull::from(store),
    };

    unsafe { ptr::write(storage.as_ptr(), value) }
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
    let depth = depth.saturating_sub(1);

    match self {
      Self::Open(v) => f.write_fmt(format_args!(
        "Upvalue::Open(*{:?})",
        &DebugWrap(unsafe { v.as_ref() }, depth)
      )),
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
