use crate::value::Value;
use std::fmt;
use std::ptr;
use std::rc::Rc;
use std::time::SystemTime;

pub enum NativeResult<'a> {
  /// The result of the native function call was a success with this value
  Success(Value<'a>),

  /// The result of the native function call was an error with this runtime
  /// message
  RuntimeError(String),
}

pub trait NativeFun<'a> {
  /// Meta data to this native function
  fn meta(&self) -> &NativeMeta;

  /// Call the native functions
  fn call(&self, values: &[Value<'a>]) -> NativeResult<'a>;

  // /// Check if this native function is equal to another
  fn eq(&self, rhs: &dyn NativeFun<'a>) -> bool;
}

impl<'a> PartialEq<dyn NativeFun<'a>> for dyn NativeFun<'a> {
  fn eq(&self, rhs: &dyn NativeFun<'a>) -> bool {
    self.eq(rhs)
  }
}

impl<'a> fmt::Debug for dyn NativeFun<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let meta = self.meta();
    write!(
      f,
      "NativeFun {{ name: {}, arity: {} }}",
      meta.name, meta.arity
    )
  }
}

#[derive(Clone, Debug)]
pub struct NativeMeta {
  pub name: String,
  pub arity: u8,
}

pub fn create_natives<'a>() -> Vec<Rc<dyn NativeFun<'a>>> {
  let mut natives: Vec<Rc<dyn NativeFun<'a>>> = Vec::new();

  natives.push(Rc::new(NativeClock::new()));
  natives.push(Rc::new(NativeAssert::new()));
  natives.push(Rc::new(NativeAssertEq::new()));
  natives.push(Rc::new(NativeAssertNe::new()));

  natives
}

fn native_eq<'a>(lhs: &dyn NativeFun<'a>, rhs: &dyn NativeFun<'a>) -> bool {
  ptr::eq(lhs.meta(), rhs.meta())
}

#[derive(Clone, Debug)]
struct NativeClock {
  meta: Box<NativeMeta>,
  start: SystemTime,
}

impl NativeClock {
  pub fn new() -> Self {
    Self {
      meta: Box::new(NativeMeta {
        name: "clock".to_string(),
        arity: 0,
      }),
      start: SystemTime::now(),
    }
  }
}

impl<'a> NativeFun<'a> for NativeClock {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn eq(&self, rhs: &dyn NativeFun<'a>) -> bool {
    native_eq(self, rhs)
  }

  fn call(&self, _: &[Value<'a>]) -> NativeResult<'a> {
    match self.start.elapsed() {
      Ok(elasped) => NativeResult::Success(Value::Number((elasped.as_micros() as f64) / 1000000.0)),
      Err(e) => NativeResult::RuntimeError(format!("clock failed {}", e)),
    }
  }
}

#[derive(Clone, Debug)]
struct NativeAssert {
  meta: Box<NativeMeta>,
  start: SystemTime,
}

impl NativeAssert {
  pub fn new() -> Self {
    Self {
      meta: Box::new(NativeMeta {
        name: "assert".to_string(),
        arity: 1,
      }),
      start: SystemTime::now(),
    }
  }
}

impl<'a> NativeFun<'a> for NativeAssert {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn eq(&self, rhs: &dyn NativeFun<'a>) -> bool {
    native_eq(self, rhs)
  }

  fn call(&self, args: &[Value<'a>]) -> NativeResult<'a> {
    match args[0] {
      Value::Bool(b) => {
        if b {
          return NativeResult::Success(Value::Nil);
        }
        NativeResult::RuntimeError(format!("assert expected true received false"))
      }
      _ => NativeResult::RuntimeError(format!("assert expected a boolean value")),
    }
  }
}

#[derive(Clone, Debug)]
struct NativeAssertEq {
  meta: Box<NativeMeta>,
  start: SystemTime,
}

impl NativeAssertEq {
  pub fn new() -> Self {
    Self {
      meta: Box::new(NativeMeta {
        name: "assertEq".to_string(),
        arity: 2,
      }),
      start: SystemTime::now(),
    }
  }
}

impl<'a> NativeFun<'a> for NativeAssertEq {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn eq(&self, rhs: &dyn NativeFun<'a>) -> bool {
    native_eq(self, rhs)
  }

  fn call(&self, args: &[Value<'a>]) -> NativeResult<'a> {
    if args[0] == args[1] {
      return NativeResult::Success(Value::Nil);
    }

    NativeResult::RuntimeError(format!("{} and {} where not equal", args[0], args[1]))
  }
}

#[derive(Clone, Debug)]
struct NativeAssertNe {
  meta: Box<NativeMeta>,
  start: SystemTime,
}

impl NativeAssertNe {
  pub fn new() -> Self {
    Self {
      meta: Box::new(NativeMeta {
        name: "assertNe".to_string(),
        arity: 2,
      }),
      start: SystemTime::now(),
    }
  }
}

impl<'a> NativeFun<'a> for NativeAssertNe {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn eq(&self, rhs: &dyn NativeFun<'a>) -> bool {
    native_eq(self, rhs)
  }

  fn call(&self, args: &[Value<'a>]) -> NativeResult<'a> {
    if args[0] != args[1] {
      return NativeResult::Success(Value::Nil);
    }

    NativeResult::RuntimeError(format!("{} and {} where equal", args[0], args[1]))
  }
}
