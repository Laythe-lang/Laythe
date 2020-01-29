use crate::value::Value;
use std::fmt;
use std::rc::Rc;
use std::time::SystemTime;

#[derive(Clone)]
pub struct NativeFun<'a> {
  pub meta: Box<NativeMeta>,
  pub fun: Rc<dyn Fn(&[Value<'a>]) -> NativeResult<'a> + 'a>,
}

#[derive(Clone)]
pub struct NativeMeta {
  pub name: String,
  pub arity: u8,
}

impl<'a> PartialEq for NativeFun<'a> {
  fn eq(&self, rhs: &NativeFun<'a>) -> bool {
    Rc::ptr_eq(&self.fun, &rhs.fun)
  }
}

impl<'a> fmt::Debug for NativeFun<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "NativeFun {{ arity: {}, name: {} }}",
      self.meta.arity, self.meta.name
    )
  }
}

impl<'a> NativeFun<'a> {
  pub fn new(fun: Rc<dyn Fn(&[Value<'a>]) -> NativeResult<'a> + 'a>,  name: String, arity: u8) -> Self {
    NativeFun {
      meta: Box::new(NativeMeta {
        name,
        arity
      }),
      fun
    }
  }
}

pub enum NativeResult<'a> {
  Success(Value<'a>),
  RuntimeError(String),
}

pub fn create_natives<'a>() -> Vec<NativeFun<'a>> {
  let mut natives = Vec::new();

  natives.push(NativeFun::new(
    Rc::new(create_clock()),
    "clock".to_string(),
    0,
  ));

  natives.push(NativeFun::new(
    Rc::new(assert),
    "assert".to_string(),
    1,
  ));

  natives.push(NativeFun::new(
    Rc::new(assert_eq),
    "assertEq".to_string(),
    2,
  ));

  natives.push(NativeFun::new(
    Rc::new(assert_ne),
    "assertNe".to_string(),
    2,
  ));

  natives
}

fn create_clock<'a>() -> Box<dyn Fn(&[Value<'a>]) -> NativeResult<'a>> {
  let now = SystemTime::now();

  Box::new(move |_: &[Value<'a>]| match now.elapsed() {
    Ok(elasped) => NativeResult::Success(Value::Number((elasped.as_micros() as f64) / 1000000.0)),
    Err(e) => NativeResult::RuntimeError(format!("clock failed {}", e)),
  })
}

fn assert<'a>(args: &[Value<'a>]) -> NativeResult<'a> {
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

fn assert_eq<'a>(args: &[Value<'a>]) -> NativeResult<'a> {
  if args[0] == args[1] {
    return NativeResult::Success(Value::Nil);
  }

  NativeResult::RuntimeError(format!("{} and {} where not equal", args[0], args[1]))
}

fn assert_ne<'a>(args: &[Value<'a>]) -> NativeResult<'a> {
  if args[0] != args[1] {
    return NativeResult::Success(Value::Nil);
  }

  NativeResult::RuntimeError(format!("{} and {} where equal", args[0], args[1]))
}
