use crate::value::Value;
use std::fmt;
use std::rc::Rc;
use std::time::SystemTime;

#[derive(Clone)]
pub struct NativeFun<'a> {
  pub arity: u8,
  pub name: String,
  pub fun: Rc<dyn Fn(&[Value<'a>]) -> NativeResult<'a> + 'a>,
}

impl<'a> PartialEq for NativeFun<'a> {
  fn eq(&self, rhs: &NativeFun<'a>) -> bool {
    if self.arity != rhs.arity {
      return false;
    }

    if self.name != rhs.name {
      return false;
    }

    Rc::ptr_eq(&self.fun, &rhs.fun)
  }
}

impl<'a> fmt::Debug for NativeFun<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "NativeFun {{ arity: {}, name: {} }}",
      self.arity, self.name
    )
  }
}

pub enum NativeResult<'a> {
  Success(Value<'a>),
  RuntimeError(String),
}

pub fn create_natives<'a>() -> Vec<NativeFun<'a>> {
  let mut natives = Vec::new();

  natives.push(NativeFun {
    name: "clock".to_string(),
    arity: 0,
    fun: Rc::new(create_clock()),
  });

  natives.push(NativeFun {
    name: "assert".to_string(),
    arity: 1,
    fun: Rc::new(assert),
  });

  natives.push(NativeFun {
    name: "assertEq".to_string(),
    arity: 2,
    fun: Rc::new(assert_eq),
  });

  natives.push(NativeFun {
    name: "assertNe".to_string(),
    arity: 2,
    fun: Rc::new(assert_ne),
  });

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
