use crate::{
  native, native_with_error,
  support::{export_and_insert, load_class_from_module},
  StdResult,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  managed::Trace,
  managed::{DebugHeap, Gc, GcObj},
  module::Module,
  object::{Enumerate, Enumerator, LyNative, Native, NativeMetaBuilder, ObjectKind},
  signature::{Arity, ParameterBuilder, ParameterKind},
  val,
  value::Value,
  Call, LyError,
};
use std::io::Write;

use super::{
  class_inheritance,
  error::{FORMAT_ERROR_NAME, VALUE_ERROR_NAME},
};

pub const NUMBER_CLASS_NAME: &str = "Number";

const NUMBER_STR: NativeMetaBuilder = NativeMetaBuilder::method("str", Arity::Fixed(0));
const NUMBER_TIMES: NativeMetaBuilder = NativeMetaBuilder::method("times", Arity::Fixed(0));
const NUMBER_UNTIL: NativeMetaBuilder = NativeMetaBuilder::method("until", Arity::Default(1, 2))
  .with_params(&[
    ParameterBuilder::new("upper", ParameterKind::Number),
    ParameterBuilder::new("stride", ParameterKind::Number),
  ])
  .with_stack();

const NUMBER_FLOOR: NativeMetaBuilder = NativeMetaBuilder::method("floor", Arity::Fixed(0));
const NUMBER_CEIL: NativeMetaBuilder = NativeMetaBuilder::method("ceil", Arity::Fixed(0));
const NUMBER_ROUND: NativeMetaBuilder = NativeMetaBuilder::method("round", Arity::Fixed(0));

const NUMBER_CMP: NativeMetaBuilder =
  NativeMetaBuilder::fun("cmp", Arity::Fixed(2)).with_params(&[
    ParameterBuilder::new("a", ParameterKind::Number),
    ParameterBuilder::new("b", ParameterKind::Number),
  ]);

const NUMBER_PARSE: NativeMetaBuilder = NativeMetaBuilder::fun("parse", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("str", ParameterKind::String)])
  .with_stack();

pub fn declare_number_class(hooks: &GcHooks, module: Gc<Module>) -> StdResult<()> {
  let class = class_inheritance(hooks, module, NUMBER_CLASS_NAME)?;
  export_and_insert(hooks, module, class.name(), val!(class))
}

pub fn define_number_class(hooks: &GcHooks, module: Gc<Module>) -> StdResult<()> {
  let mut class = load_class_from_module(hooks, module, NUMBER_CLASS_NAME)?;
  let format_error = val!(load_class_from_module(hooks, module, FORMAT_ERROR_NAME)?);
  let value_error = val!(load_class_from_module(hooks, module, VALUE_ERROR_NAME)?);

  class.add_method(
    hooks.manage_str(NUMBER_STR.name),
    val!(NumberStr::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(NUMBER_FLOOR.name),
    val!(NumberFloor::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(NUMBER_CEIL.name),
    val!(NumberCeil::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(NUMBER_ROUND.name),
    val!(NumberRound::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(NUMBER_TIMES.name),
    val!(NumberTimes::native(hooks, value_error)),
  );

  class.add_method(
    hooks.manage_str(NUMBER_UNTIL.name),
    val!(NumberUntil::native(hooks, value_error)),
  );

  class.meta_class().expect("Meta class not set.").add_method(
    hooks.manage_str(NUMBER_PARSE.name),
    val!(NumberParse::native(hooks, format_error)),
  );

  class.meta_class().expect("Meta class not set.").add_method(
    hooks.manage_str(NUMBER_CMP.name),
    val!(NumberCmp::native(hooks)),
  );

  Ok(())
}

native!(NumberStr, NUMBER_STR);

impl LyNative for NumberStr {
  fn call(&self, hook: &mut Hooks, args: &[Value]) -> Call {
    Call::Ok(val!(hook.manage_str(args[0].to_num().to_string())))
  }
}

native!(NumberFloor, NUMBER_FLOOR);

impl LyNative for NumberFloor {
  fn call(&self, _hook: &mut Hooks, args: &[Value]) -> Call {
    Call::Ok(val!(args[0].to_num().floor()))
  }
}

native!(NumberCeil, NUMBER_CEIL);

impl LyNative for NumberCeil {
  fn call(&self, _hook: &mut Hooks, args: &[Value]) -> Call {
    Call::Ok(val!(args[0].to_num().ceil()))
  }
}

native!(NumberRound, NUMBER_ROUND);

impl LyNative for NumberRound {
  fn call(&self, _hook: &mut Hooks, args: &[Value]) -> Call {
    Call::Ok(val!(args[0].to_num().round()))
  }
}

native_with_error!(NumberParse, NUMBER_PARSE);

impl LyNative for NumberParse {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let str = args[0].to_obj().to_str();
    match str.parse::<f64>() {
      Ok(num) => Call::Ok(val!(num)),
      Err(_) => self.call_error(hooks, format!("Unable to parse number from {str}")),
    }
  }
}

native!(NumberCmp, NUMBER_CMP);

impl LyNative for NumberCmp {
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
    Call::Ok(val!(args[0].to_num() - args[1].to_num()))
  }
}

native_with_error!(NumberTimes, NUMBER_TIMES);

impl LyNative for NumberTimes {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let max = args[0].to_num();
    if max < 0.0 || max.fract() != 0.0 {
      return self.call_error(hooks, "times requires a positive integer.");
    }

    let inner_iter: Box<dyn Enumerate> = Box::new(TimesIterator::new(max));
    let iter = Enumerator::new(inner_iter);
    let iter = hooks.manage_obj(iter);

    Call::Ok(val!(iter))
  }
}

#[derive(Debug)]
struct TimesIterator {
  current: f64,
  max: f64,
}

impl TimesIterator {
  fn new(max: f64) -> Self {
    Self {
      current: -1.0,
      max: max - 1.0,
    }
  }
}

impl Enumerate for TimesIterator {
  fn name(&self) -> &str {
    "Times"
  }

  fn current(&self) -> Value {
    val!(self.current)
  }

  fn next(&mut self, _hooks: &mut Hooks) -> Call {
    if self.current < self.max {
      self.current += 1.0;
      Call::Ok(val!(true))
    } else {
      Call::Ok(val!(false))
    }
  }

  fn size_hint(&self) -> Option<usize> {
    Some((self.max + 1.0) as usize)
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

impl Trace for TimesIterator {}

impl DebugHeap for TimesIterator {
  fn fmt_heap(&self, f: &mut std::fmt::Formatter, _: usize) -> std::fmt::Result {
    f.write_fmt(format_args!("{self:?}"))
  }
}

native_with_error!(NumberUntil, NUMBER_UNTIL);

impl LyNative for NumberUntil {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let start = args[0].to_num();
    let end = args[1].to_num();
    let stride = if args.len() > 2 {
      args[2].to_num()
    } else {
      1.0
    };

    if stride <= 0.0 {
      return self.call_error(hooks, "until requires a positive stride.");
    }

    let inner_iter: Box<dyn Enumerate> = Box::new(UntilIterator::new(start, end, stride));
    let iter = Enumerator::new(inner_iter);
    let iter = hooks.manage_obj(iter);

    Call::Ok(val!(iter))
  }
}

#[derive(Debug)]
struct UntilIterator {
  current: f64,
  max: f64,
  stride: f64,
}

impl UntilIterator {
  fn new(min: f64, max: f64, stride: f64) -> Self {
    Self {
      current: min - stride,
      max: max - stride,
      stride,
    }
  }
}

impl Enumerate for UntilIterator {
  fn name(&self) -> &str {
    "Until"
  }

  fn current(&self) -> Value {
    val!(self.current)
  }

  fn next(&mut self, _hooks: &mut Hooks) -> Call {
    if self.current < self.max {
      self.current += self.stride;
      Call::Ok(val!(true))
    } else {
      Call::Ok(val!(false))
    }
  }

  fn size_hint(&self) -> Option<usize> {
    None
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

impl Trace for UntilIterator {}

impl DebugHeap for UntilIterator {
  fn fmt_heap(&self, f: &mut std::fmt::Formatter, _: usize) -> std::fmt::Result {
    f.write_fmt(format_args!("{self:?}"))
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod str {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let number_str = NumberStr::native(&hooks.as_gc());

      let result = number_str.call(&mut hooks, &[val!(10.0)]);
      match result {
        Call::Ok(r) => assert_eq!(*r.to_obj().to_str(), "10".to_string()),
        _ => assert!(false),
      }
    }
  }

  mod times {
    use super::*;
    use crate::support::{test_error_class, MockedContext};

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(5.0)]);
      let mut hooks = Hooks::new(&mut context);

      let error = val!(test_error_class(&hooks.as_gc()));
      let number_times = NumberTimes::native(&hooks.as_gc(), error);

      let result = number_times.call(&mut hooks, &[val!(3.0)]);
      match result {
        Call::Ok(r) => {
          let mut number_times = r.to_obj().to_enumerator();
          assert_eq!(number_times.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(number_times.current(), val!(0.0));

          assert_eq!(number_times.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(number_times.current(), val!(1.0));

          assert_eq!(number_times.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(number_times.current(), val!(2.0));

          assert_eq!(number_times.next(&mut hooks).unwrap(), val!(false));
        },
        _ => assert!(false),
      }
    }
  }

  mod cmp {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let number_cmp = NumberCmp::native(&hooks.as_gc());

      let result = number_cmp
        .call(&mut hooks, &[val!(5.0), val!(3.0)])
        .unwrap();

      assert_eq!(result, val!(2.0));
    }
  }

  mod floor {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let number_floor = NumberFloor::native(&hooks.as_gc());

      let result = number_floor.call(&mut hooks, &[val!(10.5)]).unwrap();
      assert_eq!(result.to_num(), 10.0);
    }
  }

  mod ceil {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let number_ceil = NumberCeil::native(&hooks.as_gc());

      let result = number_ceil.call(&mut hooks, &[val!(10.5)]).unwrap();
      assert_eq!(result.to_num(), 11.0);
    }
  }

  mod round {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let number_round = NumberRound::native(&hooks.as_gc());

      let result = number_round.call(&mut hooks, &[val!(10.3)]).unwrap();
      assert_eq!(result.to_num(), 10.0);
    }
  }

  mod parse {
    use super::*;
    use crate::support::{test_error_class, MockedContext};

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let error = val!(test_error_class(&hooks.as_gc()));

      let number_parse = NumberParse::native(&hooks.as_gc(), error);

      let args = val!(hooks.manage_str("1"));
      let result = number_parse.call(&mut hooks, &[args]).unwrap();

      assert_eq!(result, val!(1.0));
    }
  }

  mod until {
    use super::*;
    use crate::support::{test_error_class, MockedContext};

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(5.0)]);
      let mut hooks = Hooks::new(&mut context);
      let error = val!(test_error_class(&hooks.as_gc()));

      let number_until = NumberUntil::native(&hooks.as_gc(), error);

      let result = number_until.call(&mut hooks, &[val!(2.0), val!(6.0), val!(2.0)]);
      match result {
        Call::Ok(r) => {
          let mut number_until = r.to_obj().to_enumerator();
          assert_eq!(number_until.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(number_until.current(), val!(2.0));

          assert_eq!(number_until.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(number_until.current(), val!(4.0));

          assert_eq!(number_until.next(&mut hooks).unwrap(), val!(false));
        },
        _ => assert!(false),
      }
    }
  }
}
