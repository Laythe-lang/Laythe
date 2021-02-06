use crate::{
  native, native_with_error,
  support::{export_and_insert, load_class_from_module, to_dyn_native},
  StdResult,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  managed::Trace,
  module::Module,
  object::{LyIter, LyIterator, MetaData, Native, NativeMeta, NativeMetaBuilder},
  signature::{Arity, ParameterBuilder, ParameterKind},
  val,
  value::Value,
  Call,
};
use std::io::Write;
use std::mem;

use super::{
  class_inheritance,
  error::{FORMAT_CLASS_NAME, VALUE_ERROR_NAME},
};

pub const NUMBER_CLASS_NAME: &str = "Number";

const NUMBER_STR: NativeMetaBuilder = NativeMetaBuilder::method("str", Arity::Fixed(0));
const NUMBER_TIMES: NativeMetaBuilder = NativeMetaBuilder::method("times", Arity::Fixed(0));
const NUMBER_UNTIL: NativeMetaBuilder = NativeMetaBuilder::method("until", Arity::Default(1, 2))
  .with_params(&[
    ParameterBuilder::new("upper", ParameterKind::Number),
    ParameterBuilder::new("stride", ParameterKind::Number),
  ]);

const NUMBER_FLOOR: NativeMetaBuilder = NativeMetaBuilder::method("floor", Arity::Fixed(0));
const NUMBER_CEIL: NativeMetaBuilder = NativeMetaBuilder::method("ceil", Arity::Fixed(0));
const NUMBER_ROUND: NativeMetaBuilder = NativeMetaBuilder::method("round", Arity::Fixed(0));

const NUMBER_CMP: NativeMetaBuilder =
  NativeMetaBuilder::fun("cmp", Arity::Fixed(2)).with_params(&[
    ParameterBuilder::new("a", ParameterKind::Number),
    ParameterBuilder::new("b", ParameterKind::Number),
  ]);

const NUMBER_PARSE: NativeMetaBuilder = NativeMetaBuilder::fun("parse", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("str", ParameterKind::String)]);

pub fn declare_number_class(hooks: &GcHooks, module: &mut Module) -> StdResult<()> {
  let class = class_inheritance(hooks, module, NUMBER_CLASS_NAME)?;
  export_and_insert(hooks, module, class.name(), val!(class))
}

pub fn define_number_class(hooks: &GcHooks, module: &Module) -> StdResult<()> {
  let mut class = load_class_from_module(hooks, module, NUMBER_CLASS_NAME)?;
  let format_error = val!(load_class_from_module(hooks, module, FORMAT_CLASS_NAME)?);
  let value_error = val!(load_class_from_module(hooks, module, VALUE_ERROR_NAME)?);

  class.add_method(
    hooks,
    hooks.manage_str(NUMBER_STR.name),
    val!(to_dyn_native(hooks, NumberStr::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(NUMBER_FLOOR.name),
    val!(to_dyn_native(hooks, NumberFloor::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(NUMBER_CEIL.name),
    val!(to_dyn_native(hooks, NumberCeil::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(NUMBER_ROUND.name),
    val!(to_dyn_native(hooks, NumberRound::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(NUMBER_TIMES.name),
    val!(to_dyn_native(hooks, NumberTimes::new(hooks, value_error))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(NUMBER_UNTIL.name),
    val!(to_dyn_native(hooks, NumberUntil::new(hooks, value_error))),
  );

  class.meta_class().expect("Meta class not set.").add_method(
    hooks,
    hooks.manage_str(NUMBER_PARSE.name),
    val!(to_dyn_native(hooks, NumberParse::new(hooks, format_error))),
  );

  class.meta_class().expect("Meta class not set.").add_method(
    hooks,
    hooks.manage_str(NUMBER_CMP.name),
    val!(to_dyn_native(hooks, NumberCmp::from(hooks))),
  );

  Ok(())
}

native!(NumberStr, NUMBER_STR);

impl Native for NumberStr {
  fn call(&self, hook: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    Call::Ok(val!(hook.manage_str(this.unwrap().to_num().to_string())))
  }
}

native!(NumberFloor, NUMBER_FLOOR);

impl Native for NumberFloor {
  fn call(&self, _hook: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    Call::Ok(val!(this.unwrap().to_num().floor()))
  }
}

native!(NumberCeil, NUMBER_CEIL);

impl Native for NumberCeil {
  fn call(&self, _hook: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    Call::Ok(val!(this.unwrap().to_num().ceil()))
  }
}

native!(NumberRound, NUMBER_ROUND);

impl Native for NumberRound {
  fn call(&self, _hook: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    Call::Ok(val!(this.unwrap().to_num().round()))
  }
}

native_with_error!(NumberParse, NUMBER_PARSE);

impl Native for NumberParse {
  fn call(&self, hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> Call {
    let str = args[0].to_str();
    match str.parse::<f64>() {
      Ok(num) => Call::Ok(val!(num)),
      Err(_) => self.call_error(hooks, format!("Unable to parse number from {}", str)),
    }
  }
}

native!(NumberCmp, NUMBER_CMP);

impl Native for NumberCmp {
  fn call(&self, _hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> Call {
    Call::Ok(val!(args[0].to_num() - args[1].to_num()))
  }
}

native_with_error!(NumberTimes, NUMBER_TIMES);

impl Native for NumberTimes {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    let max = this.unwrap().to_num();
    if max < 0.0 || max.fract() != 0.0 {
      return self.call_error(hooks, "times requires a positive integer.");
    }

    let inner_iter: Box<dyn LyIter> = Box::new(TimesIterator::new(max));
    let iter = LyIterator::new(inner_iter);
    let iter = hooks.manage(iter);

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

impl LyIter for TimesIterator {
  fn name(&self) -> &str {
    "Times Iterator"
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

  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }
}

impl Trace for TimesIterator {}

native_with_error!(NumberUntil, NUMBER_UNTIL);

impl Native for NumberUntil {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    let start = this.unwrap().to_num();
    let end = _args[0].to_num();
    let stride = if _args.len() > 1 {
      _args[1].to_num()
    } else {
      1.0
    };

    if stride <= 0.0 {
      return self.call_error(hooks, "until requires a positive stride.");
    }

    let inner_iter: Box<dyn LyIter> = Box::new(UntilIterator::new(start, end, stride));
    let iter = LyIterator::new(inner_iter);
    let iter = hooks.manage(iter);

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

impl LyIter for UntilIterator {
  fn name(&self) -> &str {
    "Times Iterator"
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

  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }
}

impl Trace for UntilIterator {}

#[cfg(test)]
mod test {
  use super::*;

  mod str {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = Hooks::new(&mut context);

      let number_str = NumberStr::from(&hooks);

      assert_eq!(number_str.meta().name, "str");
      assert_eq!(number_str.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let number_str = NumberStr::from(&hooks);

      let result = number_str.call(&mut hooks, Some(val!(10.0)), &[]);
      match result {
        Call::Ok(r) => assert_eq!(*r.to_str(), "10".to_string()),
        _ => assert!(false),
      }
    }
  }

  mod times {
    use super::*;
    use crate::support::{test_error_class, MockedContext};

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let error = val!(test_error_class(&hooks));
      let number_times = NumberTimes::new(&hooks, error);

      assert_eq!(number_times.meta().name, "times");
      assert_eq!(number_times.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(5.0)]);
      let mut hooks = Hooks::new(&mut context);

      let error = val!(test_error_class(&hooks.as_gc()));
      let number_times = NumberTimes::new(&hooks.as_gc(), error);

      let result = number_times.call(&mut hooks, Some(val!(3.0)), &[]);
      match result {
        Call::Ok(r) => {
          let mut number_times = r.to_iter();
          assert_eq!(number_times.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(number_times.current(), val!(0.0));

          assert_eq!(number_times.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(number_times.current(), val!(1.0));

          assert_eq!(number_times.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(number_times.current(), val!(2.0));

          assert_eq!(number_times.next(&mut hooks).unwrap(), val!(false));
        }
        _ => assert!(false),
      }
    }
  }

  mod cmp {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = Hooks::new(&mut context);

      let number_cmp = NumberCmp::from(&hooks);

      assert_eq!(number_cmp.meta().name, "cmp");
      assert_eq!(number_cmp.meta().signature.arity, Arity::Fixed(2));
      assert_eq!(
        number_cmp.meta().signature.parameters[0].kind,
        ParameterKind::Number
      );
      assert_eq!(
        number_cmp.meta().signature.parameters[1].kind,
        ParameterKind::Number
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let number_cmp = NumberCmp::from(&hooks);

      let result = number_cmp
        .call(&mut hooks, None, &[val!(5.0), val!(3.0)])
        .unwrap();

      assert_eq!(result, val!(2.0));
    }
  }

  mod floor {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = Hooks::new(&mut context);

      let number_floor = NumberFloor::from(&hooks);

      assert_eq!(number_floor.meta().name, "floor");
      assert_eq!(number_floor.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let number_floor = NumberFloor::from(&hooks);

      let result = number_floor
        .call(&mut hooks, Some(val!(10.5)), &[])
        .unwrap();
      assert_eq!(result.to_num(), 10.0);
    }
  }

  mod ceil {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = Hooks::new(&mut context);

      let number_ceil = NumberCeil::from(&hooks);

      assert_eq!(number_ceil.meta().name, "ceil");
      assert_eq!(number_ceil.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let number_ceil = NumberCeil::from(&hooks);

      let result = number_ceil.call(&mut hooks, Some(val!(10.5)), &[]).unwrap();
      assert_eq!(result.to_num(), 11.0);
    }
  }

  mod round {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = Hooks::new(&mut context);

      let number_round = NumberRound::from(&hooks);

      assert_eq!(number_round.meta().name, "round");
      assert_eq!(number_round.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let number_round = NumberRound::from(&hooks);

      let result = number_round
        .call(&mut hooks, Some(val!(10.3)), &[])
        .unwrap();
      assert_eq!(result.to_num(), 10.0);
    }
  }

  mod parse {
    use super::*;
    use crate::support::{test_error_class, MockedContext};

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);
      let error = val!(test_error_class(&hooks));

      let number_parse = NumberParse::new(&hooks, error);

      assert_eq!(number_parse.meta().name, "parse");
      assert_eq!(number_parse.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        number_parse.meta().signature.parameters[0].kind,
        ParameterKind::String
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let error = val!(test_error_class(&hooks.as_gc()));

      let number_parse = NumberParse::new(&hooks.as_gc(), error);

      let args = val!(hooks.manage_str("1"));
      let result = number_parse.call(&mut hooks, None, &[args]).unwrap();

      assert_eq!(result, val!(1.0));
    }
  }

  mod until {
    use super::*;
    use crate::support::{test_error_class, MockedContext};

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = Hooks::new(&mut context);
      let error = val!(test_error_class(&hooks.as_gc()));

      let number_until = NumberUntil::new(&hooks.as_gc(), error);

      assert_eq!(number_until.meta().name, "until");
      assert_eq!(number_until.meta().signature.arity, Arity::Default(1, 2));
      assert_eq!(
        number_until.meta().signature.parameters[0].kind,
        ParameterKind::Number
      );
      assert_eq!(
        number_until.meta().signature.parameters[1].kind,
        ParameterKind::Number
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(5.0)]);
      let mut hooks = Hooks::new(&mut context);
      let error = val!(test_error_class(&hooks.as_gc()));

      let number_until = NumberUntil::new(&hooks.as_gc(), error);

      let result = number_until.call(&mut hooks, Some(val!(2.0)), &[val!(6.0), val!(2.0)]);
      match result {
        Call::Ok(r) => {
          let mut number_until = r.to_iter();
          assert_eq!(number_until.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(number_until.current(), val!(2.0));

          assert_eq!(number_until.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(number_until.current(), val!(4.0));

          assert_eq!(number_until.next(&mut hooks).unwrap(), val!(false));
        }
        _ => assert!(false),
      }
    }
  }
}
