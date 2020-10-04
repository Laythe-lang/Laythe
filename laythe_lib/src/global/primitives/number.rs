use crate::{
  native,
  support::{default_class_inheritance, export_and_insert, load_class_from_module, to_dyn_native},
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  iterator::{LyIter, LyIterator},
  module::Module,
  native::{MetaData, Native, NativeMeta, NativeMetaBuilder},
  package::Package,
  signature::{Arity, ParameterBuilder, ParameterKind},
  val,
  value::Value,
  CallResult, LyResult,
};
use laythe_env::managed::Trace;
use std::io::Write;
use std::mem;

pub const NUMBER_CLASS_NAME: &str = "Number";
const NUMBER_STR: NativeMetaBuilder = NativeMetaBuilder::method("str", Arity::Fixed(0));
const NUMBER_TIMES: NativeMetaBuilder = NativeMetaBuilder::method("times", Arity::Fixed(0));
const NUMBER_UNTIL: NativeMetaBuilder = NativeMetaBuilder::method("until", Arity::Default(1, 2))
  .with_params(&[
    ParameterBuilder::new("upper", ParameterKind::Number),
    ParameterBuilder::new("stride", ParameterKind::Number),
  ]);

const NUMBER_PARSE: NativeMetaBuilder = NativeMetaBuilder::fun("parse", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("str", ParameterKind::String)]);

pub fn declare_number_class(
  hooks: &GcHooks,
  module: &mut Module,
  package: &Package,
) -> LyResult<()> {
  let class = default_class_inheritance(hooks, package, NUMBER_CLASS_NAME)?;
  export_and_insert(hooks, module, class.name, val!(class))
}

pub fn define_number_class(hooks: &GcHooks, module: &Module, _: &Package) -> LyResult<()> {
  let mut class = load_class_from_module(hooks, module, NUMBER_CLASS_NAME)?;

  class.add_method(
    hooks,
    hooks.manage_str(NUMBER_STR.name),
    val!(to_dyn_native(hooks, NumberStr::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(NUMBER_TIMES.name),
    val!(to_dyn_native(hooks, NumberTimes::from(hooks))),
  );

  class.meta().expect("Meta class not set.").add_method(
    hooks,
    hooks.manage_str(NUMBER_PARSE.name),
    val!(to_dyn_native(hooks, NumberParse::from(hooks))),
  );

  Ok(())
}

native!(NumberStr, NUMBER_STR);

impl Native for NumberStr {
  fn call(&self, hook: &mut Hooks, this: Option<Value>, _args: &[Value]) -> CallResult {
    Ok(val!(hook.manage_str(this.unwrap().to_num().to_string())))
  }
}

native!(NumberParse, NUMBER_PARSE);

impl Native for NumberParse {
  fn call(&self, hook: &mut Hooks, _this: Option<Value>, args: &[Value]) -> CallResult {
    let str = args[0].to_str();
    match str.parse::<f64>() {
      Ok(num) => Ok(val!(num)),
      Err(_) => hook.error(format!("Unable to parse number from {}", str)),
    }
  }
}

native!(NumberTimes, NUMBER_TIMES);

impl Native for NumberTimes {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> CallResult {
    let max = this.unwrap().to_num();
    if max < 0.0 {
      return hooks.error("times requires a positive number.");
    }

    let inner_iter: Box<dyn LyIter> = Box::new(TimesIterator::new(max));
    let iter = LyIterator::new(inner_iter);
    let iter = hooks.manage(iter);

    Ok(val!(iter))
  }
}

#[derive(Debug, Trace)]
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

  fn next(&mut self, _hooks: &mut Hooks) -> CallResult {
    if self.current < self.max {
      self.current += 1.0;
      Ok(val!(true))
    } else {
      Ok(val!(false))
    }
  }

  fn size_hint(&self) -> Option<usize> {
    Some((self.max + 1.0) as usize)
  }

  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }
}

native!(NumberUntil, NUMBER_UNTIL);

impl Native for NumberUntil {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> CallResult {
    let min = this.unwrap().to_num();
    let max = _args[0].to_num();
    let stride = if _args.len() > 1 {
      _args[1].to_num()
    } else {
      1.0
    };

    let inner_iter: Box<dyn LyIter> = Box::new(UntilIterator::new(min, max, stride));
    let iter = LyIterator::new(inner_iter);
    let iter = hooks.manage(iter);

    Ok(val!(iter))
  }
}

#[derive(Debug, Trace)]
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

  fn next(&mut self, _hooks: &mut Hooks) -> CallResult {
    if self.current < self.max {
      self.current += self.stride;
      Ok(val!(true))
    } else {
      Ok(val!(false))
    }
  }

  fn size_hint(&self) -> Option<usize> {
    None
  }

  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }
}

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
      let hooks = Hooks::new(&mut context);

      let number_str = NumberStr::from(&hooks);
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let result = number_str.call(&mut hooks, Some(val!(10.0)), &[]);
      match result {
        Ok(r) => assert_eq!(*r.to_str(), "10".to_string()),
        Err(_) => assert!(false),
      }
    }
  }

  mod times {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = Hooks::new(&mut context);

      let number_times = NumberTimes::from(&hooks);

      assert_eq!(number_times.meta().name, "times");
      assert_eq!(number_times.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(5.0)]);
      let mut hooks = Hooks::new(&mut context);
      let number_times = NumberTimes::from(&hooks);

      let result = number_times.call(&mut hooks, Some(val!(3.0)), &[]);
      match result {
        Ok(r) => {
          let mut number_times = r.to_iter();
          assert_eq!(number_times.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(number_times.current(), val!(0.0));

          assert_eq!(number_times.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(number_times.current(), val!(1.0));

          assert_eq!(number_times.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(number_times.current(), val!(2.0));

          assert_eq!(number_times.next(&mut hooks).unwrap(), val!(false));
        }
        Err(_) => assert!(false),
      }
    }
  }

  mod until {
    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = Hooks::new(&mut context);

      let number_until = NumberUntil::from(&hooks);

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
      let number_until = NumberUntil::from(&hooks);

      let result = number_until.call(&mut hooks, Some(val!(2.0)), &[val!(6.0), val!(2.0)]);
      match result {
        Ok(r) => {
          let mut number_until = r.to_iter();
          assert_eq!(number_until.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(number_until.current(), val!(2.0));

          assert_eq!(number_until.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(number_until.current(), val!(4.0));

          assert_eq!(number_until.next(&mut hooks).unwrap(), val!(false));
        }
        Err(_) => assert!(false),
      }
    }
  }
}
