use crate::support::{
  default_class_inheritance, export_and_insert, load_class_from_module, to_dyn_method,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  iterator::{LyIter, LyIterator},
  module::Module,
  native::{NativeMeta, NativeMethod},
  package::Package,
  signature::Arity,
  value::Value,
  CallResult, LyResult,
};
use laythe_env::{managed::Trace, stdio::Stdio};
use std::mem;

pub const NUMBER_CLASS_NAME: &'static str = "Number";
const NUMBER_STR: NativeMeta = NativeMeta::new("str", Arity::Fixed(0), &[]);
const NUMBER_TIMES: NativeMeta = NativeMeta::new("times", Arity::Fixed(0), &[]);

pub fn declare_number_class(
  hooks: &GcHooks,
  module: &mut Module,
  package: &Package,
) -> LyResult<()> {
  let class = default_class_inheritance(hooks, package, NUMBER_CLASS_NAME)?;
  export_and_insert(hooks, module, class.name, Value::from(class))
}

pub fn define_number_class(hooks: &GcHooks, module: &Module, _: &Package) -> LyResult<()> {
  let mut class = load_class_from_module(hooks, module, NUMBER_CLASS_NAME)?;

  class.add_method(
    hooks,
    hooks.manage_str(String::from(NUMBER_STR.name)),
    Value::from(to_dyn_method(hooks, NumberStr())),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(NUMBER_TIMES.name)),
    Value::from(to_dyn_method(hooks, NumberTimes())),
  );

  Ok(())
}

#[derive(Clone, Debug, Trace)]
struct NumberStr();

impl NativeMethod for NumberStr {
  fn meta(&self) -> &NativeMeta {
    &NUMBER_STR
  }

  fn call(&self, hook: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    Ok(Value::from(hook.manage_str(this.to_num().to_string())))
  }
}

#[derive(Clone, Debug, Trace)]
struct NumberTimes();

impl NativeMethod for NumberTimes {
  fn meta(&self) -> &NativeMeta {
    &NUMBER_TIMES
  }

  fn call(&self, hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    let max = this.to_num();
    if max < 0.0 {
      return Err(hooks.make_error("times requires a positive number.".to_string()));
    }

    let inner_iter: Box<dyn LyIter> = Box::new(TimesIterator::new(max));
    let iter = LyIterator::new(inner_iter);
    let iter = hooks.manage(iter);

    Ok(Value::from(iter))
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
    Value::from(self.current)
  }

  fn next(&mut self, _hooks: &mut Hooks) -> CallResult {
    if self.current < self.max {
      self.current = self.current + 1.0;
      Ok(Value::from(true))
    } else {
      Ok(Value::from(false))
    }
  }

  fn size_hint(&self) -> Option<usize> {
    Some(self.max as usize)
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
    use crate::support::{test_native_dependencies, MockedContext};

    #[test]
    fn new() {
      let number_str = NumberStr();

      assert_eq!(number_str.meta().name, "str");
      assert_eq!(number_str.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let number_str = NumberStr();
      let gc = test_native_dependencies();
      let mut context = MockedContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let result = number_str.call(&mut hooks, Value::from(10.0), &[]);
      match result {
        Ok(r) => assert_eq!(*r.to_str(), "10".to_string()),
        Err(_) => assert!(false),
      }
    }
  }

  mod times {
    use super::*;
    use crate::support::{test_native_dependencies, MockedContext};

    #[test]
    fn new() {
      let number_times = NumberTimes();

      assert_eq!(number_times.meta().name, "times");
      assert_eq!(number_times.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let gc = test_native_dependencies();
      let mut context = MockedContext::new(&gc, &[Value::from(5.0)]);
      let mut hooks = Hooks::new(&mut context);
      let number_times = NumberTimes();

      let result = number_times.call(&mut hooks, Value::from(3.0), &[]);
      match result {
        Ok(r) => {
          let mut number_times = r.to_iter();
          assert_eq!(number_times.next(&mut hooks).unwrap(), Value::from(true));
          assert_eq!(number_times.current(), Value::from(0.0));

          assert_eq!(number_times.next(&mut hooks).unwrap(), Value::from(true));
          assert_eq!(number_times.current(), Value::from(1.0));

          assert_eq!(number_times.next(&mut hooks).unwrap(), Value::from(true));
          assert_eq!(number_times.current(), Value::from(2.0));

          assert_eq!(number_times.next(&mut hooks).unwrap(), Value::from(false));
        }
        Err(_) => assert!(false),
      }
    }
  }
}
