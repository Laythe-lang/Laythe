use crate::support::{export_and_insert, to_dyn_method};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  iterator::{SlIter, SlIterator},
  module::Module,
  native::{NativeMeta, NativeMethod},
  object::Class,
  package::Package,
  signature::Arity,
  value::Value,
  CallResult, ModuleResult,
};
use laythe_env::{managed::Trace, stdio::StdIo};
use std::mem;

pub const NUMBER_CLASS_NAME: &'static str = "Number";
const NUMBER_STR: NativeMeta = NativeMeta::new("str", Arity::Fixed(0), &[]);
const NUMBER_TIMES: NativeMeta = NativeMeta::new("times", Arity::Fixed(0), &[]);

pub fn declare_number_class(hooks: &GcHooks, self_module: &mut Module) -> ModuleResult<()> {
  let name = hooks.manage_str(String::from(NUMBER_CLASS_NAME));
  let class = hooks.manage(Class::new(name));

  export_and_insert(hooks, self_module, name, Value::from(class))
}

pub fn define_number_class(hooks: &GcHooks, self_module: &Module, _: &Package) {
  let name = hooks.manage_str(String::from(NUMBER_CLASS_NAME));
  let mut class = self_module
    .import(hooks)
    .get_field(&name)
    .unwrap()
    .to_class();

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

    let inner_iter: Box<dyn SlIter> = Box::new(TimesIterator::new(max));
    let iter = SlIterator::new(inner_iter);
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

impl SlIter for TimesIterator {
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

  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod str {
    use super::*;
    use crate::support::{test_native_dependencies, TestContext};

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
      let mut context = TestContext::new(&gc, &[]);
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
    use crate::support::{test_native_dependencies, TestContext};

    #[test]
    fn new() {
      let number_times = NumberTimes();

      assert_eq!(number_times.meta().name, "times");
      assert_eq!(number_times.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[Value::from(5.0)]);
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
