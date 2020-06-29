use crate::support::{export_and_insert, to_dyn_method};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  iterator::{SlIter, SlIterator},
  module::Module,
  native::{NativeMeta, NativeMethod},
  object::Class,
  package::Package,
  signature::{Arity, Parameter, ParameterKind},
  utils::is_falsey,
  value::{Value, VALUE_NIL},
  CallResult, ModuleResult,
};
use laythe_env::{
  managed::{Managed, Trace},
  stdio::StdIo,
};
use std::mem;

pub const ITER_CLASS_NAME: &'static str = "Iter";
const ITER_STR: NativeMeta = NativeMeta::new("str", Arity::Fixed(0), &[]);
const ITER_NEXT: NativeMeta = NativeMeta::new("next", Arity::Fixed(0), &[]);
const ITER_ITER: NativeMeta = NativeMeta::new("iter", Arity::Fixed(0), &[]);
const ITER_MAP: NativeMeta = NativeMeta::new(
  "map",
  Arity::Fixed(1),
  &[Parameter::new("fun", ParameterKind::Fun)],
);
const ITER_FILTER: NativeMeta = NativeMeta::new(
  "filter",
  Arity::Fixed(1),
  &[Parameter::new("fun", ParameterKind::Fun)],
);
const ITER_REDUCE: NativeMeta = NativeMeta::new(
  "reduce",
  Arity::Fixed(2),
  &[
    Parameter::new("initial", ParameterKind::Any),
    Parameter::new("fun", ParameterKind::Fun),
  ],
);
const ITER_EACH: NativeMeta = NativeMeta::new(
  "each",
  Arity::Fixed(1),
  &[Parameter::new("fun", ParameterKind::Fun)],
);

pub fn declare_iter_class(hooks: &GcHooks, self_module: &mut Module) -> ModuleResult<()> {
  let name = hooks.manage_str(String::from(ITER_CLASS_NAME));
  let class = hooks.manage(Class::bare(name));

  export_and_insert(hooks, self_module, name, Value::from(class))
}

pub fn define_iter_class(hooks: &GcHooks, self_module: &Module, _: &Package) {
  let name = hooks.manage_str(String::from(ITER_CLASS_NAME));
  let mut class = self_module
    .import(hooks)
    .get_field(&name)
    .unwrap()
    .to_class();

  class.add_method(
    hooks,
    hooks.manage_str(String::from(ITER_STR.name)),
    Value::from(to_dyn_method(hooks, IterStr())),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(ITER_NEXT.name)),
    Value::from(to_dyn_method(hooks, IterNext())),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(ITER_ITER.name)),
    Value::from(to_dyn_method(hooks, IterIter())),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(ITER_MAP.name)),
    Value::from(to_dyn_method(hooks, IterMap())),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(ITER_FILTER.name)),
    Value::from(to_dyn_method(hooks, IterFilter())),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(ITER_REDUCE.name)),
    Value::from(to_dyn_method(hooks, IterReduce())),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(ITER_EACH.name)),
    Value::from(to_dyn_method(hooks, IterEach())),
  );
}

#[derive(Clone, Debug, Trace)]
struct IterStr();

impl NativeMethod for IterStr {
  fn meta(&self) -> &NativeMeta {
    &ITER_STR
  }

  fn call(&self, hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    Ok(Value::from(
      hooks.manage_str(this.to_iter().name().to_string()),
    ))
  }
}

#[derive(Clone, Debug, Trace)]
struct IterNext();

impl NativeMethod for IterNext {
  fn meta(&self) -> &NativeMeta {
    &ITER_NEXT
  }

  fn call(&self, hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    this.to_iter().next(hooks)
  }
}

#[derive(Clone, Debug, Trace)]
struct IterIter();

impl NativeMethod for IterIter {
  fn meta(&self) -> &NativeMeta {
    &ITER_ITER
  }

  fn call(&self, _hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    Ok(this)
  }
}

#[derive(Trace)]
struct IterMap();

impl NativeMethod for IterMap {
  fn meta(&self) -> &NativeMeta {
    &ITER_MAP
  }

  fn call(&self, hooks: &mut Hooks, this: Value, args: &[Value]) -> CallResult {
    let inner_iter: Box<dyn SlIter> = Box::new(MapIterator::new(this.to_iter(), args[0]));
    let iter = SlIterator::new(inner_iter);
    let iter = hooks.manage(iter);

    Ok(Value::from(iter))
  }
}

#[derive(Debug)]
struct MapIterator {
  current: Value,
  iter: Managed<SlIterator>,
  callable: Value,
}

impl MapIterator {
  fn new(iter: Managed<SlIterator>, callable: Value) -> Self {
    Self {
      current: VALUE_NIL,
      iter,
      callable,
    }
  }
}

impl SlIter for MapIterator {
  fn name(&self) -> &str {
    "Map Iterator"
  }

  fn current(&self) -> Value {
    self.current
  }

  fn next(&mut self, hooks: &mut Hooks) -> CallResult {
    if is_falsey(self.iter.next(hooks)?) {
      Ok(Value::from(false))
    } else {
      let current = self.iter.current();
      self.current = hooks.call(self.callable, &[current])?;
      Ok(Value::from(true))
    }
  }

  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }
}

impl Trace for MapIterator {
  fn trace(&self) -> bool {
    self.current.trace();
    self.iter.trace();
    self.callable.trace();
    true
  }

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    self.current.trace_debug(stdio);
    self.iter.trace_debug(stdio);
    self.callable.trace_debug(stdio);
    true
  }
}

#[derive(Trace)]
struct IterFilter();

impl NativeMethod for IterFilter {
  fn meta(&self) -> &NativeMeta {
    &ITER_FILTER
  }

  fn call(&self, hooks: &mut Hooks, this: Value, args: &[Value]) -> CallResult {
    let inner_iter: Box<dyn SlIter> = Box::new(FilterIterator::new(this.to_iter(), args[0]));
    let iter = SlIterator::new(inner_iter);
    let iter = hooks.manage(iter);

    Ok(Value::from(iter))
  }
}

#[derive(Debug)]
struct FilterIterator {
  current: Value,
  iter: Managed<SlIterator>,
  callable: Value,
}

impl FilterIterator {
  fn new(iter: Managed<SlIterator>, callable: Value) -> Self {
    Self {
      current: VALUE_NIL,
      iter,
      callable,
    }
  }
}

impl SlIter for FilterIterator {
  fn name(&self) -> &str {
    "Filter Iterator"
  }

  fn current(&self) -> Value {
    self.current
  }

  fn next(&mut self, hooks: &mut Hooks) -> CallResult {
    while !is_falsey(self.iter.next(hooks)?) {
      let current = self.iter.current();
      let should_keep = hooks.call(self.callable, &[current])?;

      if !is_falsey(should_keep) {
        self.current = current;
        return Ok(Value::from(true));
      }
    }

    Ok(Value::from(false))
  }

  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }
}

impl Trace for FilterIterator {
  fn trace(&self) -> bool {
    self.current.trace();
    self.iter.trace();
    self.callable.trace();
    true
  }

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    self.current.trace_debug(stdio);
    self.iter.trace_debug(stdio);
    self.callable.trace_debug(stdio);
    true
  }
}

#[derive(Trace)]
struct IterReduce();

impl NativeMethod for IterReduce {
  fn meta(&self) -> &NativeMeta {
    &ITER_REDUCE
  }

  fn call(&self, hooks: &mut Hooks, this: Value, args: &[Value]) -> CallResult {
    let mut accumulator = args[0];
    let callable = args[1];
    let mut iter = this.to_iter();

    while !is_falsey(iter.next(hooks)?) {
      let current = iter.current();
      accumulator = hooks.call(callable, &[accumulator, current])?;
    }

    Ok(accumulator)
  }
}

#[derive(Trace)]
struct IterEach();

impl NativeMethod for IterEach {
  fn meta(&self) -> &NativeMeta {
    &ITER_EACH
  }

  fn call(&self, hooks: &mut Hooks, this: Value, args: &[Value]) -> CallResult {
    let callable = args[0];
    let mut iter = this.to_iter();

    while !is_falsey(iter.next(hooks)?) {
      let current = iter.current();
      hooks.call(callable, &[current])?;
    }

    Ok(VALUE_NIL)
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use laythe_core::iterator::SlIter;

  #[derive(Trace, Debug)]
  struct TestIterator {
    current: usize,
  }

  impl TestIterator {
    fn new() -> Self {
      Self { current: 0 }
    }
  }

  impl SlIter for TestIterator {
    fn name(&self) -> &str {
      "Test Iterator"
    }

    fn current(&self) -> Value {
      Value::from(self.current as f64)
    }

    fn next(&mut self, _hooks: &mut Hooks) -> CallResult {
      if self.current > 4 {
        return Ok(Value::from(false));
      }

      self.current += 1;
      Ok(Value::from(true))
    }

    fn size(&self) -> usize {
      8
    }
  }

  fn test_input() -> Box<dyn SlIter> {
    Box::new(TestIterator::new())
  }

  #[cfg(test)]
  mod str {
    use super::*;
    use crate::support::{test_native_dependencies, TestContext};
    use laythe_core::iterator::SlIterator;

    #[test]
    fn new() {
      let iter_str = IterStr();

      assert_eq!(iter_str.meta().name, "str");
      assert_eq!(iter_str.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let iter_str = IterStr();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let iter = test_input();
      let this = hooks.manage(SlIterator::new(iter));

      let result = iter_str.call(&mut hooks, Value::from(this), &[]);
      match result {
        Ok(r) => assert_eq!(&*r.to_str(), "Test Iterator"),
        Err(_) => assert!(false),
      }
    }
  }

  #[cfg(test)]
  mod next {
    use super::*;
    use crate::support::{test_native_dependencies, TestContext};
    use laythe_core::iterator::SlIterator;

    #[test]
    fn new() {
      let iter_next = IterNext();

      assert_eq!(iter_next.meta().name, "next");
      assert_eq!(iter_next.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let iter_next = IterNext();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let iter = test_input();
      let this = hooks.manage(SlIterator::new(iter));

      let result = iter_next.call(&mut hooks, Value::from(this), &[]);
      match result {
        Ok(r) => assert_eq!(r.to_bool(), true),
        Err(_) => assert!(false),
      }
    }
  }
  #[cfg(test)]
  mod iter {
    use super::*;
    use crate::support::{test_native_dependencies, TestContext};
    use laythe_core::iterator::SlIterator;

    #[test]
    fn new() {
      let iter_iter = IterIter();

      assert_eq!(iter_iter.meta().name, "iter");
      assert_eq!(iter_iter.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let iter_iter = IterIter();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let iter = test_input();
      let managed = hooks.manage(SlIterator::new(iter));
      let this = Value::from(managed);

      let result = iter_iter.call(&mut hooks, this, &[]);
      match result {
        Ok(r) => assert_eq!(r, this),
        Err(_) => assert!(false),
      }
    }
  }

  #[cfg(test)]
  mod map {
    use super::*;
    use crate::support::{fun_from_hooks, test_native_dependencies, TestContext};
    use laythe_core::{iterator::SlIterator, object::Closure};

    #[test]
    fn new() {
      let iter_map = IterMap();

      assert_eq!(iter_map.meta().name, "map");
      assert_eq!(iter_map.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        iter_map.meta().signature.parameters[0].kind,
        ParameterKind::Fun
      );
    }

    #[test]
    fn call() {
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[Value::from(5.0)]);
      let mut hooks = Hooks::new(&mut context);
      let iter_map = IterMap();

      let iter = test_input();
      let managed = hooks.manage(SlIterator::new(iter));
      let this = Value::from(managed);
      let fun = Value::from(hooks.manage(Closure::new(fun_from_hooks(
        &hooks.to_gc(),
        "example".to_string(),
        "module",
      ))));

      fun.to_closure().fun.arity = Arity::Fixed(1);

      let result = iter_map.call(&mut hooks, this, &[fun]);
      match result {
        Ok(r) => {
          let mut map_iter = r.to_iter();
          assert_eq!(map_iter.next(&mut hooks).unwrap(), Value::from(true));
          assert_eq!(map_iter.current(), Value::from(5.0));
        }
        Err(_) => assert!(false),
      }
    }
  }

  mod filter {
    use super::*;
    use crate::support::{fun_from_hooks, test_native_dependencies, TestContext};
    use laythe_core::{iterator::SlIterator, object::Closure};

    #[test]
    fn new() {
      let iter_filter = IterFilter();

      assert_eq!(iter_filter.meta().name, "filter");
      assert_eq!(iter_filter.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        iter_filter.meta().signature.parameters[0].kind,
        ParameterKind::Fun
      );
    }

    #[test]
    fn call() {
      let gc = test_native_dependencies();
      let mut context = TestContext::new(
        &gc,
        &[Value::from(false), Value::from(true), Value::from(true)],
      );
      let mut hooks = Hooks::new(&mut context);
      let iter_filter = IterFilter();

      let iter = test_input();
      let managed = hooks.manage(SlIterator::new(iter));
      let this = Value::from(managed);
      let fun = Value::from(hooks.manage(Closure::new(fun_from_hooks(
        &hooks.to_gc(),
        "example".to_string(),
        "module",
      ))));

      fun.to_closure().fun.arity = Arity::Fixed(1);

      let result = iter_filter.call(&mut hooks, this, &[fun]);
      match result {
        Ok(r) => {
          let mut filter_iter = r.to_iter();
          assert_eq!(filter_iter.next(&mut hooks).unwrap(), Value::from(true));
          assert_eq!(filter_iter.current(), Value::from(2.0));
          assert_eq!(filter_iter.next(&mut hooks).unwrap(), Value::from(true));
          assert_eq!(filter_iter.current(), Value::from(3.0));
        }
        Err(_) => assert!(false),
      }
    }
  }

  mod reduce {
    use super::*;
    use crate::support::{fun_from_hooks, test_native_dependencies, TestContext};
    use laythe_core::{iterator::SlIterator, object::Closure};

    #[test]
    fn new() {
      let iter_reduce = IterReduce();

      assert_eq!(iter_reduce.meta().name, "reduce");
      assert_eq!(iter_reduce.meta().signature.arity, Arity::Fixed(2));
      assert_eq!(
        iter_reduce.meta().signature.parameters[0].kind,
        ParameterKind::Any
      );
      assert_eq!(
        iter_reduce.meta().signature.parameters[1].kind,
        ParameterKind::Fun
      );
    }

    #[test]
    fn call() {
      let gc = test_native_dependencies();
      let mut context = TestContext::new(
        &gc,
        &[
          Value::from(false),
          Value::from(false),
          Value::from(false),
          Value::from(false),
          Value::from(10.1),
        ],
      );
      let mut hooks = Hooks::new(&mut context);
      let iter_reduce = IterReduce();

      let iter = test_input();
      let managed = hooks.manage(SlIterator::new(iter));
      let this = Value::from(managed);
      let fun = Value::from(hooks.manage(Closure::new(fun_from_hooks(
        &hooks.to_gc(),
        "example".to_string(),
        "module",
      ))));

      fun.to_closure().fun.arity = Arity::Fixed(2);

      let result = iter_reduce.call(&mut hooks, this, &[Value::from(0.0), fun]);
      match result {
        Ok(r) => {
          assert!(r.is_num());
          assert_eq!(r.to_num(), 10.1);
        }
        Err(_) => assert!(false),
      }
    }
  }

  mod each {
    use super::*;
    use crate::support::{fun_from_hooks, test_native_dependencies, TestContext};
    use laythe_core::{iterator::SlIterator, object::Closure};

    #[test]
    fn new() {
      let iter_each = IterEach();

      assert_eq!(iter_each.meta().name, "each");
      assert_eq!(iter_each.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        iter_each.meta().signature.parameters[0].kind,
        ParameterKind::Fun
      );
    }

    #[test]
    fn call() {
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[Value::from(false); 5]);
      let mut hooks = Hooks::new(&mut context);
      let iter_reduce = IterEach();

      let iter = test_input();
      let managed = hooks.manage(SlIterator::new(iter));
      let this = Value::from(managed);
      let fun = Value::from(hooks.manage(Closure::new(fun_from_hooks(
        &hooks.to_gc(),
        "example".to_string(),
        "module",
      ))));

      fun.to_closure().fun.arity = Arity::Fixed(1);

      let result = iter_reduce.call(&mut hooks, this, &[fun]);
      match result {
        Ok(r) => assert!(r.is_nil()),
        Err(_) => assert!(false),
      }
    }
  }
}
