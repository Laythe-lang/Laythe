use crate::{
  native,
  support::{default_class_inheritance, export_and_insert, load_class_from_module, to_dyn_native},
  InitResult,
};
use laythe_core::{
  get,
  hooks::{GcHooks, Hooks},
  iterator::{LyIter, LyIterator},
  module::Module,
  native::{MetaData, Native, NativeMeta, NativeMetaBuilder},
  object::List,
  package::Package,
  signature::{Arity, ParameterBuilder, ParameterKind},
  utils::is_falsey,
  val,
  value::{Value, VALUE_NIL},
  Call,
};
use laythe_env::managed::{Managed, Trace};
use std::io::Write;
use std::mem;

pub const ITER_CLASS_NAME: &str = "Iter";
const ITER_STR: NativeMetaBuilder = NativeMetaBuilder::method("str", Arity::Fixed(0));

/// This might need to have a stack once we implement yield or the iterator class
const ITER_NEXT: NativeMetaBuilder = NativeMetaBuilder::method("next", Arity::Fixed(0));
const ITER_CURRENT: NativeMetaBuilder = NativeMetaBuilder::method("current", Arity::Fixed(0));
const ITER_ITER: NativeMetaBuilder = NativeMetaBuilder::method("iter", Arity::Fixed(0));

const ITER_FIRST: NativeMetaBuilder = NativeMetaBuilder::method("first", Arity::Fixed(0));
const ITER_LAST: NativeMetaBuilder = NativeMetaBuilder::method("last", Arity::Fixed(0));

const ITER_MAP: NativeMetaBuilder = NativeMetaBuilder::method("map", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("fun", ParameterKind::Fun)])
  .with_stack();

const ITER_FILTER: NativeMetaBuilder = NativeMetaBuilder::method("filter", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("fun", ParameterKind::Fun)])
  .with_stack();

const ITER_REDUCE: NativeMetaBuilder = NativeMetaBuilder::method("reduce", Arity::Fixed(2))
  .with_params(&[
    ParameterBuilder::new("initial", ParameterKind::Any),
    ParameterBuilder::new("fun", ParameterKind::Fun),
  ])
  .with_stack();

const ITER_LEN: NativeMetaBuilder = NativeMetaBuilder::method("len", Arity::Fixed(0));

const ITER_EACH: NativeMetaBuilder = NativeMetaBuilder::method("each", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("fun", ParameterKind::Fun)])
  .with_stack();

const ITER_ZIP: NativeMetaBuilder = NativeMetaBuilder::method("zip", Arity::Variadic(0))
  .with_params(&[ParameterBuilder::new("iterators", ParameterKind::Iter)]);

const ITER_CHAIN: NativeMetaBuilder = NativeMetaBuilder::method("chain", Arity::Variadic(0))
  .with_params(&[ParameterBuilder::new("iterators", ParameterKind::Iter)]);

const ITER_ALL: NativeMetaBuilder = NativeMetaBuilder::method("all", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("fun", ParameterKind::Fun)])
  .with_stack();

const ITER_ANY: NativeMetaBuilder = NativeMetaBuilder::method("any", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("fun", ParameterKind::Fun)])
  .with_stack();

const ITER_INTO: NativeMetaBuilder = NativeMetaBuilder::method("into", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("fun", ParameterKind::Fun)])
  .with_stack();

pub fn declare_iter_class(
  hooks: &GcHooks,
  module: &mut Module,
  package: &Package,
) -> InitResult<()> {
  let class = default_class_inheritance(hooks, package, ITER_CLASS_NAME)?;
  export_and_insert(hooks, module, class.name, val!(class))
}

pub fn define_iter_class(hooks: &GcHooks, module: &Module, _: &Package) -> InitResult<()> {
  let mut class = load_class_from_module(hooks, module, ITER_CLASS_NAME)?;

  class.add_method(
    hooks,
    hooks.manage_str(ITER_STR.name),
    val!(to_dyn_native(hooks, IterStr::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(ITER_NEXT.name),
    val!(to_dyn_native(hooks, IterNext::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(ITER_CURRENT.name),
    val!(to_dyn_native(hooks, IterCurrent::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(ITER_ITER.name),
    val!(to_dyn_native(hooks, IterIter::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(ITER_FIRST.name),
    val!(to_dyn_native(hooks, IterFirst::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(ITER_LAST.name),
    val!(to_dyn_native(hooks, IterLast::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(ITER_MAP.name),
    val!(to_dyn_native(hooks, IterMap::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(ITER_FILTER.name),
    val!(to_dyn_native(hooks, IterFilter::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(ITER_REDUCE.name),
    val!(to_dyn_native(hooks, IterReduce::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(ITER_LEN.name),
    val!(to_dyn_native(hooks, IterLen::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(ITER_EACH.name),
    val!(to_dyn_native(hooks, IterEach::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(ITER_ZIP.name),
    val!(to_dyn_native(hooks, IterZip::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(ITER_CHAIN.name),
    val!(to_dyn_native(hooks, IterChain::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(ITER_ALL.name),
    val!(to_dyn_native(hooks, IterAll::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(ITER_ANY.name),
    val!(to_dyn_native(hooks, IterAny::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(ITER_INTO.name),
    val!(to_dyn_native(hooks, IterInto::from(hooks))),
  );

  Ok(())
}

native!(IterStr, ITER_STR);

impl Native for IterStr {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    Call::Ok(val!(hooks.manage_str(this.unwrap().to_iter().name())))
  }
}

native!(IterNext, ITER_NEXT);

impl Native for IterNext {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    this.unwrap().to_iter().next(hooks)
  }
}

native!(IterCurrent, ITER_CURRENT);

impl Native for IterCurrent {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    Call::Ok(this.unwrap().to_iter().current())
  }
}

native!(IterIter, ITER_ITER);

impl Native for IterIter {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    Call::Ok(this.unwrap())
  }
}

native!(IterFirst, ITER_FIRST);

impl Native for IterFirst {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    let mut iter = this.unwrap().to_iter();

    if !is_falsey(get!(iter.next(hooks))) {
      let current = iter.current();
      Call::Ok(current)
    } else {
      Call::Ok(VALUE_NIL)
    }
  }
}

native!(IterLast, ITER_LAST);

impl Native for IterLast {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    let mut iter = this.unwrap().to_iter();
    let mut result = VALUE_NIL;

    while !is_falsey(get!(iter.next(hooks))) {
      result = iter.current();
    }

    Call::Ok(result)
  }
}

native!(IterMap, ITER_MAP);

impl Native for IterMap {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let inner_iter: Box<dyn LyIter> = Box::new(MapIterator::new(this.unwrap().to_iter(), args[0]));
    let iter = LyIterator::new(inner_iter);
    let iter = hooks.manage(iter);

    Call::Ok(val!(iter))
  }
}

#[derive(Debug)]
struct MapIterator {
  current: Value,
  iter: Managed<LyIterator>,
  callable: Value,
}

impl MapIterator {
  fn new(iter: Managed<LyIterator>, callable: Value) -> Self {
    Self {
      current: VALUE_NIL,
      iter,
      callable,
    }
  }
}

impl LyIter for MapIterator {
  fn name(&self) -> &str {
    "Map Iterator"
  }

  fn current(&self) -> Value {
    self.current
  }

  fn next(&mut self, hooks: &mut Hooks) -> Call {
    if is_falsey(get!(self.iter.next(hooks))) {
      Call::Ok(val!(false))
    } else {
      let current = self.iter.current();
      self.current = get!(hooks.call(self.callable, &[current]));
      Call::Ok(val!(true))
    }
  }

  fn size_hint(&self) -> Option<usize> {
    self.iter.size_hint()
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

  fn trace_debug(&self, stdout: &mut dyn Write) -> bool {
    self.current.trace_debug(stdout);
    self.iter.trace_debug(stdout);
    self.callable.trace_debug(stdout);
    true
  }
}

native!(IterFilter, ITER_FILTER);

impl Native for IterFilter {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let inner_iter: Box<dyn LyIter> =
      Box::new(FilterIterator::new(this.unwrap().to_iter(), args[0]));
    let iter = LyIterator::new(inner_iter);
    let iter = hooks.manage(iter);

    Call::Ok(val!(iter))
  }
}

#[derive(Debug)]
struct FilterIterator {
  current: Value,
  iter: Managed<LyIterator>,
  callable: Value,
}

impl FilterIterator {
  fn new(iter: Managed<LyIterator>, callable: Value) -> Self {
    Self {
      current: VALUE_NIL,
      iter,
      callable,
    }
  }
}

impl LyIter for FilterIterator {
  fn name(&self) -> &str {
    "Filter Iterator"
  }

  fn current(&self) -> Value {
    self.current
  }

  fn next(&mut self, hooks: &mut Hooks) -> Call {
    while !is_falsey(get!(self.iter.next(hooks))) {
      let current = self.iter.current();
      let should_keep = get!(hooks.call(self.callable, &[current]));

      if !is_falsey(should_keep) {
        self.current = current;
        return Call::Ok(val!(true));
      }
    }

    Call::Ok(val!(false))
  }

  fn size_hint(&self) -> Option<usize> {
    None
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

  fn trace_debug(&self, stdout: &mut dyn Write) -> bool {
    self.current.trace_debug(stdout);
    self.iter.trace_debug(stdout);
    self.callable.trace_debug(stdout);
    true
  }
}

native!(IterReduce, ITER_REDUCE);

impl Native for IterReduce {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let mut accumulator = args[0];
    let callable = args[1];
    let mut iter = this.unwrap().to_iter();

    while !is_falsey(get!(iter.next(hooks))) {
      let current = iter.current();
      accumulator = get!(hooks.call(callable, &[accumulator, current]));
    }

    Call::Ok(accumulator)
  }
}

native!(IterLen, ITER_LEN);

impl Native for IterLen {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    let mut iter = this.unwrap().to_iter();

    match iter.size_hint() {
      Some(size) => Call::Ok(val!(size as f64)),
      None => {
        let mut size: usize = 0;
        while !is_falsey(get!(iter.next(hooks))) {
          size += 1;
        }

        Call::Ok(val!(size as f64))
      }
    }
  }
}

native!(IterEach, ITER_EACH);

impl Native for IterEach {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let callable = args[0];
    let mut iter = this.unwrap().to_iter();

    while !is_falsey(get!(iter.next(hooks))) {
      let current = iter.current();
      get!(hooks.call(callable, &[current]));
    }

    Call::Ok(VALUE_NIL)
  }
}

native!(IterZip, ITER_ZIP);

impl Native for IterZip {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let iters: Vec<Managed<LyIterator>> = [this.unwrap()]
      .iter()
      .chain(args.iter())
      .map(|arg| arg.to_iter())
      .collect();

    let inner_iter: Box<dyn LyIter> = Box::new(ZipIterator::new(iters));
    let iter = LyIterator::new(inner_iter);
    let iter = hooks.manage(iter);

    Call::Ok(val!(iter))
  }
}

#[derive(Debug)]
struct ZipIterator {
  current: Value,
  iters: Vec<Managed<LyIterator>>,
}

impl ZipIterator {
  fn new(iters: Vec<Managed<LyIterator>>) -> Self {
    Self {
      current: VALUE_NIL,
      iters,
    }
  }
}

impl LyIter for ZipIterator {
  fn name(&self) -> &str {
    "Zip Iterator"
  }

  fn current(&self) -> Value {
    self.current
  }

  fn next(&mut self, hooks: &mut Hooks) -> Call {
    let mut results = hooks.manage(List::with_capacity(self.iters.len()));

    for iter in &mut self.iters {
      let next = get!(iter.next(hooks));
      if is_falsey(next) {
        return Call::Ok(val!(false));
      }

      results.push(iter.current());
    }

    self.current = val!(results);
    Call::Ok(val!(true))
  }

  fn size_hint(&self) -> Option<usize> {
    use std::cmp;

    self.iters.iter().fold(Some(std::usize::MAX), |acc, curr| {
      acc.and_then(|acc| curr.size_hint().map(|curr| cmp::min(acc, curr)))
    })
  }

  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }
}

impl Trace for ZipIterator {
  fn trace(&self) -> bool {
    self.current.trace();
    self.iters.iter().for_each(|iter| {
      iter.trace();
    });
    true
  }

  fn trace_debug(&self, stdout: &mut dyn Write) -> bool {
    self.current.trace_debug(stdout);
    self.iters.iter().for_each(|iter| {
      iter.trace_debug(stdout);
    });
    true
  }
}

native!(IterChain, ITER_CHAIN);

impl Native for IterChain {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let iters: Vec<Managed<LyIterator>> = [this.unwrap()]
      .iter()
      .chain(args.iter())
      .map(|arg| arg.to_iter())
      .collect();

    let inner_iter: Box<dyn LyIter> = Box::new(ChainIterator::new(iters));
    let iter = LyIterator::new(inner_iter);
    let iter = hooks.manage(iter);

    Call::Ok(val!(iter))
  }
}

#[derive(Debug)]
struct ChainIterator {
  current: Value,
  iter_index: usize,
  iters: Vec<Managed<LyIterator>>,
}

impl ChainIterator {
  fn new(iters: Vec<Managed<LyIterator>>) -> Self {
    Self {
      current: VALUE_NIL,
      iter_index: 0,
      iters,
    }
  }
}

impl LyIter for ChainIterator {
  fn name(&self) -> &str {
    "Chain Iterator"
  }

  fn current(&self) -> Value {
    self.current
  }

  fn next(&mut self, hooks: &mut Hooks) -> Call {
    loop {
      if self.iter_index >= self.iters.len() {
        return Call::Ok(val!(false));
      }

      let mut iter = self.iters[self.iter_index];
      let next = get!(iter.next(hooks));
      if !is_falsey(next) {
        self.current = iter.current();
        break;
      }

      self.iter_index += 1;
    }

    Call::Ok(val!(true))
  }

  fn size_hint(&self) -> Option<usize> {
    self
      .iters
      .iter()
      .fold(Some(0), |acc, curr| match (acc, curr.size_hint()) {
        (Some(acc), Some(curr)) => Some(acc + curr),
        _ => None,
      })
  }

  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }
}

impl Trace for ChainIterator {
  fn trace(&self) -> bool {
    self.current.trace();
    self.iters.iter().for_each(|iter| {
      iter.trace();
    });
    true
  }

  fn trace_debug(&self, stdout: &mut dyn Write) -> bool {
    self.current.trace_debug(stdout);
    self.iters.iter().for_each(|iter| {
      iter.trace_debug(stdout);
    });
    true
  }
}

native!(IterAll, ITER_ALL);

impl Native for IterAll {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let callable = args[0];
    let mut iter = this.unwrap().to_iter();

    while !is_falsey(get!(iter.next(hooks))) {
      let current = iter.current();
      if is_falsey(get!(hooks.call(callable, &[current]))) {
        return Call::Ok(val!(false));
      }
    }

    Call::Ok(val!(true))
  }
}

native!(IterAny, ITER_ANY);

impl Native for IterAny {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let callable = args[0];
    let mut iter = this.unwrap().to_iter();

    while !is_falsey(get!(iter.next(hooks))) {
      let current = iter.current();
      if !is_falsey(get!(hooks.call(callable, &[current]))) {
        return Call::Ok(val!(true));
      }
    }

    Call::Ok(val!(false))
  }
}

native!(IterInto, ITER_INTO);

impl Native for IterInto {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let callable = args[0];
    let iter = this.unwrap().to_iter();

    hooks.push_root(iter);
    let result = hooks.call(callable, &[this.unwrap()]);
    hooks.pop_roots(1);

    result
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::support::{test_iter, MockedContext};
  use laythe_core::iterator::LyIterator;

  const TEST_FUN: NativeMetaBuilder = NativeMetaBuilder::fun("test", Arity::Fixed(1))
    .with_params(&[ParameterBuilder::new("any", ParameterKind::Any)]);

  native!(TestFun, TEST_FUN);

  impl Native for TestFun {
    fn call(&self, _hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> Call {
      Call::Ok(args[0])
    }
  }

  #[cfg(test)]
  mod str {
    use super::*;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let iter_str = IterStr::from(&hooks);

      assert_eq!(iter_str.meta().name, "str");
      assert_eq!(iter_str.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let iter_str = IterStr::from(&hooks.as_gc());

      let iter = test_iter();
      let this = hooks.manage(LyIterator::new(iter));

      let result = iter_str.call(&mut hooks, Some(val!(this)), &[]);
      match result {
        Call::Ok(r) => assert_eq!(&*r.to_str(), "Test Iterator"),
        _ => assert!(false),
      }
    }
  }

  #[cfg(test)]
  mod next {
    use super::*;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let iter_next = IterNext::from(&hooks);

      assert_eq!(iter_next.meta().name, "next");
      assert_eq!(iter_next.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let iter_next = IterNext::from(&hooks.as_gc());

      let iter = test_iter();
      let this = hooks.manage(LyIterator::new(iter));

      let result = iter_next.call(&mut hooks, Some(val!(this)), &[]);
      match result {
        Call::Ok(r) => assert_eq!(r.to_bool(), true),
        _ => assert!(false),
      }
    }
  }

  #[cfg(test)]
  mod current {
    use super::*;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let iter_current = IterCurrent::from(&hooks);

      assert_eq!(iter_current.meta().name, "current");
      assert_eq!(iter_current.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let iter_current = IterCurrent::from(&hooks.as_gc());

      let iter = test_iter();
      let this = hooks.manage(LyIterator::new(iter));

      let result = iter_current
        .call(&mut hooks, Some(val!(this)), &[])
        .unwrap();
      assert!(result.is_nil());
    }
  }

  #[cfg(test)]
  mod iter {
    use super::*;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let iter_iter = IterIter::from(&hooks);

      assert_eq!(iter_iter.meta().name, "iter");
      assert_eq!(iter_iter.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let iter_iter = IterIter::from(&hooks.as_gc());

      let iter = test_iter();
      let managed = hooks.manage(LyIterator::new(iter));
      let this = val!(managed);

      let result = iter_iter.call(&mut hooks, Some(this), &[]);
      match result {
        Call::Ok(r) => assert_eq!(r, this),
        _ => assert!(false),
      }
    }
  }

  #[cfg(test)]
  mod first {
    use super::*;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let iter_first = IterFirst::from(&hooks);

      assert_eq!(iter_first.meta().name, "first");
      assert_eq!(iter_first.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let iter_first = IterFirst::from(&hooks.as_gc());

      let iter = test_iter();
      let managed = hooks.manage(LyIterator::new(iter));
      let this = val!(managed);

      let result = iter_first.call(&mut hooks, Some(this), &[]).unwrap();
      assert_eq!(result, val!(1.0));
    }
  }

  #[cfg(test)]
  mod last {
    use super::*;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let iter_last = IterLast::from(&hooks);

      assert_eq!(iter_last.meta().name, "last");
      assert_eq!(iter_last.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let iter_last = IterLast::from(&hooks.as_gc());

      let iter = test_iter();
      let managed = hooks.manage(LyIterator::new(iter));
      let this = val!(managed);

      let result = iter_last.call(&mut hooks, Some(this), &[]).unwrap();
      assert_eq!(result, val!(4.0));
    }
  }

  #[cfg(test)]
  mod map {
    use super::*;
    use crate::support::fun_from_hooks;
    use laythe_core::object::Closure;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let iter_map = IterMap::from(&hooks);

      assert_eq!(iter_map.meta().name, "map");
      assert_eq!(iter_map.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        iter_map.meta().signature.parameters[0].kind,
        ParameterKind::Fun
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(5.0)]);
      let mut hooks = Hooks::new(&mut context);
      let iter_map = IterMap::from(&hooks.as_gc());

      let iter = test_iter();
      let managed = hooks.manage(LyIterator::new(iter));
      let this = val!(managed);
      let fun = val!(hooks.manage(Closure::new(fun_from_hooks(
        &hooks.as_gc(),
        "example",
        "module",
      ))));

      fun.to_closure().fun.arity = Arity::Fixed(1);

      let result = iter_map.call(&mut hooks, Some(this), &[fun]);
      match result {
        Call::Ok(r) => {
          let mut map_iter = r.to_iter();
          assert_eq!(map_iter.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(map_iter.current(), val!(5.0));
        }
        _ => assert!(false),
      }
    }
  }

  mod filter {
    use super::*;
    use crate::support::{fun_from_hooks, MockedContext};
    use laythe_core::{iterator::LyIterator, object::Closure};

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let iter_filter = IterFilter::from(&hooks);

      assert_eq!(iter_filter.meta().name, "filter");
      assert_eq!(iter_filter.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        iter_filter.meta().signature.parameters[0].kind,
        ParameterKind::Fun
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(false), val!(true), val!(true)]);
      let mut hooks = Hooks::new(&mut context);
      let iter_filter = IterFilter::from(&hooks.as_gc());

      let iter = test_iter();
      let managed = hooks.manage(LyIterator::new(iter));
      let this = val!(managed);
      let fun = val!(hooks.manage(Closure::new(fun_from_hooks(
        &hooks.as_gc(),
        "example",
        "module",
      ))));

      fun.to_closure().fun.arity = Arity::Fixed(1);

      let result = iter_filter.call(&mut hooks, Some(this), &[fun]);
      match result {
        Call::Ok(r) => {
          let mut filter_iter = r.to_iter();
          assert_eq!(filter_iter.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(filter_iter.current(), val!(2.0));
          assert_eq!(filter_iter.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(filter_iter.current(), val!(3.0));
        }
        _ => assert!(false),
      }
    }
  }

  mod reduce {
    use super::*;
    use crate::support::{fun_from_hooks, MockedContext};
    use laythe_core::{iterator::LyIterator, object::Closure};

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let iter_reduce = IterReduce::from(&hooks);

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
      let mut context = MockedContext::new(&[val!(false), val!(false), val!(false), val!(10.1)]);
      let mut hooks = Hooks::new(&mut context);
      let iter_reduce = IterReduce::from(&hooks.as_gc());

      let iter = test_iter();
      let managed = hooks.manage(LyIterator::new(iter));
      let this = val!(managed);
      let fun = val!(hooks.manage(Closure::new(fun_from_hooks(
        &hooks.as_gc(),
        "example",
        "module",
      ))));

      fun.to_closure().fun.arity = Arity::Fixed(2);

      let result = iter_reduce.call(&mut hooks, Some(this), &[val!(0.0), fun]);
      match result {
        Call::Ok(r) => {
          assert!(r.is_num());
          assert_eq!(r.to_num(), 10.1);
        }
        _ => assert!(false),
      }
    }
  }

  mod len {
    use super::*;
    use crate::support::MockedContext;
    use laythe_core::iterator::LyIterator;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let iter_size = IterLen::from(&hooks);

      assert_eq!(iter_size.meta().name, "len");
      assert_eq!(iter_size.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let iter_size = IterLen::from(&hooks);

      let this = val!(hooks.manage(LyIterator::new(test_iter())));
      let result = iter_size.call(&mut hooks, Some(this), &[]);
      match result {
        Call::Ok(r) => {
          assert!(r.is_num());
          assert_eq!(r.to_num(), 4.0);
        }
        _ => assert!(false),
      }
    }
  }

  mod each {
    use super::*;
    use crate::support::{fun_from_hooks, MockedContext};
    use laythe_core::{iterator::LyIterator, object::Closure};

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let iter_each = IterEach::from(&hooks);

      assert_eq!(iter_each.meta().name, "each");
      assert_eq!(iter_each.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        iter_each.meta().signature.parameters[0].kind,
        ParameterKind::Fun
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(false); 5]);
      let mut hooks = Hooks::new(&mut context);
      let iter_each = IterEach::from(&hooks.as_gc());

      let iter = test_iter();
      let managed = hooks.manage(LyIterator::new(iter));
      let this = val!(managed);
      let fun = val!(hooks.manage(Closure::new(fun_from_hooks(
        &hooks.as_gc(),
        "example",
        "module",
      ))));

      fun.to_closure().fun.arity = Arity::Fixed(1);

      let result = iter_each.call(&mut hooks, Some(this), &[fun]);
      match result {
        Call::Ok(r) => assert!(r.is_nil()),
        _ => assert!(false),
      }
    }
  }

  mod zip {
    use super::*;
    use crate::support::MockedContext;
    use laythe_core::iterator::LyIterator;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let iter_zip = IterZip::from(&hooks);

      assert_eq!(iter_zip.meta().name, "zip");
      assert_eq!(iter_zip.meta().signature.arity, Arity::Variadic(0));
      assert_eq!(
        iter_zip.meta().signature.parameters[0].kind,
        ParameterKind::Iter,
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(1.0); 10]);
      let mut hooks = Hooks::new(&mut context);
      let iter_zip = IterZip::from(&hooks.as_gc());

      let iter = test_iter();
      let managed = hooks.manage(LyIterator::new(iter));
      let this = val!(managed);

      let iter2 = test_iter();
      let managed = hooks.manage(LyIterator::new(iter2));
      let arg = val!(managed);

      let result = iter_zip.call(&mut hooks, Some(this), &[arg]);
      match result {
        Call::Ok(r) => assert!(r.is_iter()),
        _ => assert!(false),
      }

      let mut zip = result.unwrap().to_iter();
      assert!(zip.next(&mut hooks).unwrap().to_bool());
      assert!(zip.current().is_list());
      assert_eq!(zip.current().to_list().len(), 2);
      assert_eq!(zip.current().to_list()[0].to_num(), 1.0);
      assert_eq!(zip.current().to_list()[1].to_num(), 1.0);
    }
  }

  mod chain {
    use super::*;
    use crate::support::MockedContext;
    use laythe_core::iterator::LyIterator;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let iter_chain = IterChain::from(&hooks);

      assert_eq!(iter_chain.meta().name, "chain");
      assert_eq!(iter_chain.meta().signature.arity, Arity::Variadic(0));
      assert_eq!(
        iter_chain.meta().signature.parameters[0].kind,
        ParameterKind::Iter,
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(1.0); 10]);
      let mut hooks = Hooks::new(&mut context);
      let iter_chain = IterChain::from(&hooks.as_gc());

      let iter = test_iter();
      let managed = hooks.manage(LyIterator::new(iter));
      let this = val!(managed);

      let iter2 = test_iter();
      let managed = hooks.manage(LyIterator::new(iter2));
      let arg = val!(managed);

      let result = iter_chain.call(&mut hooks, Some(this), &[arg]);
      match result {
        Call::Ok(r) => assert!(r.is_iter()),
        _ => assert!(false),
      }

      let mut chain = result.unwrap().to_iter();
      let expected = vec![1.0, 2.0, 3.0, 4.0, 1.0, 2.0, 3.0, 4.0];

      for expect in expected {
        assert!(chain.next(&mut hooks).unwrap().to_bool());
        assert!(chain.current().is_num());
        assert_eq!(chain.current().to_num(), expect);
      }

      assert!(!chain.next(&mut hooks).unwrap().to_bool());
    }
  }

  mod all {
    use super::*;
    use crate::support::MockedContext;
    use laythe_core::{iterator::LyIterator, value::VALUE_TRUE};

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let iter_all = IterAll::from(&hooks);

      assert_eq!(iter_all.meta().name, "all");
      assert_eq!(iter_all.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        iter_all.meta().signature.parameters[0].kind,
        ParameterKind::Fun
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(1.0); 10]);
      let mut hooks = Hooks::new(&mut context);
      let gc_hooks = hooks.as_gc();
      let iter_all = IterAll::from(&gc_hooks);

      let identity = val!(to_dyn_native(&gc_hooks, TestFun::from(&gc_hooks)));

      let iter = test_iter();
      let managed = hooks.manage(LyIterator::new(iter));
      let this = val!(managed);

      let result = iter_all.call(&mut hooks, Some(this), &[identity]);
      match result {
        Call::Ok(r) => assert_eq!(r, VALUE_TRUE),
        _ => assert!(false),
      }
    }
  }

  mod any {
    use super::*;
    use crate::support::MockedContext;
    use laythe_core::{iterator::LyIterator, value::VALUE_TRUE};

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let iter_any = IterAny::from(&hooks);

      assert_eq!(iter_any.meta().name, "any");
      assert_eq!(iter_any.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        iter_any.meta().signature.parameters[0].kind,
        ParameterKind::Fun
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(0.0), val!(0.0), val!(1.0)]);
      let mut hooks = Hooks::new(&mut context);
      let gc_hooks = hooks.as_gc();
      let iter_any = IterAny::from(&gc_hooks);

      let identity = val!(to_dyn_native(&gc_hooks, TestFun::from(&gc_hooks)));

      let iter = test_iter();
      let managed = hooks.manage(LyIterator::new(iter));
      let this = val!(managed);

      let result = iter_any.call(&mut hooks, Some(this), &[identity]);
      match result {
        Call::Ok(r) => assert_eq!(r, VALUE_TRUE),
        _ => assert!(false),
      }
    }
  }

  mod into {
    use super::*;
    use crate::support::MockedContext;
    use laythe_core::{iterator::LyIterator, native::Native};

    const M: NativeMetaBuilder = NativeMetaBuilder::fun("", Arity::Fixed(1))
      .with_params(&[ParameterBuilder::new("", ParameterKind::Any)]);

    native!(EchoFun, M);

    impl Native for EchoFun {
      fn call(&self, _hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> Call {
        Call::Ok(args[0])
      }
    }

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let iter_into = IterInto::from(&hooks);

      assert_eq!(iter_into.meta().name, "into");
      assert_eq!(iter_into.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        iter_into.meta().signature.parameters[0].kind,
        ParameterKind::Fun
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(true); 5]);
      let mut hooks = Hooks::new(&mut context);
      let iter_into = IterInto::from(&hooks);

      let iter = test_iter();
      let managed = hooks.manage(LyIterator::new(iter));
      let this = val!(managed);
      let echo = val!(hooks.manage(Box::new(EchoFun::from(&hooks)) as Box<dyn Native>));

      let result = iter_into.call(&mut hooks, Some(this), &[echo]);
      match result {
        Call::Ok(r) => assert_eq!(r.to_bool(), true),
        _ => assert!(false),
      }
    }
  }
}
