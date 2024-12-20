use crate::{
  global::VALUE_ERROR_NAME,
  native, native_with_error,
  support::{export_and_insert, load_class_from_module},
  StdResult,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  list,
  managed::{DebugHeap, DebugWrap, Trace},
  module::Module,
  object::{Enumerate, Enumerator, List, LyNative, Native, NativeMetaBuilder, ObjectKind},
  signature::{Arity, ParameterBuilder, ParameterKind},
  utils::is_falsey,
  val,
  value::{Value, VALUE_NIL},
  Call, LyError, ObjRef, Ref, VecBuilder,
};
use std::io::Write;

use super::class_inheritance;

pub const ITER_CLASS_NAME: &str = "Iter";
const ITER_STR: NativeMetaBuilder = NativeMetaBuilder::method("str", Arity::Fixed(0));

/// This might need to have a stack once we implement yield or the iterator class
const ITER_NEXT: NativeMetaBuilder = NativeMetaBuilder::method("next", Arity::Fixed(0));
const ITER_CURRENT: NativeMetaBuilder = NativeMetaBuilder::method("current", Arity::Fixed(0));
const ITER_ITER: NativeMetaBuilder = NativeMetaBuilder::method("iter", Arity::Fixed(0));

const ITER_FIRST: NativeMetaBuilder = NativeMetaBuilder::method("first", Arity::Fixed(0));
const ITER_LAST: NativeMetaBuilder = NativeMetaBuilder::method("last", Arity::Fixed(0));

const ITER_TAKE: NativeMetaBuilder = NativeMetaBuilder::method("take", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("count", ParameterKind::Number)])
  .with_stack();

const ITER_SKIP: NativeMetaBuilder = NativeMetaBuilder::method("skip", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("count", ParameterKind::Number)])
  .with_stack();

const ITER_MAP: NativeMetaBuilder = NativeMetaBuilder::method("map", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("fun", ParameterKind::Callable)])
  .with_stack();

const ITER_FILTER: NativeMetaBuilder = NativeMetaBuilder::method("filter", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("fun", ParameterKind::Callable)])
  .with_stack();

const ITER_REDUCE: NativeMetaBuilder = NativeMetaBuilder::method("reduce", Arity::Fixed(2))
  .with_params(&[
    ParameterBuilder::new("initial", ParameterKind::Object),
    ParameterBuilder::new("fun", ParameterKind::Callable),
  ])
  .with_stack();

const ITER_LEN: NativeMetaBuilder = NativeMetaBuilder::method("len", Arity::Fixed(0));

const ITER_EACH: NativeMetaBuilder = NativeMetaBuilder::method("each", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("fun", ParameterKind::Callable)])
  .with_stack();

const ITER_ZIP: NativeMetaBuilder = NativeMetaBuilder::method("zip", Arity::Variadic(0))
  .with_params(&[ParameterBuilder::new("iterators", ParameterKind::Object)]);

const ITER_CHAIN: NativeMetaBuilder = NativeMetaBuilder::method("chain", Arity::Variadic(0))
  .with_params(&[ParameterBuilder::new("iterators", ParameterKind::Object)]);

const ITER_ALL: NativeMetaBuilder = NativeMetaBuilder::method("all", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("fun", ParameterKind::Callable)])
  .with_stack();

const ITER_ANY: NativeMetaBuilder = NativeMetaBuilder::method("any", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("fun", ParameterKind::Callable)])
  .with_stack();

const ITER_LIST: NativeMetaBuilder = NativeMetaBuilder::method("list", Arity::Fixed(0));

const ITER_INTO: NativeMetaBuilder = NativeMetaBuilder::method("into", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("fun", ParameterKind::Callable)])
  .with_stack();

pub fn declare_iter_class(hooks: &GcHooks, module: Ref<Module>) -> StdResult<()> {
  let class = class_inheritance(hooks, module, ITER_CLASS_NAME)?;
  export_and_insert(hooks, module, class.name(), val!(class))
}

pub fn define_iter_class(hooks: &GcHooks, module: Ref<Module>) -> StdResult<()> {
  let mut class = load_class_from_module(hooks, module, ITER_CLASS_NAME)?;
  let value_error = val!(load_class_from_module(hooks, module, VALUE_ERROR_NAME)?);

  class.add_method(
    hooks.manage_str(ITER_STR.name),
    val!(IterStr::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(ITER_NEXT.name),
    val!(IterNext::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(ITER_CURRENT.name),
    val!(IterCurrent::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(ITER_ITER.name),
    val!(IterIter::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(ITER_FIRST.name),
    val!(IterFirst::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(ITER_LAST.name),
    val!(IterLast::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(ITER_TAKE.name),
    val!(IterTake::native(hooks, value_error)),
  );

  class.add_method(
    hooks.manage_str(ITER_SKIP.name),
    val!(IterSkip::native(hooks, value_error)),
  );

  class.add_method(
    hooks.manage_str(ITER_MAP.name),
    val!(IterMap::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(ITER_FILTER.name),
    val!(IterFilter::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(ITER_REDUCE.name),
    val!(IterReduce::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(ITER_LEN.name),
    val!(IterLen::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(ITER_EACH.name),
    val!(IterEach::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(ITER_ZIP.name),
    val!(IterZip::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(ITER_CHAIN.name),
    val!(IterChain::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(ITER_ALL.name),
    val!(IterAll::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(ITER_ANY.name),
    val!(IterAny::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(ITER_LIST.name),
    val!(IterToList::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(ITER_INTO.name),
    val!(IterInto::native(hooks)),
  );

  Ok(())
}

native!(IterStr, ITER_STR);

impl LyNative for IterStr {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    Call::Ok(val!(
      hooks.manage_str(args[0].to_obj().to_enumerator().name())
    ))
  }
}

native!(IterNext, ITER_NEXT);

impl LyNative for IterNext {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    args[0].to_obj().to_enumerator().next(hooks)
  }
}

native!(IterCurrent, ITER_CURRENT);

impl LyNative for IterCurrent {
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
    Call::Ok(args[0].to_obj().to_enumerator().current())
  }
}

native!(IterIter, ITER_ITER);

impl LyNative for IterIter {
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
    Call::Ok(args[0])
  }
}

native!(IterFirst, ITER_FIRST);

impl LyNative for IterFirst {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let mut iter = args[0].to_obj().to_enumerator();

    if !is_falsey(iter.next(hooks)?) {
      let current = iter.current();
      Call::Ok(current)
    } else {
      Call::Ok(VALUE_NIL)
    }
  }
}

native!(IterLast, ITER_LAST);

impl LyNative for IterLast {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let mut iter = args[0].to_obj().to_enumerator();
    let mut result = VALUE_NIL;

    while !is_falsey(iter.next(hooks)?) {
      result = iter.current();
    }

    Call::Ok(result)
  }
}

native_with_error!(IterTake, ITER_TAKE);

impl LyNative for IterTake {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let iter = args[0].to_obj().to_enumerator();
    let take_count = args[1].to_num();

    if take_count.fract() != 0.0 {
      return Call::Err(
        self
          .call_error(hooks, "Method skip takes an integer parameter.")
          .expect_err("Expected Err"),
      );
    }

    let take_count = take_count as usize;
    let inner_iter: Box<dyn Enumerate> = Box::new(TakeIterator::new(iter, take_count));
    let take_iter = hooks.manage_obj(Enumerator::new(inner_iter));
    Call::Ok(val!(take_iter))
  }
}

#[derive(Debug)]
struct TakeIterator {
  current: usize,
  iter: ObjRef<Enumerator>,
  take_count: usize,
}

impl TakeIterator {
  fn new(iter: ObjRef<Enumerator>, take_count: usize) -> Self {
    Self {
      current: 0,
      iter,
      take_count,
    }
  }
}

impl Enumerate for TakeIterator {
  fn name(&self) -> &str {
    "Take"
  }

  fn current(&self) -> Value {
    self.iter.current()
  }

  fn next(&mut self, hooks: &mut Hooks) -> Call {
    if self.current >= self.take_count || is_falsey(self.iter.next(hooks)?) {
      Call::Ok(val!(false))
    } else {
      self.current += 1;
      Call::Ok(val!(true))
    }
  }

  fn size_hint(&self) -> Option<usize> {
    self.iter.size_hint().map(|hint| hint.min(self.take_count))
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

impl Trace for TakeIterator {
  fn trace(&self) {
    self.iter.trace();
  }

  fn trace_debug(&self, log: &mut dyn Write) {
    self.iter.trace_debug(log)
  }
}

impl DebugHeap for TakeIterator {
  fn fmt_heap(&self, f: &mut std::fmt::Formatter, depth: usize) -> std::fmt::Result {
    f.debug_struct("TakeIterator")
      .field("current", &self.current)
      .field("iter", &DebugWrap(&self.iter, depth))
      .field("take_count", &self.take_count)
      .finish()
  }
}

native_with_error!(IterSkip, ITER_SKIP);

impl LyNative for IterSkip {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let mut iter = args[0].to_obj().to_enumerator();
    let skip_count = args[1].to_num();

    if skip_count.fract() != 0.0 {
      return Call::Err(
        self
          .call_error(
            hooks,
            "Method skip takes an non negative integer parameter.",
          )
          .expect_err("Expected Err"),
      );
    }

    if skip_count < 0.0 {
      return Call::Err(
        self
          .call_error(
            hooks,
            "Method skip takes an non negative integer parameter.",
          )
          .expect_err("Expected Err"),
      );
    }

    let mut current = 0usize;
    let skip_count = skip_count as usize;

    while current < skip_count && !is_falsey(iter.next(hooks)?) {
      current += 1;
    }

    let inner_iter: Box<dyn Enumerate> = Box::new(SkipIterator::new(iter, skip_count));
    let skip_iter = hooks.manage_obj(Enumerator::new(inner_iter));
    Call::Ok(val!(skip_iter))
  }
}

#[derive(Debug)]
struct SkipIterator {
  skip_count: usize,
  iter: ObjRef<Enumerator>,
}

impl SkipIterator {
  fn new(iter: ObjRef<Enumerator>, skip_count: usize) -> Self {
    Self { skip_count, iter }
  }
}

impl Enumerate for SkipIterator {
  fn name(&self) -> &str {
    "Skip"
  }

  fn current(&self) -> Value {
    self.iter.current()
  }

  fn next(&mut self, hooks: &mut Hooks) -> Call {
    self.iter.next(hooks)
  }

  fn size_hint(&self) -> Option<usize> {
    self
      .iter
      .size_hint()
      .map(|hint| hint.saturating_sub(self.skip_count))
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

impl Trace for SkipIterator {
  fn trace(&self) {
    self.iter.trace();
  }

  fn trace_debug(&self, log: &mut dyn Write) {
    self.iter.trace_debug(log)
  }
}

impl DebugHeap for SkipIterator {
  fn fmt_heap(&self, f: &mut std::fmt::Formatter, depth: usize) -> std::fmt::Result {
    f.debug_struct("SkipIterator")
      .field("skip_count", &self.skip_count)
      .field("iter", &DebugWrap(&self.iter, depth))
      .finish()
  }
}

native!(IterMap, ITER_MAP);

impl LyNative for IterMap {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let inner_iter: Box<dyn Enumerate> =
      Box::new(MapIterator::new(args[0].to_obj().to_enumerator(), args[1]));
    let iter = Enumerator::new(inner_iter);
    let iter = hooks.manage_obj(iter);

    Call::Ok(val!(iter))
  }
}

#[derive(Debug)]
struct MapIterator {
  current: Value,
  iter: ObjRef<Enumerator>,
  callable: Value,
}

impl MapIterator {
  fn new(iter: ObjRef<Enumerator>, callable: Value) -> Self {
    Self {
      current: VALUE_NIL,
      iter,
      callable,
    }
  }
}

impl Enumerate for MapIterator {
  fn name(&self) -> &str {
    "Map"
  }

  fn current(&self) -> Value {
    self.current
  }

  fn next(&mut self, hooks: &mut Hooks) -> Call {
    if is_falsey(self.iter.next(hooks)?) {
      Call::Ok(val!(false))
    } else {
      let current = self.iter.current();
      self.current = hooks.call(self.callable, &[current])?;
      Call::Ok(val!(true))
    }
  }

  fn size_hint(&self) -> Option<usize> {
    self.iter.size_hint()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

impl Trace for MapIterator {
  fn trace(&self) {
    self.current.trace();
    self.iter.trace();
    self.callable.trace();
  }

  fn trace_debug(&self, stdout: &mut dyn Write) {
    self.current.trace_debug(stdout);
    self.iter.trace_debug(stdout);
    self.callable.trace_debug(stdout);
  }
}

impl DebugHeap for MapIterator {
  fn fmt_heap(&self, f: &mut std::fmt::Formatter, depth: usize) -> std::fmt::Result {
    f.debug_struct("MapIterator")
      .field("current", &DebugWrap(&self.current, depth))
      .field("iter", &DebugWrap(&self.iter, depth))
      .field("callable", &DebugWrap(&self.callable, depth))
      .finish()
  }
}

native!(IterFilter, ITER_FILTER);

impl LyNative for IterFilter {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let inner_iter: Box<dyn Enumerate> = Box::new(FilterIterator::new(
      args[0].to_obj().to_enumerator(),
      args[1],
    ));
    let iter = Enumerator::new(inner_iter);
    let iter = hooks.manage_obj(iter);

    Call::Ok(val!(iter))
  }
}

#[derive(Debug)]
struct FilterIterator {
  current: Value,
  iter: ObjRef<Enumerator>,
  callable: Value,
}

impl FilterIterator {
  fn new(iter: ObjRef<Enumerator>, callable: Value) -> Self {
    Self {
      current: VALUE_NIL,
      iter,
      callable,
    }
  }
}

impl Enumerate for FilterIterator {
  fn name(&self) -> &str {
    "Filter"
  }

  fn current(&self) -> Value {
    self.current
  }

  fn next(&mut self, hooks: &mut Hooks) -> Call {
    while !is_falsey(self.iter.next(hooks)?) {
      let current = self.iter.current();

      if !is_falsey(hooks.call(self.callable, &[current])?) {
        self.current = current;
        return Call::Ok(val!(true));
      }
    }

    Call::Ok(val!(false))
  }

  fn size_hint(&self) -> Option<usize> {
    None
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

impl Trace for FilterIterator {
  fn trace(&self) {
    self.current.trace();
    self.iter.trace();
    self.callable.trace();
  }

  fn trace_debug(&self, stdout: &mut dyn Write) {
    self.current.trace_debug(stdout);
    self.iter.trace_debug(stdout);
    self.callable.trace_debug(stdout);
  }
}

impl DebugHeap for FilterIterator {
  fn fmt_heap(&self, f: &mut std::fmt::Formatter, depth: usize) -> std::fmt::Result {
    f.debug_struct("FilterIterator")
      .field("current", &DebugWrap(&self.current, depth))
      .field("iter", &DebugWrap(&self.iter, depth))
      .field("callable", &DebugWrap(&self.callable, depth))
      .finish()
  }
}

native!(IterReduce, ITER_REDUCE);

impl LyNative for IterReduce {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let mut accumulator = args[1];
    let callable = args[2];

    hooks.push_root(accumulator);
    hooks.push_root(callable);

    let mut iter = args[0].to_obj().to_enumerator();

    while !is_falsey(iter.next(hooks)?) {
      let current = iter.current();
      accumulator = hooks.call(callable, &[accumulator, current])?;
    }

    hooks.pop_roots(2);

    Call::Ok(accumulator)
  }
}

native!(IterLen, ITER_LEN);

impl LyNative for IterLen {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let mut iter = args[0].to_obj().to_enumerator();

    match iter.size_hint() {
      Some(size) => Call::Ok(val!(size as f64)),
      None => {
        let mut size: usize = 0;
        while !is_falsey(iter.next(hooks)?) {
          size += 1;
        }

        Call::Ok(val!(size as f64))
      },
    }
  }
}

native!(IterEach, ITER_EACH);

impl LyNative for IterEach {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let callable = args[1];
    let mut iter = args[0].to_obj().to_enumerator();

    hooks.push_root(callable);

    while !is_falsey(iter.next(hooks)?) {
      let current = iter.current();
      hooks.call(callable, &[current])?;
    }

    hooks.pop_roots(1);

    Call::Ok(VALUE_NIL)
  }
}

native!(IterZip, ITER_ZIP);

impl LyNative for IterZip {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let iters: Vec<ObjRef<Enumerator>> = args
      .iter()
      .map(|arg| arg.to_obj().to_enumerator())
      .collect();

    let inner_iter: Box<dyn Enumerate> = Box::new(ZipIterator::new(iters));
    let iter = Enumerator::new(inner_iter);
    let iter = hooks.manage_obj(iter);

    Call::Ok(val!(iter))
  }
}

#[derive(Debug)]
struct ZipIterator {
  current: Value,
  iters: Vec<ObjRef<Enumerator>>,
}

impl ZipIterator {
  fn new(iters: Vec<ObjRef<Enumerator>>) -> Self {
    Self {
      current: VALUE_NIL,
      iters,
    }
  }
}

impl Enumerate for ZipIterator {
  fn name(&self) -> &str {
    "Zip"
  }

  fn current(&self) -> Value {
    self.current
  }

  fn next(&mut self, hooks: &mut Hooks) -> Call {
    let mut results = hooks.manage_obj(&*vec![VALUE_NIL; self.iters.len()]);

    hooks.push_root(results);
    for (iter, slot) in &mut self.iters.iter_mut().zip(results.iter_mut()) {
      let next = iter.next(hooks)?;

      if is_falsey(next) {
        hooks.pop_roots(1);
        return Call::Ok(val!(false));
      }

      *slot = iter.current();
    }
    hooks.pop_roots(1);

    self.current = val!(results);
    Call::Ok(val!(true))
  }

  fn size_hint(&self) -> Option<usize> {
    use std::cmp;

    self.iters.iter().try_fold(usize::MAX, |acc, current| {
      current.size_hint().map(|current| cmp::min(acc, current))
    })
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

impl Trace for ZipIterator {
  fn trace(&self) {
    self.current.trace();
    self.iters.iter().for_each(|iter| {
      iter.trace();
    });
  }

  fn trace_debug(&self, stdout: &mut dyn Write) {
    self.current.trace_debug(stdout);
    self.iters.iter().for_each(|iter| {
      iter.trace_debug(stdout);
    });
  }
}

impl DebugHeap for ZipIterator {
  fn fmt_heap(&self, f: &mut std::fmt::Formatter, depth: usize) -> std::fmt::Result {
    let iter = &f
      .debug_list()
      .entries(self.iters.iter().map(|iter| DebugWrap(iter, depth)))
      .finish();

    f.debug_struct("ZipIterator")
      .field("current", &DebugWrap(&self.current, depth))
      .field("iter", iter)
      .finish()
  }
}

native!(IterChain, ITER_CHAIN);

impl LyNative for IterChain {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let iters: Vec<ObjRef<Enumerator>> = args
      .iter()
      .map(|arg| arg.to_obj().to_enumerator())
      .collect();

    let inner_iter: Box<dyn Enumerate> = Box::new(ChainIterator::new(iters));
    let iter = Enumerator::new(inner_iter);
    let iter = hooks.manage_obj(iter);

    Call::Ok(val!(iter))
  }
}

#[derive(Debug)]
struct ChainIterator {
  current: Value,
  iter_index: usize,
  iters: Vec<ObjRef<Enumerator>>,
}

impl ChainIterator {
  fn new(iters: Vec<ObjRef<Enumerator>>) -> Self {
    Self {
      current: VALUE_NIL,
      iter_index: 0,
      iters,
    }
  }
}

impl Enumerate for ChainIterator {
  fn name(&self) -> &str {
    "Chain"
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
      let next = iter.next(hooks)?;
      if !is_falsey(next) {
        self.current = iter.current();
        break;
      }

      self.iter_index += 1;
    }

    Call::Ok(val!(true))
  }

  fn size_hint(&self) -> Option<usize> {
    self.iters.iter().try_fold(0, |acc, current| {
      current.size_hint().map(|current| acc + current)
    })
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

impl Trace for ChainIterator {
  fn trace(&self) {
    self.current.trace();
    self.iters.iter().for_each(|iter| {
      iter.trace();
    });
  }

  fn trace_debug(&self, stdout: &mut dyn Write) {
    self.current.trace_debug(stdout);
    self.iters.iter().for_each(|iter| {
      iter.trace_debug(stdout);
    });
  }
}

impl DebugHeap for ChainIterator {
  fn fmt_heap(&self, f: &mut std::fmt::Formatter, depth: usize) -> std::fmt::Result {
    let iter = &f
      .debug_list()
      .entries(self.iters.iter().map(|iter| DebugWrap(iter, depth)))
      .finish();

    f.debug_struct("ChainIterator")
      .field("current", &DebugWrap(&self.current, depth))
      .field("iter_index", &self.iter_index)
      .field("iter", iter)
      .finish()
  }
}

native!(IterAll, ITER_ALL);

impl LyNative for IterAll {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let callable = args[1];
    let mut iter = args[0].to_obj().to_enumerator();

    hooks.push_root(callable);

    while !is_falsey(iter.next(hooks)?) {
      let current = iter.current();
      if is_falsey(hooks.call(callable, &[current])?) {
        hooks.pop_roots(1);
        return Call::Ok(val!(false));
      }
    }

    hooks.pop_roots(1);
    Call::Ok(val!(true))
  }
}

native!(IterAny, ITER_ANY);

impl LyNative for IterAny {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let callable = args[1];
    let mut iter = args[0].to_obj().to_enumerator();

    hooks.push_root(callable);

    while !is_falsey(iter.next(hooks)?) {
      let current = iter.current();
      if !is_falsey(hooks.call(callable, &[current])?) {
        hooks.pop_roots(1);
        return Call::Ok(val!(true));
      }
    }

    hooks.pop_roots(1);
    Call::Ok(val!(false))
  }
}

native!(IterToList, ITER_LIST);

impl LyNative for IterToList {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let mut iter = args[0].to_obj().to_enumerator();
    let mut list = List::new(match iter.size_hint() {
      Some(size) => hooks.manage_obj(VecBuilder::cap_only(size)),
      None => hooks.manage_obj(list!()),
    });

    hooks.push_root(list);

    while !is_falsey(iter.next(hooks)?) {
      list.push(iter.current(), &hooks.as_gc());
    }

    hooks.pop_roots(1);
    Call::Ok(val!(list))
  }
}

native!(IterInto, ITER_INTO);

impl LyNative for IterInto {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let callable = args[1];
    let iter = args[0].to_obj().to_enumerator();

    hooks.push_root(iter);
    hooks.push_root(callable);
    let result = hooks.call(callable, &[args[0]]);
    hooks.pop_roots(2);

    result
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::support::{test_iter, MockedContext};
  use laythe_core::object::Enumerator;

  const TEST_FUN: NativeMetaBuilder = NativeMetaBuilder::fun("test", Arity::Fixed(1))
    .with_params(&[ParameterBuilder::new("any", ParameterKind::Object)]);

  native!(TestFun, TEST_FUN);

  impl LyNative for TestFun {
    fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
      Call::Ok(args[0])
    }
  }

  #[cfg(test)]
  mod str {
    use super::*;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let iter_str = IterStr::native(&hooks.as_gc());

      let iter = test_iter();
      let this = hooks.manage_obj(Enumerator::new(iter));

      let result = iter_str.call(&mut hooks, &[val!(this)]);
      match result {
        Call::Ok(r) => assert_eq!(&*r.to_obj().to_str(), "Test"),
        _ => panic!(),
      }
    }
  }

  #[cfg(test)]
  mod next {
    use super::*;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let iter_next = IterNext::native(&hooks.as_gc());

      let iter = test_iter();
      let this = hooks.manage_obj(Enumerator::new(iter));

      let result = iter_next.call(&mut hooks, &[val!(this)]);
      match result {
        Call::Ok(r) => assert!(r.to_bool()),
        _ => panic!(),
      }
    }
  }

  #[cfg(test)]
  mod current {
    use super::*;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let iter_current = IterCurrent::native(&hooks.as_gc());

      let iter = test_iter();
      let this = hooks.manage_obj(Enumerator::new(iter));

      let result = iter_current.call(&mut hooks, &[val!(this)]).unwrap();
      assert!(result.is_nil());
    }
  }

  #[cfg(test)]
  mod iter {
    use super::*;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let iter_iter = IterIter::native(&hooks.as_gc());

      let iter = test_iter();
      let managed = hooks.manage_obj(Enumerator::new(iter));
      let this = val!(managed);

      let result = iter_iter.call(&mut hooks, &[this]);
      match result {
        Call::Ok(r) => assert_eq!(r, this),
        _ => panic!(),
      }
    }
  }

  #[cfg(test)]
  mod first {
    use super::*;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let iter_first = IterFirst::native(&hooks.as_gc());

      let iter = test_iter();
      let managed = hooks.manage_obj(Enumerator::new(iter));
      let this = val!(managed);

      let result = iter_first.call(&mut hooks, &[this]).unwrap();
      assert_eq!(result, val!(1.0));
    }
  }

  #[cfg(test)]
  mod take {
    use crate::support::test_error_class;

    use super::*;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let error = val!(test_error_class(&hooks.as_gc()));

      let iter_take = IterTake::native(&hooks.as_gc(), error);

      let iter = test_iter();
      let managed = hooks.manage_obj(Enumerator::new(iter));
      let this = val!(managed);

      let result = iter_take.call(&mut hooks, &[this, val!(1.0)]).unwrap();

      assert!(result.is_obj_kind(ObjectKind::Enumerator));

      let mut iter = result.to_obj().to_enumerator();
      assert_eq!(iter.next(&mut hooks).unwrap(), val!(true));
      assert_eq!(iter.current(), val!(1.0));
      assert_eq!(iter.next(&mut hooks).unwrap(), val!(false));
    }
  }

  #[cfg(test)]
  mod skip {
    use crate::support::test_error_class;

    use super::*;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let error = val!(test_error_class(&hooks.as_gc()));

      let iter_skip = IterSkip::native(&hooks.as_gc(), error);

      let iter = test_iter();
      let managed = hooks.manage_obj(Enumerator::new(iter));
      let this = val!(managed);

      let result = iter_skip.call(&mut hooks, &[this, val!(2.0)]).unwrap();

      let mut iter = result.to_obj().to_enumerator();
      assert_eq!(iter.next(&mut hooks).unwrap(), val!(true));
      assert_eq!(iter.current(), val!(3.0));
      assert_eq!(iter.next(&mut hooks).unwrap(), val!(true));
      assert_eq!(iter.current(), val!(4.0));
      assert_eq!(iter.next(&mut hooks).unwrap(), val!(false));
    }
  }

  #[cfg(test)]
  mod last {
    use super::*;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let iter_last = IterLast::native(&hooks.as_gc());

      let iter = test_iter();
      let managed = hooks.manage_obj(Enumerator::new(iter));
      let this = val!(managed);

      let result = iter_last.call(&mut hooks, &[this]).unwrap();
      assert_eq!(result, val!(4.0));
    }
  }

  #[cfg(test)]
  mod map {
    use super::*;
    use crate::support::test_fun_builder;
    use laythe_core::{object::Closure, Captures, Chunk};

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(5.0)]);
      let mut hooks = Hooks::new(&mut context);
      let iter_map = IterMap::native(&hooks.as_gc());

      let iter = test_iter();
      let managed = hooks.manage_obj(Enumerator::new(iter));
      let this = val!(managed);
      let builder = test_fun_builder(&hooks.as_gc(), "example", "module", Arity::Fixed(1));
      let captures = Captures::build(&hooks.as_gc(), &[]);

      let fun = val!(hooks.manage_obj(Closure::new(
        hooks.manage_obj(builder.build(Chunk::stub(&hooks.as_gc()))),
        captures
      )));

      let result = iter_map.call(&mut hooks, &[this, fun]);
      match result {
        Call::Ok(r) => {
          let mut map_iter = r.to_obj().to_enumerator();
          assert_eq!(map_iter.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(map_iter.current(), val!(5.0));
        },
        _ => panic!(),
      }
    }
  }

  mod filter {
    use super::*;
    use crate::support::{test_fun_builder, MockedContext};
    use laythe_core::{
      object::{Closure, Enumerator},
      Captures, Chunk,
    };

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(false), val!(true), val!(true)]);
      let mut hooks = Hooks::new(&mut context);
      let iter_filter = IterFilter::native(&hooks.as_gc());

      let iter = test_iter();
      let managed = hooks.manage_obj(Enumerator::new(iter));
      let this = val!(managed);
      let builder = test_fun_builder(&hooks.as_gc(), "example", "module", Arity::Fixed(1));
      let captures = Captures::build(&hooks.as_gc(), &[]);

      let fun = val!(hooks.manage_obj(Closure::new(
        hooks.manage_obj(builder.build(Chunk::stub(&hooks.as_gc()))),
        captures
      )));

      let result = iter_filter.call(&mut hooks, &[this, fun]);
      match result {
        Call::Ok(r) => {
          let mut filter_iter = r.to_obj().to_enumerator();
          assert_eq!(filter_iter.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(filter_iter.current(), val!(2.0));
          assert_eq!(filter_iter.next(&mut hooks).unwrap(), val!(true));
          assert_eq!(filter_iter.current(), val!(3.0));
        },
        _ => panic!(),
      }
    }
  }

  mod reduce {
    use super::*;
    use crate::support::{test_fun_builder, MockedContext};
    use laythe_core::{object::{Closure, Enumerator}, Captures, Chunk};

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(false), val!(false), val!(false), val!(10.1)]);
      let mut hooks = Hooks::new(&mut context);
      let iter_reduce = IterReduce::native(&hooks.as_gc());

      let iter = test_iter();
      let managed = hooks.manage_obj(Enumerator::new(iter));
      let this = val!(managed);

      let builder = test_fun_builder(&hooks.as_gc(), "example", "module", Arity::Fixed(2));
      let captures = Captures::build(&hooks.as_gc(), &[]);

      let fun = val!(hooks.manage_obj(Closure::new(
        hooks.manage_obj(builder.build(Chunk::stub(&hooks.as_gc()))),
        captures
      )));

      let result = iter_reduce.call(&mut hooks, &[this, val!(0.0), fun]);
      match result {
        Call::Ok(r) => {
          assert!(r.is_num());
          assert_eq!(r.to_num(), 10.1);
        },
        _ => panic!(),
      }
    }
  }

  mod len {
    use super::*;
    use crate::support::MockedContext;
    use laythe_core::object::Enumerator;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);
      let iter_size = IterLen::native(&hooks.as_gc());

      let this = val!(hooks.manage_obj(Enumerator::new(test_iter())));
      let result = iter_size.call(&mut hooks, &[this]);
      match result {
        Call::Ok(r) => {
          assert!(r.is_num());
          assert_eq!(r.to_num(), 4.0);
        },
        _ => panic!(),
      }
    }
  }

  mod each {
    use super::*;
    use crate::support::{test_fun_builder, MockedContext};
    use laythe_core::{
      object::{Closure, Enumerator}, Captures, Chunk,
    };

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(false); 5]);
      let mut hooks = Hooks::new(&mut context);
      let iter_each = IterEach::native(&hooks.as_gc());

      let iter = test_iter();
      let managed = hooks.manage_obj(Enumerator::new(iter));
      let this = val!(managed);

      let builder = test_fun_builder(&hooks.as_gc(), "example", "module", Arity::Fixed(1));
      let captures = Captures::build(&hooks.as_gc(), &[]);

      let fun = val!(hooks.manage_obj(Closure::new(
        hooks.manage_obj(builder.build(Chunk::stub(&hooks.as_gc()))),
        captures
      )));

      let result = iter_each.call(&mut hooks, &[this, fun]);
      match result {
        Call::Ok(r) => assert!(r.is_nil()),
        _ => panic!(),
      }
    }
  }

  mod zip {
    use super::*;
    use crate::support::MockedContext;
    use laythe_core::object::Enumerator;

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(1.0); 10]);
      let mut hooks = Hooks::new(&mut context);
      let iter_zip = IterZip::native(&hooks.as_gc());

      let iter = test_iter();
      let managed = hooks.manage_obj(Enumerator::new(iter));
      let this = val!(managed);

      let iter2 = test_iter();
      let managed = hooks.manage_obj(Enumerator::new(iter2));
      let arg = val!(managed);

      let result = iter_zip.call(&mut hooks, &[this, arg]);
      match result {
        Call::Ok(r) => assert!(r.is_obj_kind(ObjectKind::Enumerator)),
        _ => panic!(),
      }

      let mut zip = result.unwrap().to_obj().to_enumerator();
      assert!(zip.next(&mut hooks).unwrap().to_bool());
      assert!(zip.current().is_obj_kind(ObjectKind::Tuple));
      assert_eq!(zip.current().to_obj().to_tuple().len(), 2);
      assert_eq!(zip.current().to_obj().to_tuple()[0].to_num(), 1.0);
      assert_eq!(zip.current().to_obj().to_tuple()[1].to_num(), 1.0);
    }
  }

  mod chain {
    use super::*;
    use crate::support::MockedContext;
    use laythe_core::object::Enumerator;

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(1.0); 10]);
      let mut hooks = Hooks::new(&mut context);
      let iter_chain = IterChain::native(&hooks.as_gc());

      let iter = test_iter();
      let managed = hooks.manage_obj(Enumerator::new(iter));
      let this = val!(managed);

      let iter2 = test_iter();
      let managed = hooks.manage_obj(Enumerator::new(iter2));
      let arg = val!(managed);

      let result = iter_chain.call(&mut hooks, &[this, arg]);
      match result {
        Call::Ok(r) => assert!(r.is_obj_kind(ObjectKind::Enumerator)),
        _ => panic!(),
      }

      let mut chain = result.unwrap().to_obj().to_enumerator();
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
    use laythe_core::{object::Enumerator, value::VALUE_TRUE};

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(1.0); 10]);
      let mut hooks = Hooks::new(&mut context);
      let gc_hooks = hooks.as_gc();
      let iter_all = IterAll::native(&gc_hooks);

      let identity = val!(TestFun::native(&gc_hooks));

      let iter = test_iter();
      let managed = hooks.manage_obj(Enumerator::new(iter));
      let this = val!(managed);

      let result = iter_all.call(&mut hooks, &[this, identity]);
      match result {
        Call::Ok(r) => assert_eq!(r, VALUE_TRUE),
        _ => panic!(),
      }
    }
  }

  mod any {
    use super::*;
    use crate::support::MockedContext;
    use laythe_core::{object::Enumerator, value::VALUE_TRUE};

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(0.0), val!(0.0), val!(1.0)]);
      let mut hooks = Hooks::new(&mut context);
      let gc_hooks = hooks.as_gc();
      let iter_any = IterAny::native(&gc_hooks);

      let identity = val!(TestFun::native(&gc_hooks));

      let iter = test_iter();
      let managed = hooks.manage_obj(Enumerator::new(iter));
      let this = val!(managed);

      let result = iter_any.call(&mut hooks, &[this, identity]);
      match result {
        Call::Ok(r) => assert_eq!(r, VALUE_TRUE),
        _ => panic!(),
      }
    }
  }

  mod into {
    use super::*;
    use crate::support::MockedContext;
    use laythe_core::object::{Enumerator, LyNative};

    const M: NativeMetaBuilder = NativeMetaBuilder::fun("", Arity::Fixed(1))
      .with_params(&[ParameterBuilder::new("", ParameterKind::Object)]);

    native!(EchoFun, M);

    impl LyNative for EchoFun {
      fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
        Call::Ok(args[0])
      }
    }

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(true); 5]);
      let mut hooks = Hooks::new(&mut context);
      let iter_into = IterInto::native(&hooks.as_gc());

      let iter = test_iter();
      let managed = hooks.manage_obj(Enumerator::new(iter));
      let this = val!(managed);
      let echo = val!(EchoFun::native(&hooks.as_gc()));

      let result = iter_into.call(&mut hooks, &[this, echo]);
      match result {
        Call::Ok(r) => assert!(r.to_bool()),
        _ => panic!(),
      }
    }
  }

  mod to_list {
    use super::*;
    use crate::support::MockedContext;
    use laythe_core::object::Enumerator;

    #[test]
    fn call() {
      let mut context = MockedContext::new(&[val!(true); 5]);
      let mut hooks = Hooks::new(&mut context);
      let iter_to_list = IterToList::native(&hooks.as_gc());

      let iter = test_iter();
      let managed = hooks.manage_obj(Enumerator::new(iter));
      let this = val!(managed);

      let result = iter_to_list.call(&mut hooks, &[this]).unwrap();

      assert!(result.is_obj_kind(ObjectKind::List));
      let list = result.to_obj().to_list();

      assert_eq!(list.len(), 4);
      assert_eq!(list[0], val!(1.0));
      assert_eq!(list[1], val!(2.0));
      assert_eq!(list[2], val!(3.0));
      assert_eq!(list[3], val!(4.0));
    }
  }
}
