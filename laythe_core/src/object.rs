use crate::{
  chunk::{AlignedByteCode, Chunk},
  constants::{INDEX_GET, INDEX_SET, INIT},
  dynamic_map::DynamicMap,
  hooks::GcHooks,
  module::Module,
  signature::Arity,
  value::Value,
  value::VALUE_NIL,
};
use core::slice;
use fnv::FnvBuildHasher;
use hash_map::Entry;
use hashbrown::{hash_map, HashMap};
use laythe_env::managed::{DebugHeap, DebugWrap, Manage, Managed, Trace};
use slice::SliceIndex;
use smol_str::SmolStr;
use std::{
  cmp::Ordering,
  fmt,
  hash::Hash,
  io::Write,
  iter::FromIterator,
  mem,
  ops::Deref,
  ops::{Index, IndexMut},
  ptr::NonNull,
};

#[derive(PartialEq, Clone, Debug)]
pub enum Upvalue {
  Open(NonNull<Value>),
  Closed(Value),
}

impl Upvalue {
  /// Close over the upvalue by moving it onto the stack to the heap
  ///
  /// # Examples
  /// ```
  /// use laythe_core::val;
  /// use laythe_core::value::Value;
  /// use laythe_core::object::Upvalue;
  /// use std::rc::Rc;
  /// use std::ptr::NonNull;
  ///
  /// let value = val!(10.0);
  ///
  /// let mut upvalue = Upvalue::Open(NonNull::from(&value));
  /// upvalue.hoist();
  ///
  /// match upvalue {
  ///   Upvalue::Closed(store) => assert_eq!(store, val!(10.0)),
  ///   Upvalue::Open(_) => assert!(false),
  /// };
  /// ```
  pub fn hoist(&mut self) {
    match self {
      Upvalue::Open(stack_ptr) => {
        let value = *unsafe { stack_ptr.as_ref() };
        *self = Upvalue::Closed(value);
      }
      Upvalue::Closed(_) => panic!("Attempted to hoist already hoisted upvalue."),
    }
  }

  /// Is this upvalue currently open
  ///
  /// # Examples
  /// ```
  /// use laythe_core::value::{Value, VALUE_NIL};
  /// use laythe_core::object::Upvalue;
  /// use std::ptr::NonNull;
  ///
  /// let value = VALUE_NIL;
  ///
  /// let upvalue = Upvalue::Open(NonNull::from(&value));
  /// assert_eq!(upvalue.is_open(), true);
  /// ```
  pub fn is_open(&self) -> bool {
    match self {
      Upvalue::Open(_) => true,
      Upvalue::Closed(_) => false,
    }
  }

  pub fn value(&self) -> Value {
    match self {
      Upvalue::Open(stack_ptr) => *unsafe { stack_ptr.as_ref() },
      Upvalue::Closed(store) => *store,
    }
  }
}

impl Trace for Upvalue {
  fn trace(&self) -> bool {
    match self {
      Upvalue::Closed(upvalue) => upvalue.trace(),
      _ => true,
    }
  }

  fn trace_debug(&self, stdio: &mut dyn Write) -> bool {
    match self {
      Upvalue::Closed(upvalue) => upvalue.trace_debug(stdio),
      _ => true,
    }
  }
}

impl DebugHeap for Upvalue {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    let depth = depth.saturating_sub(1);

    match self {
      Self::Open(v) => f.write_fmt(format_args!(
        "Upvalue::Open(*{:?})",
        &DebugWrap(unsafe { v.as_ref() }, depth)
      )),
      Self::Closed(v) => f.write_fmt(format_args!("Upvalue::Closed({:?})", &DebugWrap(v, depth))),
    }
  }
}

impl Manage for Upvalue {
  fn alloc_type(&self) -> &str {
    "upvalue"
  }

  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum FunKind {
  Fun,
  Method,
  StaticMethod,
  Initializer,
  Script,
}

#[derive(Clone)]
pub struct TryBlock {
  /// Start of the try block
  start: u16,

  /// End of the try block
  end: u16,
}

impl TryBlock {
  pub fn new(start: u16, end: u16) -> Self {
    TryBlock { start, end }
  }
}

#[derive(Clone)]
pub struct Fun {
  /// Name if not top-level script
  pub name: Managed<SmolStr>,

  /// Arity of this function
  pub arity: Arity,

  /// Number of upvalues
  pub upvalue_count: usize,

  /// The module this function belongs to
  pub module: Managed<Module>,

  /// Catch block present in this function
  try_blocks: Vec<TryBlock>,

  /// Code for the function body
  chunk: Chunk,
}

impl Fun {
  pub fn new(name: Managed<SmolStr>, module: Managed<Module>) -> Self {
    Self {
      arity: Arity::default(),
      upvalue_count: 0,
      chunk: Chunk::default(),
      module,
      name,
      try_blocks: Vec::new(),
    }
  }

  pub fn chunk(&self) -> &Chunk {
    &self.chunk
  }

  pub fn add_try(&mut self, try_block: TryBlock) {
    self.try_blocks.push(try_block);
  }

  pub fn has_catch_jump(&self, ip: u16) -> Option<u16> {
    let mut min_range = std::u16::MAX;
    let mut jump = None;

    for try_block in &self.try_blocks {
      if ip >= try_block.start && ip < try_block.end {
        let len = try_block.end - try_block.start;

        if len < min_range {
          min_range = len;
          jump = Some(try_block.end);
        }
      }
    }

    jump
  }

  pub fn write_instruction(&mut self, hooks: &GcHooks, op_code: AlignedByteCode, line: u32) {
    hooks.grow(self, |fun| fun.chunk.write_instruction(op_code, line));
  }

  pub fn replace_instruction(&mut self, index: usize, instruction: u8) {
    self.chunk.instructions[index] = instruction;
  }

  pub fn add_constant(&mut self, hooks: &GcHooks, constant: Value) -> usize {
    hooks.grow(self, |fun| fun.chunk.add_constant(constant))
  }

  pub fn shrink_to_fit(&mut self, hooks: &GcHooks) {
    hooks.shrink(self, |fun| fun.chunk.shrink_to_fit());
  }

  pub fn shrink_to_fit_internal(&mut self) {
    self.chunk.shrink_to_fit();
    self.try_blocks.shrink_to_fit();
  }
}

impl fmt::Display for Fun {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<fn {}>", self.name)
  }
}

impl fmt::Debug for Fun {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.fmt_heap(f, 1)
  }
}

impl Trace for Fun {
  fn trace(&self) -> bool {
    self.name.trace();
    self.chunk.constants.iter().for_each(|constant| {
      constant.trace();
    });
    self.module.trace();

    true
  }

  fn trace_debug(&self, stdio: &mut dyn Write) -> bool {
    self.name.trace_debug(stdio);
    self.chunk.constants.iter().for_each(|constant| {
      constant.trace_debug(stdio);
    });
    self.module.trace_debug(stdio);

    true
  }
}

impl DebugHeap for Fun {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    let depth = depth.saturating_sub(1);

    f.debug_struct("Fun")
      .field("name", &DebugWrap(&self.name, depth))
      .field("arity", &self.arity)
      .field("upvalue_count", &self.upvalue_count)
      .field("Module", &DebugWrap(&self.module, depth))
      .field("chunk", &self.chunk)
      .finish()
  }
}

impl Manage for Fun {
  fn alloc_type(&self) -> &str {
    "function"
  }

  fn size(&self) -> usize {
    mem::size_of::<Self>()
      + self.chunk.size()
      + mem::size_of::<TryBlock>()
      + self.try_blocks.capacity()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

#[derive(Clone, Debug)]
pub struct List<T>(Vec<T>);

impl<T> List<T> {
  pub fn new() -> Self {
    Self(Vec::new())
  }

  pub fn with_capacity(capacity: usize) -> Self {
    Self(Vec::with_capacity(capacity))
  }

  pub fn iter(&self) -> slice::Iter<'_, T> {
    self.0.iter()
  }

  pub fn capacity(&self) -> usize {
    self.0.capacity()
  }

  pub fn len(&self) -> usize {
    self.0.len()
  }

  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }

  pub fn push(&mut self, value: T) {
    self.0.push(value)
  }

  pub fn pop(&mut self) -> Option<T> {
    self.0.pop()
  }

  pub fn remove(&mut self, index: usize) -> T {
    self.0.remove(index)
  }

  pub fn insert(&mut self, index: usize, value: T) {
    self.0.insert(index, value)
  }

  pub fn sort_by<F: FnMut(&T, &T) -> Ordering>(&mut self, compare: F) {
    self.0.sort_by(compare)
  }

  pub fn clear(&mut self) {
    self.0.clear()
  }
}

impl<T: PartialEq> List<T> {
  pub fn contains(&mut self, value: &T) -> bool {
    self.0.contains(value)
  }
}

impl<T> Default for List<T> {
  fn default() -> Self {
    Self(Vec::new())
  }
}

impl<T: Clone> List<T> {
  pub fn to_list(&self) -> List<T> {
    List(self.0.to_vec())
  }

  pub fn extend_from_slice(&mut self, other: &[T]) {
    self.0.extend_from_slice(other)
  }
}

impl<T> FromIterator<T> for List<T> {
  fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
    List(Vec::from_iter(iter))
  }
}

impl<T, I: SliceIndex<[T]>> Index<I> for List<T> {
  type Output = <I as SliceIndex<[T]>>::Output;

  fn index(&self, index: I) -> &<Vec<T> as Index<I>>::Output {
    &self.0[index]
  }
}

impl<T, I: SliceIndex<[T]>> IndexMut<I> for List<T> {
  fn index_mut(&mut self, index: I) -> &mut <Vec<T> as Index<I>>::Output {
    &mut self.0[index]
  }
}

impl<T> Deref for List<T> {
  type Target = [T];

  fn deref(&self) -> &[T] {
    self.0.deref()
  }
}

impl<T> From<Vec<T>> for List<T> {
  fn from(vec: Vec<T>) -> Self {
    List(vec)
  }
}

impl<T: Clone> From<&[T]> for List<T> {
  fn from(slice: &[T]) -> Self {
    List(Vec::from(slice))
  }
}

impl<T: 'static + Trace> Trace for List<T> {
  fn trace(&self) -> bool {
    self.iter().for_each(|value| {
      value.trace();
    });

    true
  }

  fn trace_debug(&self, stdio: &mut dyn Write) -> bool {
    self.iter().for_each(|value| {
      value.trace_debug(stdio);
    });

    true
  }
}

impl<T: 'static + Trace + DebugHeap> DebugHeap for List<T> {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    let depth = depth.saturating_sub(1);

    f.debug_list()
      .entries(self.0.iter().map(|x| DebugWrap(x, depth)))
      .finish()
  }
}

impl<T: 'static + Trace + DebugHeap> Manage for List<T> {
  fn alloc_type(&self) -> &str {
    "list"
  }

  fn size(&self) -> usize {
    mem::size_of::<Vec<T>>() + mem::size_of::<T>() * self.capacity()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

#[derive(Clone, Debug)]
pub struct Map<K, V>(HashMap<K, V, FnvBuildHasher>);

impl<K, V> Map<K, V> {
  pub fn with_capacity(capacity: usize) -> Self {
    Self(HashMap::<K, V, FnvBuildHasher>::with_capacity_and_hasher(
      capacity,
      FnvBuildHasher::default(),
    ))
  }

  pub fn len(&self) -> usize {
    self.0.len()
  }

  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }

  pub fn capacity(&self) -> usize {
    self.0.capacity()
  }

  pub fn iter(&self) -> hash_map::Iter<'_, K, V> {
    self.0.iter()
  }

  pub fn keys(&self) -> hash_map::Keys<'_, K, V> {
    self.0.keys()
  }

  pub fn values(&self) -> hash_map::Values<'_, K, V> {
    self.0.values()
  }
}

impl<K: Eq + Hash, V> Map<K, V> {
  pub fn reserve(&mut self, additional: usize) {
    self.0.reserve(additional)
  }

  pub fn get(&self, key: &K) -> Option<&V> {
    self.0.get(key)
  }

  pub fn contains_key(&self, key: &K) -> bool {
    self.0.contains_key(key)
  }

  pub fn remove(&mut self, key: &K) -> Option<V> {
    self.0.remove(key)
  }

  pub fn entry(&mut self, key: K) -> Entry<'_, K, V, FnvBuildHasher> {
    self.0.entry(key)
  }

  pub fn insert(&mut self, key: K, value: V) -> Option<V> {
    self.0.insert(key, value)
  }
}

impl<K, V> Default for Map<K, V> {
  fn default() -> Self {
    Map(HashMap::default())
  }
}

impl<K: 'static + Trace, V: 'static + Trace> Trace for Map<K, V> {
  fn trace(&self) -> bool {
    self.iter().for_each(|(key, value)| {
      key.trace();
      value.trace();
    });

    true
  }

  fn trace_debug(&self, stdio: &mut dyn Write) -> bool {
    self.iter().for_each(|(key, value)| {
      key.trace_debug(stdio);
      value.trace_debug(stdio);
    });

    true
  }
}

impl<K: 'static + DebugHeap, V: 'static + DebugHeap> DebugHeap for Map<K, V> {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    let depth = depth.saturating_sub(1);

    f.debug_map()
      .entries(
        self
          .0
          .iter()
          .map(|(k, v)| (DebugWrap(k, depth), DebugWrap(v, depth))),
      )
      .finish()
  }
}

impl<K, V> Manage for Map<K, V>
where
  K: 'static + DebugHeap + Trace,
  V: 'static + DebugHeap + Trace,
{
  fn alloc_type(&self) -> &str {
    "map"
  }

  fn size(&self) -> usize {
    mem::size_of::<Map<Value, Value>>() + self.capacity() * mem::size_of::<Value>() * 2
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

#[derive(PartialEq, Clone)]
pub struct Closure {
  pub fun: Managed<Fun>,
  pub upvalues: Vec<Value>,
}

impl Closure {
  /// Create a new closure using a pointer to an underlying Fun
  ///
  /// # Example
  /// ```
  /// use laythe_core::object::{Closure, Class, Fun};
  /// use laythe_core::signature::Arity;
  /// use laythe_env::memory::{Gc, NO_GC};
  /// use laythe_core::module::Module;
  /// use laythe_core::chunk::Chunk;
  /// use laythe_core::hooks::{NoContext, Hooks, HookContext};
  /// use std::path::PathBuf;
  ///
  /// let gc = Gc::default();
  /// let mut context = NoContext::new(&gc);
  /// let hooks = Hooks::new(&mut context);
  ///
  /// let module = hooks.manage(Module::new(
  ///   hooks.manage(Class::bare(hooks.manage_str("module"))),
  ///   hooks.manage(PathBuf::from("self/module.ly")),
  /// ));
  /// let mut fun = Fun::new(hooks.manage_str("example"), module);
  /// let managed_fun = hooks.manage(fun);
  ///
  /// let closure = Closure::new(managed_fun);
  /// assert_eq!(&*closure.fun.name, "example");
  /// ```
  pub fn new(fun: Managed<Fun>) -> Self {
    Closure {
      upvalues: Vec::with_capacity(fun.upvalue_count as usize),
      fun,
    }
  }
}

impl fmt::Debug for Closure {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.fmt_heap(f, 1)
  }
}

impl Trace for Closure {
  fn trace(&self) -> bool {
    self.upvalues.iter().for_each(|upvalue| {
      upvalue.trace();
    });

    self.fun.trace();
    true
  }

  fn trace_debug(&self, stdio: &mut dyn Write) -> bool {
    self.upvalues.iter().for_each(|upvalue| {
      upvalue.trace_debug(stdio);
    });

    self.fun.trace_debug(stdio);
    true
  }
}

impl DebugHeap for Closure {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    let depth = depth.saturating_sub(1);

    f.debug_struct("Closure")
      .field("fun", &DebugWrap(&self.fun, depth))
      .field("upvalues", &DebugWrap(&&*self.upvalues, depth))
      .finish()
  }
}

impl Manage for Closure {
  fn alloc_type(&self) -> &str {
    "closure"
  }

  fn size(&self) -> usize {
    mem::size_of::<Self>() + mem::size_of::<Value>() * self.upvalues.capacity()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

#[derive(PartialEq, Clone)]
pub struct Class {
  pub name: Managed<SmolStr>,
  pub init: Option<Value>,
  pub index_get: Option<Value>,
  pub index_set: Option<Value>,
  methods: DynamicMap<Managed<SmolStr>, Value>,
  fields: DynamicMap<Managed<SmolStr>, u16>,
  meta_class: Option<Managed<Class>>,
  super_class: Option<Managed<Class>>,
}

impl fmt::Display for Class {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<cls {}>", self.name)
  }
}

impl Class {
  pub fn new(
    hooks: &GcHooks,
    name: Managed<SmolStr>,
    super_class: Managed<Class>,
  ) -> Managed<Self> {
    let super_meta = super_class
      .meta()
      .expect("Expected super class to have meta class");
    let super_meta_meta = super_meta
      .meta()
      .expect("Expected super meta class to have meta class");

    let meta_class = Class::with_meta(
      hooks,
      hooks.manage_str(format!("{} metaClass", name)),
      super_meta_meta,
      super_meta_meta,
    );

    let mut class = hooks.manage(Self {
      name,
      init: None,
      index_get: None,
      index_set: None,
      methods: DynamicMap::new(),
      fields: DynamicMap::new(),
      meta_class: Some(meta_class),
      super_class: None,
    });

    hooks.push_root(class);
    class.inherit(hooks, super_class);
    hooks.pop_roots(1);

    class
  }

  fn with_meta(
    hooks: &GcHooks,
    name: Managed<SmolStr>,
    meta_class: Managed<Class>,
    super_class: Managed<Class>,
  ) -> Managed<Self> {
    let mut class = hooks.manage(Self {
      name,
      init: None,
      index_get: None,
      index_set: None,
      methods: DynamicMap::new(),
      fields: DynamicMap::new(),
      meta_class: Some(meta_class),
      super_class: None,
    });

    hooks.push_root(class);
    class.inherit(hooks, super_class);
    hooks.pop_roots(1);

    class
  }

  pub fn bare(name: Managed<SmolStr>) -> Self {
    Self {
      name,
      init: None,
      index_get: None,
      index_set: None,
      methods: DynamicMap::new(),
      fields: DynamicMap::new(),
      meta_class: None,
      super_class: None,
    }
  }

  pub fn meta(&self) -> &Option<Managed<Class>> {
    &self.meta_class
  }

  pub fn is_subclass(&self, class: Managed<Class>) -> bool {
    if self == &*class {
      return true;
    }

    match self.super_class {
      Some(super_class) => super_class.is_subclass(class),
      None => false,
    }
  }

  pub fn super_class(&mut self) -> &Option<Managed<Class>> {
    &self.super_class
  }

  pub fn set_meta(&mut self, meta_class: Managed<Class>) -> &mut Self {
    if self.meta_class.is_some() {
      panic!("Meta class already set!");
    }

    self.meta_class = Some(meta_class);
    self
  }

  pub fn add_field(&mut self, hooks: &GcHooks, name: Managed<SmolStr>) -> Option<u16> {
    let len = self.fields.len();

    hooks.grow(self, |class| class.fields.insert(name, len as u16))
  }

  pub fn add_method(
    &mut self,
    hooks: &GcHooks,
    name: Managed<SmolStr>,
    method: Value,
  ) -> Option<Value> {
    match name.as_str() {
      INIT => self.init = Some(method),
      INDEX_GET => self.index_get = Some(method),
      INDEX_SET => self.index_set = Some(method),
      _ => (),
    }

    hooks.grow(self, |class| class.methods.insert(name, method))
  }

  pub fn get_method(&self, name: &Managed<SmolStr>) -> Option<Value> {
    self.methods.get(name).copied()
  }

  pub fn get_field_index(&self, name: &Managed<SmolStr>) -> Option<u16> {
    self.fields.get(name).copied()
  }

  pub fn inherit(&mut self, hooks: &GcHooks, super_class: Managed<Class>) {
    hooks.grow(self, |class| {
      super_class.methods.for_each(|(key, value)| {
        if class.methods.get(&*key).is_none() {
          class.methods.insert(*key, *value);
        }
      });

      super_class.fields.for_each(|(field, _index)| {
        if class.fields.get(&field).is_none() {
          let len = class.fields.len();
          class.fields.insert(*field, len as u16);
        }
      })
    });

    debug_assert!(self
      .super_class
      .map(|super_class| &*super_class.name == "Object")
      .unwrap_or(true));

    self.super_class = Some(super_class);
    self.init = self.init.or(super_class.init);
  }
}

impl fmt::Debug for Class {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.fmt_heap(f, 1)
  }
}

impl Trace for Class {
  fn trace(&self) -> bool {
    self.name.trace();

    self.methods.for_each(|(key, val)| {
      key.trace();
      val.trace();
    });
    self.fields.for_each(|(key, _)| {
      key.trace();
    });

    self.super_class.map(|class| class.trace());
    self.meta_class.map(|class| class.trace());

    true
  }

  fn trace_debug(&self, stdio: &mut dyn Write) -> bool {
    self.name.trace_debug(stdio);

    self.methods.for_each(|(key, val)| {
      key.trace_debug(stdio);
      val.trace_debug(stdio);
    });
    self.fields.for_each(|(key, _)| {
      key.trace_debug(stdio);
    });

    self.super_class.map(|class| class.trace_debug(stdio));
    self.meta_class.map(|class| class.trace_debug(stdio));

    true
  }
}

impl DebugHeap for Class {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    let depth = depth.saturating_sub(1);

    f.debug_struct("Class")
      .field("name", &DebugWrap(&self.name, depth))
      .field("methods", &DebugWrap(&self.methods, depth))
      .field("init", &DebugWrap(&self.init, depth))
      .finish()
  }
}

impl Manage for Class {
  fn alloc_type(&self) -> &str {
    "class"
  }

  fn size(&self) -> usize {
    mem::size_of::<Class>()
      + (mem::size_of::<Managed<SmolStr>>() + mem::size_of::<Value>()) * self.methods.capacity()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

#[derive(PartialEq, Clone)]
pub struct Instance {
  pub class: Managed<Class>,
  fields: Box<[Value]>,
}

impl Instance {
  pub fn new(class: Managed<Class>) -> Self {
    Instance {
      class,
      fields: vec![VALUE_NIL; class.fields.len()].into_boxed_slice(),
    }
  }

  pub fn fields(&self) -> &[Value] {
    &self.fields
  }

  pub fn set_field(&mut self, name: Managed<SmolStr>, value: Value) -> bool {
    match self.class.get_field_index(&name) {
      Some(index) => {
        self.fields[index as usize] = value;
        true
      }
      None => false,
    }
  }

  pub fn get_field(&self, name: &Managed<SmolStr>) -> Option<&Value> {
    self
      .class
      .get_field_index(&name)
      .map(|index| &self.fields[index as usize])
  }
}

impl Index<usize> for Instance {
  type Output = Value;

  fn index(&self, index: usize) -> &Value {
    &self.fields[index]
  }
}

impl IndexMut<usize> for Instance {
  fn index_mut(&mut self, index: usize) -> &mut Value {
    &mut self.fields[index]
  }
}

impl fmt::Debug for Instance {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.fmt_heap(f, 1)
  }
}

impl Trace for Instance {
  fn trace(&self) -> bool {
    self.class.trace();

    self.fields.iter().for_each(|val| {
      val.trace();
    });

    true
  }

  fn trace_debug(&self, stdio: &mut dyn Write) -> bool {
    self.class.trace_debug(stdio);

    self.fields.iter().for_each(|val| {
      val.trace_debug(stdio);
    });

    true
  }
}

impl DebugHeap for Instance {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    let depth = depth.saturating_sub(1);

    f.debug_struct("Instance")
      .field("class", &DebugWrap(&self.class, depth))
      .field("fields", &DebugWrap(&&*self.fields, depth))
      .finish()
  }
}

impl Manage for Instance {
  fn alloc_type(&self) -> &str {
    "instance"
  }

  fn size(&self) -> usize {
    mem::size_of::<Instance>()
      + (mem::size_of::<Managed<SmolStr>>() + mem::size_of::<Value>()) * self.fields.len()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

#[derive(PartialEq, Clone)]
pub struct Method {
  pub receiver: Value,
  pub method: Value,
}

impl Method {
  pub fn new(receiver: Value, method: Value) -> Self {
    Self { receiver, method }
  }
}

impl fmt::Debug for Method {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.fmt_heap(f, 1)
  }
}

impl Trace for Method {
  fn trace(&self) -> bool {
    self.receiver.trace();
    self.method.trace();
    true
  }

  fn trace_debug(&self, stdio: &mut dyn Write) -> bool {
    self.receiver.trace_debug(stdio);
    self.method.trace_debug(stdio);
    true
  }
}

impl DebugHeap for Method {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    let depth = depth.saturating_sub(1);

    f.debug_struct("Method")
      .field("receiver", &DebugWrap(&self.receiver, depth))
      .field("method", &DebugWrap(&self.method, depth))
      .finish()
  }
}

impl Manage for Method {
  fn alloc_type(&self) -> &str {
    "method"
  }

  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}
