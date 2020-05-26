use crate::{
  arity::ArityKind,
  chunk::{AlignedByteCode, Chunk},
  constants::INIT,
  dynamic_map::DynamicMap,
  hooks::Hooks,
  io::StdIo,
  managed::{Manage, Managed, Trace},
  module::Module,
  utils::do_if_some,
  value::{Value, ValueVariant},
  SlHashMap,
};
use std::{fmt, mem, ptr::NonNull};

pub struct BuiltInClasses {
  pub nil: Managed<Class>,
  pub bool: Managed<Class>,
  pub class: Managed<Class>,
  pub number: Managed<Class>,
  pub string: Managed<Class>,
  pub list: Managed<Class>,
  pub map: Managed<Class>,
  pub closure: Managed<Class>,
  pub method: Managed<Class>,
  pub native_fun: Managed<Class>,
  pub native_method: Managed<Class>,
}

impl BuiltInClasses {
  pub fn for_variant(&self, value: Value, variant: ValueVariant) -> Managed<Class> {
    match variant {
      ValueVariant::Bool => self.bool,
      ValueVariant::Nil => self.nil,
      ValueVariant::Number => self.number,
      ValueVariant::String => self.string,
      ValueVariant::List => self.list,
      ValueVariant::Map => self.map,
      ValueVariant::Fun => panic!(),
      ValueVariant::Closure => self.closure,
      ValueVariant::Class => self.class,
      ValueVariant::Instance => value.to_instance().class,
      ValueVariant::Iter => value.to_iter().class,
      ValueVariant::Method => self.method,
      ValueVariant::NativeFun => self.native_fun,
      ValueVariant::NativeMethod => self.native_method,
      ValueVariant::Upvalue => value.to_upvalue().value().value_class(self),
    }
  }
}

impl Trace for BuiltInClasses {
  fn trace(&self) -> bool {
    self.bool.trace();
    self.nil.trace();
    self.number.trace();
    self.string.trace();
    self.list.trace();
    self.map.trace();
    self.closure.trace();
    self.method.trace();
    self.native_fun.trace();
    self.native_method.trace();

    true
  }

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    self.bool.trace_debug(stdio);
    self.nil.trace_debug(stdio);
    self.number.trace_debug(stdio);
    self.string.trace_debug(stdio);
    self.list.trace_debug(stdio);
    self.map.trace_debug(stdio);
    self.closure.trace_debug(stdio);
    self.method.trace_debug(stdio);
    self.native_fun.trace_debug(stdio);
    self.native_method.trace_debug(stdio);

    true
  }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Upvalue {
  Open(NonNull<Value>),
  Closed(Box<Value>),
}

impl Upvalue {
  /// Close over the upvalue by moving it onto the stack to the heap
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::value::Value;
  /// use spacelox_core::object::Upvalue;
  /// use std::rc::Rc;
  /// use std::ptr::NonNull;
  ///
  /// let value = Value::from(10.0);
  ///
  /// let mut upvalue = Upvalue::Open(NonNull::from(&value));
  /// upvalue.hoist();
  ///
  /// match upvalue {
  ///   Upvalue::Closed(store) => assert_eq!(*store, Value::from(10.0)),
  ///   Upvalue::Open(_) => assert!(false),
  /// };
  /// ```
  pub fn hoist(&mut self) {
    match self {
      Upvalue::Open(stack_ptr) => {
        let value = *unsafe { stack_ptr.as_ref() };
        mem::replace(self, Upvalue::Closed(Box::new(value)));
      }
      Upvalue::Closed(_) => panic!("Attempted to hoist already hoisted upvalue."),
    }
  }

  /// Is this upvalue currently open
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::value::{Value, VALUE_NIL};
  /// use spacelox_core::object::Upvalue;
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
      Upvalue::Closed(store) => **store,
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

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    match self {
      Upvalue::Closed(upvalue) => upvalue.trace_debug(stdio),
      _ => true,
    }
  }
}

impl Manage for Upvalue {
  fn alloc_type(&self) -> &str {
    "upvalue"
  }

  fn debug(&self) -> String {
    format!("{:?}", self)
  }

  fn debug_free(&self) -> String {
    match self {
      Self::Open(_) => "Upvalue::Open({{ ... }})".to_string(),
      Self::Closed(_) => "Upvalue::Closed({{ ... }})".to_string(),
    }
  }

  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }
}

#[derive(Debug, PartialEq, Clone)]
pub enum FunKind {
  Fun,
  Method,
  Initializer,
  Script,
}

#[derive(PartialEq, Clone)]
pub struct Fun {
  /// Name if not top-level script
  pub name: Managed<String>,

  /// Arity of this function
  pub arity: ArityKind,

  /// Number of upvalues
  pub upvalue_count: usize,

  /// The module this function belongs to
  pub module: Managed<Module>,

  /// Code for the function body
  chunk: Chunk,
}

impl Fun {
  pub fn new(name: Managed<String>, module: Managed<Module>) -> Self {
    Self {
      arity: ArityKind::Fixed(0),
      upvalue_count: 0,
      chunk: Chunk::default(),
      module,
      name,
    }
  }

  pub fn chunk(&self) -> &Chunk {
    &self.chunk
  }

  pub fn write_instruction(&mut self, hooks: &Hooks, op_code: AlignedByteCode, line: u32) {
    hooks.grow(self, |fun| fun.chunk.write_instruction(op_code, line));
  }

  pub fn replace_instruction(&mut self, index: usize, instruction: u8) {
    self.chunk.instructions[index] = instruction;
  }

  pub fn add_constant(&mut self, hooks: &Hooks, constant: Value) -> usize {
    hooks.grow(self, |fun| fun.chunk.add_constant(constant))
  }

  pub fn shrink_to_fit(&mut self, hooks: &Hooks) {
    hooks.shrink(self, |fun| fun.chunk.shrink_to_fit());
  }

  pub fn shrink_to_fit_internal(&mut self) {
    self.chunk.shrink_to_fit();
  }
}

impl fmt::Display for Fun {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<fn {}>", self.name)
  }
}

impl fmt::Debug for Fun {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    f.debug_struct("Fun")
      .field("name", &"Managed(String {...})")
      .field("arity", &self.arity)
      .field("upvalue_count", &self.upvalue_count)
      .field("Module", &"Manged(Module: { ... })")
      .field("chunk", &"Chunk { ... }")
      .finish()
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

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    self.name.trace_debug(stdio);
    self.chunk.constants.iter().for_each(|constant| {
      constant.trace_debug(stdio);
    });
    self.module.trace_debug(stdio);

    true
  }
}

impl Manage for Fun {
  fn alloc_type(&self) -> &str {
    "function"
  }

  fn debug(&self) -> String {
    format!("{:?}", self)
  }

  fn debug_free(&self) -> String {
    format!("{:?}", self)
  }

  fn size(&self) -> usize {
    mem::size_of::<Self>() + self.chunk.size()
  }
}

impl Trace for String {
  fn trace(&self) -> bool {
    true
  }

  fn trace_debug(&self, _: &dyn StdIo) -> bool {
    true
  }
}

impl Manage for String {
  fn alloc_type(&self) -> &str {
    "string"
  }

  fn debug(&self) -> String {
    format!("{:?}", self)
  }

  fn debug_free(&self) -> String {
    format!("{:?}", self)
  }

  fn size(&self) -> usize {
    mem::size_of::<Self>() + self.capacity()
  }
}

impl<T: 'static + Trace> Trace for Vec<T> {
  fn trace(&self) -> bool {
    self.iter().for_each(|value| {
      value.trace();
    });

    true
  }

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    self.iter().for_each(|value| {
      value.trace_debug(stdio);
    });

    true
  }
}

impl<T: 'static + Trace + fmt::Debug> Manage for Vec<T> {
  fn alloc_type(&self) -> &str {
    "list"
  }

  fn debug(&self) -> String {
    format!("{:?}", self)
  }

  fn debug_free(&self) -> String {
    "List: [...]".to_string()
  }

  fn size(&self) -> usize {
    mem::size_of::<Vec<T>>() + mem::size_of::<T>() * self.capacity()
  }
}

impl<T: 'static + Trace> Trace for SlHashMap<T, T> {
  fn trace(&self) -> bool {
    self.iter().for_each(|(key, value)| {
      key.trace();
      value.trace();
    });

    true
  }

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    self.iter().for_each(|(key, value)| {
      key.trace_debug(stdio);
      value.trace_debug(stdio);
    });

    true
  }
}

impl<T: 'static + Trace + fmt::Debug> Manage for SlHashMap<T, T> {
  fn alloc_type(&self) -> &str {
    "map"
  }

  fn debug(&self) -> String {
    format!("{:?}", self)
  }

  fn debug_free(&self) -> String {
    "Map: {...}".to_string()
  }

  fn size(&self) -> usize {
    mem::size_of::<SlHashMap<Value, Value>>() + self.capacity() * mem::size_of::<Value>() * 2
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
  /// use spacelox_core::object::{Closure, Fun};
  /// use spacelox_core::arity::ArityKind;
  /// use spacelox_core::memory::{Gc, NO_GC};
  /// use spacelox_core::module::Module;
  /// use spacelox_core::chunk::Chunk;
  ///
  /// let gc = Gc::default();
  /// let module = gc.manage(Module::new(gc.manage_str("module".to_string(), &NO_GC)), &NO_GC);
  /// let mut fun = Fun::new(gc.manage_str("example".to_string(), &NO_GC), module);
  /// let managed_fun = gc.manage(fun, &NO_GC);
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
    f.debug_struct("Closure")
      .field("fun", &self.fun)
      .field("upvalues", &format!("[UpValue; {}]", &self.upvalues.len()))
      .finish()
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

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    self.upvalues.iter().for_each(|upvalue| {
      upvalue.trace_debug(stdio);
    });

    self.fun.trace_debug(stdio);
    true
  }
}

impl Manage for Closure {
  fn alloc_type(&self) -> &str {
    "closure"
  }

  fn debug(&self) -> String {
    format!("{:?}", self)
  }

  fn debug_free(&self) -> String {
    "Closure: {{ fun: {{ ... }}, upvalues: {{ ... }} }}".to_string()
  }

  fn size(&self) -> usize {
    mem::size_of::<Self>() + mem::size_of::<Value>() * self.upvalues.capacity()
  }
}

#[derive(PartialEq, Clone)]
pub struct Class {
  pub name: Managed<String>,
  pub init: Option<Value>,
  methods: DynamicMap<Managed<String>, Value>,
}

impl Class {
  pub fn new(name: Managed<String>) -> Self {
    Class {
      name,
      init: None,
      methods: DynamicMap::new(),
    }
  }

  pub fn add_method(&mut self, hooks: &Hooks, name: Managed<String>, method: Value) {
    if *name == INIT {
      self.init = Some(method);
    }

    hooks.grow(self, |class| {
      class.methods.insert(name, method);
    });
  }

  pub fn get_method(&self, name: &Managed<String>) -> Option<&Value> {
    self.methods.get(name)
  }

  pub fn inherit(&mut self, hooks: &Hooks, super_class: Managed<Class>) {
    hooks.grow(self, |class| {
      super_class.methods.for_each(|(key, value)| {
        match class.methods.get(&*key) {
          None => class.methods.insert(*key, *value),
          _ => None,
        };
      });
    });

    self.init = self.init.or(super_class.init);
  }
}

impl fmt::Debug for Class {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    f.debug_struct("Class")
      .field("name", &self.name)
      .field("methods", &"Methods: { ... }")
      .field("init", &self.init)
      .finish()
  }
}

impl Trace for Class {
  fn trace(&self) -> bool {
    self.name.trace();
    do_if_some(self.init, |init| {
      init.trace();
    });

    self.methods.for_each(|(key, val)| {
      key.trace();
      val.trace();
    });

    true
  }

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    self.name.trace_debug(stdio);
    do_if_some(self.init, |init| {
      init.trace_debug(stdio);
    });

    self.methods.for_each(|(key, val)| {
      key.trace_debug(stdio);
      val.trace_debug(stdio);
    });

    true
  }
}

impl Manage for Class {
  fn alloc_type(&self) -> &str {
    "class"
  }

  fn debug(&self) -> String {
    format!("{:?}", self)
  }

  fn debug_free(&self) -> String {
    "Class: {{ init: {{...}}, name: {{...}}, methods: {{...}}}}".to_string()
  }

  fn size(&self) -> usize {
    mem::size_of::<Class>()
      + (mem::size_of::<Managed<String>>() + mem::size_of::<Value>()) * self.methods.capacity()
  }
}

#[derive(PartialEq, Clone)]
pub struct Instance {
  pub class: Managed<Class>,
  fields: DynamicMap<Managed<String>, Value>,
}

impl Instance {
  pub fn new(class: Managed<Class>) -> Self {
    Instance {
      class,
      fields: DynamicMap::new(),
    }
  }

  pub fn set_field(&mut self, hooks: &Hooks, name: Managed<String>, value: Value) {
    hooks.grow(self, |instance: &mut Instance| {
      instance.fields.insert(name, value);
    });
  }

  pub fn get_field(&self, name: &Managed<String>) -> Option<&Value> {
    self.fields.get(name)
  }
}

impl fmt::Debug for Instance {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    f.debug_struct("Instance")
      .field("class", &self.class)
      .field("fields", &self.fields)
      .finish()
  }
}

impl Trace for Instance {
  fn trace(&self) -> bool {
    self.class.trace();

    self.fields.for_each(|(key, val)| {
      key.trace();
      val.trace();
    });

    true
  }

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    self.class.trace_debug(stdio);

    self.fields.for_each(|(key, val)| {
      key.trace_debug(stdio);
      val.trace_debug(stdio);
    });

    true
  }
}

impl Manage for Instance {
  fn alloc_type(&self) -> &str {
    "instance"
  }

  fn debug(&self) -> String {
    format!("{:?}", self)
  }

  fn debug_free(&self) -> String {
    "Instance: {{ class: {{...}}, fields: {{...}} }}".to_string()
  }

  fn size(&self) -> usize {
    mem::size_of::<Instance>()
      + (mem::size_of::<Managed<String>>() + mem::size_of::<Value>()) * self.fields.capacity()
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
    f.debug_struct("BoundMethod")
      .field("receiver", &format!("{}", self.receiver))
      .field("method", &self.method)
      .finish()
  }
}

impl Trace for Method {
  fn trace(&self) -> bool {
    self.receiver.trace();
    self.method.trace();
    true
  }

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    self.receiver.trace_debug(stdio);
    self.method.trace_debug(stdio);
    true
  }
}

impl Manage for Method {
  fn alloc_type(&self) -> &str {
    "method"
  }

  fn debug(&self) -> String {
    format!("{:?}", self)
  }

  fn debug_free(&self) -> String {
    "Method: {{ method: {{...}}, receiver: {{...}}}}".to_string()
  }

  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }
}
