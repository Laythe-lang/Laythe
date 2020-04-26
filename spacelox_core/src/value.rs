use crate::chunk::{AlignedByteCode, Chunk};
use crate::io::StdIo;
use crate::{
  arity::ArityKind,
  constants::INIT,
  dynamic_map::DynamicMap,
  hooks::Hooks,
  managed::{Manage, Managed, Trace},
  native::{NativeFun, NativeMethod},
  utils::do_if_some,
};
use fnv::FnvHashMap;
use std::fmt;
use std::mem;
use std::{hash::Hash, ptr::NonNull};

/// Enum of value types in spacelox
#[derive(Clone, Copy, Debug)]
pub enum Value {
  Bool(bool),
  Nil,
  Number(f64),
  String(Managed<String>),
  List(Managed<Vec<Value>>),
  Map(Managed<FnvHashMap<Value, Value>>),
  Fun(Managed<Fun>),
  Closure(Managed<Closure>),
  Class(Managed<Class>),
  Instance(Managed<Instance>),
  Method(Managed<Method>),
  NativeFun(Managed<Box<dyn NativeFun>>),
  NativeMethod(Managed<Box<dyn NativeMethod>>),
  Upvalue(Managed<Upvalue>),
}

/// Enum of value types in spacelox
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub enum ValueVariant {
  Bool,
  Nil,
  Number,
  String,
  List,
  Map,
  Fun,
  Closure,
  Class,
  Instance,
  Method,
  NativeFun,
  NativeMethod,
  Upvalue,
}

impl Value {
  /// Is this spacelox value nil
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::value::Value;
  ///
  /// let val1 = Value::Nil;
  /// assert_eq!(val1.is_nil(), true);
  /// ```
  pub fn is_nil(&self) -> bool {
    match self {
      Value::Nil => true,
      _ => false,
    }
  }

  /// Convert spacelox value to number, panics if not a number
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::value::Value;
  ///
  /// let val1 = Value::Number(20.0);
  /// assert_eq!(val1.to_num(), 20.0);
  /// ```
  pub fn to_num(&self) -> f64 {
    match self {
      Value::Number(num) => *num,
      _ => panic!("Value is not number"),
    }
  }

  /// Convert spacelox value to boolean, panics if not a bool
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::value::Value;
  ///
  /// let b1 = Value::Bool(false);
  /// assert_eq!(b1.to_bool(), false);
  /// ```
  pub fn to_bool(&self) -> bool {
    match self {
      Value::Bool(b1) => *b1,
      _ => panic!("Value is not boolean"),
    }
  }

  /// Unwrap and reference a spacelox string, panics if not a string
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::value::Value;
  /// use spacelox_core::memory::{Gc, NO_GC};
  ///
  /// let gc = Gc::default();
  /// let managed =  gc.manage_str(String::from("example"), &NO_GC);
  ///
  /// let value = Value::String(managed);
  /// assert_eq!(&*value.to_str(), "example")
  /// ```
  pub fn to_str(&self) -> Managed<String> {
    match self {
      Self::String(str1) => *str1,
      _ => panic!("Expected string."),
    }
  }

  /// Unwrap and reference a spacelox list, panics if not a list
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::value::Value;
  /// use spacelox_core::managed::{Allocation, Managed};
  /// use std::ptr::NonNull;
  ///
  /// let list: Vec<Value> = vec![Value::Nil];
  /// let mut alloc = Box::new(Allocation::new(list));
  /// let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };
  /// let managed = Managed::from(ptr);
  ///
  /// let value = Value::List(managed);
  /// assert_eq!(value.to_list()[0], Value::Nil)
  /// ```
  pub fn to_list(&self) -> Managed<Vec<Value>> {
    match self {
      Self::List(list) => *list,
      _ => panic!("Expected list."),
    }
  }

  /// Unwrap and reference a spacelox list, panics if not a list
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::value::Value;
  /// use spacelox_core::managed::{Allocation, Managed, make_managed};
  /// use std::ptr::NonNull;
  /// use fnv::FnvHashMap;
  ///
  /// let map: FnvHashMap<Value, Value> = FnvHashMap::default();
  /// let mut alloc = Box::new(Allocation::new(map));
  /// let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };
  /// let managed = Managed::from(ptr);
  ///
  /// let value = Value::Map(managed);
  /// assert_eq!(value.to_map().len(), 0)
  /// ```
  pub fn to_map(&self) -> Managed<FnvHashMap<Value, Value>> {
    match self {
      Self::Map(map) => *map,
      _ => panic!("Expected list."),
    }
  }

  /// Unwrap and reference a spacelox function, panics if not a function
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::value::{Value, Fun};
  /// use spacelox_core::arity::ArityKind;
  /// use spacelox_core::memory::{Gc, NO_GC};
  /// use spacelox_core::chunk::Chunk;
  ///
  /// let gc = Gc::default();
  /// let fun: Fun = Fun::new(gc.manage_str(String::from("add"), &NO_GC));
  /// let managed = gc.manage(fun, &NO_GC);
  ///
  /// let value = Value::Fun(managed);
  /// assert_eq!(&*value.to_fun().name, "add");
  /// ```
  pub fn to_fun(&self) -> Managed<Fun> {
    match self {
      Self::Fun(fun) => *fun,
      _ => panic!("Expected function!"),
    }
  }

  /// Unwrap a spacelox native function, panics if not a native function
  pub fn to_native_fun(&self) -> Managed<Box<dyn NativeFun>> {
    match self {
      Self::NativeFun(native_fun) => *native_fun,
      _ => panic!("Expected native function!"),
    }
  }

  /// Unwrap a spacelox native method, panics if not a native method
  pub fn to_native_method(&self) -> Managed<Box<dyn NativeMethod>> {
    match self {
      Self::NativeMethod(native_method) => *native_method,
      _ => panic!("Expected native method")
    }
  }

  /// Unwrap and reference a spacelox closure, panics if not a closure
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::value::{Value, Closure, Fun};
  /// use spacelox_core::arity::ArityKind;
  /// use spacelox_core::memory::{Gc, NO_GC};
  /// use spacelox_core::chunk::Chunk;
  ///
  /// let gc = Gc::default();
  /// let fun = Fun::new(gc.manage_str("add".to_string(), &NO_GC));
  /// let managed_fun = gc.manage(fun, &NO_GC);
  ///
  /// let closure = Closure::new(managed_fun);
  /// let managed_closure = gc.manage(closure, &NO_GC);
  ///
  /// let value = Value::Closure(managed_closure);
  /// assert_eq!(&*value.to_closure().fun.name.clone(), "add");
  /// ```
  pub fn to_closure(&self) -> Managed<Closure> {
    match self {
      Self::Closure(closure) => *closure,
      _ => panic!("Expected closure!"),
    }
  }

  /// Unwrap and reference a spacelox method, panics if not a method
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::value::{Value, Closure, Method, Fun};
  /// use spacelox_core::arity::ArityKind;
  /// use spacelox_core::memory::{Gc, NO_GC};
  /// use spacelox_core::chunk::Chunk;
  ///
  /// let gc = Gc::default();
  /// let fun = Fun::new(gc.manage_str("add".to_string(), &NO_GC));
  /// let managed_fun = gc.manage(fun, &NO_GC);
  ///
  /// let closure = Closure::new(managed_fun);
  /// let managed_closure = gc.manage(closure, &NO_GC);
  /// 
  /// let nil = Value::Nil;
  /// 
  /// let method = Method::new(nil, Value::Closure(managed_closure));
  /// let managed_method = gc.manage(method, &NO_GC);
  ///
  /// let value = Value::Method(managed_method);
  /// assert_eq!(value.to_method(), managed_method);
  /// ```
  pub fn to_method(&self) -> Managed<Method> {
    match self {
      Self::Method(method) => *method,
      _ => panic!("Expected method!"),
    }
  }

  /// Unwrap and reference a spacelox upvalue, panics if not a upvalue.
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::value::{Value, Upvalue};
  /// use spacelox_core::managed::{Allocation, Managed, make_managed};
  /// use std::ptr::NonNull;
  ///
  /// let value = Value::Number(5.0);
  /// let (upvalue, upvalue_alloc) = make_managed(Upvalue::Open(NonNull::from(&value)));
  /// let value = Value::Upvalue(upvalue);
  ///
  /// match *value.to_upvalue() {
  ///   Upvalue::Open(stack_ptr) => assert_eq!(*unsafe { stack_ptr.as_ref() }, Value::Number(5.0)),
  ///   Upvalue::Closed(_) => assert!(false),
  /// };
  /// ```
  pub fn to_upvalue(&self) -> Managed<Upvalue> {
    match self {
      Self::Upvalue(upvalue) => *upvalue,
      _ => panic!("Expected upvalue!"),
    }
  }

  /// Unwrap and reference a spacelox instance, panics if not a instance
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::value::{Value, Instance, Class};
  /// use spacelox_core::managed::{Managed, Allocation, make_managed};
  /// use std::ptr::NonNull;
  ///
  /// let (name, name_alloc) = make_managed("example".to_string());
  /// let (class, class_alloc) = make_managed(Class::new(name));
  ///
  /// let value = Value::Class(class);
  /// assert_eq!(value.to_class().name, name);
  /// ```
  pub fn to_class(&self) -> Managed<Class> {
    match self {
      Self::Class(class) => *class,
      _ => panic!("Expected class.",),
    }
  }

  /// Unwrap and reference a spacelox instance, panics if not a instance
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::value::{Value, Instance, Class};
  /// use spacelox_core::managed::{Managed, Allocation, make_managed};
  /// use std::ptr::NonNull;
  ///
  /// let (name, name_alloc) = make_managed("example".to_string());
  /// let (class, class_alloc) = make_managed(Class::new(name));
  /// let (instance, instance_alloc) = make_managed(Instance::new(class));
  ///
  /// let value = Value::Instance(instance);
  /// assert_eq!(value.to_instance().class, class);
  pub fn to_instance(&self) -> Managed<Instance> {
    match self {
      Self::Instance(instance) => *instance,
      _ => panic!("Expected instance!"),
    }
  }

  /// Get a string representation of the underlying type this value representing
  ///
  /// # Examples
  /// use spacelox_core::value::Value;
  /// use spacelox_core::object::{Obj, ObjValue};
  ///
  /// let nil = Value::Nil;
  /// let bool = Value::Bool(true);
  /// let number = Value::Number(10);
  /// let string = Value::Obj(Obj::new(ObjValue::String("something")));
  ///
  /// assert_eq!(nil.value_type(), "nil");
  /// assert_eq!(bool.value_type(), "bool");
  /// assert_eq!(number.value_type(), "number");
  /// assert_eq!(string.value_type(), "string");
  pub fn value_type(&self) -> String {
    match self {
      Value::Nil => "nil".to_string(),
      Value::Bool(_) => "bool".to_string(),
      Value::Number(_) => "number".to_string(),
      Value::String(_) => "string".to_string(),
      Value::List(_) => "list".to_string(),
      Value::Map(_) => "map".to_string(),
      Value::Fun(_) => "function".to_string(),
      Value::Closure(_) => "closure".to_string(),
      Value::Method(_) => "method".to_string(),
      Value::Class(_) => "class".to_string(),
      Value::Instance(_) => "instance".to_string(),
      Value::Upvalue(_) => "upvalue".to_string(),
      Value::NativeFun(_) => "native function".to_string(),
      Value::NativeMethod(_) => "native method".to_string(),
    }
  }

  pub fn value_class(&self, builtin: &BuiltInClasses) -> Managed<Class> {
    match self {
      Value::Nil => builtin.nil,
      Value::Bool(_) => builtin.bool,
      Value::Number(_) => builtin.number,
      Value::String(_) => builtin.string,
      Value::List(_) => builtin.list,
      Value::Map(_) => builtin.map,
      Value::Fun(_) => panic!("TODO"),
      Value::Closure(_) => builtin.closure,
      Value::Method(_) => builtin.method,
      Value::Class(_) => panic!("TODO"),
      Value::Instance(instance) => instance.class,
      Value::Upvalue(upvalue) => upvalue.value().value_class(builtin),
      Value::NativeFun(_) => builtin.native,
      Value::NativeMethod(_) => builtin.native,
    }
  }
}

impl fmt::Display for Value {
  /// Implement display for value in spacelox
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Number(num) => write!(f, "{}", num),
      Self::Bool(b) => write!(f, "{}", b),
      Self::Nil => write!(f, "nil"),
      Self::String(string) => write!(f, "'{}'", string.as_str()),
      Self::List(list) => {
        let mut strings: Vec<String> = Vec::with_capacity(list.len());
        for item in list.iter() {
          strings.push(format!("{}", item));
        }

        write!(f, "[{}]", strings.join(", "))
      }
      Self::Map(map) => {
        let strings: Vec<String> = map
          .iter()
          .map(|(key, value)| format!("{}: {}", key, value))
          .collect();
        write!(f, "{{ {} }}", strings.join(", "))
      }
      Self::Fun(fun) => write!(f, "{}", fun),
      Self::Upvalue(upvalue) => match &**upvalue {
        Upvalue::Open(stack_ptr) => write!(f, "{}", unsafe { stack_ptr.as_ref() }),
        Upvalue::Closed(store) => write!(f, "{}", store),
      },
      Self::Closure(closure) => write!(f, "{}", *closure.fun),
      Self::Method(bound) => write!(f, "{}.{}", bound.receiver, bound.method),
      Self::Class(class) => write!(f, "{}", &class.name.as_str()),
      Self::Instance(instance) => write!(f, "{} instance", &instance.class.name.as_str()),
      Self::NativeFun(native_fun) => write!(f, "<native {}>", native_fun.meta().name),
      Self::NativeMethod(native_method) => write!(f, "<native {}>", native_method.meta().name),
    }
  }
}

impl PartialEq for Value {
  /// Determine if this `Value` and another `Value` are equal inside
  /// of the spacelox runtime
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::value::Value;
  ///
  /// let val1 = Value::Bool(false);
  /// let val2 = Value::Bool(true);
  ///
  /// assert_eq!(val1 == val2, false);
  /// ```
  fn eq(&self, other: &Value) -> bool {
    // check the the variants have the same value
    match (self, other) {
      (Self::Number(num1), Self::Number(num2)) => num1 == num2,
      (Self::Bool(b1), Self::Bool(b2)) => b1 == b2,
      (Self::Nil, Self::Nil) => true,
      (Self::String(string1), Self::String(string2)) => string1 == string2,
      (Self::List(list1), Self::List(list2)) => list1 == list2,
      (Self::Map(map1), Self::Map(map2)) => map1 == map2,
      (Self::Fun(fun1), Self::Fun(fun2)) => fun1 == fun2,
      (Self::Closure(closure1), Self::Closure(closure2)) => closure1 == closure2,
      (Self::Method(method1), Self::Method(method2)) => method1 == method2,
      (Self::NativeFun(native1), Self::NativeFun(native2)) => native1 == native2,
      (Self::NativeMethod(native1), Self::NativeMethod(native2)) => native1 == native2,
      (Self::Upvalue(upvalue1), Self::Upvalue(upvalue2)) => upvalue1 == upvalue2,
      (Self::Class(class1), Self::Class(class2)) => class1 == class2,
      (Self::Instance(instance1), Self::Instance(instance2)) => instance1 == instance2,
      _ => false,
    }
  }
}

impl Eq for Value {}

impl Hash for Value {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    // check the the variants have the same value
    match self {
      Self::Number(num) => {
        ValueVariant::Number.hash(state);
        (*num as u64).hash(state);
      }
      Self::Bool(b) => {
        ValueVariant::Bool.hash(state);
        b.hash(state);
      }
      Self::Nil => ValueVariant::Nil.hash(state),
      Self::String(string) => {
        ValueVariant::String.hash(state);
        string.hash(state);
      }
      Self::List(list) => {
        ValueVariant::List.hash(state);
        list.hash(state);
      }
      Self::Map(map) => {
        ValueVariant::Map.hash(state);
        map.hash(state);
      }
      Self::Fun(fun) => {
        ValueVariant::Fun.hash(state);
        fun.hash(state);
      }
      Self::Closure(closure) => {
        ValueVariant::Closure.hash(state);
        closure.hash(state);
      }
      Self::Method(method) => {
        ValueVariant::Method.hash(state);
        method.hash(state);
      }
      Self::NativeFun(native) => {
        ValueVariant::NativeFun.hash(state);
        native.hash(state);
      }
      Self::NativeMethod(native) => {
        ValueVariant::NativeMethod.hash(state);
        native.hash(state);
      }
      Self::Upvalue(upvalue) => {
        ValueVariant::Upvalue.hash(state);
        upvalue.hash(state);
      }
      Self::Class(class) => {
        ValueVariant::Class.hash(state);
        class.hash(state);
      }
      Self::Instance(instance) => {
        ValueVariant::Instance.hash(state);
        instance.hash(state);
      }
    };
  }
}

impl Trace for Value {
  fn trace(&self) -> bool {
    match self {
      Value::Nil => true,
      Value::Bool(_) => true,
      Value::Number(_) => true,
      Value::String(string) => string.trace(),
      Value::List(list) => list.trace(),
      Value::Map(map) => map.trace(),
      Value::Fun(fun) => fun.trace(),
      Value::Closure(closure) => closure.trace(),
      Value::Method(method) => method.trace(),
      Value::Class(class) => class.trace(),
      Value::Instance(instance) => instance.trace(),
      Value::Upvalue(upvalue) => upvalue.trace(),
      Value::NativeFun(native) => native.trace(),
      Value::NativeMethod(native) => native.trace(),
    }
  }

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    match self {
      Value::Nil => true,
      Value::Bool(_) => true,
      Value::Number(_) => true,
      Value::String(string) => string.trace_debug(stdio),
      Value::List(list) => list.trace_debug(stdio),
      Value::Map(map) => map.trace_debug(stdio),
      Value::Fun(fun) => fun.trace_debug(stdio),
      Value::Closure(closure) => closure.trace_debug(stdio),
      Value::Method(method) => method.trace_debug(stdio),
      Value::Class(class) => class.trace_debug(stdio),
      Value::Instance(instance) => instance.trace_debug(stdio),
      Value::Upvalue(upvalue) => upvalue.trace_debug(stdio),
      Value::NativeFun(native) => native.trace_debug(stdio),
      Value::NativeMethod(native) => native.trace_debug(stdio),
    }
  }
}

pub struct BuiltInClasses {
  pub nil: Managed<Class>,
  pub bool: Managed<Class>,
  pub number: Managed<Class>,
  pub string: Managed<Class>,
  pub list: Managed<Class>,
  pub map: Managed<Class>,
  pub closure: Managed<Class>,
  pub method: Managed<Class>,
  pub native: Managed<Class>,
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
    self.native.trace();

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
    self.native.trace_debug(stdio);

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
  /// use spacelox_core::value::{Value, Upvalue};
  /// use std::rc::Rc;
  /// use std::ptr::NonNull;
  ///
  /// let value = Value::Number(10.0);
  ///
  /// let mut upvalue = Upvalue::Open(NonNull::from(&value));
  /// upvalue.hoist();
  ///
  /// match upvalue {
  ///   Upvalue::Closed(store) => assert_eq!(*store, Value::Number(10.0)),
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
  /// use spacelox_core::value::{Value, Upvalue};
  /// use std::ptr::NonNull;
  ///
  /// let value = Value::Nil;
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
      Self::Open(_) => String::from("Upvalue::Open({{ ... }})"),
      Self::Closed(_) => String::from("Upvalue::Closed({{ ... }})"),
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
  /// Arity of this function
  pub arity: ArityKind,

  /// Number of upvalues
  pub upvalue_count: usize,

  /// Code for the function body
  chunk: Chunk,

  /// Name if not top-level script
  pub name: Managed<String>,
}

impl Fun {
  pub fn new(name: Managed<String>) -> Self {
    Self {
      arity: ArityKind::Fixed(0),
      upvalue_count: 0,
      chunk: Chunk::default(),
      name,
    }
  }

  pub fn chunk(&self) -> &Chunk {
    &self.chunk
  }

  pub fn write_instruction(&mut self, hooks: &Hooks, op_code: AlignedByteCode, line: u32) {
    hooks.resize(self, |fun| fun.chunk.write_instruction(op_code, line));
  }

  pub fn replace_instruction(&mut self, index: usize, instruction: u8) {
    self.chunk.instructions[index] = instruction;
  }

  pub fn add_constant(&mut self, hooks: &Hooks, constant: Value) -> usize {
    hooks.resize(self, |fun| fun.chunk.add_constant(constant))
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
      .field("arity", &self.arity)
      .field("upvalue_count", &self.upvalue_count)
      .field("chunk", &"Chunk { ... }")
      .field("name", &"Managed(String {...})")
      .finish()
  }
}

impl Trace for Fun {
  fn trace(&self) -> bool {
    self.name.trace();
    self.chunk.constants.iter().for_each(|constant| {
      constant.trace();
    });

    true
  }

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    self.name.trace_debug(stdio);
    self.chunk.constants.iter().for_each(|constant| {
      constant.trace_debug(stdio);
    });

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

impl Trace for Vec<Value> {
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

impl Manage for Vec<Value> {
  fn alloc_type(&self) -> &str {
    "list"
  }

  fn debug(&self) -> String {
    format!("{:?}", self)
  }

  fn debug_free(&self) -> String {
    String::from("List: [...]")
  }

  fn size(&self) -> usize {
    mem::size_of::<Vec<Value>>() + mem::size_of::<Value>() * self.capacity()
  }
}

impl Trace for FnvHashMap<Value, Value> {
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

impl Manage for FnvHashMap<Value, Value> {
  fn alloc_type(&self) -> &str {
    "list"
  }

  fn debug(&self) -> String {
    format!("{:?}", self)
  }

  fn debug_free(&self) -> String {
    String::from("List: [...]")
  }

  fn size(&self) -> usize {
    mem::size_of::<FnvHashMap<Value, Value>>() + self.capacity() * mem::size_of::<Value>() * 2
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
  /// use spacelox_core::value::{Closure, Fun};
  /// use spacelox_core::arity::ArityKind;
  /// use spacelox_core::memory::{Gc, NO_GC};
  /// use spacelox_core::chunk::Chunk;
  ///
  /// let gc = Gc::default();
  /// let mut fun = Fun::new(gc.manage_str("example".to_string(), &NO_GC));
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
    String::from("Closure: {{ fun: {{ ... }}, upvalues: {{ ... }} }}")
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

    hooks.resize(self, |class| {
      class.methods.insert(name, method);
    });
  }

  pub fn get_method(&self, name: &Managed<String>) -> Option<&Value> {
    self.methods.get(name)
  }

  pub fn inherit(&mut self, hooks: &Hooks, super_class: Managed<Class>) {
    hooks.resize(self, |class| {
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
    String::from("Class: {{ init: {{...}}, name: {{...}}, methods: {{...}}}}")
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
    hooks.resize(self, |instance: &mut Instance| {
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
    String::from("Instance: {{ class: {{...}}, fields: {{...}} }}")
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
    String::from("Method: {{ method: {{...}}, receiver: {{...}}}}")
  }

  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::managed::Allocation;
  use std::ptr::NonNull;

  fn example_each(string: Managed<String>) -> Vec<Value> {
    vec![
      Value::Bool(true),
      Value::Nil,
      Value::Number(10.0),
      Value::String(string),
    ]
  }

  #[test]
  fn test_diff_type_no_equal() {
    // let string = "example";
    let mut string_alloc = Box::new(Allocation::new("data".to_string()));

    // "blah".to_string().into_boxed_str();
    let string_ptr = unsafe { NonNull::new_unchecked(&mut *string_alloc) };
    let managed_string = Managed::from(string_ptr);

    let examples = example_each(managed_string);
    for i in 0..examples.len() {
      for j in 0..examples.len() {
        if i == j {
          assert_eq!(examples[i] == examples[j], true);
        } else {
          assert_eq!(examples[i] == examples[j], false);
        }
      }
    }
  }
}
