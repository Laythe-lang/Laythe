use crate::chunk::Chunk;
use crate::io::StdIo;
use crate::{
  dynamic_map::DynamicMap,
  managed::{Manage, Managed, Trace},
  native::{NativeFun, NativeMethod},
  utils::do_if_some,
};
use std::fmt;
use std::mem;
use std::ptr::NonNull;

/// Enum of value types in spacelox
#[derive(Clone, Copy, Debug)]
pub enum Value {
  Bool(bool),
  Nil,
  Number(f64),
  String(Managed<String>),
  List(Managed<Vec<Value>>),
  Fun(Managed<Fun>),
  Closure(Managed<Closure>),
  Class(Managed<Class>),
  Instance(Managed<Instance>),
  Method(Managed<Method>),
  NativeFun(Managed<Box<dyn NativeFun>>),
  NativeMethod(Managed<Box<dyn NativeMethod>>),
  Upvalue(Managed<Upvalue>),
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
  /// use spacelox_core::managed::{Allocation, Managed, make_managed};
  /// use std::ptr::NonNull;
  ///
  /// let (managed, alloc) = make_managed::<String>(String::from("example"));
  ///
  /// let value = Value::String(managed);
  /// assert_eq!(*value.to_string(), "example".to_string())
  /// ```
  pub fn to_string(&self) -> Managed<String> {
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

  /// Unwrap and reference a spacelox function, panics if not a function
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::value::{Value, Fun, ArityKind};
  /// use spacelox_core::managed::{Allocation, Managed, make_managed};
  /// use spacelox_core::chunk::Chunk;
  /// use std::ptr::NonNull;
  ///
  /// let fun: Fun = Fun {
  ///   name: Some("add".to_string()),
  ///   arity: ArityKind::Fixed(3),
  ///   upvalue_count: 0,
  ///   chunk: Chunk::default()
  /// };
  /// let (managed, alloc) = make_managed(fun);
  ///
  /// let value = Value::Fun(managed);
  /// assert_eq!(value.to_fun().name.clone().unwrap(), "add");
  /// ```
  pub fn to_fun(&self) -> Managed<Fun> {
    match self {
      Self::Fun(fun) => *fun,
      _ => panic!("Expected function!"),
    }
  }

  /// Unwrap and reference a spacelox native function, panics if not a native function
  pub fn to_native_fun(&self) -> Managed<Box<dyn NativeFun>> {
    match self {
      Self::NativeFun(native) => *native,
      _ => panic!("Expected function!"),
    }
  }

  /// Unwrap and reference a spacelox closure, panics if not a closure
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::value::{Value, Closure, Fun, ArityKind};
  /// use spacelox_core::managed::{Managed, Allocation, make_managed};
  /// use spacelox_core::chunk::Chunk;
  /// use std::ptr::NonNull;
  ///
  /// let fun = Fun {
  ///   name: Some("add".to_string()),
  ///   arity: ArityKind::Fixed(3),
  ///   upvalue_count: 0,
  ///   chunk: Chunk::default()
  /// };
  /// let (managed_fun, alloc_fun) = make_managed(fun);
  ///
  /// let closure = Closure::new(managed_fun);
  /// let (managed_closure, alloc_closure) = make_managed(closure);
  ///
  /// let value = Value::Closure(managed_closure);
  /// assert_eq!(value.to_closure().fun.name.clone().unwrap(), "add");
  /// ```
  pub fn to_closure(&self) -> Managed<Closure> {
    match self {
      Self::Closure(closure) => *closure,
      _ => panic!("Expected closure!"),
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
      Value::Fun(_) => builtin.fun,
      Value::Closure(_) => builtin.fun,
      Value::Method(_) => builtin.fun,
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
      Self::List(_) => write!(f, "list"),
      Self::Fun(fun) => match &fun.name {
        Some(name) => write!(f, "<fn {}>", name),
        None => write!(f, "<script>"),
      },
      Self::Upvalue(upvalue) => match &**upvalue {
        Upvalue::Open(stack_ptr) => write!(f, "<upvalue open {}>", unsafe { stack_ptr.as_ref() }),
        Upvalue::Closed(store) => write!(f, "<upvalue closed {}>", store),
      },
      Self::Closure(closure) => write!(f, "{}", closure.name()),
      Self::Method(bound) => write!(f, "{}", bound.method),
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
      (Self::Fun(fun1), Self::Fun(fun2)) => fun1 == fun2,
      (Self::Closure(closure1), Self::Closure(closure2)) => closure1 == closure2,
      (Self::Method(method1), Self::Method(method2)) => method1 == method2,
      (Self::NativeFun(native1), Self::NativeFun(native2)) => native1 == native2,
      (Self::Upvalue(upvalue1), Self::Upvalue(upvalue2)) => upvalue1 == upvalue2,
      (Self::Class(class1), Self::Class(class2)) => class1 == class2,
      (Self::Instance(instance1), Self::Instance(instance2)) => instance1 == instance2,
      _ => false,
    }
  }
}

impl Trace for Value {
  fn trace(&self) -> bool {
    match self {
      Value::String(string) => string.trace(),
      Value::List(list) => list.trace(),
      Value::Fun(fun) => fun.trace(),
      Value::Closure(closure) => closure.trace(),
      Value::Method(method) => method.trace(),
      Value::Class(class) => class.trace(),
      Value::Instance(instance) => instance.trace(),
      Value::Upvalue(upvalue) => upvalue.trace(),
      Value::NativeFun(native) => native.trace(),
      _ => true,
    }
  }

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    match self {
      Value::String(string) => string.trace_debug(stdio),
      Value::Fun(fun) => fun.trace_debug(stdio),
      Value::Closure(closure) => closure.trace_debug(stdio),
      Value::Method(method) => method.trace_debug(stdio),
      Value::Class(class) => class.trace_debug(stdio),
      Value::Instance(instance) => instance.trace_debug(stdio),
      Value::Upvalue(upvalue) => upvalue.trace_debug(stdio),
      Value::NativeFun(native) => native.trace_debug(stdio),
      _ => true,
    }
  }
}

pub struct BuiltInClasses {
  pub nil: Managed<Class>,
  pub bool: Managed<Class>,
  pub number: Managed<Class>,
  pub string: Managed<Class>,
  pub list: Managed<Class>,
  pub fun: Managed<Class>,
  pub native: Managed<Class>,
}

impl Trace for BuiltInClasses {
  fn trace(&self) -> bool {
    self.bool.trace();
    self.nil.trace();
    self.number.trace();
    self.string.trace();
    self.list.trace();
    self.fun.trace();
    self.native.trace();

    true
  }

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    self.bool.trace_debug(stdio);
    self.nil.trace_debug(stdio);
    self.number.trace_debug(stdio);
    self.string.trace_debug(stdio);
    self.list.trace_debug(stdio);
    self.fun.trace_debug(stdio);
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ArityKind {
  Fixed(u8),
  Variadic(u8),
}

#[derive(PartialEq, Clone)]
pub struct Fun {
  /// Arity of this function
  pub arity: ArityKind,

  /// Number of upvalues
  pub upvalue_count: usize,

  /// Code for the function body
  pub chunk: Chunk,

  /// Name if not top-level script
  pub name: Option<String>,
}

impl Default for Fun {
  fn default() -> Self {
    Self {
      arity: ArityKind::Fixed(0),
      upvalue_count: 0,
      chunk: Chunk::default(),
      name: Some("null function".to_string()),
    }
  }
}

impl fmt::Debug for Fun {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    f.debug_struct("Fun")
      .field("arity", &self.arity)
      .field("upvalue_count", &self.upvalue_count)
      .field("chunk", &"Chunk { ... }")
      .field("name", &self.name)
      .finish()
  }
}

impl Trace for Fun {
  fn trace(&self) -> bool {
    self.chunk.constants.iter().for_each(|constant| {
      constant.trace();
    });

    true
  }

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
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
    mem::size_of::<Self>()
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
    mem::size_of_val(self) + self.capacity()
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
    mem::size_of_val(self) + self.capacity()
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
  /// use spacelox_core::value::{Closure, Fun, ArityKind};
  /// use spacelox_core::managed::{Managed, Allocation, make_managed};
  /// use spacelox_core::chunk::Chunk;
  /// use std::ptr::NonNull;
  ///
  /// let mut fun = Fun {
  ///   arity: ArityKind::Fixed(3),
  ///   upvalue_count: 2,
  ///   chunk: Chunk::default(),
  ///   name: Some("example".to_string())
  /// };
  ///
  /// let (managed_fun, alloc_fun) = make_managed(fun);
  ///
  /// let closure = Closure::new(managed_fun);
  /// assert_eq!(closure.fun.name.as_ref().unwrap().clone(), "example".to_string());
  /// ```
  pub fn new(fun: Managed<Fun>) -> Self {
    Closure {
      upvalues: Vec::with_capacity(fun.upvalue_count as usize),
      fun,
    }
  }

  pub fn name(&self) -> String {
    match &self.fun.name {
      Some(name) => format!("<fn {}>", name),
      None => "<script>".to_string(),
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
    mem::size_of::<Self>()
  }
}

#[derive(PartialEq, Clone)]
pub struct Class {
  pub name: Managed<String>,
  pub init: Option<Value>,
  // pub methods: FnvHashMap<Managed<String>, Value>,
  pub methods: DynamicMap<Managed<String>, Value>,
}

impl Class {
  pub fn new(name: Managed<String>) -> Self {
    Class {
      name,
      init: None,
      // methods: FnvHashMap::with_capacity_and_hasher(4, Default::default()),
      methods: DynamicMap::new(),
    }
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

    // self.methods.iter().for_each(|(key, val)| {
    //   key.trace();
    //   val.trace();
    // });

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

    // self.methods.iter().for_each(|(key, val)| {
    //   key.trace_debug(stdio);
    //   val.trace_debug(stdio);
    // });

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
  }
}

#[derive(PartialEq, Clone)]
pub struct Instance {
  pub class: Managed<Class>,
  pub fields: DynamicMap<Managed<String>, Value>,
}

impl Instance {
  pub fn new(class: Managed<Class>) -> Self {
    Instance {
      class,
      fields: DynamicMap::new(),
    }
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
