use crate::chunk::Chunk;
use crate::{
  managed::{Manage, Managed, Trace},
  native::NativeFun,
  utils::do_if_some,
};
use spacelox_interner::IStr;
use std::collections::HashMap;
use std::fmt;
use std::mem::discriminant;
use std::mem::replace;
use std::rc::Rc;

/// Enum of value types in spacelox
#[derive(Clone, Copy, Debug)]
pub enum Value {
  Bool(bool),
  Nil,
  Number(f64),
  String(Managed<IStr>),
  Fun(Managed<Fun>),
  Closure(Managed<Closure>),
  Class(Managed<Class>),
  Instance(Managed<Instance>),
  Method(Managed<BoundMethod>),
  Native(Managed<Rc<dyn NativeFun>>),
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
  /// use spacelox_core::managed::{Allocation, Managed};
  /// use spacelox_interner::IStr;
  /// use std::ptr::NonNull;
  ///
  /// let str = IStr::new("example");
  /// let mut alloc = Box::new(Allocation::new(str));
  /// let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };
  /// let managed = Managed::from(ptr);
  ///
  /// let value = Value::String(managed);
  /// assert_eq!(*value.to_string(), IStr::new("example"));
  /// ```
  pub fn to_string<'o>(&'o self) -> Managed<IStr> {
    match self {
      Self::String(str1) => *str1,
      _ => panic!("Expected string"),
    }
  }

  /// Unwrap and reference a spacelox function, panics if not a function
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::value::{Value, Fun};
  /// use spacelox_core::managed::{Allocation, Managed};
  /// use spacelox_core::chunk::Chunk;
  /// use spacelox_interner::IStr;
  /// use std::ptr::NonNull;
  ///
  /// let fun = Fun {
  ///   name: Some(IStr::new("add")),
  ///   arity: 3,
  ///   upvalue_count: 0,
  ///   chunk: Chunk::default()
  /// };
  /// let mut alloc = Box::new(Allocation::new(fun));
  /// let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };
  /// let managed = Managed::from(ptr);
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

  /// Unwrap and reference a spacelox closure, panics if not a closure
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::value::{Value, Closure, Fun};
  /// use spacelox_core::managed::{Managed, Allocation};
  /// use spacelox_core::chunk::Chunk;
  /// use spacelox_interner::IStr;
  /// use std::ptr::NonNull;
  ///
  /// let fun = Fun {
  ///   name: Some(IStr::new("add")),
  ///   arity: 3,
  ///   upvalue_count: 0,
  ///   chunk: Chunk::default()
  /// };
  /// let mut alloc_fun = Box::new(Allocation::new(fun));
  /// let managed_fun = Managed::from(unsafe { NonNull::new_unchecked(&mut *alloc_fun) });
  ///
  /// let closure = Closure::new(managed_fun);
  ///
  /// let mut alloc_closure = Box::new(Allocation::new(closure));
  /// let managed_closure = Managed::from(unsafe { NonNull::new_unchecked(&mut *alloc_closure) });
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
  /// use spacelox_core::managed::{Allocation, Managed};
  /// use std::ptr::NonNull;
  ///
  /// let upvalue = Upvalue::Open(0);
  ///
  /// let mut alloc = Box::new(Allocation::new(upvalue));
  /// let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };
  /// let managed = Managed::from(ptr);
  ///
  /// let value = Value::Upvalue(managed);
  ///
  /// match *value.to_upvalue() {
  ///   Upvalue::Open(index) => assert_eq!(index, 0),
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
  /// use spacelox_interner::IStr;
  /// use std::ptr::NonNull;
  ///
  /// let (name, name_alloc) = make_managed(IStr::new("example"));
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
      Value::Fun(_) => "function".to_string(),
      Value::Closure(_) => "closure".to_string(),
      Value::Method(_) => "method".to_string(),
      Value::Class(_) => "class".to_string(),
      Value::Instance(_) => "instance".to_string(),
      Value::Upvalue(_) => "upvalue".to_string(),
      Value::Native(_) => "native".to_string(),
    }
  }

  pub fn get_dyn_managed(&self) -> Option<Managed<dyn Manage>> {
    match self {
      Value::String(string) => Some(string.clone_dyn()),
      Value::Fun(fun) => Some(fun.clone_dyn()),
      Value::Closure(closure) => Some(closure.clone_dyn()),
      Value::Native(native) => Some(native.clone_dyn()),
      Value::Upvalue(upvalue) => Some(upvalue.clone_dyn()),
      Value::Method(method) => Some(method.clone_dyn()),
      Value::Class(class) => Some(class.clone_dyn()),
      Value::Instance(instance) => Some(instance.clone_dyn()),
      _ => None,
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
      Self::String(store) => write!(f, "'{}'", store.as_str()),
      Self::Fun(fun) => match &fun.name {
        Some(name) => write!(f, "<fn {}>", name),
        None => write!(f, "<script>"),
      },
      Self::Upvalue(upvalue) => match &**upvalue {
        Upvalue::Open(index) => write!(f, "<upvalue open {}>", index),
        Upvalue::Closed(store) => write!(f, "<upvalue closed {}>", store),
      },
      Self::Closure(closure) => match &closure.fun.name {
        Some(name) => write!(f, "<fn {}>", name),
        None => write!(f, "<script>"),
      },
      Self::Method(bound) => match &bound.method.fun.name {
        Some(name) => write!(f, "<fn {}>", name),
        None => panic!("Method not assigned a name"),
      },
      Self::Class(class) => write!(f, "{}", &class.name.as_str()),
      Self::Instance(instance) => write!(f, "{} instance", &instance.class.name.as_str()),
      Self::Native(native_fun) => write!(f, "<native {}>", native_fun.meta().name),
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
    // check we're the same variant
    if discriminant(self) != discriminant(other) {
      return false;
    }

    // check the the variants have the same value
    match (self, other) {
      (Self::Number(num1), Self::Number(num2)) => num1 == num2,
      (Self::Bool(b1), Self::Bool(b2)) => b1 == b2,
      (Self::Nil, Self::Nil) => true,
      (Self::String(string1), Self::String(string2)) => string1 == string2,
      (Self::Fun(fun1), Self::Fun(fun2)) => fun1 == fun2,
      (Self::Closure(closure1), Self::Closure(closure2)) => closure1 == closure2,
      (Self::Method(method1), Self::Method(method2)) => method1 == method2,
      (Self::Native(native1), Self::Native(native2)) => native1 == native2,
      (Self::Upvalue(upvalue1), Self::Upvalue(upvalue2)) => upvalue1 == upvalue2,
      (Self::Class(class1), Self::Class(class2)) => class1 == class2,
      (Self::Instance(instance1), Self::Instance(instance2)) => instance1 == instance2,
      _ => false,
    }
  }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Upvalue {
  Open(usize),
  Closed(Box<Value>),
}

impl Upvalue {
  /// Close over the upvalue by moving it onto the stack to the heap
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::value::{Value, Upvalue};
  /// use std::rc::Rc;
  ///
  /// let stack = vec![
  ///   Value::Number(10.0)
  /// ];
  ///
  /// let mut upvalue = Upvalue::Open(0);
  /// upvalue.hoist(&stack);
  ///
  /// match upvalue {
  ///   Upvalue::Closed(store) => assert_eq!(*store, Value::Number(10.0)),
  ///   Upvalue::Open(_) => assert!(false),
  /// };
  /// ```
  pub fn hoist(&mut self, stack: &Vec<Value>) {
    match self {
      Upvalue::Open(index) => {
        let value = unsafe { stack.get_unchecked(*index) }.clone();
        replace(self, Upvalue::Closed(Box::new(value)));
      }
      Upvalue::Closed(_) => panic!("Attempted to hoist already hoisted upvalue."),
    }
  }

  /// Is this upvalue currently open
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::value::Upvalue;
  ///
  /// let upvalue = Upvalue::Open(0);
  /// assert_eq!(upvalue.is_open(), true);
  /// ```
  pub fn is_open(&self) -> bool {
    match self {
      Upvalue::Open(_) => true,
      Upvalue::Closed(_) => false,
    }
  }
}

impl Trace for Upvalue {
  fn trace(&self, mark: &mut dyn FnMut(Managed<dyn Manage>)) -> bool {
    match self {
      Upvalue::Closed(upvalue) => do_if_some(upvalue.get_dyn_managed(), |obj| mark(obj)),
      _ => (),
    }

    true
  }
}

impl Manage for Upvalue {
  fn alloc_type(&self) -> &str {
    "upvalue"
  }

  fn debug(&self) -> String {
    format!("{:?}", self).to_string()
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
  pub arity: u16,

  /// Number of upvalues
  pub upvalue_count: usize,

  /// Code for the function body
  pub chunk: Chunk,

  /// Name if not top-level script
  pub name: Option<IStr>,
}

impl Default for Fun {
  fn default() -> Self {
    Self {
      arity: 0,
      upvalue_count: 0,
      chunk: Chunk::default(),
      name: Some(IStr::new("null function")),
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
  fn trace(&self, mark: &mut dyn FnMut(Managed<dyn Manage>)) -> bool {
    self
      .chunk
      .constants
      .iter()
      .for_each(|constant| do_if_some(constant.get_dyn_managed(), |obj| mark(obj)));

    true
  }
}

impl Manage for Fun {
  fn alloc_type(&self) -> &str {
    "function"
  }

  fn debug(&self) -> String {
    format!("{:?}", self).to_string()
  }
}

impl Trace for IStr {
  fn trace(&self, _: &mut dyn FnMut(Managed<dyn Manage>)) -> bool {
    true
  }
}

impl Manage for IStr {
  fn alloc_type(&self) -> &str {
    "string"
  }

  fn debug(&self) -> String {
    format!("{:?}", self).to_string()
  }
}

impl Trace for Rc<dyn NativeFun> {
  fn trace(&self, _: &mut dyn FnMut(Managed<dyn Manage>)) -> bool {
    true
  }
}

impl Manage for Rc<dyn NativeFun> {
  fn alloc_type(&self) -> &str {
    "native"
  }

  fn debug(&self) -> String {
    format!("{:?}", self).to_string()
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
  /// use spacelox_core::managed::{Managed, Allocation};
  /// use spacelox_core::chunk::Chunk;
  /// use spacelox_interner::IStr;
  /// use std::ptr::NonNull;
  ///
  /// let mut fun = Box::new(Allocation::new(Fun {
  ///   arity: 3,
  ///   upvalue_count: 2,
  ///   chunk: Chunk::default(),
  ///   name: Some(IStr::new("example"))
  /// }));
  ///
  /// let managed_fun = Managed::from(unsafe { NonNull::new_unchecked(&mut *fun) });
  ///
  /// let closure = Closure::new(managed_fun);
  /// assert_eq!(closure.fun.name.as_ref().unwrap().clone(), "example".to_string());
  /// ```
  pub fn new(fun: Managed<Fun>) -> Self {
    Closure {
      upvalues: Vec::with_capacity(fun.upvalue_count),
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
  fn trace(&self, mark: &mut dyn FnMut(Managed<dyn Manage>)) -> bool {
    self
      .upvalues
      .iter()
      .for_each(|constant| do_if_some(constant.get_dyn_managed(), |obj| mark(obj)));

    mark(self.fun.clone_dyn());
    true
  }
}

impl Manage for Closure {
  fn alloc_type(&self) -> &str {
    "closure"
  }

  fn debug(&self) -> String {
    format!("{:?}", self).to_string()
  }
}

#[derive(PartialEq, Clone)]
pub struct Class {
  pub name: Managed<IStr>,
  pub init: Option<Managed<Closure>>,
  pub methods: HashMap<Managed<IStr>, Managed<Closure>>,
}

impl Class {
  pub fn new(name: Managed<IStr>) -> Self {
    Class {
      name,
      init: None,
      methods: HashMap::new(),
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
  fn trace(&self, mark: &mut dyn FnMut(Managed<dyn Manage>)) -> bool {
    mark(self.name.clone_dyn());
    do_if_some(self.init, |init| mark(init.clone_dyn()));

    self.methods.iter().for_each(|(key, val)| {
      mark(key.clone_dyn());
      mark(val.clone_dyn());
    });

    true
  }
}

impl Manage for Class {
  fn alloc_type(&self) -> &str {
    "class"
  }

  fn debug(&self) -> String {
    format!("{:?}", self).to_string()
  }
}

#[derive(PartialEq, Clone)]
pub struct Instance {
  pub class: Managed<Class>,
  pub fields: HashMap<Managed<IStr>, Value>,
}

impl Instance {
  pub fn new(class: Managed<Class>) -> Self {
    Instance {
      class,
      fields: HashMap::new(),
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
  fn trace(&self, mark: &mut dyn FnMut(Managed<dyn Manage>)) -> bool {
    mark(self.class.clone_dyn());

    self.fields.iter().for_each(|(key, val)| {
      mark(key.clone_dyn());
      do_if_some(val.get_dyn_managed(), |obj| mark(obj));
    });

    true
  }
}

impl Manage for Instance {
  fn alloc_type(&self) -> &str {
    "instance"
  }

  fn debug(&self) -> String {
    format!("{:?}", self).to_string()
  }
}

#[derive(PartialEq, Clone)]
pub struct BoundMethod {
  pub receiver: Value,
  pub method: Managed<Closure>,
}

impl BoundMethod {
  pub fn new(receiver: Value, method: Managed<Closure>) -> Self {
    Self { receiver, method }
  }
}

impl fmt::Debug for BoundMethod {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    f.debug_struct("BoundMethod")
      .field("receiver", &self.receiver)
      .field("method", &self.method)
      .finish()
  }
}

impl Trace for BoundMethod {
  fn trace(&self, mark: &mut dyn FnMut(Managed<dyn Manage>)) -> bool {
    do_if_some(self.receiver.get_dyn_managed(), |obj| mark(obj));
    mark(self.method.clone_dyn());
    true
  }
}

impl Manage for BoundMethod {
  fn alloc_type(&self) -> &str {
    "method"
  }

  fn debug(&self) -> String {
    format!("{:?}", self).to_string()
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::managed::Allocation;
  use std::ptr::NonNull;

  fn example_each(string: Managed<IStr>) -> Vec<Value> {
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
    let mut string_alloc = Box::new(Allocation::new(IStr::new("data")));

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
