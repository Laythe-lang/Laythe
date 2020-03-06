use crate::chunk::Chunk;
use crate::native::NativeFun;
use spacelox_interner::IStr;
use std::cell::Cell;
use std::cell::RefCell;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::mem::discriminant;
use std::ops::Deref;
use std::ptr;
use std::ptr::NonNull;
use std::rc::Rc;

#[derive(PartialEq, Clone)]
pub enum Upvalue {
  Open(usize),
  Closed(Rc<RefCell<Value>>),
}

impl Upvalue {
  /// Close over the upvalue by moving it onto the stack to the heap
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::object::Upvalue;
  /// use spacelox_core::value::Value;
  ///
  /// let stack = vec![
  ///   Value::Number(10)
  /// ];
  ///
  /// let mut upvalue = Upvalue::Open(0);
  /// upvalue.hoist(stack);
  ///
  /// match upvalue {
  ///   Upvalue::Open(_) => panic!(),
  ///   Upvalue::Closed(box) => assert_eq!(*box, Value::Number(10)),
  /// }
  /// ```
  pub fn hoist(&self, stack: &Vec<Value>) -> Upvalue {
    match self {
      Upvalue::Open(index) => {
        let value = unsafe { stack.get_unchecked(*index) }.clone();
        Upvalue::Closed(Rc::new(RefCell::new(value)))
      }
      Upvalue::Closed(_) => panic!("Attempted to hoist already hoisted upvalue."),
    }
  }

  /// Is this upvalue currently open
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::object::Upvalue;
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

#[derive(Debug, PartialEq, Clone)]
pub enum FunKind {
  Fun,
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
  pub name: Option<String>,
}

impl Default for Fun {
  fn default() -> Self {
    Self {
      arity: 0,
      upvalue_count: 0,
      chunk: Chunk::default(),
      name: Some("null function".to_string()),
    }
  }
}

/// Enum of value types in spacelox
#[derive(Clone)]
pub enum Value {
  Bool(bool),
  Nil,
  Number(f64),
  String(Managed<IStr>),
  Fun(Managed<Fun>),
  Closure(Managed<Closure>),
  Native(Managed<Rc<dyn NativeFun>>),
  Upvalue(Managed<Upvalue>),
}

impl Trace for IStr {}
impl Trace for Fun {}
impl Trace for Closure {}
impl Trace for Rc<dyn NativeFun> {}
impl Trace for Upvalue {}

impl fmt::Display for Value {
  /// Implement display for value in spacelox
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Number(num) => write!(f, "{}", num),
      Self::Bool(b) => write!(f, "{}", b),
      Self::Nil => write!(f, "nil"),
      Self::String(store) => write!(f, "'{}'", store),
      Self::Fun(fun) => match &fun.name {
        Some(name) => write!(f, "<fn {}>", name),
        None => write!(f, "<script>"),
      },
      Self::Upvalue(upvalue) => match upvalue.deref() {
        Upvalue::Open(index) => write!(f, "<upvalue open {}>", index),
        Upvalue::Closed(store) => write!(f, "<upvalue closed {}>", store.borrow()),
      },
      Self::Closure(closure) => match unsafe { &(*closure.fun).name } {
        Some(name) => write!(f, "<fn {}>", name),
        None => write!(f, "<script>"),
      },
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
      (Self::Fun(fun1), Self::Fun(fun2)) => ptr::eq(fun1, fun2),
      (Self::Closure(closure1), Self::Closure(closure2)) => ptr::eq(closure1, closure2),
      (Self::Native(native1), Self::Native(native2)) => ptr::eq(native1, native2),
      (Self::Upvalue(upvalue1), Self::Upvalue(upvalue2)) => ptr::eq(upvalue1, upvalue2),
      _ => false,
    }
  }
}

impl Value {
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

  /// Convert spacelox value to string, panics if not a string
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::object::{Obj, ObjValue};
  /// use std::ptr::NonNull;
  ///
  /// let str = "example";
  /// let obj1 = Obj::new(ObjValue::String(NonNull::from(str)));
  /// assert_eq!(obj1.ref_string(), "example");
  /// ```
  pub fn ref_string<'o>(&'o self) -> &'o str {
    match &self {
      Self::String(str1) => &str1,
      _ => panic!("Expected string"),
    }
  }

  /// Convert spacelox value to function by ref, panics if not a function
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::object::{Obj, ObjValue, Fun};
  /// use spacelox_core::chunk::{Chunk};
  /// use std::rc::Rc;
  ///
  /// let func = Fun {
  ///   name: Some("add".to_string()),
  ///   arity: 3,
  ///   upvalue_count: 0,
  ///   chunk: Chunk::default()
  /// };
  ///
  /// let obj1 = Obj::new(ObjValue::Fun(Box::new(func)));
  /// assert_eq!(obj1.ref_fun().name.clone().unwrap(), "add");
  /// ```
  pub fn ref_fun<'o>(&'o self) -> &'o Fun {
    match &self {
      Self::Fun(fun) => fun,
      _ => panic!("Expected function!"),
    }
  }

  /// Convert spacelox value to function, panics if not a function
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::object::{Obj, ObjValue, Fun, Closure};
  /// use spacelox_core::chunk::{Chunk};
  /// use std::rc::Rc;
  ///
  /// let fun = Fun {
  ///   name: Some("add".to_string()),
  ///   arity: 3,
  ///   upvalue_count: 0,
  ///   chunk: Chunk::default()
  /// };
  ///
  /// let obj1 = Obj::new(ObjValue::Closure(Closure::new(&fun)));
  /// assert_eq!(obj1.ref_closure().fun.name.clone().unwrap(), "add".to_string());
  /// ```
  pub fn ref_closure(&self) -> &Closure {
    match &self {
      Self::Closure(closure) => closure,
      _ => panic!("Expected closure!"),
    }
  }

  /// Convert spacelox value to function, panics if not a function
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::object::{Obj, ObjValue, Fun, Closure};
  /// use spacelox_core::chunk::{Chunk};
  /// use std::rc::Rc;
  ///
  /// let fun = Fun {
  ///   name: Some("add".to_string()),
  ///   arity: 3,
  ///   upvalue_count: 0,
  ///   chunk: Chunk::default()
  /// };
  ///
  /// let obj1 = Obj::new(ObjValue::Closure(Closure::new(&fun)));
  /// assert_eq!(obj1.ref_closure().fun.name.clone().unwrap(), "add".to_string());
  /// ```
  pub fn ref_upvalue(&self) -> &Upvalue {
    match &self {
      Self::Upvalue(upvalue) => &upvalue,
      _ => panic!("Expected upvalue!"),
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
      Value::Upvalue(_) => "upvalue".to_string(),
      Value::Native(_) => "native".to_string(),
    }
  }
}

#[derive(PartialEq, Clone)]
pub struct Closure {
  pub fun: *const Fun,
  pub upvalues: Vec<Value>,
}

impl Closure {
  /// Create a new closure using a pointer to an underlying Fun
  ///
  /// # Example
  /// ```
  /// use spacelox_core::object::{Closure, Fun};
  /// use spacelox_core::chunk::Chunk;
  ///
  /// let fun = Box::new(Fun {
  ///   arity: 3,
  ///   upvalue_count: 2,
  ///   chunk: Chunk::default(),
  ///   name: Some("example".to_string())
  /// });
  ///
  /// let closure = Closure::new(&fun);
  /// assert_eq!(closure.get_fun().name.as_ref().unwrap().clone(), "example".to_string());
  /// ```
  pub fn new(fun: &Fun) -> Self {
    Closure {
      upvalues: Vec::with_capacity(fun.upvalue_count),
      fun,
    }
  }

  /// Dereferenced the underlying function captured by this closure
  ///
  /// # Example
  /// ```
  /// use spacelox_core::object::{Closure, Fun};
  /// use spacelox_core::chunk::Chunk;
  ///
  /// let fun = Box::new(Fun {
  ///   arity: 3,
  ///   upvalue_count: 2,
  ///   chunk: Chunk::default(),
  ///   name: Some("example".to_string())
  /// });
  ///
  /// let closure = Closure::new(&fun);
  /// assert_eq!(closure.get_fun().name.as_ref().unwrap().clone(), "example".to_string());
  /// ```
  pub fn get_fun(&self) -> &Fun {
    unsafe { &*self.fun }
  }
}

pub trait Trace {}

#[derive(Debug, Default)]
pub struct Header {
  marked: Cell<bool>,
}

#[derive(Debug)]
pub struct Allocation<T: 'static + Trace + ?Sized> {
  header: Header,
  data: T,
}

impl<T: 'static + Trace> Allocation<T> {
  pub fn new(data: T) -> Self {
    Self {
      data,
      header: Header::default(),
    }
  }
}

impl<T: 'static + Trace + ?Sized> Allocation<T> {
  pub fn mark(&self) {
    self.header.marked.replace(true);
  }

  pub fn marked(&self) -> bool {
    self.header.marked.get()
  }
}

pub struct Managed<T: 'static + Trace + ?Sized> {
  ptr: NonNull<Allocation<T>>,
}

impl<T: 'static + Trace + ?Sized> From<NonNull<Allocation<T>>> for Managed<T> {
  fn from(fun: NonNull<Allocation<T>>) -> Self {
    Self { ptr: fun }
  }
}

impl<T: 'static + Trace + ?Sized> Managed<T> {
  pub fn obj(&self) -> &Allocation<T> {
    unsafe { &self.ptr.as_ref() }
  }
}

impl<T: 'static + Trace + ?Sized> Copy for Managed<T> {}
impl<T: 'static + Trace + ?Sized> Clone for Managed<T> {
  fn clone(&self) -> Managed<T> {
    *self
  }
}

impl<T: 'static + Trace + ?Sized> Deref for Managed<T> {
  type Target = T;

  fn deref(&self) -> &T {
    &self.obj().data
  }
}

impl<T: 'static + PartialEq + Trace + ?Sized> PartialEq for Managed<T> {
  fn eq(&self, other: &Managed<T>) -> bool {
    let left_inner: &T = &*self;
    let right_inner: &T = &*other;
    left_inner.eq(right_inner)
  }
}
impl<T: 'static + Eq + Trace + ?Sized> Eq for Managed<T> {}

impl<T: 'static + Hash + Trace + ?Sized> Hash for Managed<T> {
  fn hash<H: Hasher>(&self, state: &mut H) {
    let inner: &T = &*self;
    inner.hash(state)
  }
}

impl<T: 'static + fmt::Debug + Trace + ?Sized> fmt::Debug for Managed<T> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let inner: &T = &*self;
    write!(f, "Managed({:?})", inner)
  }
}

impl<T: 'static + fmt::Display + Trace + ?Sized> fmt::Display for Managed<T> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let inner: &T = &*self;
    inner.fmt(f)
  }
}

#[cfg(test)]
mod test {
  use super::*;

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
    let mut string_alloc = Box::new(Allocation {
      header: Header::default(),
      data: IStr::new("data"),
    });

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
