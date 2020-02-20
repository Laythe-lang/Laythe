use crate::chunk::Chunk;
use crate::native::NativeFun;
use crate::scanner::Token;
use crate::utils::{next_boundary, previous_boundary};
use crate::value::Value;
use std::cell::Cell;
use std::cell::RefCell;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::mem::{discriminant, replace};
use std::ptr;
use std::ptr::NonNull;
use std::rc::Rc;

#[derive(PartialEq, Clone, Debug)]
pub struct Closure<'a> {
  pub fun: *const Fun<'a>,
  pub upvalues: Vec<Rc<RefCell<Upvalue<'a>>>>,
}

impl<'a> Closure<'a> {
  /// Create a new closure using a pointer to an underlying Fun
  ///
  /// # Example
  /// ```
  /// use space_lox::object::{Closure, Fun};
  /// use space_lox::chunk::Chunk;
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
  pub fn new(fun: &Fun<'a>) -> Self {
    Closure {
      upvalues: Vec::with_capacity(fun.upvalue_count),
      fun,
    }
  }

  /// Dereferenced the underlying function captured by this closure
  ///
  /// # Example
  /// ```
  /// use space_lox::object::{Closure, Fun};
  /// use space_lox::chunk::Chunk;
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
  pub fn get_fun(&self) -> &Fun<'a> {
    unsafe { &*self.fun }
  }
}

/// A struct to capture the value by moving it to the heap
#[derive(PartialEq, Clone)]
pub struct Upvalue<'a> {
  /// Location of the value on the stack
  pub location: UpvalueLocation<'a>,

  /// reference to another upvalue
  pub next: Option<Rc<RefCell<Upvalue<'a>>>>,
}

impl<'a> Upvalue<'a> {
  /// Create a new instance of an upvalue
  ///
  /// # Examples
  /// ```
  /// use space_lox::object::{Obj, ObjValue, Upvalue};
  /// use space_lox::value::Value;
  /// use std::ptr::NonNull;
  ///
  /// let example = "something";
  /// let value = Value::Obj(Obj::new(ObjValue::String(NonNull::from(example))));
  ///
  /// let upvalue = Upvalue::new(&value, None);
  /// ```
  pub fn new(value: *const Value<'a>, next: Option<Rc<RefCell<Upvalue<'a>>>>) -> Self {
    Upvalue {
      location: UpvalueLocation::Stack(value),
      next,
    }
  }

  /// Return the underlying value to the caller as a ptr
  ///
  /// # Examples
  /// ```
  /// use space_lox::object::{Obj, ObjValue, Upvalue};
  /// use space_lox::value::Value;
  /// use std::ptr::{eq, NonNull};
  ///
  /// let example = "something";
  /// let value = Value::Obj(Obj::new(ObjValue::String(NonNull::from(example))));
  ///
  /// let upvalue = Upvalue::new(&value, None);
  /// assert!(eq(upvalue.as_ptr(), &value as *const Value))
  /// ```
  pub fn as_ptr(&self) -> *const Value<'a> {
    match &self.location {
      UpvalueLocation::Stack(loc) => *loc,
      UpvalueLocation::Heap(loc) => &**loc as *const Value<'a>,
    }
  }

  pub fn hoist(&mut self) {
    if let UpvalueLocation::Stack(loc) = self.location {
      let value = unsafe { &*loc }.clone();
      self.location = UpvalueLocation::Heap(Box::new(value))
    }
  }

  pub fn set(&mut self, value: *const Value<'a>) {
    if let UpvalueLocation::Heap(inner) = &mut self.location {
      replace(&mut **inner, unsafe { &*value }.clone());
    } else {
      self.location = UpvalueLocation::Stack(value);
    }
  }
}

impl<'a> fmt::Debug for Upvalue<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let location = match self.location {
      UpvalueLocation::Stack(_) => "stack".to_string(),
      UpvalueLocation::Heap(_) => "heap".to_string(),
    };

    write!(
      f,
      "Upvalue {{ value: {}, location: {} }}",
      unsafe { &*self.as_ptr() },
      location
    )
  }
}

#[derive(PartialEq, Clone)]
pub enum UpvalueLocation<'a> {
  Stack(*const Value<'a>),
  Heap(Box<Value<'a>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum FunKind {
  Fun,
  Script,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Fun<'a> {
  /// Arity of this function
  pub arity: u16,

  /// Number of upvalues
  pub upvalue_count: usize,

  /// Code for the function body
  pub chunk: Chunk<'a>,

  /// Name if not top-level script
  pub name: Option<String>,
}

impl<'a> Default for Fun<'a> {
  fn default() -> Self {
    Self {
      arity: 0,
      upvalue_count: 0,
      chunk: Chunk::default(),
      name: Some("null function".to_string()),
    }
  }
}

#[derive(Clone, Debug)]
pub enum ObjValue<'a> {
  String(NonNull<str>),
  Fun(Box<Fun<'a>>),
  Closure(Closure<'a>),
  NativeFn(Rc<dyn NativeFun<'a>>),
  Upvalue(Upvalue<'a>),
}

impl<'a> PartialEq for ObjValue<'a> {
  /// Determine if this `ObjValue` and another `ObjValue` are equal inside
  /// of the spacelox runtime
  ///
  /// # Examples
  /// ```
  /// use space_lox::object::{Obj, ObjValue};
  /// use std::ptr::NonNull;
  ///
  /// let str1 = "example1";
  /// let str2 = "example2";
  ///
  /// let obj1 = ObjValue::String(NonNull::from(str1));
  /// let obj2 = ObjValue::String(NonNull::from(str2));
  ///
  /// assert_eq!(obj1 == obj2, false);
  /// ```
  fn eq(&self, other: &ObjValue<'a>) -> bool {
    if discriminant(&self) != discriminant(&other) {
      return false;
    }

    match (&self, &other) {
      (ObjValue::String(str1), ObjValue::String(str2)) => str1 == str2,
      (ObjValue::Fun(fun1), ObjValue::Fun(fun2)) => fun1 == fun2,
      (ObjValue::Upvalue(upvalue1), ObjValue::Upvalue(upvalue2)) => upvalue1 == upvalue2,
      (ObjValue::Closure(func1), ObjValue::Closure(func2)) => func1 == func2,
      (ObjValue::NativeFn(native_fun1), ObjValue::NativeFn(native_fun2)) => {
        native_fun1 == native_fun2
      }
      _ => false,
    }
  }
}

#[derive(Clone, Debug)]
pub struct Obj<'a> {
  pub next: Cell<Option<&'a Obj<'a>>>,
  pub value: ObjValue<'a>,
}

impl<'a> PartialEq for Obj<'a> {
  /// Determine if this `Obj` and another `Obj` are equal inside
  /// of the spacelox runtime
  ///
  /// # Examples
  /// ```
  /// use space_lox::object::{Obj, ObjValue};
  /// use std::ptr::NonNull;
  ///
  /// let str1 = "example1";
  /// let str2 = "example2";
  ///
  /// let obj1 = Obj::new(ObjValue::String(NonNull::from(str1)));
  /// let obj2 = Obj::new(ObjValue::String(NonNull::from(str2)));
  ///
  /// assert_eq!(obj1 == obj2, false);
  /// ```
  fn eq(&self, other: &Obj<'a>) -> bool {
    self.value == other.value
  }
}

impl<'a> Eq for Obj<'a> {}

impl<'a> Hash for Obj<'a> {
  /// Produces a hash for the lox object provided.alloc
  ///
  /// # Example
  /// ```
  /// use space_lox::object::{Obj, ObjValue};
  /// use std::collections::{HashMap};
  /// use std::ptr::NonNull;
  ///
  /// let mut hash_map: HashMap<Obj, i32> = HashMap::new();
  /// let str1 = "example";
  /// let str2 = "example";
  /// let str3 = "example";
  ///
  /// let string_obj1 = Obj::new(ObjValue::String(NonNull::from(str1)));
  /// let string_obj2 = Obj::new(ObjValue::String(NonNull::from(str2)));
  /// let string_obj3 = Obj::new(ObjValue::String(NonNull::from(str3)));
  ///
  /// hash_map.insert(string_obj1, 10);
  /// assert_eq!(*hash_map.get(&string_obj3).unwrap(), 10);
  ///
  /// hash_map.insert(string_obj2, 20);
  /// assert_eq!(*hash_map.get(&string_obj3).unwrap(), 20);
  /// ```
  fn hash<H: Hasher>(&self, state: &mut H) {
    match &self.value {
      ObjValue::String(string) => string.hash(state),
      ObjValue::Fun(fun) => {
        ptr::hash(&**&fun, state);
      }
      ObjValue::Closure(closure) => {
        ptr::hash(closure.fun, state);
      }
      ObjValue::Upvalue(upvalue) => {
        ptr::hash(unsafe { &**&upvalue.as_ptr() }, state);
      }
      ObjValue::NativeFn(native_func) => ptr::hash(&native_func.meta(), state),
    }
  }
}

impl<'a> fmt::Display for Obj<'a> {
  /// Produce a human readable version of the display object
  ///
  /// # Example
  /// ```
  /// use space_lox::object::{Obj, ObjValue};
  /// use std::ptr::NonNull;
  ///
  /// let str = "example";
  /// let string_obj = Obj::new(ObjValue::String(NonNull::from(str)));
  /// assert_eq!(format!("{}", string_obj), "'example'");
  /// ```
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.value {
      ObjValue::String(store) => write!(f, "'{}'", unsafe { store.as_ref() }),
      ObjValue::Fun(fun) => match &fun.name {
        Some(name) => write!(f, "<fn {}>", name),
        None => write!(f, "<script>"),
      },
      ObjValue::Upvalue(upvalue) => write!(f, "<upvalue {}>", unsafe { &*upvalue.as_ptr() }),
      ObjValue::Closure(closure) => match unsafe { &(*closure.fun).name } {
        Some(name) => write!(f, "<fn {}>", name),
        None => write!(f, "<script>"),
      },
      ObjValue::NativeFn(native_fun) => write!(f, "<native {}>", native_fun.meta().name),
    }
  }
}

impl<'a> Obj<'a> {
  /// Construct a new object for spacelox
  pub fn new(value: ObjValue<'a>) -> Obj<'a> {
    Obj {
      value,
      next: Cell::new(Option::None),
    }
  }

  /// Convert spacelox value to string, panics if not a string
  ///
  /// # Examples
  /// ```
  /// use space_lox::object::{Obj, ObjValue};
  /// use std::ptr::NonNull;
  ///
  /// let str = "example";
  /// let obj1 = Obj::new(ObjValue::String(NonNull::from(str)));
  /// assert_eq!(obj1.ref_string(), "example");
  /// ```
  pub fn ref_string<'o>(&'o self) -> &'o str {
    match &self.value {
      ObjValue::String(str1) => unsafe { str1.as_ref() },
      _ => panic!("Expected string"),
    }
  }

  /// Convert spacelox value to function by move, panics if not a function
  ///
  /// # Examples
  /// ```
  /// use space_lox::object::{Obj, ObjValue, Fun};
  /// use space_lox::chunk::{Chunk};
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
  /// assert_eq!(obj1.move_fun().name.clone().unwrap(), "add");
  /// ```
  pub fn move_fun(self) -> Box<Fun<'a>> {
    match self.value {
      ObjValue::Fun(fun) => fun,
      _ => panic!("Expected function!"),
    }
  }

  /// Convert spacelox value to function by ref, panics if not a function
  ///
  /// # Examples
  /// ```
  /// use space_lox::object::{Obj, ObjValue, Fun};
  /// use space_lox::chunk::{Chunk};
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
  pub fn ref_fun<'o>(&'o self) -> &'o Fun<'a> {
    match &self.value {
      ObjValue::Fun(fun) => fun,
      _ => panic!("Expected function!"),
    }
  }

  /// Convert spacelox value to function, panics if not a function
  ///
  /// # Examples
  /// ```
  /// use space_lox::object::{Obj, ObjValue, Fun, Closure};
  /// use space_lox::chunk::{Chunk};
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
  /// assert_eq!(obj1.to_string(), "<fn add>");
  /// ```
  pub fn move_closure(self) -> Closure<'a> {
    match self.value {
      ObjValue::Closure(closure) => closure,
      _ => panic!("Expected function!"),
    }
  }

  /// Get a string representation of the underlying type this object representing
  ///
  /// # Examples
  /// ```
  /// use space_lox::value::Value;
  /// use space_lox::object::{Obj, ObjValue, Upvalue, Closure, Fun};
  /// use space_lox::chunk::{Chunk};
  /// use space_lox::native::create_natives;
  /// use std::ptr::NonNull;
  /// use std::rc::Rc;
  ///
  /// let fun_inner = Fun {
  ///   name: Some("add".to_string()),
  ///   arity: 3,
  ///   upvalue_count: 0,
  ///   chunk: Chunk::default()
  /// };
  ///
  /// let fun = Obj::new(ObjValue::Fun(Box::new(fun_inner.clone())));
  ///
  /// let str = "example";
  /// let string = Obj::new(ObjValue::String(NonNull::from(str)));
  ///
  /// let closure = Obj::new(ObjValue::Closure(Closure::new(&fun_inner)));
  ///
  /// let value = Value::Nil;
  /// let upvalue = Obj::new(ObjValue::Upvalue(Upvalue::new(&value, None)));
  ///
  /// let native = Obj::new(ObjValue::NativeFn(Rc::clone(&create_natives()[0])));
  ///
  /// assert_eq!(string.obj_type(), "string");
  /// assert_eq!(fun.obj_type(), "function");
  /// assert_eq!(closure.obj_type(), "closure");
  /// assert_eq!(upvalue.obj_type(), "upvalue");
  /// assert_eq!(native.obj_type(), "native");
  /// ```
  pub fn obj_type(&self) -> String {
    match self.value {
      ObjValue::String(_) => "string".to_string(),
      ObjValue::Fun(_) => "function".to_string(),
      ObjValue::Closure(_) => "closure".to_string(),
      ObjValue::Upvalue(_) => "upvalue".to_string(),
      ObjValue::NativeFn(_) => "native".to_string(),
    }
  }
}

/// Copy a string from the str backing the provided token. Note this copy
/// emits the enclosing quotes
///
/// # Examples
/// ```
/// use space_lox::object::{Obj, ObjValue, copy_string};
/// use space_lox::scanner::{Token, TokenKind};
///
/// let token = Token {
///   kind: TokenKind::String,
///   lexeme: "\"a cat in a hat\"".to_string(),
///   line: 0
/// };
///
/// let copy = copy_string(&token);
/// assert_eq!(copy, "a cat in a hat".to_string());
/// assert_ne!(copy, "\"a cat in a hat\"".to_string());
///
/// ```
pub fn copy_string(token: &Token) -> String {
  let start = next_boundary(&token.lexeme, 0);
  let end = previous_boundary(&token.lexeme, token.lexeme.len());

  token.lexeme[start..end].to_string()
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::native::create_natives;

  fn example_each<'a>(string: &'a str, value: &Value<'a>, fun: &Fun<'a>) -> Vec<Obj<'a>> {
    let natives = create_natives();

    vec![
      Obj::new(ObjValue::String(NonNull::from(string))),
      Obj::new(ObjValue::Fun(Box::new(fun.clone()))),
      Obj::new(ObjValue::Closure(Closure::new(fun))),
      Obj::new(ObjValue::NativeFn(Rc::clone(&natives[0]))),
      Obj::new(ObjValue::Upvalue(Upvalue::new(value, None))),
    ]
  }

  // value: *const Value<'a>, next: Option<Rc<RefCell<Upvalue<'a>>>>
  // String(&'a str),
  // Fun(Box<Fun<'a>>),
  // Closure(Closure<'a>),
  // NativeFn(NativeFun<'a>),
  // Upvalue(Upvalue<'a>),

  #[test]
  fn test_diff_type_no_equal() {
    let string = "example".to_string();
    let value = Value::Nil;
    let fun = Fun {
      arity: 0,
      upvalue_count: 0,
      chunk: Chunk::default(),
      name: Some("some_fun".to_string()),
    };

    let examples = example_each(&string, &value, &fun);
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
