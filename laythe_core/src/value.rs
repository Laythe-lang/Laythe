pub struct Nil();

/// Enum of value types in laythe
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum ValueKind {
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
  Iter,
  Method,
  Native,
  Upvalue,
}

#[macro_export]
macro_rules! val {
  ( $x:expr ) => {
    Value::from($x)
  };
}

#[cfg(not(feature = "nan_boxing"))]
pub use self::unboxed::*;

#[cfg(feature = "nan_boxing")]
pub use self::boxed::*;

#[cfg(not(feature = "nan_boxing"))]
mod unboxed {
  use super::*;
  use crate::{
    iterator::LyIterator,
    native::Native,
    object::{Class, Closure, Fun, Instance, List, Map, Method, Upvalue},
  };
  use laythe_env::{
    managed::{Managed, Trace},
    stdio::Stdio,
  };

  use smol_str::SmolStr;
  use std::fmt;
  use std::hash::Hash;

  pub const VALUE_NIL: Value = Value::Nil;
  pub const VALUE_FALSE: Value = Value::Bool(false);
  pub const VALUE_TRUE: Value = Value::Bool(true);

  /// Enum of value types in laythe
  #[derive(Clone, Copy, Debug)]
  pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    String(Managed<SmolStr>),
    List(Managed<List<Value>>),
    Map(Managed<Map<Value, Value>>),
    Fun(Managed<Fun>),
    Closure(Managed<Closure>),
    Class(Managed<Class>),
    Instance(Managed<Instance>),
    Method(Managed<Method>),
    Iter(Managed<LyIterator>),
    Native(Managed<Box<dyn Native>>),
    Upvalue(Managed<Upvalue>),
  }

  impl Value {
    /// Is this laythe value nil
    ///
    /// # Examples
    /// ```
    /// use laythe_core::value::Value;
    ///
    /// let val1 = Value::Nil;
    /// assert_eq!(val1.is_nil(), true);
    /// ```
    #[inline]
    pub fn is_nil(&self) -> bool {
      match self {
        Value::Nil => true,
        _ => false,
      }
    }

    #[inline]
    pub fn is_bool(&self) -> bool {
      match self {
        Value::Bool(_) => true,
        _ => false,
      }
    }

    #[inline]
    pub fn is_false(&self) -> bool {
      match self {
        Value::Bool(b) => !b,
        _ => false,
      }
    }

    #[inline]
    pub fn is_num(&self) -> bool {
      match self {
        Value::Number(_) => true,
        _ => false,
      }
    }

    #[inline]
    pub fn is_str(&self) -> bool {
      match self {
        Value::String(_) => true,
        _ => false,
      }
    }

    #[inline]
    pub fn is_list(&self) -> bool {
      match self {
        Value::List(_) => true,
        _ => false,
      }
    }

    #[inline]
    pub fn is_map(&self) -> bool {
      match self {
        Value::Map(_) => true,
        _ => false,
      }
    }

    #[inline]
    pub fn is_iter(&self) -> bool {
      match self {
        Value::Iter(_) => true,
        _ => false,
      }
    }

    #[inline]
    pub fn is_closure(&self) -> bool {
      match self {
        Value::Closure(_) => true,
        _ => false,
      }
    }

    #[inline]
    pub fn is_fun(&self) -> bool {
      match self {
        Value::Fun(_) => true,
        _ => false,
      }
    }

    #[inline]
    pub fn is_class(&self) -> bool {
      match self {
        Value::Class(_) => true,
        _ => false,
      }
    }

    #[inline]
    pub fn is_instance(&self) -> bool {
      match self {
        Value::Instance(_) => true,
        _ => false,
      }
    }

    #[inline]
    pub fn is_method(&self) -> bool {
      match self {
        Value::Method(_) => true,
        _ => false,
      }
    }

    #[inline]
    pub fn is_native(&self) -> bool {
      match self {
        Value::Native(_) => true,
        _ => false,
      }
    }

    #[inline]
    pub fn is_upvalue(&self) -> bool {
      match self {
        Value::Upvalue(_) => true,
        _ => false,
      }
    }

    /// Convert laythe value to number, panics if not a number
    ///
    /// # Examples
    /// ```
    /// use laythe_core::value::Value;
    ///
    /// let val1 = Value::Number(20.0);
    /// assert_eq!(val1.to_num(), 20.0);
    /// ```
    #[inline]
    pub fn to_num(&self) -> f64 {
      match self {
        Value::Number(num) => *num,
        _ => panic!("Value is not number"),
      }
    }

    /// Convert laythe value to boolean, panics if not a bool
    ///
    /// # Examples
    /// ```
    /// use laythe_core::value::Value;
    ///
    /// let b1 = Value::Bool(false);
    /// assert_eq!(b1.to_bool(), false);
    /// ```
    #[inline]
    pub fn to_bool(&self) -> bool {
      match self {
        Value::Bool(b1) => *b1,
        _ => panic!("Value is not boolean"),
      }
    }

    /// Unwrap and reference a laythe string, panics if not a string
    ///
    /// # Examples
    /// ```
    /// use laythe_core::value::Value;
    /// use laythe_core::hooks::{Hooks, NoContext};
    /// use laythe_env::memory::Gc;
    ///
    /// let gc = Gc::default();
    /// let mut context = NoContext::new(&gc);
    /// let hooks = Hooks::new(&mut context);
    /// let managed =  hooks.manage_str("example");
    ///
    /// let value = Value::String(managed);
    /// assert_eq!(&*value.to_str(), "example")
    /// ```
    #[inline]
    pub fn to_str(&self) -> Managed<SmolStr> {
      match self {
        Self::String(str1) => *str1,
        _ => panic!("Expected string."),
      }
    }

    /// Unwrap and reference a laythe list, panics if not a list
    ///
    /// # Examples
    /// ```
    /// use laythe_core::value::Value;
    /// use laythe_core::object::LyVec;
    /// use laythe_env::managed::{Allocation, Managed};
    ///
    /// use std::ptr::NonNull;
    ///
    /// let list = LyVec::from(vec![Value::Nil]);
    /// let mut alloc = Box::new(Allocation::new(list));
    /// let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };
    /// let managed = Managed::from(ptr);
    ///
    /// let value = Value::List(managed);
    /// assert_eq!(value.to_list()[0], Value::Nil)
    /// ```
    #[inline]
    pub fn to_list(&self) -> Managed<List<Value>> {
      match self {
        Self::List(list) => *list,
        _ => panic!("Expected list."),
      }
    }

    /// Unwrap and reference a laythe list, panics if not a list
    ///
    /// # Examples
    /// ```
    /// use laythe_core::value::Value;
    /// use laythe_env::managed::{Allocation, Managed, make_managed};
    /// use laythe_core::object::LyHashMap;
    /// use std::ptr::NonNull;
    ///
    /// let map: LyHashMap<Value, Value> = LyHashMap::default();
    /// let mut alloc = Box::new(Allocation::new(map));
    /// let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };
    /// let managed = Managed::from(ptr);
    ///
    /// let value = Value::Map(managed);
    /// assert_eq!(value.to_map().len(), 0)
    /// ```
    #[inline]
    pub fn to_map(&self) -> Managed<Map<Value, Value>> {
      match self {
        Self::Map(map) => *map,
        _ => panic!("Expected list."),
      }
    }

    /// Unwrap and reference a laythe iterator, panics if not a iterator
    #[inline]
    pub fn to_iter(&self) -> Managed<LyIterator> {
      match self {
        Self::Iter(iter) => *iter,
        _ => panic!("Expected iterator."),
      }
    }

    /// Unwrap and reference a laythe function, panics if not a function
    ///
    /// # Examples
    /// ```
    /// use laythe_core::value::Value;
    /// use laythe_core::object::Fun;
    /// use laythe_core::module::Module;
    /// use laythe_core::chunk::Chunk;
    /// use laythe_core::hooks::{Hooks, NoContext};
    /// use laythe_env::memory::Gc;
    /// use std::path::PathBuf;
    ///
    /// let gc = Gc::default();
    /// let mut context = NoContext::new(&gc);
    /// let hooks = Hooks::new(&mut context);
    ///
    /// let module = hooks.manage(Module::new(hooks.manage_str("module"), hooks.manage(PathBuf::from("self/module.ly"))));
    /// let fun: Fun = Fun::new(hooks.manage_str("add"), module);
    /// let managed = hooks.manage(fun);
    ///
    /// let value = Value::Fun(managed);
    /// assert_eq!(&*value.to_fun().name, "add");
    /// ```
    #[inline]
    pub fn to_fun(&self) -> Managed<Fun> {
      match self {
        Self::Fun(fun) => *fun,
        _ => panic!("Expected function!"),
      }
    }

    /// Unwrap a laythe native function, panics if not a native function
    #[inline]
    pub fn to_native(&self) -> Managed<Box<dyn Native>> {
      match self {
        Self::Native(native_fun) => *native_fun,
        _ => panic!("Expected native function!"),
      }
    }

    /// Unwrap and reference a laythe closure, panics if not a closure
    ///
    /// # Examples
    /// ```
    /// use laythe_core::value::Value;
    /// use laythe_core::object::{Closure, Fun};
    /// use laythe_env::memory::{Gc, NO_GC};
    /// use laythe_core::module::Module;
    /// use laythe_core::chunk::Chunk;
    /// use laythe_core::hooks::{Hooks, NoContext};
    /// use std::path::PathBuf;
    ///
    /// let gc = Gc::default();
    /// let mut context = NoContext::new(&gc);
    /// let hooks = Hooks::new(&mut context);
    ///
    /// let module = hooks.manage(Module::new(hooks.manage_str("module"), hooks.manage(PathBuf::from("self/module.ly"))));
    /// let fun: Fun = Fun::new(hooks.manage_str("add"), module);
    /// let managed_fun = hooks.manage(fun);
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

    /// Unwrap and reference a laythe method, panics if not a method
    ///
    /// # Examples
    /// ```
    /// use laythe_core::value::Value;
    /// use laythe_core::object::{Closure, Method, Fun};
    /// use laythe_core::module::Module;
    /// use laythe_env::memory::{Gc, NO_GC};
    /// use laythe_core::chunk::Chunk;
    /// use laythe_core::hooks::{Hooks, NoContext};
    /// use std::path::PathBuf;
    ///
    /// let gc = Gc::default();
    /// let mut context = NoContext::new(&gc);
    /// let hooks = Hooks::new(&mut context);
    ///
    /// let module = hooks.manage(Module::new(hooks.manage_str("module"), hooks.manage(PathBuf::from("self/module.ly"))));
    /// let fun: Fun = Fun::new(hooks.manage_str("add"), module);
    /// let managed_fun = hooks.manage(fun);
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

    /// Unwrap and reference a laythe upvalue, panics if not a upvalue.
    ///
    /// # Examples
    /// ```
    /// use laythe_core::value::Value;
    /// use laythe_core::object::Upvalue;
    /// use laythe_env::managed::{Allocation, Managed, make_managed};
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

    /// Unwrap and reference a laythe instance, panics if not a instance
    ///
    /// # Examples
    /// ```
    /// use laythe_core::value::Value;
    /// use laythe_core::object::{Instance, Class};
    /// use laythe_env::managed::{Managed, Allocation, make_managed};
    ///
    /// let (name, name_alloc) = make_managed("example".to_string());
    /// let (class, class_alloc) = make_managed(Class::bare(name));
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

    /// Unwrap and reference a laythe instance, panics if not a instance
    ///
    /// # Examples
    /// ```
    /// use laythe_core::value::Value;
    /// use laythe_core::object::{Instance, Class};
    /// use laythe_env::managed::{Managed, Allocation, make_managed};
    ///
    /// let (name, name_alloc) = make_managed("example".to_string());
    /// let (class, class_alloc) = make_managed(Class::bare(name));
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
    /// use laythe_core::value::Value;
    /// use laythe_core::object::{Obj, ObjValue};
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
        Value::Iter(_) => "iterator".to_string(),
        Value::Upvalue(_) => "upvalue".to_string(),
        Value::Native(_) => "native".to_string(),
      }
    }

    pub fn kind(&self) -> ValueKind {
      match self {
        Value::Nil => ValueKind::Nil,
        Value::Bool(_) => ValueKind::Bool,
        Value::Number(_) => ValueKind::Number,
        Value::String(_) => ValueKind::String,
        Value::List(_) => ValueKind::List,
        Value::Map(_) => ValueKind::Map,
        Value::Fun(_) => ValueKind::Fun,
        Value::Closure(_) => ValueKind::Closure,
        Value::Method(_) => ValueKind::Method,
        Value::Class(_) => ValueKind::Class,
        Value::Instance(_) => ValueKind::Instance,
        Value::Iter(_) => ValueKind::Iter,
        Value::Upvalue(_) => ValueKind::Upvalue,
        Value::Native(_) => ValueKind::Native,
      }
    }
  }

  impl From<Nil> for Value {
    fn from(_: Nil) -> Self {
      Value::Nil
    }
  }

  impl From<bool> for Value {
    fn from(b: bool) -> Self {
      Value::Bool(b)
    }
  }

  impl From<f64> for Value {
    fn from(num: f64) -> Self {
      Value::Number(num)
    }
  }

  impl From<Managed<SmolStr>> for Value {
    fn from(managed: Managed<SmolStr>) -> Value {
      Value::String(managed)
    }
  }

  impl From<Managed<List<Value>>> for Value {
    fn from(managed: Managed<List<Value>>) -> Value {
      Value::List(managed)
    }
  }

  impl From<Managed<Map<Value, Value>>> for Value {
    fn from(managed: Managed<Map<Value, Value>>) -> Value {
      Value::Map(managed)
    }
  }

  impl From<Managed<LyIterator>> for Value {
    fn from(managed: Managed<LyIterator>) -> Value {
      Value::Iter(managed)
    }
  }

  impl From<Managed<Closure>> for Value {
    fn from(managed: Managed<Closure>) -> Value {
      Value::Closure(managed)
    }
  }

  impl From<Managed<Fun>> for Value {
    fn from(managed: Managed<Fun>) -> Value {
      Value::Fun(managed)
    }
  }

  impl From<Managed<Class>> for Value {
    fn from(managed: Managed<Class>) -> Value {
      Value::Class(managed)
    }
  }

  impl From<Managed<Instance>> for Value {
    fn from(managed: Managed<Instance>) -> Value {
      Value::Instance(managed)
    }
  }

  impl From<Managed<Method>> for Value {
    fn from(managed: Managed<Method>) -> Value {
      Value::Method(managed)
    }
  }

  impl From<Managed<Box<dyn Native>>> for Value {
    fn from(managed: Managed<Box<dyn Native>>) -> Value {
      Value::Native(managed)
    }
  }

  impl From<Managed<Upvalue>> for Value {
    fn from(managed: Managed<Upvalue>) -> Value {
      Value::Upvalue(managed)
    }
  }

  impl fmt::Display for Value {
    /// Implement display for value in laythe
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
        Self::Iter(iterator) => write!(f, "{} iterator", &iterator.name()),
        Self::Native(native_fun) => write!(f, "<native {}>", native_fun.meta().name),
      }
    }
  }

  impl PartialEq for Value {
    /// Determine if this `Value` and another `Value` are equal inside
    /// of the laythe runtime
    ///
    /// # Examples
    /// ```
    /// use laythe_core::value::Value;
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
        (Self::Iter(iter1), Self::Iter(iter2)) => iter1 == iter2,
        (Self::Map(map1), Self::Map(map2)) => map1 == map2,
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

  impl Eq for Value {}

  impl Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
      // check the the variants have the same value
      match self {
        Self::Number(num) => {
          ValueKind::Number.hash(state);
          (*num as u64).hash(state);
        }
        Self::Bool(b) => {
          ValueKind::Bool.hash(state);
          b.hash(state);
        }
        Self::Nil => ValueKind::Nil.hash(state),
        Self::String(string) => {
          ValueKind::String.hash(state);
          string.hash(state);
        }
        Self::List(list) => {
          ValueKind::List.hash(state);
          list.hash(state);
        }
        Self::Map(map) => {
          ValueKind::Map.hash(state);
          map.hash(state);
        }
        Self::Fun(fun) => {
          ValueKind::Fun.hash(state);
          fun.hash(state);
        }
        Self::Closure(closure) => {
          ValueKind::Closure.hash(state);
          closure.hash(state);
        }
        Self::Method(method) => {
          ValueKind::Method.hash(state);
          method.hash(state);
        }
        Self::Native(native) => {
          ValueKind::Native.hash(state);
          native.hash(state);
        }
        Self::Upvalue(upvalue) => {
          ValueKind::Upvalue.hash(state);
          upvalue.hash(state);
        }
        Self::Class(class) => {
          ValueKind::Class.hash(state);
          class.hash(state);
        }
        Self::Instance(instance) => {
          ValueKind::Instance.hash(state);
          instance.hash(state);
        }
        Self::Iter(iter) => {
          ValueKind::Iter.hash(state);
          iter.hash(state);
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
        Value::Iter(iter) => iter.trace(),
        Value::Upvalue(upvalue) => upvalue.trace(),
        Value::Native(native) => native.trace(),
      }
    }

    fn trace_debug(&self, stdio: &mut Stdio) -> bool {
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
        Value::Iter(iter) => iter.trace_debug(stdio),
        Value::Upvalue(upvalue) => upvalue.trace_debug(stdio),
        Value::Native(native) => native.trace_debug(stdio),
      }
    }
  }
}

#[cfg(feature = "nan_boxing")]
mod boxed {
  use super::{Nil, ValueKind};
  use crate::{
    iterator::LyIterator,
    native::Native,
    object::{Class, Closure, Fun, Instance, List, Map, Method, Upvalue},
  };
  use laythe_env::{
    managed::{Allocation, Manage, Managed, Trace},
    stdio::Stdio,
  };

  use smol_str::SmolStr;
  use std::fmt;
  use std::ptr::NonNull;

  const BIT_SIGN: u64 = 0xc000_0000_0000_0000;
  const PTR_BITS: u64 = 0x0000_0000_0000_0007;
  const VARIANT_MASK: u64 = BIT_SIGN | QNAN | PTR_BITS;

  const TAG_NIL: u64 = 1 | QNAN; // 001
  const TAG_FALSE: u64 = 2 | QNAN; // 010
  const TAG_TRUE: u64 = 3 | QNAN; // 011
  const TAG_STRING: u64 = 4 | QNAN; // 100
  const TAG_LIST: u64 = 5 | QNAN; // 101
  const TAG_MAP: u64 = 6 | QNAN; // 110
  const TAG_CLOSURE: u64 = 7 | QNAN; // 111
  const TAG_FUN: u64 = 0 | BIT_SIGN | QNAN;
  const TAG_CLASS: u64 = 1 | BIT_SIGN | QNAN;
  const TAG_INSTANCE: u64 = 2 | BIT_SIGN | QNAN;
  const TAG_METHOD: u64 = 3 | BIT_SIGN | QNAN;
  const TAG_ITER: u64 = 4 | BIT_SIGN | QNAN;
  const TAG_NATIVE: u64 = 5 | BIT_SIGN | QNAN;
  const TAG_UPVALUE: u64 = 6 | BIT_SIGN | QNAN;

  const VALUE_KIND_MAP: [ValueKind; 15] = [
    ValueKind::Number,
    ValueKind::Nil,
    ValueKind::Bool,
    ValueKind::Bool,
    ValueKind::String,
    ValueKind::List,
    ValueKind::Map,
    ValueKind::Closure,
    ValueKind::Fun,
    ValueKind::Class,
    ValueKind::Instance,
    ValueKind::Method,
    ValueKind::Iter,
    ValueKind::Native,
    ValueKind::Upvalue,
  ];

  #[derive(Clone, Copy)]
  #[repr(C)]
  union NumberUnion {
    bits: u64,
    num: f64,
  }

  // rust f64::NAN
  // 0111 1111 1111 1000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
  //
  // rust div by 0
  // 0111 1111 1111 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
  //
  // quiet NaN with signal indefinite
  // 0111 1111 1111 1100 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
  const QNAN: u64 = 0x7ffc_0000_0000_0000;

  // 0111 1111 1111 1100 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0001
  pub const VALUE_NIL: Value = Value(TAG_NIL);

  // 0111 1111 1111 1100 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0010
  pub const VALUE_TRUE: Value = Value(TAG_TRUE);

  // 0111 1111 1111 1100 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0011
  pub const VALUE_FALSE: Value = Value(TAG_FALSE);

  #[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
  pub struct Value(u64);

  impl Value {
    #[inline]
    pub fn is_nil(&self) -> bool {
      self.0 == VALUE_NIL.0
    }

    #[inline]
    pub fn is_bool(&self) -> bool {
      self.0 == VALUE_FALSE.0 || self.0 == VALUE_TRUE.0
    }

    #[inline]
    pub fn is_false(&self) -> bool {
      self.0 == VALUE_FALSE.0
    }

    #[inline]
    pub fn is_num(&self) -> bool {
      (self.0 & QNAN) != QNAN
    }

    #[inline]
    fn is_obj_tag(&self, tag: u64) -> bool {
      (self.0 & VARIANT_MASK) == tag
    }

    #[inline]
    pub fn is_str(&self) -> bool {
      self.is_obj_tag(TAG_STRING)
    }

    #[inline]
    pub fn is_list(&self) -> bool {
      self.is_obj_tag(TAG_LIST)
    }

    #[inline]
    pub fn is_map(&self) -> bool {
      self.is_obj_tag(TAG_MAP)
    }

    #[inline]
    pub fn is_iter(&self) -> bool {
      self.is_obj_tag(TAG_ITER)
    }

    #[inline]
    pub fn is_closure(&self) -> bool {
      self.is_obj_tag(TAG_CLOSURE)
    }

    #[inline]
    pub fn is_fun(&self) -> bool {
      self.is_obj_tag(TAG_FUN)
    }

    #[inline]
    pub fn is_class(&self) -> bool {
      self.is_obj_tag(TAG_CLASS)
    }

    #[inline]
    pub fn is_instance(&self) -> bool {
      self.is_obj_tag(TAG_INSTANCE)
    }

    #[inline]
    pub fn is_method(&self) -> bool {
      self.is_obj_tag(TAG_METHOD)
    }

    #[inline]
    pub fn is_native(&self) -> bool {
      self.is_obj_tag(TAG_NATIVE)
    }

    #[inline]
    pub fn is_upvalue(&self) -> bool {
      self.is_obj_tag(TAG_UPVALUE)
    }

    #[inline]
    pub fn to_bool(&self) -> bool {
      *self == VALUE_TRUE
    }

    #[inline]
    pub fn to_num(&self) -> f64 {
      let union = NumberUnion { bits: self.0 };
      unsafe { union.num }
    }

    #[inline]
    pub fn to_obj_tag<T: 'static + Manage>(&self, tag: u64) -> Managed<T> {
      let as_unsigned = self.0 & !tag;
      let ptr = unsafe { NonNull::new_unchecked(as_unsigned as usize as *mut Allocation<T>) };
      Managed::from(ptr)
    }

    #[inline]
    pub fn to_str(&self) -> Managed<SmolStr> {
      self.to_obj_tag(TAG_STRING)
    }

    #[inline]
    pub fn to_list(&self) -> Managed<List<Value>> {
      self.to_obj_tag(TAG_LIST)
    }

    #[inline]
    pub fn to_map(&self) -> Managed<Map<Value, Value>> {
      self.to_obj_tag(TAG_MAP)
    }

    #[inline]
    pub fn to_iter(&self) -> Managed<LyIterator> {
      self.to_obj_tag(TAG_ITER)
    }

    #[inline]
    pub fn to_closure(&self) -> Managed<Closure> {
      self.to_obj_tag(TAG_CLOSURE)
    }

    #[inline]
    pub fn to_fun(&self) -> Managed<Fun> {
      self.to_obj_tag(TAG_FUN)
    }

    #[inline]
    pub fn to_class(&self) -> Managed<Class> {
      self.to_obj_tag(TAG_CLASS)
    }

    #[inline]
    pub fn to_instance(&self) -> Managed<Instance> {
      self.to_obj_tag(TAG_INSTANCE)
    }

    #[inline]
    pub fn to_method(&self) -> Managed<Method> {
      self.to_obj_tag(TAG_METHOD)
    }

    #[inline]
    pub fn to_native(&self) -> Managed<Box<dyn Native>> {
      self.to_obj_tag(TAG_NATIVE)
    }

    #[inline]
    pub fn to_upvalue(&self) -> Managed<Upvalue> {
      self.to_obj_tag(TAG_UPVALUE)
    }

    #[inline]
    pub fn kind(&self) -> ValueKind {
      if self.is_num() {
        return ValueKind::Number;
      }

      let top_bit = self.0 >= BIT_SIGN;
      let lower_byte = self.0 as u8 & 0x7;

      let index = lower_byte + (top_bit as u8) * 8;

      debug_assert!(index > 0 && index < 15);
      unsafe { *VALUE_KIND_MAP.get_unchecked(index as usize) }
    }

    /// Get a string representation of the underlying type this value representing
    ///
    /// # Examples
    /// use laythe_core::value::Value;
    /// use laythe_core::object::{Obj, ObjValue};
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
      match self.kind() {
        ValueKind::Nil => "nil".to_string(),
        ValueKind::Bool => "bool".to_string(),
        ValueKind::Number => "number".to_string(),
        ValueKind::String => "string".to_string(),
        ValueKind::List => "list".to_string(),
        ValueKind::Map => "map".to_string(),
        ValueKind::Fun => "function".to_string(),
        ValueKind::Closure => "closure".to_string(),
        ValueKind::Method => "method".to_string(),
        ValueKind::Class => "class".to_string(),
        ValueKind::Instance => "instance".to_string(),
        ValueKind::Iter => "iterator".to_string(),
        ValueKind::Upvalue => "upvalue".to_string(),
        ValueKind::Native => "native".to_string(),
      }
    }
  }

  impl Trace for Value {
    fn trace(&self) -> bool {
      match self.kind() {
        ValueKind::String => self.to_str().trace(),
        ValueKind::List => self.to_list().trace(),
        ValueKind::Map => self.to_map().trace(),
        ValueKind::Fun => self.to_fun().trace(),
        ValueKind::Closure => self.to_closure().trace(),
        ValueKind::Method => self.to_method().trace(),
        ValueKind::Class => self.to_class().trace(),
        ValueKind::Instance => self.to_instance().trace(),
        ValueKind::Iter => self.to_iter().trace(),
        ValueKind::Upvalue => self.to_upvalue().trace(),
        ValueKind::Native => self.to_native().trace(),
        _ => true,
      }
    }
    fn trace_debug(&self, stdio: &mut Stdio) -> bool {
      match self.kind() {
        ValueKind::String => self.to_str().trace_debug(stdio),
        ValueKind::List => self.to_list().trace_debug(stdio),
        ValueKind::Map => self.to_map().trace_debug(stdio),
        ValueKind::Fun => self.to_fun().trace_debug(stdio),
        ValueKind::Closure => self.to_closure().trace_debug(stdio),
        ValueKind::Method => self.to_method().trace_debug(stdio),
        ValueKind::Class => self.to_class().trace_debug(stdio),
        ValueKind::Instance => self.to_instance().trace_debug(stdio),
        ValueKind::Iter => self.to_iter().trace_debug(stdio),
        ValueKind::Upvalue => self.to_upvalue().trace_debug(stdio),
        ValueKind::Native => self.to_native().trace_debug(stdio),
        _ => true,
      }
    }
  }

  impl From<Nil> for Value {
    fn from(_: Nil) -> Self {
      VALUE_NIL
    }
  }

  impl From<bool> for Value {
    fn from(b: bool) -> Self {
      if b {
        VALUE_TRUE
      } else {
        VALUE_FALSE
      }
    }
  }

  impl From<f64> for Value {
    fn from(num: f64) -> Self {
      let union = NumberUnion { num };
      unsafe { Self(union.bits) }
    }
  }

  impl From<Managed<SmolStr>> for Value {
    fn from(managed: Managed<SmolStr>) -> Value {
      Self(managed.to_usize() as u64 | TAG_STRING)
    }
  }

  impl From<Managed<List<Value>>> for Value {
    fn from(managed: Managed<List<Value>>) -> Value {
      Self(managed.to_usize() as u64 | TAG_LIST)
    }
  }

  impl From<Managed<Map<Value, Value>>> for Value {
    fn from(managed: Managed<Map<Value, Value>>) -> Value {
      Self(managed.to_usize() as u64 | TAG_MAP)
    }
  }

  impl From<Managed<LyIterator>> for Value {
    fn from(managed: Managed<LyIterator>) -> Value {
      Self(managed.to_usize() as u64 | TAG_ITER)
    }
  }

  impl From<Managed<Closure>> for Value {
    fn from(managed: Managed<Closure>) -> Value {
      Self(managed.to_usize() as u64 | TAG_CLOSURE)
    }
  }

  impl From<Managed<Fun>> for Value {
    fn from(managed: Managed<Fun>) -> Value {
      Self(managed.to_usize() as u64 | TAG_FUN)
    }
  }

  impl From<Managed<Class>> for Value {
    fn from(managed: Managed<Class>) -> Value {
      Self(managed.to_usize() as u64 | TAG_CLASS)
    }
  }

  impl From<Managed<Instance>> for Value {
    fn from(managed: Managed<Instance>) -> Value {
      Self(managed.to_usize() as u64 | TAG_INSTANCE)
    }
  }

  impl From<Managed<Method>> for Value {
    fn from(managed: Managed<Method>) -> Value {
      Self(managed.to_usize() as u64 | TAG_METHOD)
    }
  }

  impl From<Managed<Box<dyn Native>>> for Value {
    fn from(managed: Managed<Box<dyn Native>>) -> Value {
      Self(managed.to_usize() as u64 | TAG_NATIVE)
    }
  }

  impl From<Managed<Upvalue>> for Value {
    fn from(managed: Managed<Upvalue>) -> Value {
      Self(managed.to_usize() as u64 | TAG_UPVALUE)
    }
  }

  impl fmt::Display for Value {
    /// Implement display for value in laythe
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      match self.kind() {
        ValueKind::Number => write!(f, "{}", self.to_num()),
        ValueKind::Bool => write!(f, "{}", self.to_bool()),
        ValueKind::Nil => write!(f, "nil"),
        ValueKind::String => write!(f, "'{}'", self.to_str()),
        ValueKind::List => {
          let list = self.to_list();
          let mut strings: Vec<String> = Vec::with_capacity(list.len());
          for item in list.iter() {
            strings.push(format!("{}", item));
          }

          write!(f, "[{}]", strings.join(", "))
        }
        ValueKind::Map => {
          let map = self.to_map();
          let strings: Vec<String> = map
            .iter()
            .map(|(key, value)| format!("{}: {}", key, value))
            .collect();
          write!(f, "{{ {} }}", strings.join(", "))
        }
        ValueKind::Fun => write!(f, "{}", self.to_fun()),
        ValueKind::Upvalue => match &*self.to_upvalue() {
          Upvalue::Open(stack_ptr) => write!(f, "{}", unsafe { stack_ptr.as_ref() }),
          Upvalue::Closed(store) => write!(f, "{}", store),
        },
        ValueKind::Closure => write!(f, "{}", *self.to_closure().fun),
        ValueKind::Method => {
          let bound = self.to_method();
          write!(f, "{}.{}", bound.receiver, bound.method)
        }
        ValueKind::Class => write!(f, "{}", &self.to_class().name.as_str()),
        ValueKind::Instance => write!(f, "{} instance", &self.to_instance().class.name),
        ValueKind::Iter => write!(f, "{}", &self.to_iter().name()),
        ValueKind::Native => write!(f, "<native {}>", self.to_native().meta().name),
      }
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::{
    module::Module,
    object::{Class, Closure, Fun, List, Map},
  };
  use laythe_env::managed::{Allocation, Manage, Managed};
  use smol_str::SmolStr;
  use std::{path::PathBuf, ptr::NonNull};
  type Allocs = Vec<Box<Allocation<dyn Manage>>>;

  const VARIANTS: [ValueKind; 14] = [
    ValueKind::Bool,
    ValueKind::Nil,
    ValueKind::Number,
    ValueKind::String,
    ValueKind::List,
    ValueKind::Map,
    ValueKind::Fun,
    ValueKind::Closure,
    ValueKind::Class,
    ValueKind::Instance,
    ValueKind::Iter,
    ValueKind::Method,
    ValueKind::Native,
    ValueKind::Upvalue,
  ];

  fn is_type(val: Value, variant: ValueKind) -> bool {
    match variant {
      ValueKind::Bool => val.is_bool(),
      ValueKind::Nil => val.is_nil(),
      ValueKind::Number => val.is_num(),
      ValueKind::String => val.is_str(),
      ValueKind::List => val.is_list(),
      ValueKind::Map => val.is_map(),
      ValueKind::Fun => val.is_fun(),
      ValueKind::Closure => val.is_closure(),
      ValueKind::Class => val.is_class(),
      ValueKind::Instance => val.is_instance(),
      ValueKind::Iter => val.is_iter(),
      ValueKind::Method => val.is_method(),
      ValueKind::Native => val.is_native(),
      ValueKind::Upvalue => val.is_upvalue(),
    }
  }

  fn assert_type(val: Value, target: ValueKind) {
    VARIANTS.iter().for_each(|variant| {
      if target == *variant {
        assert!(is_type(val, *variant), "Expected to be {:?}", *variant);
      } else {
        assert!(!is_type(val, *variant), "Expected not to be {:?}", *variant);
      };
    });
  }

  fn test_string() -> (Allocs, Managed<SmolStr>) {
    let string = SmolStr::from("sup");
    let mut alloc = Box::new(Allocation::new(string));
    let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };

    let managed = Managed::from(ptr);
    (vec![alloc], managed)
  }

  fn test_path() -> (Allocs, Managed<PathBuf>) {
    let path = PathBuf::from("test/sup.ly");
    let mut alloc = Box::new(Allocation::new(path));
    let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };

    let managed = Managed::from(ptr);
    (vec![alloc], managed)
  }

  fn test_module() -> (Allocs, Managed<Module>) {
    let (allocs_class, class) = test_class();
    let (allocs_path, path) = test_path();
    let mut alloc = Box::new(Allocation::new(Module::new(class, path)));
    let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };

    let managed = Managed::from(ptr);
    let mut allocs: Allocs = vec![alloc];
    allocs.extend(allocs_class);
    allocs.extend(allocs_path);
    (allocs, managed)
  }

  fn test_fun() -> (Allocs, Managed<Fun>) {
    let (allocs_string, name) = test_string();
    let (allocs_module, module) = test_module();

    let fun = Fun::new(name, module);
    let mut alloc = Box::new(Allocation::new(fun));
    let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };

    let managed = Managed::from(ptr);
    let mut allocs: Allocs = vec![alloc];
    allocs.extend(allocs_string);
    allocs.extend(allocs_module);

    (allocs, managed)
  }

  fn test_closure() -> (Allocs, Managed<Closure>) {
    let (allocs_fun, fun) = test_fun();

    let closure = Closure::new(fun);
    let mut alloc = Box::new(Allocation::new(closure));
    let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };

    let managed = Managed::from(ptr);
    let mut allocs: Allocs = vec![alloc];
    allocs.extend(allocs_fun);

    (allocs, managed)
  }

  fn test_class() -> (Allocs, Managed<Class>) {
    let (allocs_string, string) = test_string();

    let class = Class::bare(string);
    let mut alloc = Box::new(Allocation::new(class));
    let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };

    let managed = Managed::from(ptr);

    let mut allocs: Allocs = vec![alloc];
    allocs.extend(allocs_string);
    (allocs, managed)
  }

  #[test]
  fn bool() {
    let val_true = val!(true);
    let val_false = val!(false);

    assert_type(val_true, ValueKind::Bool);
    assert_type(val_false, ValueKind::Bool);

    assert_eq!(val_true.to_bool(), true);
    assert_eq!(val_false.to_bool(), false);
  }

  #[test]
  fn num() {
    let val_div_zero = val!(1.0 / 0.0);
    let val_nan = val!(f64::NAN);
    let val_neg_infinity = val!(f64::NEG_INFINITY);
    let val_normal = val!(5.3);

    assert_type(val_div_zero, ValueKind::Number);
    assert_type(val_nan, ValueKind::Number);
    assert_type(val_neg_infinity, ValueKind::Number);
    assert_type(val_normal, ValueKind::Number);

    assert_eq!(val_div_zero.to_num(), 1.0 / 0.0);
    assert!(val_nan.to_num().is_nan());
    assert_eq!(val_neg_infinity.to_num(), f64::NEG_INFINITY);
    assert_eq!(val_normal.to_num(), 5.3);
  }

  #[test]
  fn string() {
    let string = SmolStr::from("sup");
    let mut alloc = Box::new(Allocation::new(string));
    let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };

    let managed = Managed::from(ptr);
    let value = val!(managed);
    let managed2: Managed<SmolStr> = value.to_str();

    assert_type(value, ValueKind::String);

    assert_eq!(&*managed, &*managed2);
    assert_eq!(managed, managed2);
  }

  #[test]
  fn list() {
    let list = List::from(vec![VALUE_NIL, VALUE_TRUE, VALUE_FALSE]);
    let mut alloc = Box::new(Allocation::new(list));
    let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };

    let managed = Managed::from(ptr);
    let value = val!(managed);
    let managed2 = value.to_list();

    assert_type(value, ValueKind::List);

    assert_eq!(managed.len(), managed2.len());
    assert_eq!(managed[0], managed2[0]);
    assert_eq!(managed[1], managed2[1]);
    assert_eq!(managed[2], managed2[2]);
    assert_eq!(managed, managed2);
  }

  #[test]
  fn map() {
    let mut map: Map<Value, Value> = Map::default();
    map.insert(VALUE_NIL, VALUE_TRUE);
    map.insert(val!(10.0), VALUE_FALSE);

    let mut alloc = Box::new(Allocation::new(map));
    let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };

    let managed = Managed::from(ptr);
    let value = val!(managed);
    let managed2 = value.to_map();

    assert_type(value, ValueKind::Map);

    assert_eq!(managed.len(), managed2.len());
    assert_eq!(managed.get(&VALUE_NIL), managed2.get(&VALUE_NIL));
    assert_eq!(managed.get(&val!(10.0)), managed2.get(&val!(10.0)));
  }

  #[test]
  fn fun() {
    let (_, managed) = test_fun();
    let value = val!(managed);
    let managed2 = value.to_fun();

    assert_type(value, ValueKind::Fun);

    assert_eq!(managed.name, managed2.name);
    assert_eq!(managed.arity, managed2.arity);
  }

  #[test]
  fn closure() {
    let (_, managed) = test_closure();

    let value = val!(managed);
    let managed2 = value.to_closure();

    assert_type(value, ValueKind::Closure);

    assert_eq!(managed.fun, managed2.fun);
    assert_eq!(managed.upvalues.len(), managed2.upvalues.len());
  }

  #[test]
  fn class() {
    let (_, managed) = test_class();

    let value = val!(managed);
    let managed2 = value.to_class();

    assert_type(value, ValueKind::Class);

    assert_eq!(managed.name, managed2.name);
  }
}
