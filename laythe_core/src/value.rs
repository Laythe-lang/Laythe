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
  use crate::{
    iterator::LyIterator,
    native::Native,
    object::{Class, Closure, Fun, Instance, List, Map, Method, Upvalue},
  };
  use laythe_core::managed::{DebugHeap, DebugWrap, Gc, GcStr, Trace};

  use super::{Nil, ValueKind};
  use std::fmt;
  use std::fmt::Debug;
  use std::hash::Hash;
  use std::io::Write;

  pub const VALUE_NIL: Value = Value::Nil;
  pub const VALUE_FALSE: Value = Value::Bool(false);
  pub const VALUE_TRUE: Value = Value::Bool(true);

  /// Enum of value types in laythe
  #[derive(Clone, Copy, Debug)]
  pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    String(GcStr),
    List(Gc<List<Value>>),
    Map(Gc<Map<Value, Value>>),
    Fun(Gc<Fun>),
    Closure(Gc<Closure>),
    Class(Gc<Class>),
    Instance(Gc<Instance>),
    Method(Gc<Method>),
    Iter(Gc<LyIterator>),
    Native(Gc<Box<dyn Native>>),
    Upvalue(Gc<Upvalue>),
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
      matches!(self, Value::Nil)
    }

    #[inline]
    pub fn is_bool(&self) -> bool {
      matches!(self, Value::Bool(_))
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
      matches!(self, Value::Number(_))
    }

    #[inline]
    pub fn is_str(&self) -> bool {
      matches!(self, Value::String(_))
    }

    #[inline]
    pub fn is_list(&self) -> bool {
      matches!(self, Value::List(_))
    }

    #[inline]
    pub fn is_map(&self) -> bool {
      matches!(self, Value::Map(_))
    }

    #[inline]
    pub fn is_iter(&self) -> bool {
      matches!(self, Value::Iter(_))
    }

    #[inline]
    pub fn is_closure(&self) -> bool {
      matches!(self, Value::Closure(_))
    }

    #[inline]
    pub fn is_fun(&self) -> bool {
      matches!(self, Value::Fun(_))
    }

    #[inline]
    pub fn is_class(&self) -> bool {
      matches!(self, Value::Class(_))
    }

    #[inline]
    pub fn is_instance(&self) -> bool {
      matches!(self, Value::Instance(_))
    }

    #[inline]
    pub fn is_method(&self) -> bool {
      matches!(self, Value::Method(_))
    }

    #[inline]
    pub fn is_native(&self) -> bool {
      matches!(self, Value::Native(_))
    }

    #[inline]
    pub fn is_upvalue(&self) -> bool {
      matches!(self, Value::Upvalue(_))
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
    /// use laythe_core::memory::Allocator;
    ///
    /// let gc = Allocator::default();
    /// let mut context = NoContext::new(&gc);
    /// let hooks = Hooks::new(&mut context);
    /// let managed =  hooks.manage_str("example");
    ///
    /// let value = Value::String(managed);
    /// assert_eq!(&*value.to_str(), "example")
    /// ```
    #[inline]
    pub fn to_str(&self) -> GcStr {
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
    /// use laythe_core::object::List;
    /// use laythe_core::managed::{Allocation, Gc};
    ///
    /// use std::ptr::NonNull;
    ///
    /// let list = List::from(vec![Value::Nil]);
    /// let mut alloc = Box::new(Allocation::new(list));
    /// let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };
    /// let managed = Gc::from(ptr);
    ///
    /// let value = Value::List(managed);
    /// assert_eq!(value.to_list()[0], Value::Nil)
    /// ```
    #[inline]
    pub fn to_list(&self) -> Gc<List<Value>> {
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
    /// use laythe_core::managed::{Allocation, Gc};
    /// use laythe_core::object::Map;
    /// use laythe_core::hooks::{Hooks, support::TestContext};
    ///
    /// let mut context = TestContext::default();
    /// let hooks = Hooks::new(&mut context);
    /// let map: Map<Value, Value> = Map::default();
    ///
    /// let value = Value::Map(hooks.manage(map));
    /// assert_eq!(value.to_map().len(), 0)
    /// ```
    #[inline]
    pub fn to_map(&self) -> Gc<Map<Value, Value>> {
      match self {
        Self::Map(map) => *map,
        _ => panic!("Expected list."),
      }
    }

    /// Unwrap and reference a laythe iterator, panics if not a iterator
    #[inline]
    pub fn to_iter(&self) -> Gc<LyIterator> {
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
    /// use laythe_core::hooks::{Hooks, support::TestContext};
    /// use laythe_core::managed::Gc;
    ///
    /// let mut context = TestContext::default();
    /// let hooks = Hooks::new(&mut context);
    ///
    /// let fun: Fun = Fun::new(hooks.manage_str("add"), Gc::dangling());
    /// let managed = hooks.manage(fun);
    ///
    /// let value = Value::Fun(managed);
    /// assert_eq!(&*value.to_fun().name, "add");
    /// ```
    #[inline]
    pub fn to_fun(&self) -> Gc<Fun> {
      match self {
        Self::Fun(fun) => *fun,
        _ => panic!("Expected function!"),
      }
    }

    /// Unwrap a laythe native function, panics if not a native function
    #[inline]
    pub fn to_native(&self) -> Gc<Box<dyn Native>> {
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
    /// use laythe_core::hooks::{Hooks, support::TestContext};
    /// use laythe_core::managed::Gc;
    ///
    /// let mut context = TestContext::default();
    /// let hooks = Hooks::new(&mut context);
    /// let fun: Fun = Fun::new(hooks.manage_str("add"), Gc::dangling());
    /// let managed_fun = hooks.manage(fun);
    ///
    /// let closure = Closure::new(managed_fun);
    /// let managed_closure = hooks.manage(closure);
    ///
    /// let value = Value::Closure(managed_closure);
    /// assert_eq!(&*value.to_closure().fun.name.clone(), "add");
    /// ```
    pub fn to_closure(&self) -> Gc<Closure> {
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
    /// use laythe_core::object::{Closure, Method};
    /// use laythe_core::hooks::{Hooks, support::TestContext};
    /// use laythe_core::managed::Gc;
    ///
    /// let mut context = TestContext::default();
    /// let hooks = Hooks::new(&mut context);
    ///
    /// let method = Method::new(Value::Nil, Value::Closure(Gc::dangling()));
    /// let managed_method = hooks.manage(method);
    ///
    /// let value = Value::Method(managed_method);
    /// assert_eq!(value.to_method(), managed_method);
    /// ```
    pub fn to_method(&self) -> Gc<Method> {
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
    /// use laythe_core::hooks::{Hooks, support::TestContext};
    /// use std::ptr::NonNull;
    ///
    /// let mut context = TestContext::default();
    /// let hooks = Hooks::new(&mut context);
    /// let value = Value::Number(5.0);
    /// let upvalue = hooks.manage(Upvalue::Open(NonNull::from(&value)));
    /// let value = Value::Upvalue(upvalue);
    ///
    /// match *value.to_upvalue() {
    ///   Upvalue::Open(stack_ptr) => assert_eq!(*unsafe { stack_ptr.as_ref() }, Value::Number(5.0)),
    ///   Upvalue::Closed(_) => assert!(false),
    /// };
    /// ```
    pub fn to_upvalue(&self) -> Gc<Upvalue> {
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
    /// use laythe_core::hooks::{Hooks, support::TestContext};
    ///
    /// let mut context = TestContext::default();
    /// let hooks = Hooks::new(&mut context);
    /// let name = hooks.manage_str("example");
    /// let class = hooks.manage(Class::bare(name));
    ///
    /// let value = Value::Class(class);
    /// assert_eq!(value.to_class().name, name);
    /// ```
    pub fn to_class(&self) -> Gc<Class> {
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
    /// use laythe_core::hooks::{Hooks, support::TestContext};
    ///
    /// let mut context = TestContext::default();
    /// let hooks = Hooks::new(&mut context);
    /// let name = hooks.manage_str("example".to_string());
    /// let class = hooks.manage(Class::bare(name));
    /// let instance = hooks.manage(Instance::new(class));
    ///
    /// let value = Value::Instance(instance);
    /// assert_eq!(value.to_instance().class, class);
    pub fn to_instance(&self) -> Gc<Instance> {
      match self {
        Self::Instance(instance) => *instance,
        _ => panic!("Expected instance!"),
      }
    }

    /// Get a string representation of the underlying type this value representing
    ///
    /// # Examples
    /// ```
    /// use laythe_core::value::Value;
    ///
    /// let nil = Value::Nil;
    /// let bool = Value::Bool(true);
    /// let number = Value::Number(10.0);
    ///
    /// assert_eq!(nil.value_type(), "nil");
    /// assert_eq!(bool.value_type(), "bool");
    /// assert_eq!(number.value_type(), "number");
    /// ```
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

  impl From<GcStr> for Value {
    fn from(managed: GcStr) -> Value {
      Value::String(managed)
    }
  }

  impl From<Gc<List<Value>>> for Value {
    fn from(managed: Gc<List<Value>>) -> Value {
      Value::List(managed)
    }
  }

  impl From<Gc<Map<Value, Value>>> for Value {
    fn from(managed: Gc<Map<Value, Value>>) -> Value {
      Value::Map(managed)
    }
  }

  impl From<Gc<LyIterator>> for Value {
    fn from(managed: Gc<LyIterator>) -> Value {
      Value::Iter(managed)
    }
  }

  impl From<Gc<Closure>> for Value {
    fn from(managed: Gc<Closure>) -> Value {
      Value::Closure(managed)
    }
  }

  impl From<Gc<Fun>> for Value {
    fn from(managed: Gc<Fun>) -> Value {
      Value::Fun(managed)
    }
  }

  impl From<Gc<Class>> for Value {
    fn from(managed: Gc<Class>) -> Value {
      Value::Class(managed)
    }
  }

  impl From<Gc<Instance>> for Value {
    fn from(managed: Gc<Instance>) -> Value {
      Value::Instance(managed)
    }
  }

  impl From<Gc<Method>> for Value {
    fn from(managed: Gc<Method>) -> Value {
      Value::Method(managed)
    }
  }

  impl From<Gc<Box<dyn Native>>> for Value {
    fn from(managed: Gc<Box<dyn Native>>) -> Value {
      Value::Native(managed)
    }
  }

  impl From<Gc<Upvalue>> for Value {
    fn from(managed: Gc<Upvalue>) -> Value {
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
        Self::String(string) => write!(f, "'{}'", string),
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
        Self::Closure(closure) => write!(f, "{}", *closure.fun()),
        Self::Method(bound) => write!(f, "{}.{}", bound.receiver(), bound.method()),
        Self::Class(class) => write!(f, "{}", class.name()),
        Self::Instance(instance) => write!(f, "{} instance", instance.class().name()),
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
    fn trace(&self) {
      match self {
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
        _ => (),
      }
    }

    fn trace_debug(&self, stdout: &mut dyn Write) {
      match self {
        Value::String(string) => string.trace_debug(stdout),
        Value::List(list) => list.trace_debug(stdout),
        Value::Map(map) => map.trace_debug(stdout),
        Value::Fun(fun) => fun.trace_debug(stdout),
        Value::Closure(closure) => closure.trace_debug(stdout),
        Value::Method(method) => method.trace_debug(stdout),
        Value::Class(class) => class.trace_debug(stdout),
        Value::Instance(instance) => instance.trace_debug(stdout),
        Value::Iter(iter) => iter.trace_debug(stdout),
        Value::Upvalue(upvalue) => upvalue.trace_debug(stdout),
        Value::Native(native) => native.trace_debug(stdout),
        _ => (),
      }
    }
  }

  impl DebugHeap for Value {
    fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
      match self {
        Value::Nil => f.write_str("nil"),
        Value::Bool(b) => f.write_fmt(format_args!("{}", b)),
        Value::Number(num) => f.write_fmt(format_args!("{}", num)),
        Value::String(string) => f.write_fmt(format_args!("{:?}", DebugWrap(string, depth))),
        Value::List(list) => f.write_fmt(format_args!("{:?}", DebugWrap(list, depth))),
        Value::Map(map) => f.write_fmt(format_args!("{:?}", DebugWrap(map, depth))),
        Value::Fun(fun) => f.write_fmt(format_args!("{:?}", DebugWrap(fun, depth))),
        Value::Closure(closure) => f.write_fmt(format_args!("{:?}", DebugWrap(closure, depth))),
        Value::Method(method) => f.write_fmt(format_args!("{:?}", DebugWrap(method, depth))),
        Value::Class(class) => f.write_fmt(format_args!("{:?}", DebugWrap(class, depth))),
        Value::Instance(instance) => f.write_fmt(format_args!("{:?}", DebugWrap(instance, depth))),
        Value::Iter(iter) => f.write_fmt(format_args!("{:?}", DebugWrap(iter, depth))),
        Value::Upvalue(upvalue) => f.write_fmt(format_args!("{:?}", DebugWrap(upvalue, depth))),
        Value::Native(native) => f.write_fmt(format_args!("{:?}", DebugWrap(native, depth))),
      }
    }
  }
}

#[cfg(feature = "nan_boxing")]
mod boxed {
  use super::{Nil, ValueKind};
  use crate::{
    iterator::LyIterator,
    managed::{Allocation, DebugHeap, DebugWrap, Gc, GcStr, Manage, Trace},
    native::Native,
    object::{Class, Closure, Fun, Instance, List, Map, Method, Upvalue},
  };

  use std::ptr::NonNull;
  use std::{fmt, io::Write};

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
  const TAG_FUN: u64 = BIT_SIGN | QNAN;
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
    pub fn to_obj_tag<T: 'static + Manage>(&self, tag: u64) -> Gc<T> {
      let as_unsigned = self.0 & !tag;
      let ptr = unsafe { NonNull::new_unchecked(as_unsigned as usize as *mut Allocation<T>) };
      Gc::from(ptr)
    }

    #[inline]
    pub fn to_str(&self) -> GcStr {
      let as_unsigned = self.0 & !TAG_STRING;
      unsafe {
        let ptr = NonNull::new_unchecked(as_unsigned as usize as *mut u8);
        GcStr::from_alloc_ptr(ptr)
      }
    }

    #[inline]
    pub fn to_list(&self) -> Gc<List<Value>> {
      self.to_obj_tag(TAG_LIST)
    }

    #[inline]
    pub fn to_map(&self) -> Gc<Map<Value, Value>> {
      self.to_obj_tag(TAG_MAP)
    }

    #[inline]
    pub fn to_iter(&self) -> Gc<LyIterator> {
      self.to_obj_tag(TAG_ITER)
    }

    #[inline]
    pub fn to_closure(&self) -> Gc<Closure> {
      self.to_obj_tag(TAG_CLOSURE)
    }

    #[inline]
    pub fn to_fun(&self) -> Gc<Fun> {
      self.to_obj_tag(TAG_FUN)
    }

    #[inline]
    pub fn to_class(&self) -> Gc<Class> {
      self.to_obj_tag(TAG_CLASS)
    }

    #[inline]
    pub fn to_instance(&self) -> Gc<Instance> {
      self.to_obj_tag(TAG_INSTANCE)
    }

    #[inline]
    pub fn to_method(&self) -> Gc<Method> {
      self.to_obj_tag(TAG_METHOD)
    }

    #[inline]
    pub fn to_native(&self) -> Gc<Box<dyn Native>> {
      self.to_obj_tag(TAG_NATIVE)
    }

    #[inline]
    pub fn to_upvalue(&self) -> Gc<Upvalue> {
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
    fn trace(&self) {
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
        _ => (),
      }
    }
    fn trace_debug(&self, stdout: &mut dyn Write) {
      match self.kind() {
        ValueKind::String => self.to_str().trace_debug(stdout),
        ValueKind::List => self.to_list().trace_debug(stdout),
        ValueKind::Map => self.to_map().trace_debug(stdout),
        ValueKind::Fun => self.to_fun().trace_debug(stdout),
        ValueKind::Closure => self.to_closure().trace_debug(stdout),
        ValueKind::Method => self.to_method().trace_debug(stdout),
        ValueKind::Class => self.to_class().trace_debug(stdout),
        ValueKind::Instance => self.to_instance().trace_debug(stdout),
        ValueKind::Iter => self.to_iter().trace_debug(stdout),
        ValueKind::Upvalue => self.to_upvalue().trace_debug(stdout),
        ValueKind::Native => self.to_native().trace_debug(stdout),
        _ => (),
      }
    }
  }

  impl DebugHeap for Value {
    fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
      match self.kind() {
        ValueKind::Bool => f.write_fmt(format_args!("{}", self.to_bool())),
        ValueKind::Nil => f.write_str("nil"),
        ValueKind::Number => f.write_fmt(format_args!("{}", self.to_num())),
        ValueKind::String => f.write_fmt(format_args!("{:?}", DebugWrap(&self.to_str(), depth))),
        ValueKind::List => f.write_fmt(format_args!("{:?}", DebugWrap(&self.to_list(), depth))),
        ValueKind::Map => f.write_fmt(format_args!("{:?}", DebugWrap(&self.to_map(), depth))),
        ValueKind::Fun => f.write_fmt(format_args!("{:?}", DebugWrap(&self.to_fun(), depth))),
        ValueKind::Closure => {
          f.write_fmt(format_args!("{:?}", DebugWrap(&self.to_closure(), depth)))
        }
        ValueKind::Class => f.write_fmt(format_args!("{:?}", DebugWrap(&self.to_class(), depth))),
        ValueKind::Instance => {
          f.write_fmt(format_args!("{:?}", DebugWrap(&self.to_instance(), depth)))
        }
        ValueKind::Iter => f.write_fmt(format_args!("{:?}", DebugWrap(&self.to_iter(), depth))),
        ValueKind::Method => f.write_fmt(format_args!("{:?}", DebugWrap(&self.to_method(), depth))),
        ValueKind::Native => f.write_fmt(format_args!("{:?}", DebugWrap(&self.to_native(), depth))),
        ValueKind::Upvalue => {
          f.write_fmt(format_args!("{:?}", DebugWrap(&self.to_upvalue(), depth)))
        }
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

  impl From<GcStr> for Value {
    fn from(managed: GcStr) -> Value {
      Self(managed.to_usize() as u64 | TAG_STRING)
    }
  }

  impl From<Gc<List<Value>>> for Value {
    fn from(managed: Gc<List<Value>>) -> Value {
      Self(managed.to_usize() as u64 | TAG_LIST)
    }
  }

  impl From<Gc<Map<Value, Value>>> for Value {
    fn from(managed: Gc<Map<Value, Value>>) -> Value {
      Self(managed.to_usize() as u64 | TAG_MAP)
    }
  }

  impl From<Gc<LyIterator>> for Value {
    fn from(managed: Gc<LyIterator>) -> Value {
      Self(managed.to_usize() as u64 | TAG_ITER)
    }
  }

  impl From<Gc<Closure>> for Value {
    fn from(managed: Gc<Closure>) -> Value {
      Self(managed.to_usize() as u64 | TAG_CLOSURE)
    }
  }

  impl From<Gc<Fun>> for Value {
    fn from(managed: Gc<Fun>) -> Value {
      Self(managed.to_usize() as u64 | TAG_FUN)
    }
  }

  impl From<Gc<Class>> for Value {
    fn from(managed: Gc<Class>) -> Value {
      Self(managed.to_usize() as u64 | TAG_CLASS)
    }
  }

  impl From<Gc<Instance>> for Value {
    fn from(managed: Gc<Instance>) -> Value {
      Self(managed.to_usize() as u64 | TAG_INSTANCE)
    }
  }

  impl From<Gc<Method>> for Value {
    fn from(managed: Gc<Method>) -> Value {
      Self(managed.to_usize() as u64 | TAG_METHOD)
    }
  }

  impl From<Gc<Box<dyn Native>>> for Value {
    fn from(managed: Gc<Box<dyn Native>>) -> Value {
      Self(managed.to_usize() as u64 | TAG_NATIVE)
    }
  }

  impl From<Gc<Upvalue>> for Value {
    fn from(managed: Gc<Upvalue>) -> Value {
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
        ValueKind::Closure => write!(f, "{}", *self.to_closure().fun()),
        ValueKind::Method => {
          let bound = self.to_method();
          write!(f, "{}.{}", bound.receiver(), bound.method())
        }
        ValueKind::Class => write!(f, "{}", &self.to_class().name()),
        ValueKind::Instance => write!(f, "{} instance", &self.to_instance().class().name()),
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
    managed::{Allocation, Gc, GcStr},
    memory::{Allocator, NO_GC},
    module::Module,
    object::{Class, Closure, Fun, List, Map},
  };
  use std::{path::PathBuf, ptr::NonNull};

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

  fn test_string(gc: &mut Allocator) -> GcStr {
    gc.manage_str("sup", &NO_GC)
  }

  fn test_path() -> PathBuf {
    PathBuf::from("test/sup.ly")
  }

  fn test_module(gc: &mut Allocator) -> Gc<Module> {
    let class = test_class(gc);
    let path = test_path();

    gc.manage(Module::new(class, path, 0), &NO_GC)
  }

  fn test_fun(gc: &mut Allocator) -> Gc<Fun> {
    let name = test_string(gc);
    let module = test_module(gc);

    gc.manage(Fun::test(name, module), &NO_GC)
  }

  fn test_closure(gc: &mut Allocator) -> Gc<Closure> {
    let fun = test_fun(gc);

    gc.manage(Closure::without_upvalues(fun), &NO_GC)
  }

  fn test_class(gc: &mut Allocator) -> Gc<Class> {
    let name = test_string(gc);

    gc.manage(Class::bare(name), &NO_GC)
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
    let mut gc = Allocator::default();
    let string = gc.manage_str("example", &NO_GC);

    let value = val!(string);
    let string2: GcStr = value.to_str();

    assert_type(value, ValueKind::String);

    assert_eq!(string, string2);
  }

  #[test]
  fn list() {
    let list = List::from(vec![VALUE_NIL, VALUE_TRUE, VALUE_FALSE]);
    let mut alloc = Box::new(Allocation::new(list));
    let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };

    let managed = Gc::from(ptr);
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

    let managed = Gc::from(ptr);
    let value = val!(managed);
    let managed2 = value.to_map();

    assert_type(value, ValueKind::Map);

    assert_eq!(managed.len(), managed2.len());
    assert_eq!(managed.get(&VALUE_NIL), managed2.get(&VALUE_NIL));
    assert_eq!(managed.get(&val!(10.0)), managed2.get(&val!(10.0)));
  }

  #[test]
  fn fun() {
    let mut gc = Allocator::default();
    let fun = test_fun(&mut gc);
    let value = val!(fun);
    let fun2 = value.to_fun();

    assert_type(value, ValueKind::Fun);

    assert_eq!(fun.name(), fun2.name());
    assert_eq!(fun.arity(), fun2.arity());
  }

  #[test]
  fn closure() {
    let mut gc = Allocator::default();
    let closure = test_closure(&mut gc);

    let value = val!(closure);
    let closure2 = value.to_closure();

    assert_type(value, ValueKind::Closure);

    assert_eq!(closure.fun(), closure2.fun());
    assert_eq!(closure.upvalues(), closure2.upvalues());
  }

  #[test]
  fn class() {
    let mut gc = Allocator::default();
    let class = test_class(&mut gc);

    let value = val!(class);
    let class2 = value.to_class();

    assert_type(value, ValueKind::Class);

    assert_eq!(class.name(), class2.name());
  }
}
