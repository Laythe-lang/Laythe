pub struct Nil();

/// Enum of value types in laythe
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
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
  Iter,
  Method,
  NativeFun,
  NativeMethod,
  Upvalue,
}

#[cfg(not(feature = "nan_boxing"))]
pub use self::unboxed::*;

#[cfg(feature = "nan_boxing")]
pub use self::boxed::*;

#[cfg(not(feature = "nan_boxing"))]
mod unboxed {
  use super::*;
  use crate::{
    iterator::SlIterator,
    native::{NativeFun, NativeMethod},
    object::{BuiltinPrimitives, Class, Closure, Fun, Instance, LyHashMap, LyVec, Method, Upvalue},
  };
  use laythe_env::{
    managed::{Managed, Trace},
    stdio::StdIo,
  };
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
    String(Managed<String>),
    List(Managed<LyVec<Value>>),
    Map(Managed<LyHashMap<Value, Value>>),
    Fun(Managed<Fun>),
    Closure(Managed<Closure>),
    Class(Managed<Class>),
    Instance(Managed<Instance>),
    Method(Managed<Method>),
    Iter(Managed<SlIterator>),
    NativeFun(Managed<Box<dyn NativeFun>>),
    NativeMethod(Managed<Box<dyn NativeMethod>>),
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
    pub fn is_native_fun(&self) -> bool {
      match self {
        Value::NativeFun(_) => true,
        _ => false,
      }
    }

    #[inline]
    pub fn is_native_method(&self) -> bool {
      match self {
        Value::NativeMethod(_) => true,
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
    /// let managed =  hooks.manage_str("example".to_string());
    ///
    /// let value = Value::String(managed);
    /// assert_eq!(&*value.to_str(), "example")
    /// ```
    #[inline]
    pub fn to_str(&self) -> Managed<String> {
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
    pub fn to_list(&self) -> Managed<LyVec<Value>> {
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
    pub fn to_map(&self) -> Managed<LyHashMap<Value, Value>> {
      match self {
        Self::Map(map) => *map,
        _ => panic!("Expected list."),
      }
    }

    /// Unwrap and reference a laythe iterator, panics if not a iterator
    #[inline]
    pub fn to_iter(&self) -> Managed<SlIterator> {
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
    /// use laythe_core::arity::ArityKind;
    /// use laythe_core::chunk::Chunk;
    /// use laythe_core::hooks::{Hooks, NoContext};
    /// use laythe_env::memory::Gc;
    /// use std::path::PathBuf;
    ///
    /// let gc = Gc::default();
    /// let mut context = NoContext::new(&gc);
    /// let hooks = Hooks::new(&mut context);
    ///
    /// let module = hooks.manage(Module::new(hooks.manage_str("module".to_string()), hooks.manage(PathBuf::from("self/module.ly"))));
    /// let fun: Fun = Fun::new(hooks.manage_str("add".to_string()), module);
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
    pub fn to_native_fun(&self) -> Managed<Box<dyn NativeFun>> {
      match self {
        Self::NativeFun(native_fun) => *native_fun,
        _ => panic!("Expected native function!"),
      }
    }

    /// Unwrap a laythe native method, panics if not a native method
    #[inline]
    pub fn to_native_method(&self) -> Managed<Box<dyn NativeMethod>> {
      match self {
        Self::NativeMethod(native_method) => *native_method,
        _ => panic!("Expected native method"),
      }
    }

    /// Unwrap and reference a laythe closure, panics if not a closure
    ///
    /// # Examples
    /// ```
    /// use laythe_core::value::Value;
    /// use laythe_core::object::{Closure, Fun};
    /// use laythe_core::arity::ArityKind;
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
    /// let module = hooks.manage(Module::new(hooks.manage_str("module".to_string()), hooks.manage(PathBuf::from("self/module.ly"))));
    /// let fun: Fun = Fun::new(hooks.manage_str("add".to_string()), module);
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
    /// use laythe_core::arity::ArityKind;
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
    /// let module = hooks.manage(Module::new(hooks.manage_str("module".to_string()), hooks.manage(PathBuf::from("self/module.ly"))));
    /// let fun: Fun = Fun::new(hooks.manage_str("add".to_string()), module);
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

    /// Unwrap and reference a laythe instance, panics if not a instance
    ///
    /// # Examples
    /// ```
    /// use laythe_core::value::Value;
    /// use laythe_core::object::{Instance, Class};
    /// use laythe_env::managed::{Managed, Allocation, make_managed};
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
        Value::NativeFun(_) => "native function".to_string(),
        Value::NativeMethod(_) => "native method".to_string(),
      }
    }

    /// Get the class associated with this value
    pub fn value_class(&self, builtin: &BuiltinPrimitives) -> Managed<Class> {
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
        Value::Iter(_) => builtin.iter,
        Value::Upvalue(upvalue) => upvalue.value().value_class(builtin),
        Value::NativeFun(_) => builtin.native_fun,
        Value::NativeMethod(_) => builtin.native_method,
      }
    }

    pub fn kind(&self) -> ValueVariant {
      match self {
        Value::Nil => ValueVariant::Nil,
        Value::Bool(_) => ValueVariant::Bool,
        Value::Number(_) => ValueVariant::Number,
        Value::String(_) => ValueVariant::String,
        Value::List(_) => ValueVariant::List,
        Value::Map(_) => ValueVariant::Map,
        Value::Fun(_) => ValueVariant::Fun,
        Value::Closure(_) => ValueVariant::Closure,
        Value::Method(_) => ValueVariant::Method,
        Value::Class(_) => ValueVariant::Class,
        Value::Instance(_) => ValueVariant::Instance,
        Value::Iter(_) => ValueVariant::Iter,
        Value::Upvalue(_) => ValueVariant::Upvalue,
        Value::NativeFun(_) => ValueVariant::NativeFun,
        Value::NativeMethod(_) => ValueVariant::NativeMethod,
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

  impl From<Managed<String>> for Value {
    fn from(managed: Managed<String>) -> Value {
      Value::String(managed)
    }
  }

  impl From<Managed<LyVec<Value>>> for Value {
    fn from(managed: Managed<LyVec<Value>>) -> Value {
      Value::List(managed)
    }
  }

  impl From<Managed<LyHashMap<Value, Value>>> for Value {
    fn from(managed: Managed<LyHashMap<Value, Value>>) -> Value {
      Value::Map(managed)
    }
  }

  impl From<Managed<SlIterator>> for Value {
    fn from(managed: Managed<SlIterator>) -> Value {
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

  impl From<Managed<Box<dyn NativeFun>>> for Value {
    fn from(managed: Managed<Box<dyn NativeFun>>) -> Value {
      Value::NativeFun(managed)
    }
  }

  impl From<Managed<Box<dyn NativeMethod>>> for Value {
    fn from(managed: Managed<Box<dyn NativeMethod>>) -> Value {
      Value::NativeMethod(managed)
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
        Self::NativeFun(native_fun) => write!(f, "<native fun {}>", native_fun.meta().name),
        Self::NativeMethod(native_method) => {
          write!(f, "<native method {}>", native_method.meta().name)
        }
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
        Self::Iter(iter) => {
          ValueVariant::Iter.hash(state);
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
        Value::Iter(iter) => iter.trace_debug(stdio),
        Value::Upvalue(upvalue) => upvalue.trace_debug(stdio),
        Value::NativeFun(native) => native.trace_debug(stdio),
        Value::NativeMethod(native) => native.trace_debug(stdio),
      }
    }
  }

  #[cfg(test)]
  mod test {
    use super::*;
    use laythe_env::managed::Allocation;
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
}

#[cfg(feature = "nan_boxing")]
mod boxed {
  use super::ValueVariant;
  use crate::{
    iterator::SlIterator,
    native::{NativeFun, NativeMethod},
    object::{BuiltinPrimitives, Class, Closure, Fun, Instance, LyHashMap, LyVec, Method, Upvalue},
  };
  use laythe_env::{
    managed::{Allocation, Manage, Managed, Trace},
    stdio::StdIo,
  };

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
  const TAG_NATIVE_FUN: u64 = 5 | BIT_SIGN | QNAN;
  const TAG_NATIVE_METHOD: u64 = 6 | BIT_SIGN | QNAN;
  const TAG_UPVALUE: u64 = 7 | BIT_SIGN | QNAN;

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
    pub fn is_native_fun(&self) -> bool {
      self.is_obj_tag(TAG_NATIVE_FUN)
    }

    #[inline]
    pub fn is_native_method(&self) -> bool {
      self.is_obj_tag(TAG_NATIVE_METHOD)
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
    pub fn to_str(&self) -> Managed<String> {
      self.to_obj_tag(TAG_STRING)
    }

    #[inline]
    pub fn to_list(&self) -> Managed<LyVec<Value>> {
      self.to_obj_tag(TAG_LIST)
    }

    #[inline]
    pub fn to_map(&self) -> Managed<LyHashMap<Value, Value>> {
      self.to_obj_tag(TAG_MAP)
    }

    #[inline]
    pub fn to_iter(&self) -> Managed<SlIterator> {
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
    pub fn to_native_fun(&self) -> Managed<Box<dyn NativeFun>> {
      self.to_obj_tag(TAG_NATIVE_FUN)
    }

    #[inline]
    pub fn to_native_method(&self) -> Managed<Box<dyn NativeMethod>> {
      self.to_obj_tag(TAG_NATIVE_METHOD)
    }

    #[inline]
    pub fn to_upvalue(&self) -> Managed<Upvalue> {
      self.to_obj_tag(TAG_UPVALUE)
    }

    #[inline]
    pub fn kind(&self) -> ValueVariant {
      if self.is_num() {
        return ValueVariant::Number;
      }

      let top_bit = (self.0 >> 63) as u8;
      let lower_byte = self.0 as u8;

      if top_bit == 0 {
        match lower_byte & 0x07 {
          1 => ValueVariant::Nil,
          2 => ValueVariant::Bool,
          3 => ValueVariant::Bool,
          4 => ValueVariant::String,
          5 => ValueVariant::List,
          6 => ValueVariant::Map,
          7 => ValueVariant::Closure,
          _ => panic!("value kind failed."),
        }
      } else {
        match lower_byte & 0x07 {
          0 => ValueVariant::Fun,
          1 => ValueVariant::Class,
          2 => ValueVariant::Instance,
          3 => ValueVariant::Method,
          4 => ValueVariant::Iter,
          5 => ValueVariant::NativeFun,
          6 => ValueVariant::NativeMethod,
          7 => ValueVariant::Upvalue,
          _ => panic!("value kind failed."),
        }
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
      match self.kind() {
        ValueVariant::Nil => "nil".to_string(),
        ValueVariant::Bool => "bool".to_string(),
        ValueVariant::Number => "number".to_string(),
        ValueVariant::String => "string".to_string(),
        ValueVariant::List => "list".to_string(),
        ValueVariant::Map => "map".to_string(),
        ValueVariant::Fun => "function".to_string(),
        ValueVariant::Closure => "closure".to_string(),
        ValueVariant::Method => "method".to_string(),
        ValueVariant::Class => "class".to_string(),
        ValueVariant::Instance => "instance".to_string(),
        ValueVariant::Iter => "iterator".to_string(),
        ValueVariant::Upvalue => "upvalue".to_string(),
        ValueVariant::NativeFun => "native function".to_string(),
        ValueVariant::NativeMethod => "native method".to_string(),
      }
    }

    /// Get the class associated with this value
    pub fn value_class(&self, builtin: &BuiltinPrimitives) -> Managed<Class> {
      match self.kind() {
        ValueVariant::Nil => builtin.nil,
        ValueVariant::Bool => builtin.bool,
        ValueVariant::Number => builtin.number,
        ValueVariant::String => builtin.string,
        ValueVariant::List => builtin.list,
        ValueVariant::Map => builtin.map,
        ValueVariant::Fun => panic!("TODO"),
        ValueVariant::Closure => builtin.closure,
        ValueVariant::Method => builtin.method,
        ValueVariant::Class => builtin.class,
        ValueVariant::Instance => self.to_instance().class,
        ValueVariant::Iter => builtin.iter,
        ValueVariant::Upvalue => self.to_upvalue().value().value_class(builtin),
        ValueVariant::NativeFun => builtin.native_fun,
        ValueVariant::NativeMethod => builtin.native_method,
      }
    }
  }

  impl Trace for Value {
    fn trace(&self) -> bool {
      match self.kind() {
        ValueVariant::String => self.to_str().trace(),
        ValueVariant::List => self.to_list().trace(),
        ValueVariant::Map => self.to_map().trace(),
        ValueVariant::Fun => self.to_fun().trace(),
        ValueVariant::Closure => self.to_closure().trace(),
        ValueVariant::Method => self.to_method().trace(),
        ValueVariant::Class => self.to_class().trace(),
        ValueVariant::Instance => self.to_instance().trace(),
        ValueVariant::Iter => self.to_iter().trace(),
        ValueVariant::Upvalue => self.to_upvalue().trace(),
        ValueVariant::NativeFun => self.to_native_fun().trace(),
        ValueVariant::NativeMethod => self.to_native_method().trace(),
        _ => true,
      }
    }
    fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
      match self.kind() {
        ValueVariant::String => self.to_str().trace_debug(stdio),
        ValueVariant::List => self.to_list().trace_debug(stdio),
        ValueVariant::Map => self.to_map().trace_debug(stdio),
        ValueVariant::Fun => self.to_fun().trace_debug(stdio),
        ValueVariant::Closure => self.to_closure().trace_debug(stdio),
        ValueVariant::Method => self.to_method().trace_debug(stdio),
        ValueVariant::Class => self.to_class().trace_debug(stdio),
        ValueVariant::Instance => self.to_instance().trace_debug(stdio),
        ValueVariant::Iter => self.to_iter().trace_debug(stdio),
        ValueVariant::Upvalue => self.to_upvalue().trace_debug(stdio),
        ValueVariant::NativeFun => self.to_native_fun().trace_debug(stdio),
        ValueVariant::NativeMethod => self.to_native_method().trace_debug(stdio),
        _ => true,
      }
    }
  }

  pub struct Nil();

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

  impl From<Managed<String>> for Value {
    fn from(managed: Managed<String>) -> Value {
      Self(managed.to_usize() as u64 | TAG_STRING)
    }
  }

  impl From<Managed<LyVec<Value>>> for Value {
    fn from(managed: Managed<LyVec<Value>>) -> Value {
      Self(managed.to_usize() as u64 | TAG_LIST)
    }
  }

  impl From<Managed<LyHashMap<Value, Value>>> for Value {
    fn from(managed: Managed<LyHashMap<Value, Value>>) -> Value {
      Self(managed.to_usize() as u64 | TAG_MAP)
    }
  }

  impl From<Managed<SlIterator>> for Value {
    fn from(managed: Managed<SlIterator>) -> Value {
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

  impl From<Managed<Box<dyn NativeFun>>> for Value {
    fn from(managed: Managed<Box<dyn NativeFun>>) -> Value {
      Self(managed.to_usize() as u64 | TAG_NATIVE_FUN)
    }
  }

  impl From<Managed<Box<dyn NativeMethod>>> for Value {
    fn from(managed: Managed<Box<dyn NativeMethod>>) -> Value {
      Self(managed.to_usize() as u64 | TAG_NATIVE_METHOD)
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
        ValueVariant::Number => write!(f, "{}", self.to_num()),
        ValueVariant::Bool => write!(f, "{}", self.to_bool()),
        ValueVariant::Nil => write!(f, "nil"),
        ValueVariant::String => write!(f, "'{}'", self.to_str()),
        ValueVariant::List => {
          let list = self.to_list();
          let mut strings: Vec<String> = Vec::with_capacity(list.len());
          for item in list.iter() {
            strings.push(format!("{}", item));
          }

          write!(f, "[{}]", strings.join(", "))
        }
        ValueVariant::Map => {
          let map = self.to_map();
          let strings: Vec<String> = map
            .iter()
            .map(|(key, value)| format!("{}: {}", key, value))
            .collect();
          write!(f, "{{ {} }}", strings.join(", "))
        }
        ValueVariant::Fun => write!(f, "{}", self.to_fun()),
        ValueVariant::Upvalue => match &*self.to_upvalue() {
          Upvalue::Open(stack_ptr) => write!(f, "{}", unsafe { stack_ptr.as_ref() }),
          Upvalue::Closed(store) => write!(f, "{}", store),
        },
        ValueVariant::Closure => write!(f, "{}", *self.to_closure().fun),
        ValueVariant::Method => {
          let bound = self.to_method();
          write!(f, "{}.{}", bound.receiver, bound.method)
        }
        ValueVariant::Class => write!(f, "{}", &self.to_class().name.as_str()),
        ValueVariant::Instance => write!(f, "{} instance", &self.to_instance().class.name),
        ValueVariant::Iter => write!(f, "{}", &self.to_iter().name()),
        ValueVariant::NativeFun => write!(f, "<native fun {}>", self.to_native_fun().meta().name),
        ValueVariant::NativeMethod => {
          write!(f, "<native method {}>", self.to_native_method().meta().name)
        }
      }
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::{
    module::Module,
    object::{Class, Closure, Fun, LyHashMap, LyVec},
  };
  use laythe_env::managed::{Allocation, Manage, Managed};
  use std::{path::PathBuf, ptr::NonNull};
  type Allocs = Vec<Box<Allocation<dyn Manage>>>;

  const VARIANTS: [ValueVariant; 15] = [
    ValueVariant::Bool,
    ValueVariant::Nil,
    ValueVariant::Number,
    ValueVariant::String,
    ValueVariant::List,
    ValueVariant::Map,
    ValueVariant::Fun,
    ValueVariant::Closure,
    ValueVariant::Class,
    ValueVariant::Instance,
    ValueVariant::Iter,
    ValueVariant::Method,
    ValueVariant::NativeFun,
    ValueVariant::NativeMethod,
    ValueVariant::Upvalue,
  ];

  fn is_type(val: Value, variant: ValueVariant) -> bool {
    match variant {
      ValueVariant::Bool => val.is_bool(),
      ValueVariant::Nil => val.is_nil(),
      ValueVariant::Number => val.is_num(),
      ValueVariant::String => val.is_str(),
      ValueVariant::List => val.is_list(),
      ValueVariant::Map => val.is_map(),
      ValueVariant::Fun => val.is_fun(),
      ValueVariant::Closure => val.is_closure(),
      ValueVariant::Class => val.is_class(),
      ValueVariant::Instance => val.is_instance(),
      ValueVariant::Iter => val.is_iter(),
      ValueVariant::Method => val.is_method(),
      ValueVariant::NativeFun => val.is_native_fun(),
      ValueVariant::NativeMethod => val.is_native_method(),
      ValueVariant::Upvalue => val.is_upvalue(),
    }
  }

  fn assert_type(val: Value, target: ValueVariant) {
    VARIANTS.iter().for_each(|variant| {
      if target == *variant {
        assert!(is_type(val, *variant), "Expected to be {:?}", *variant);
      } else {
        assert!(!is_type(val, *variant), "Expected not to be {:?}", *variant);
      };
    });
  }

  fn test_string() -> (Allocs, Managed<String>) {
    let string = "sup".to_string();
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
    let (allocs_string, name) = test_string();
    let (allocs_path, path) = test_path();
    let mut alloc = Box::new(Allocation::new(Module::new(name, path)));
    let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };

    let managed = Managed::from(ptr);
    let mut allocs: Allocs = vec![alloc];
    allocs.extend(allocs_string);
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

  // Closure(Managed<Closure>),
  // Class(Managed<Class>),
  // Instance(Managed<Instance>),
  // Method(Managed<Method>),
  // Iter(Managed<SlIterator>),
  // NativeFun(Managed<Box<dyn NativeFun>>),
  // NativeMethod(Managed<Box<dyn NativeMethod>>),
  // Upvalue(Managed<Upvalue>),

  #[test]
  fn bool() {
    let val_true = Value::from(true);
    let val_false = Value::from(false);

    assert_type(val_true, ValueVariant::Bool);
    assert_type(val_false, ValueVariant::Bool);

    assert_eq!(val_true.to_bool(), true);
    assert_eq!(val_false.to_bool(), false);
  }

  #[test]
  fn num() {
    let val_div_zero = Value::from(1.0 / 0.0);
    let val_nan = Value::from(f64::NAN);
    let val_neg_infinity = Value::from(f64::NEG_INFINITY);
    let val_normal = Value::from(5.3);

    assert_type(val_div_zero, ValueVariant::Number);
    assert_type(val_nan, ValueVariant::Number);
    assert_type(val_neg_infinity, ValueVariant::Number);
    assert_type(val_normal, ValueVariant::Number);

    assert_eq!(val_div_zero.to_num(), 1.0 / 0.0);
    assert!(val_nan.to_num().is_nan());
    assert_eq!(val_neg_infinity.to_num(), f64::NEG_INFINITY);
    assert_eq!(val_normal.to_num(), 5.3);
  }

  #[test]
  fn string() {
    let string = "sup".to_string();
    let mut alloc = Box::new(Allocation::new(string));
    let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };

    let managed = Managed::from(ptr);
    let value = Value::from(managed);
    let managed2: Managed<String> = value.to_str();

    assert_type(value, ValueVariant::String);

    assert_eq!(&*managed, &*managed2);
    assert_eq!(managed, managed2);
  }

  #[test]
  fn list() {
    let list = LyVec::from(vec![VALUE_NIL, VALUE_TRUE, VALUE_FALSE]);
    let mut alloc = Box::new(Allocation::new(list));
    let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };

    let managed = Managed::from(ptr);
    let value = Value::from(managed);
    let managed2 = value.to_list();

    assert_type(value, ValueVariant::List);

    assert_eq!(managed.len(), managed2.len());
    assert_eq!(managed[0], managed2[0]);
    assert_eq!(managed[1], managed2[1]);
    assert_eq!(managed[2], managed2[2]);
    assert_eq!(managed, managed2);
  }

  #[test]
  fn map() {
    let mut map: LyHashMap<Value, Value> = LyHashMap::default();
    map.insert(VALUE_NIL, VALUE_TRUE);
    map.insert(Value::from(10.0), VALUE_FALSE);

    let mut alloc = Box::new(Allocation::new(map));
    let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };

    let managed = Managed::from(ptr);
    let value = Value::from(managed);
    let managed2 = value.to_map();

    assert_type(value, ValueVariant::Map);

    assert_eq!(managed.len(), managed2.len());
    assert_eq!(managed.get(&VALUE_NIL), managed2.get(&VALUE_NIL));
    assert_eq!(
      managed.get(&Value::from(10.0)),
      managed2.get(&Value::from(10.0))
    );
  }

  #[test]
  fn fun() {
    let (_, managed) = test_fun();
    let value = Value::from(managed);
    let managed2 = value.to_fun();

    assert_type(value, ValueVariant::Fun);

    assert_eq!(managed.name, managed2.name);
    assert_eq!(managed.arity, managed2.arity);
  }

  #[test]
  fn closure() {
    let (_, managed) = test_closure();

    let value = Value::from(managed);
    let managed2 = value.to_closure();

    assert_type(value, ValueVariant::Closure);

    assert_eq!(managed.fun, managed2.fun);
    assert_eq!(managed.upvalues.len(), managed2.upvalues.len());
  }

  #[test]
  fn class() {
    let (_, managed) = test_class();

    let value = Value::from(managed);
    let managed2 = value.to_class();

    assert_type(value, ValueVariant::Class);

    assert_eq!(managed.name, managed2.name);
  }
}
