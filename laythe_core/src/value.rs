pub struct Nil();

/// Enum of value types in laythe
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum ValueKind {
  Bool,
  Nil,
  Undefined,
  Number,
  Obj,
}

#[cfg(not(feature = "nan_boxing"))]
pub use self::unboxed::*;

#[cfg(feature = "nan_boxing")]
pub use self::boxed::*;

#[cfg(not(feature = "nan_boxing"))]
mod unboxed {
  use crate::{
    collections::RawSharedVector, managed::{DebugHeap, DebugWrap, Trace}, object::{
      Channel, Class, Closure, Enumerator, Fun, Instance, List, LyBox, LyStr, Map, Method, Native, ObjHeader, ObjectKind, Tuple
    }, ObjRef, ObjectRef
  };

  use super::{Nil, ValueKind};
  use std::fmt;
  use std::fmt::Debug;
  use std::hash::Hash;
  use std::io::Write;

  pub const VALUE_NIL: Value = Value::Nil;
  pub const VALUE_UNDEFINED: Value = Value::Undefined;
  pub const VALUE_FALSE: Value = Value::Bool(false);
  pub const VALUE_TRUE: Value = Value::Bool(true);

  /// Enum of value types in laythe
  #[derive(Clone, Copy, Debug)]
  pub enum Value {
    Bool(bool),
    Nil,
    Undefined,
    Number(f64),
    Obj(ObjectRef),
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
    pub fn is_undefined(&self) -> bool {
      matches!(self, Value::Undefined)
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
    pub fn is_obj(&self) -> bool {
      matches!(self, Value::Obj(_))
    }

    #[inline]
    pub fn is_obj_kind(&self, kind: ObjectKind) -> bool {
      if let Value::Obj(obj) = self {
        return obj.is_kind(kind);
      }

      false
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
    pub fn to_num(self) -> f64 {
      match self {
        Value::Number(num) => num,
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
    pub fn to_bool(self) -> bool {
      match self {
        Value::Bool(b1) => b1,
        _ => panic!("Value is not boolean"),
      }
    }

    /// Unwrap and reference a laythe string, panics if not a string
    ///
    /// # Examples
    /// ```
    /// use laythe_core::value::Value;
    /// use laythe_core::hooks::{Hooks, NoContext};
    /// use laythe_core::Allocator;
    ///
    /// let gc = Allocator::default();
    /// let mut context = NoContext::new(gc);
    /// let hooks = Hooks::new(&mut context);
    /// let managed =  hooks.manage_str("example");
    ///
    /// let value = Value::from(managed);
    /// assert_eq!(&*value.to_obj().to_str(), "example")
    /// ```
    #[inline]
    pub fn to_obj(self) -> ObjectRef {
      match self {
        Self::Obj(obj) => obj,
        _ => panic!("Expected object."),
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
    pub fn value_type(&self) -> &'static str {
      match self {
        Value::Nil => "nil",
        Value::Undefined => "undefined",
        Value::Bool(_) => "bool",
        Value::Number(_) => "number",
        Value::Obj(obj) => match obj.kind() {
          ObjectKind::Channel => "channel",
          ObjectKind::String => "string",
          ObjectKind::Tuple => "tuple",
          ObjectKind::List => "list",
          ObjectKind::Map => "map",
          ObjectKind::Fun => "function",
          ObjectKind::Closure => "closure",
          ObjectKind::Class => "class",
          ObjectKind::Instance => "instance",
          ObjectKind::Enumerator => "enumerator",
          ObjectKind::Method => "method",
          ObjectKind::Native => "native",
          ObjectKind::LyBox => "box",
        },
      }
    }

    pub fn kind(&self) -> ValueKind {
      match self {
        Value::Nil => ValueKind::Nil,
        Value::Undefined => ValueKind::Undefined,
        Value::Bool(_) => ValueKind::Bool,
        Value::Number(_) => ValueKind::Number,
        Value::Obj(_) => ValueKind::Obj,
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

  impl From<Tuple> for Value {
    fn from(managed: Tuple) -> Value {
      Value::Obj(managed.degrade())
    }
  }

  impl From<LyStr> for Value {
    fn from(managed: LyStr) -> Value {
      Value::Obj(managed.degrade())
    }
  }

  impl From<ObjRef<Channel>> for Value {
    fn from(managed: ObjRef<Channel>) -> Value {
      Value::Obj(managed.degrade())
    }
  }

  impl From<RawSharedVector<Value, ObjHeader>> for Value {
    fn from(managed: RawSharedVector<Value, ObjHeader>) -> Value {
      Value::Obj(List::new(managed).degrade())
    }
  }

  impl From<List> for Value {
    fn from(managed: List) -> Value {
      Value::Obj(managed.degrade())
    }
  }

  impl From<ObjRef<Map<Value, Value>>> for Value {
    fn from(managed: ObjRef<Map<Value, Value>>) -> Value {
      Value::Obj(managed.degrade())
    }
  }

  impl From<ObjRef<Enumerator>> for Value {
    fn from(managed: ObjRef<Enumerator>) -> Value {
      Value::Obj(managed.degrade())
    }
  }

  impl From<ObjRef<Closure>> for Value {
    fn from(managed: ObjRef<Closure>) -> Value {
      Value::Obj(managed.degrade())
    }
  }

  impl From<ObjRef<Fun>> for Value {
    fn from(managed: ObjRef<Fun>) -> Value {
      Value::Obj(managed.degrade())
    }
  }

  impl From<ObjRef<Class>> for Value {
    fn from(managed: ObjRef<Class>) -> Value {
      Value::Obj(managed.degrade())
    }
  }

  impl From<Instance> for Value {
    fn from(managed: Instance) -> Value {
      Value::Obj(managed.degrade())
    }
  }

  impl From<ObjRef<Method>> for Value {
    fn from(managed: ObjRef<Method>) -> Value {
      Value::Obj(managed.degrade())
    }
  }

  impl From<ObjRef<Native>> for Value {
    fn from(managed: ObjRef<Native>) -> Value {
      Value::Obj(managed.degrade())
    }
  }

  impl From<ObjRef<LyBox>> for Value {
    fn from(managed: ObjRef<LyBox>) -> Value {
      Value::Obj(managed.degrade())
    }
  }

  impl fmt::Display for Value {
    /// Implement display for value in laythe
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      match self {
        Self::Number(num) => write!(f, "{}", num),
        Self::Bool(b) => write!(f, "{}", b),
        Self::Nil => write!(f, "nil"),
        Self::Undefined => write!(f, "undefined"),
        Self::Obj(obj) => write!(f, "{}", obj),
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
        (Self::Obj(obj1), Self::Obj(obj2)) => obj1 == obj2,
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
        },
        Self::Bool(b) => {
          ValueKind::Bool.hash(state);
          b.hash(state);
        },
        Self::Nil => ValueKind::Nil.hash(state),
        Self::Undefined => ValueKind::Undefined.hash(state),
        Self::Obj(obj) => {
          ValueKind::Obj.hash(state);
          obj.hash(state);
        },
      };
    }
  }

  impl Trace for Value {
    #[inline]
    fn trace(&self) {
      if let Value::Obj(obj) = self {
        obj.trace();
      }
    }

    fn trace_debug(&self, stdout: &mut dyn Write) {
      if let Value::Obj(obj) = self {
        obj.trace_debug(stdout);
      }
    }
  }

  impl DebugHeap for Value {
    fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
      match self {
        Value::Nil => f.write_str("nil"),
        Value::Undefined => f.write_str("undefined"),
        Value::Bool(b) => f.write_fmt(format_args!("{}", b)),
        Value::Number(num) => f.write_fmt(format_args!("{}", num)),
        Value::Obj(obj) => f.write_fmt(format_args!("{:?}", DebugWrap(obj, depth))),
      }
    }
  }

  #[cfg(test)]
  mod test {
    use super::*;
    use std::mem;

    #[test]
    fn size() {
      assert_eq!(mem::size_of::<Map<Value, Value>>(), 32);
      assert_eq!(mem::size_of::<Closure>(), 16);
      assert_eq!(mem::size_of::<Fun>(), 56);
      assert_eq!(mem::size_of::<Class>(), 104);
      assert_eq!(mem::size_of::<Method>(), 32);
      assert_eq!(mem::size_of::<Enumerator>(), 32);
      assert_eq!(mem::size_of::<Native>(), 56);
      assert_eq!(mem::size_of::<LyBox>(), 16);
    }

    #[test]
    fn alignment() {
      let target: usize = 8;

      assert_eq!(mem::align_of::<Map<Value, Value>>(), target);
      assert_eq!(mem::align_of::<Closure>(), target);
      assert_eq!(mem::align_of::<Fun>(), target);
      assert_eq!(mem::align_of::<Class>(), target);
      assert_eq!(mem::align_of::<Method>(), target);
      assert_eq!(mem::align_of::<Enumerator>(), target);
      assert_eq!(mem::align_of::<Native>(), target);
      assert_eq!(mem::align_of::<LyBox>(), target);
    }
  }
}

#[cfg(feature = "nan_boxing")]
mod boxed {
  use super::{Nil, ValueKind};
  use crate::{
    collections::RawSharedVector,
    managed::{DebugHeap, Trace},
    object::{
      Channel, Class, Closure, Enumerator, Fun, Instance, List, LyBox, LyStr, Map, Method,
      Native, ObjHeader, ObjectKind, Tuple,
    },
    reference::{ObjRef, ObjectRef},
  };

  use std::ptr::NonNull;
  use std::{fmt, io::Write};

  const BIT_SIGN: u64 = 0xc000_0000_0000_0000;

  const TAG_NIL: u64 = 1 | QNAN; // 001
  const TAG_FALSE: u64 = 2 | QNAN; // 010
  const TAG_TRUE: u64 = 3 | QNAN; // 011
  const TAG_UNDEFINED: u64 = 4 | QNAN; // 100
  const TAG_OBJ: u64 = BIT_SIGN | QNAN;

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

  // 0111 1111 1111 1100 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0100
  pub const VALUE_UNDEFINED: Value = Value(TAG_UNDEFINED);

  #[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
  pub struct Value(u64);

  impl Value {
    #[inline]
    pub fn is_nil(&self) -> bool {
      self.0 == VALUE_NIL.0
    }

    #[inline]
    pub fn is_undefined(&self) -> bool {
      self.0 == VALUE_UNDEFINED.0
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
    pub fn is_obj(&self) -> bool {
      (self.0 & TAG_OBJ) == TAG_OBJ
    }

    #[inline]
    pub fn is_obj_kind(&self, kind: ObjectKind) -> bool {
      if !self.is_obj() {
        return false;
      }
      self.to_obj().is_kind(kind)
    }

    #[inline]
    pub fn to_bool(self) -> bool {
      self == VALUE_TRUE
    }

    #[inline]
    pub fn to_num(self) -> f64 {
      let union = NumberUnion { bits: self.0 };
      unsafe { union.num }
    }

    #[inline]
    pub fn to_obj(self) -> ObjectRef {
      let as_unsigned = self.0 & !TAG_OBJ;
      let ptr = unsafe { NonNull::new_unchecked(as_unsigned as usize as *mut u8) };
      ObjectRef::new(ptr)
    }

    #[inline]
    pub fn kind(&self) -> ValueKind {
      if self.is_num() {
        return ValueKind::Number;
      }

      if self.0 >= BIT_SIGN {
        return ValueKind::Obj;
      }

      match self.0 & 0x7 {
        1 => ValueKind::Nil,
        2 | 3 => ValueKind::Bool,
        4 => ValueKind::Undefined,
        _ => panic!("Improperly encoded value"),
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
    pub fn value_type(&self) -> &'static str {
      match self.kind() {
        ValueKind::Nil => "nil",
        ValueKind::Bool => "bool",
        ValueKind::Number => "number",
        ValueKind::Undefined => "undefined",
        ValueKind::Obj => match self.to_obj().kind() {
          ObjectKind::String => "string",
          ObjectKind::Channel => "channel",
          ObjectKind::List => "list",
          ObjectKind::Map => "map",
          ObjectKind::Fun => "function",
          ObjectKind::Closure => "closure",
          ObjectKind::Class => "class",
          ObjectKind::Instance => "instance",
          ObjectKind::Enumerator => "enumerator",
          ObjectKind::Method => "method",
          ObjectKind::Native => "native",
          ObjectKind::LyBox => "box",
          ObjectKind::Tuple => "tuple",
        },
      }
    }
  }

  impl Trace for Value {
    fn trace(&self) {
      if self.is_obj() {
        self.to_obj().trace();
      }
    }
    fn trace_debug(&self, log: &mut dyn Write) {
      if self.is_obj() {
        self.to_obj().trace_debug(log);
      }
    }
  }

  impl DebugHeap for Value {
    fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
      match self.kind() {
        ValueKind::Bool => f.write_fmt(format_args!("{}", self.to_bool())),
        ValueKind::Nil => f.write_str("nil"),
        ValueKind::Undefined => f.write_str("undefined"),
        ValueKind::Number => f.write_fmt(format_args!("{}", self.to_num())),
        ValueKind::Obj => self.to_obj().fmt_heap(f, depth),
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

  impl From<Tuple> for Value {
    fn from(managed: Tuple) -> Value {
      Self(managed.to_usize() as u64 | TAG_OBJ)
    }
  }

  impl From<LyStr> for Value {
    fn from(managed: LyStr) -> Value {
      Self(managed.to_usize() as u64 | TAG_OBJ)
    }
  }

  impl From<ObjRef<Channel>> for Value {
    fn from(managed: ObjRef<Channel>) -> Value {
      Self(managed.to_usize() as u64 | TAG_OBJ)
    }
  }

  impl From<List> for Value {
    fn from(managed: List) -> Value {
      Self(managed.to_usize() as u64 | TAG_OBJ)
    }
  }

  impl From<RawSharedVector<Value, ObjHeader>> for Value {
    fn from(managed: RawSharedVector<Value, ObjHeader>) -> Value {
      Self(List::new(managed).to_usize() as u64 | TAG_OBJ)
    }
  }

  impl From<ObjRef<Map<Value, Value>>> for Value {
    fn from(managed: ObjRef<Map<Value, Value>>) -> Value {
      Self(managed.to_usize() as u64 | TAG_OBJ)
    }
  }

  impl From<ObjRef<Enumerator>> for Value {
    fn from(managed: ObjRef<Enumerator>) -> Value {
      Self(managed.to_usize() as u64 | TAG_OBJ)
    }
  }

  impl From<ObjRef<Closure>> for Value {
    fn from(managed: ObjRef<Closure>) -> Value {
      Self(managed.to_usize() as u64 | TAG_OBJ)
    }
  }

  impl From<ObjRef<Fun>> for Value {
    fn from(managed: ObjRef<Fun>) -> Value {
      Self(managed.to_usize() as u64 | TAG_OBJ)
    }
  }

  impl From<ObjRef<Class>> for Value {
    fn from(managed: ObjRef<Class>) -> Value {
      Self(managed.to_usize() as u64 | TAG_OBJ)
    }
  }

  impl From<Instance> for Value {
    fn from(managed: Instance) -> Value {
      Self(managed.to_usize() as u64 | TAG_OBJ)
    }
  }

  impl From<ObjRef<Method>> for Value {
    fn from(managed: ObjRef<Method>) -> Value {
      Self(managed.to_usize() as u64 | TAG_OBJ)
    }
  }

  impl From<ObjRef<Native>> for Value {
    fn from(managed: ObjRef<Native>) -> Value {
      Self(managed.to_usize() as u64 | TAG_OBJ)
    }
  }

  impl From<ObjRef<LyBox>> for Value {
    fn from(managed: ObjRef<LyBox>) -> Value {
      Self(managed.to_usize() as u64 | TAG_OBJ)
    }
  }

  impl fmt::Display for Value {
    /// Implement display for value in laythe
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      match self.kind() {
        ValueKind::Number => write!(f, "{}", self.to_num()),
        ValueKind::Bool => write!(f, "{}", self.to_bool()),
        ValueKind::Nil => write!(f, "nil"),
        ValueKind::Undefined => write!(f, "undefined"),
        ValueKind::Obj => write!(f, "{}", self.to_obj()),
      }
    }
  }

  #[cfg(test)]
  mod test {
    use super::*;
    use std::mem;

    #[test]
    fn size() {
      assert_eq!(mem::size_of::<Map<Value, Value>>(), 32);
      assert_eq!(mem::size_of::<Closure>(), 16);
      assert_eq!(mem::size_of::<Fun>(), 56);
      assert_eq!(mem::size_of::<Class>(), 104);
      assert_eq!(mem::size_of::<Method>(), 16);
      assert_eq!(mem::size_of::<Enumerator>(), 24);
      assert_eq!(mem::size_of::<Native>(), 56);
      assert_eq!(mem::size_of::<LyBox>(), 8);
    }

    #[test]
    fn alignment() {
      let target: usize = 8;

      assert_eq!(mem::align_of::<Map<Value, Value>>(), target);
      assert_eq!(mem::align_of::<Closure>(), target);
      assert_eq!(mem::align_of::<Fun>(), target);
      assert_eq!(mem::align_of::<Class>(), target);
      assert_eq!(mem::align_of::<Method>(), target);
      assert_eq!(mem::align_of::<Enumerator>(), target);
      assert_eq!(mem::align_of::<Native>(), target);
      assert_eq!(mem::align_of::<LyBox>(), target);
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::{
    allocator::NO_GC,
    captures::Captures,
    hooks::{GcHooks, NoContext},
    list,
    module::Module,
    object::{Class, Closure, Fun, List, LyStr, Map, ObjectKind},
    reference::{ObjRef, Ref},
    val, Allocator,
  };

  const VALUE_VARIANTS: [ValueKind; 4] = [
    ValueKind::Bool,
    ValueKind::Nil,
    ValueKind::Number,
    ValueKind::Obj,
  ];

  const OBJECT_VARIANTS: [ObjectKind; 11] = [
    ObjectKind::Class,
    ObjectKind::Closure,
    ObjectKind::Enumerator,
    ObjectKind::Fun,
    ObjectKind::Instance,
    ObjectKind::List,
    ObjectKind::Map,
    ObjectKind::Method,
    ObjectKind::Native,
    ObjectKind::String,
    ObjectKind::LyBox,
  ];

  fn is_value_type(val: Value, variant: ValueKind) -> bool {
    match variant {
      ValueKind::Bool => val.is_bool(),
      ValueKind::Nil => val.is_nil(),
      ValueKind::Undefined => val.is_undefined(),
      ValueKind::Number => val.is_num(),
      ValueKind::Obj => val.is_obj(),
    }
  }

  fn is_object_type(val: Value, variant: ObjectKind) -> bool {
    if !val.is_obj() {
      return false;
    }

    let obj = val.to_obj();
    obj.is_kind(variant)
  }

  fn assert_value_type(val: Value, target: ValueKind) {
    VALUE_VARIANTS.iter().for_each(|variant| {
      if target == *variant {
        assert!(
          is_value_type(val, *variant),
          "Expected to be {:?}",
          *variant
        );
      } else {
        assert!(
          !is_value_type(val, *variant),
          "Expected not to be {:?}",
          *variant
        );
      };
    });
  }

  fn assert_object_type(val: Value, target: ObjectKind) {
    OBJECT_VARIANTS.iter().for_each(|variant| {
      if target == *variant {
        assert!(
          is_object_type(val, *variant),
          "Expected to be {:?}",
          *variant
        );
      } else {
        assert!(
          !is_object_type(val, *variant),
          "Expected not to be {:?}",
          *variant
        );
      };
    });
  }

  fn test_string(hooks: &GcHooks) -> LyStr {
    hooks.manage_str("sup")
  }

  fn test_module(hooks: &GcHooks) -> Ref<Module> {
    let class = test_class(hooks);

    hooks.manage(Module::new(hooks, class, "sup", 0))
  }

  fn test_fun(hooks: &GcHooks) -> ObjRef<Fun> {
    let name = test_string(hooks);
    let module = test_module(hooks);

    hooks.manage_obj(Fun::stub(hooks, name, module))
  }

  fn test_closure(hooks: &GcHooks) -> ObjRef<Closure> {
    let fun = test_fun(hooks);

    hooks.manage_obj(Closure::new(fun, Captures::build(hooks, &[])))
  }

  fn test_class(hooks: &GcHooks) -> ObjRef<Class> {
    let name = test_string(hooks);

    hooks.manage_obj(Class::bare(name))
  }

  #[test]
  fn bool() {
    let val_true = val!(true);
    let val_false = val!(false);

    assert_value_type(val_true, ValueKind::Bool);
    assert_value_type(val_false, ValueKind::Bool);

    assert!(val_true.to_bool());
    assert!(!val_false.to_bool());
  }

  #[test]
  fn num() {
    let val_div_zero = val!(1.0 / 0.0);
    let val_nan = val!(f64::NAN);
    let val_neg_infinity = val!(f64::NEG_INFINITY);
    let val_normal = val!(5.3);

    assert_value_type(val_div_zero, ValueKind::Number);
    assert_value_type(val_nan, ValueKind::Number);
    assert_value_type(val_neg_infinity, ValueKind::Number);
    assert_value_type(val_normal, ValueKind::Number);

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

    assert_value_type(value, ValueKind::Obj);
    assert_object_type(value, ObjectKind::String);

    let string2: LyStr = value.to_obj().to_str();
    assert_eq!(string, string2);
  }

  #[test]
  fn list() {
    let mut gc = Allocator::default();
    let list = List::new(gc.manage_obj(list!(&[VALUE_NIL, VALUE_TRUE, VALUE_FALSE]), &NO_GC));

    let value = val!(list);

    assert_value_type(value, ValueKind::Obj);
    assert_object_type(value, ObjectKind::List);

    let list2 = value.to_obj().to_list();

    assert_eq!(list.len(), list2.len());
    assert_eq!(list[0], list2[0]);
    assert_eq!(list[1], list2[1]);
    assert_eq!(list[2], list2[2]);
    assert_eq!(list, list2);
  }

  #[test]
  fn map() {
    let mut gc = Allocator::default();
    let mut map: Map<Value, Value> = Map::default();
    map.insert(VALUE_NIL, VALUE_TRUE);
    map.insert(val!(10.0), VALUE_FALSE);

    let map = gc.manage_obj(map, &NO_GC);
    let value = val!(map);

    assert_value_type(value, ValueKind::Obj);
    assert_object_type(value, ObjectKind::Map);
    let map2 = value.to_obj().to_map();

    assert_eq!(map.len(), map2.len());
    assert_eq!(map.get(&VALUE_NIL), map2.get(&VALUE_NIL));
    assert_eq!(map.get(&val!(10.0)), map2.get(&val!(10.0)));
  }

  #[test]
  fn fun() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);
    let fun = test_fun(&hooks);
    let value = val!(fun);

    assert_value_type(value, ValueKind::Obj);
    assert_object_type(value, ObjectKind::Fun);
    let fun2 = value.to_obj().to_fun();

    assert_eq!(fun.name(), fun2.name());
  }

  #[test]
  fn closure() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);
    let closure = test_closure(&hooks);

    let value = val!(closure);

    assert_value_type(value, ValueKind::Obj);
    assert_object_type(value, ObjectKind::Closure);
    let closure2 = value.to_obj().to_closure();

    assert_eq!(closure.fun(), closure2.fun());
    assert_eq!(closure.captures(), closure2.captures());
  }

  #[test]
  fn class() {
    let context = NoContext::default();
    let hooks = GcHooks::new(&context);
    let class = test_class(&hooks);

    let value = val!(class);
    assert_value_type(value, ValueKind::Obj);
    assert_object_type(value, ObjectKind::Class);

    let class2 = value.to_obj().to_class();
    assert_eq!(class.name(), class2.name());
  }
}
