extern crate alloc;

use super::{
  manage::{DebugHeap, DebugWrap, Manage, Trace},
  utils::{get_offset, make_layout},
  GcStr, Mark, Marked, Unmark,
};
use crate::{
  object::{
    Class, Closure, Enumerator, Fun, Instance, List, Map, Method, Native, ObjectKind, Upvalue,
  },
  value::Value,
};
use std::{
  cmp, fmt,
  hash::{Hash, Hasher},
  io::Write,
  marker::PhantomData,
  mem,
  ops::{Deref, DerefMut},
  ptr::{self, NonNull},
  sync::atomic::{AtomicBool, Ordering},
};

pub trait Object: Manage {
  fn kind(&self) -> ObjectKind;
}

#[macro_export]
macro_rules! to_obj_kind {
  ($o:expr, Class) => {
    $o.to_class()
  };
  ($o:expr, Closure) => {
    $o.to_closure()
  };
  ($o:expr, Fun) => {
    $o.to_fun()
  };
  ($o:expr, Instance) => {
    $o.to_instance()
  };
  ($o:expr, Enumerator) => {
    $o.to_enumerator()
  };
  ($o:expr, List) => {
    $o.to_list()
  };
  ($o:expr, Map) => {
    $o.to_map()
  };
  ($o:expr, Method) => {
    $o.to_method()
  };
  ($o:expr, Native) => {
    $o.to_native()
  };
  ($o:expr, String) => {
    $o.to_str()
  };
  ($o:expr, Upvalue) => {
    $o.to_upvalue()
  };
}

#[macro_export]
macro_rules! match_obj {
  (($scrutinee:expr) {
    $(ObjectKind::$obj_kind:ident($p:pat) => $e:expr,)*
    $(_ => $d:expr,)?
  }) => {
    {
      let object: &GcObject = $scrutinee;
      match object.kind() {
        $(ObjectKind::$obj_kind => {
          let $p = to_obj_kind!(object, $obj_kind);
          $e
        })*
        $(_ => $d)?
      }
    }
  };
}

/// The `Header` meta data for `GcObj<T>` and `GcObject`. This struct
/// is positioned at the front of the array such that the layout looks like
/// this
/// ```markdown
/// [Header (potential padding)| T ]
/// ```
pub struct ObjHeader {
  /// Has this allocation been marked by the garbage collector
  marked: AtomicBool,

  /// The underlying value kind of this object
  kind: ObjectKind,
}

impl ObjHeader {
  /// Create a new object header
  #[inline]
  pub fn new(kind: ObjectKind) -> Self {
    Self {
      marked: AtomicBool::new(false),
      kind,
    }
  }

  /// What is the value kind of this object
  #[inline]
  pub fn kind(&self) -> ObjectKind {
    self.kind
  }
}

impl Mark for ObjHeader {
  #[inline]
  fn mark(&self) -> bool {
    self.marked.swap(true, Ordering::Release)
  }
}

impl Unmark for ObjHeader {
  #[inline]
  fn unmark(&self) -> bool {
    self.marked.swap(false, Ordering::Release)
  }
}

impl Marked for ObjHeader {
  #[inline]
  fn marked(&self) -> bool {
    self.marked.load(Ordering::Acquire)
  }
}

pub struct GcObj<T: 'static + Object> {
  /// Pointer to the header of the allocate
  ptr: NonNull<u8>,

  /// Phantom data to hold the type parameter
  phantom: PhantomData<T>,
}

impl<T: 'static + Object> GcObj<T> {
  /// A const pointer to the header of this object
  #[inline]
  unsafe fn header_ptr(&self) -> *mut u8 {
    let offset = get_offset::<ObjHeader, T>();
    self.ptr.as_ptr().sub(offset)
  }

  /// Retrieve the header from this array
  #[inline]
  fn header(&self) -> &ObjHeader {
    #[allow(clippy::cast_ptr_alignment)]
    unsafe {
      &*(self.header_ptr() as *const ObjHeader)
    }
  }

  /// Retrieve a pointer data array
  #[inline]
  fn data(&self) -> &T {
    #[allow(clippy::cast_ptr_alignment)]
    unsafe {
      &*(self.ptr.as_ptr() as *const T)
    }
  }

  /// Retrieve a pointer data array
  #[inline]
  fn data_mut(&self) -> &mut T {
    unsafe { &mut *(self.ptr.as_ptr() as *mut T) }
  }

  /// Retrieve a static reference to the underlying data.
  /// This is require to use some rust method where we cannot
  /// determine the lifetime of the object.
  ///
  /// # Safety
  /// This method assume the allocator will only collect this
  /// object once all references include this reference have
  /// ended. If the allocator collects before this point we'll
  /// segfault or read unintended memory. In fewer word very bad
  pub unsafe fn data_static(&self) -> &'static T {
    &*(self.ptr.as_ptr() as *const T)
  }

  /// Degrade this `GcObj<T>` into a `GcObject`
  #[inline]
  pub fn degrade(self) -> GcObject {
    let shifted = unsafe { NonNull::new_unchecked(self.header_ptr()) };
    GcObject::new(shifted)
  }

  /// Return the underlying pointer as a usize. This is
  /// used by the nan boxing functionality
  #[inline]
  pub fn to_usize(&self) -> usize {
    unsafe { self.header_ptr() as *const () as usize }
  }
}

impl<T: 'static + Object> Trace for GcObj<T> {
  #[inline]
  fn trace(&self) {
    if self.mark() {
      return;
    }

    self.data().trace();
  }

  fn trace_debug(&self, log: &mut dyn Write) {
    if self.mark() {
      return;
    }

    log
      .write_fmt(format_args!(
        "{:p} mark {:?}\n",
        &*self.header(),
        DebugWrap(self, 2)
      ))
      .expect("unable to write to stdout");
    log.flush().expect("unable to flush stdout");

    self.data().trace_debug(log);
  }
}

impl<T: 'static + Object> DebugHeap for GcObj<T> {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    if depth == 0 {
      f.write_str("*")
    } else {
      f.write_fmt(format_args!(
        "{:?}",
        DebugWrap(self.data(), depth.saturating_sub(1))
      ))
    }
  }
}

impl<T: 'static + Object> Manage for GcObj<T> {
  #[inline]
  fn size(&self) -> usize {
    self.data().size()
  }

  #[inline]
  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

impl<T: 'static + Object> Mark for GcObj<T> {
  #[inline]
  fn mark(&self) -> bool {
    self.header().mark()
  }
}

impl<T: 'static + Object> Marked for GcObj<T> {
  #[inline]
  fn marked(&self) -> bool {
    self.header().marked()
  }
}

unsafe impl<T: 'static + Object> Send for GcObj<T> {}
unsafe impl<T: 'static + Object> Sync for GcObj<T> {}

impl<T: 'static + Object> Copy for GcObj<T> {}
impl<T: 'static + Object> Clone for GcObj<T> {
  #[inline]
  fn clone(&self) -> GcObj<T> {
    *self
  }
}

impl<T: 'static + Object> Deref for GcObj<T> {
  type Target = T;

  #[inline]
  fn deref(&self) -> &T {
    &self.data()
  }
}

impl<T: 'static + Object> DerefMut for GcObj<T> {
  #[inline]
  fn deref_mut(&mut self) -> &mut T {
    self.data_mut()
  }
}

impl<T: 'static + Object> PartialEq for GcObj<T> {
  #[inline]
  fn eq(&self, other: &GcObj<T>) -> bool {
    let left_inner: &T = &*self;
    let right_inner: &T = &*other;

    ptr::eq(left_inner, right_inner)
  }
}

impl<T: 'static + Object> Eq for GcObj<T> {}

impl<T: 'static + Object> Hash for GcObj<T> {
  #[inline]
  fn hash<H: Hasher>(&self, state: &mut H) {
    ptr::hash(self.ptr.as_ptr(), state)
  }
}

impl<T: 'static + Object> PartialOrd for GcObj<T> {
  #[inline]
  fn partial_cmp(&self, other: &GcObj<T>) -> Option<cmp::Ordering> {
    Some(self.cmp(other))
  }
}

impl<T: 'static + Object> Ord for GcObj<T> {
  #[inline]
  fn cmp(&self, other: &GcObj<T>) -> cmp::Ordering {
    self.ptr.cmp(&other.ptr)
  }
}

impl<T: 'static + Object + fmt::Display> fmt::Display for GcObj<T> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let inner: &T = &*self;
    write!(f, "{}", inner)
  }
}

impl<T: 'static + Object + fmt::Debug> fmt::Debug for GcObj<T> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let inner: &T = &*self;

    f.debug_struct("GcObj").field("ptr", inner).finish()
  }
}

impl<T: 'static + Object> fmt::Pointer for GcObj<T> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.ptr.fmt(f)
  }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct GcObject {
  /// Pointer to the header of the allocate
  ptr: NonNull<u8>,
}

impl GcObject {
  #[inline]
  pub fn new(ptr: NonNull<u8>) -> Self {
    GcObject { ptr }
  }

  #[inline]
  unsafe fn data_ptr<T>(&self) -> NonNull<u8> {
    let offset = get_offset::<ObjHeader, T>();
    NonNull::new_unchecked(self.ptr.as_ptr().add(offset))
  }

  /// Retrieve the header from this array
  #[inline]
  fn header(&self) -> &ObjHeader {
    #[allow(clippy::cast_ptr_alignment)]
    unsafe {
      &*(self.ptr.as_ptr() as *const ObjHeader)
    }
  }

  #[inline]
  pub fn kind(&self) -> ObjectKind {
    self.header().kind()
  }

  #[inline]
  pub fn is_kind(&self, kind: ObjectKind) -> bool {
    return self.header().kind() == kind;
  }

  #[inline]
  pub fn to_str(&self) -> GcStr {
    unsafe { GcStr::from_alloc_ptr(self.ptr) }
  }

  #[inline]
  pub fn to_class(&self) -> GcObj<Class> {
    GcObj {
      ptr: unsafe { self.data_ptr::<Class>() },
      phantom: PhantomData,
    }
  }

  #[inline]
  pub fn to_closure(&self) -> GcObj<Closure> {
    GcObj {
      ptr: unsafe { self.data_ptr::<Closure>() },
      phantom: PhantomData,
    }
  }

  #[inline]
  pub fn to_fun(&self) -> GcObj<Fun> {
    GcObj {
      ptr: unsafe { self.data_ptr::<Fun>() },
      phantom: PhantomData,
    }
  }

  #[inline]
  pub fn to_instance(&self) -> GcObj<Instance> {
    GcObj {
      ptr: unsafe { self.data_ptr::<Instance>() },
      phantom: PhantomData,
    }
  }

  #[inline]
  pub fn to_enumerator(&self) -> GcObj<Enumerator> {
    GcObj {
      ptr: unsafe { self.data_ptr::<Enumerator>() },
      phantom: PhantomData,
    }
  }

  #[inline]
  pub fn to_list(&self) -> GcObj<List<Value>> {
    GcObj {
      ptr: unsafe { self.data_ptr::<List<Value>>() },
      phantom: PhantomData,
    }
  }

  #[inline]
  pub fn to_map(&self) -> GcObj<Map<Value, Value>> {
    GcObj {
      ptr: unsafe { self.data_ptr::<Map<Value, Value>>() },
      phantom: PhantomData,
    }
  }

  #[inline]
  pub fn to_method(&self) -> GcObj<Method> {
    GcObj {
      ptr: unsafe { self.data_ptr::<Method>() },
      phantom: PhantomData,
    }
  }

  #[inline]
  pub fn to_native(&self) -> GcObj<Native> {
    GcObj {
      ptr: unsafe { self.data_ptr::<Native>() },
      phantom: PhantomData,
    }
  }

  #[inline]
  pub fn to_upvalue(&self) -> GcObj<Upvalue> {
    GcObj {
      ptr: unsafe { self.data_ptr::<Upvalue>() },
      phantom: PhantomData,
    }
  }
}

impl fmt::Display for GcObject {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match_obj!((self) {
      ObjectKind::String(string) => write!(f, "{}", string),
      ObjectKind::List(list) => write!(f, "{}", list),
      ObjectKind::Map(map) => write!(f, "{}", map),
      ObjectKind::Fun(fun) => write!(f, "{}", fun),
      ObjectKind::Upvalue(upvalue) => write!(f, "{}", upvalue),
      ObjectKind::Closure(closure) => write!(f, "{}", closure),
      ObjectKind::Method(method) => write!(f, "{}", method),
      ObjectKind::Class(class) => write!(f, "{}", class),
      ObjectKind::Instance(instance) => write!(f, "{}", instance),
      ObjectKind::Enumerator(enumerator) => write!(f, "{}", enumerator.name()),
      ObjectKind::Native(native) => write!(f, "{}", native),
    })
  }
}

impl fmt::Debug for GcObject {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match_obj!((self) {
      ObjectKind::String(string) => write!(f, "{:?}", string),
      ObjectKind::List(list) => write!(f, "{:?}", list),
      ObjectKind::Map(map) => write!(f, "{:?}", map),
      ObjectKind::Fun(fun) => write!(f, "{:?}", fun),
      ObjectKind::Upvalue(upvalue) => write!(f, "{:?}", upvalue),
      ObjectKind::Closure(closure) => write!(f, "{:?}", closure),
      ObjectKind::Method(method) => write!(f, "{:?}", method),
      ObjectKind::Class(class) => write!(f, "{:?}", class),
      ObjectKind::Instance(instance) => write!(f, "{:?}", instance),
      ObjectKind::Enumerator(enumerator) => write!(f, "{:?}", enumerator.name()),
      ObjectKind::Native(native) => write!(f, "{:?}", native),
    })
  }
}

impl Mark for GcObject {
  #[inline]
  fn mark(&self) -> bool {
    self.header().mark()
  }
}

impl Marked for GcObject {
  #[inline]
  fn marked(&self) -> bool {
    self.header().marked()
  }
}

impl Trace for GcObject {
  #[inline]
  fn trace(&self) {
    if self.marked() {
      return;
    }

    match_obj!((self) {
      ObjectKind::Class(class) => {
        class.trace();
      },
      ObjectKind::Closure(closure) => {
        closure.trace();
      },
      ObjectKind::Enumerator(enumerator) => {
        enumerator.trace();
      },
      ObjectKind::Fun(fun) => {
        fun.trace();
      },
      ObjectKind::Instance(instance) => {
        instance.trace();
      },
      ObjectKind::List(list) => {
        list.trace();
      },
      ObjectKind::Map(map) => {
        map.trace();
      },
      ObjectKind::Method(method) => {
        method.trace();
      },
      ObjectKind::Native(native) => {
        native.trace();
      },
      ObjectKind::String(string) => {
        string.trace();
      },
      ObjectKind::Upvalue(upvalue) => {
        upvalue.trace();
      },
    });
  }

  fn trace_debug(&self, log: &mut dyn Write) {
    if self.marked() {
      return;
    }

    macro_rules! trace_debug {
      ($obj:ident) => {
        log
          .write_fmt(format_args!(
            "{:p} mark {:?}\n",
            &*self.header(),
            DebugWrap(&$obj, 2)
          ))
          .expect("unable to write to stdout");
        log.flush().expect("unable to flush stdout");
        $obj.trace_debug(log)
      };
    }

    match_obj!((self) {
      ObjectKind::Class(class) => {
        trace_debug!(class);
      },
      ObjectKind::Closure(closure) => {
        trace_debug!(closure);
      },
      ObjectKind::Enumerator(enumerator) => {
        trace_debug!(enumerator);
      },
      ObjectKind::Fun(fun) => {
        trace_debug!(fun);
      },
      ObjectKind::Instance(instance) => {
        trace_debug!(instance);
      },
      ObjectKind::List(list) => {
        trace_debug!(list);
      },
      ObjectKind::Map(map) => {
        trace_debug!(map);
      },
      ObjectKind::Method(method) => {
        trace_debug!(method);
      },
      ObjectKind::Native(native) => {
        trace_debug!(native);
      },
      ObjectKind::String(string) => {
        trace_debug!(string);
      },
      ObjectKind::Upvalue(upvalue) => {
        trace_debug!(upvalue);
      },
    });
  }
}

impl DebugHeap for GcObject {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    if depth == 0 {
      return f.write_str("*");
    }

    match_obj!((self) {
      ObjectKind::Class(class) => {
        class.fmt_heap(f, depth)
      },
      ObjectKind::Closure(closure) => {
        closure.fmt_heap(f, depth)
      },
      ObjectKind::Enumerator(enumerator) => {
        enumerator.fmt_heap(f, depth)
      },
      ObjectKind::Fun(fun) => {
        fun.fmt_heap(f, depth)
      },
      ObjectKind::Instance(instance) => {
        instance.fmt_heap(f, depth)
      },
      ObjectKind::List(list) => {
        list.fmt_heap(f, depth)
      },
      ObjectKind::Map(map) => {
        map.fmt_heap(f, depth)
      },
      ObjectKind::Method(method) => {
        method.fmt_heap(f, depth)
      },
      ObjectKind::Native(native) => {
        native.fmt_heap(f, depth)
      },
      ObjectKind::String(string) => {
        string.fmt_heap(f, depth)
      },
      ObjectKind::Upvalue(upvalue) => {
        upvalue.fmt_heap(f, depth)
      },
    })
  }
}

unsafe impl Send for GcObject {}
unsafe impl Sync for GcObject {}

pub struct GcObjectHandle {
  ptr: NonNull<u8>,
}

impl GcObjectHandle {
  /// Retrieve the header from this array
  #[inline]
  fn header(&self) -> &ObjHeader {
    #[allow(clippy::cast_ptr_alignment)]
    unsafe {
      &*(self.ptr.as_ptr() as *const ObjHeader)
    }
  }

  #[inline]
  fn value(&self) -> GcObject {
    GcObject { ptr: self.ptr }
  }

  #[inline]
  pub fn kind(&self) -> ObjectKind {
    self.header().kind()
  }

  #[inline]
  pub fn size(&self) -> usize {
    macro_rules! kind_size {
      ($o:ty) => {{
        make_layout::<ObjHeader, $o>().size()
      }};
    }

    mem::size_of::<Self>()
      + match self.kind() {
        ObjectKind::List => kind_size!(List<Value>),
        ObjectKind::Map => kind_size!(Map<Value, Value>),
        ObjectKind::Fun => kind_size!(Fun),
        ObjectKind::Closure => kind_size!(Closure),
        ObjectKind::Class => kind_size!(Class),
        ObjectKind::Instance => kind_size!(Instance),
        ObjectKind::Enumerator => kind_size!(Enumerator),
        ObjectKind::Method => kind_size!(Method),
        ObjectKind::Native => kind_size!(Native),
        ObjectKind::Upvalue => kind_size!(Upvalue),
        _ => panic!("Boolean, number, string or nil should be in a GcObjectHandle"),
      }
  }
}

impl Drop for GcObjectHandle {
  #[inline]
  fn drop(&mut self) {
    unsafe {
      let kind = self.kind();

      #[allow(clippy::cast_ptr_alignment)]
      ptr::read(self.ptr.as_ptr() as *const ObjHeader);

      macro_rules! drop_kind {
        ($o:ty) => {{
          let offset = get_offset::<ObjHeader, $o>();
          ptr::read(self.ptr.as_ptr().add(offset) as *const $o);
          alloc::alloc::dealloc(self.ptr.as_ptr(), make_layout::<ObjHeader, $o>());
        }};
      }

      match kind {
        ObjectKind::List => drop_kind!(List<Value>),
        ObjectKind::Map => drop_kind!(Map<Value, Value>),
        ObjectKind::Fun => drop_kind!(Fun),
        ObjectKind::Closure => drop_kind!(Closure),
        ObjectKind::Class => drop_kind!(Class),
        ObjectKind::Instance => drop_kind!(Instance),
        ObjectKind::Enumerator => drop_kind!(Enumerator),
        ObjectKind::Method => drop_kind!(Method),
        ObjectKind::Native => drop_kind!(Native),
        ObjectKind::Upvalue => drop_kind!(Upvalue),
        _ => panic!("Boolean, number, string or nil should be in a GcObjectHandle"),
      }
    }
  }
}

impl Marked for GcObjectHandle {
  #[inline]
  fn marked(&self) -> bool {
    self.header().marked()
  }
}

impl Unmark for GcObjectHandle {
  #[inline]
  fn unmark(&self) -> bool {
    self.header().unmark()
  }
}

impl fmt::Pointer for GcObjectHandle {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.ptr.fmt(f)
  }
}

impl DebugHeap for GcObjectHandle {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    self.value().fmt_heap(f, depth)
  }
}

pub struct GcObjectHandleBuilder<T> {
  ptr: NonNull<u8>,

  /// Phantom data to hold the type parameter
  phantom: PhantomData<T>,
}

impl<T: 'static + Object> GcObjectHandleBuilder<T> {
  #[inline]
  pub fn value(&self) -> GcObj<T> {
    GcObj {
      ptr: unsafe { self.data_ptr::<T>() },
      phantom: PhantomData,
    }
  }
}

impl<T> GcObjectHandleBuilder<T> {
  unsafe fn data_ptr<U>(&self) -> NonNull<u8> {
    let offset = get_offset::<ObjHeader, U>();
    NonNull::new_unchecked(self.ptr.as_ptr().add(offset))
  }

  #[inline]
  pub fn degrade(self) -> GcObjectHandle {
    GcObjectHandle { ptr: self.ptr }
  }

  #[inline]
  pub fn size(&self) -> usize {
    mem::size_of::<Self>() + make_layout::<ObjHeader, T>().size()
  }
}

impl<T: 'static + Object> From<T> for GcObjectHandleBuilder<T> {
  #[inline]
  fn from(item: T) -> Self {
    let new_layout = make_layout::<ObjHeader, T>();
    let buf = unsafe { alloc::alloc::alloc(new_layout) };

    if buf.is_null() {
      alloc::alloc::handle_alloc_error(new_layout);
    }

    let header = ObjHeader::new(item.kind());

    #[allow(clippy::cast_ptr_alignment)]
    unsafe {
      ptr::write(buf as *mut ObjHeader, header);
      ptr::write(buf.add(get_offset::<ObjHeader, T>()) as *mut T, item);

      GcObjectHandleBuilder {
        ptr: NonNull::new_unchecked(buf),
        phantom: PhantomData,
      }
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod header {
    use super::*;

    #[test]
    pub fn kind() {
      let header = ObjHeader::new(ObjectKind::Class);
      assert_eq!(header.kind(), ObjectKind::Class);
    }
  }

  mod gc_object_handle_builder {
    use super::*;

    #[test]
    pub fn from() {
      let handle_builder = GcObjectHandleBuilder::from(List::new());
      let gc_value = handle_builder.value();

      assert_eq!(gc_value.kind(), ObjectKind::List);
    }

    #[test]
    pub fn degrade() {
      let handle_builder = GcObjectHandleBuilder::from(List::new());
      let handle = handle_builder.degrade();

      assert_eq!(handle.kind(), ObjectKind::List);
    }
  }

  mod gc_obj {
    use super::*;
    use crate::value::{VALUE_FALSE, VALUE_TRUE};

    #[test]
    fn deref() {
      let mut list = List::new();
      list.push(VALUE_FALSE);
      list.push(VALUE_TRUE);

      let handle_builder = GcObjectHandleBuilder::from(list);
      let gc_obj = handle_builder.value();

      assert_eq!(gc_obj[0], VALUE_FALSE);
      assert_eq!(gc_obj[1], VALUE_TRUE);
    }

    #[test]
    fn deref_mut() {
      let mut list = List::new();
      list.push(VALUE_FALSE);
      list.push(VALUE_TRUE);

      let handle_builder = GcObjectHandleBuilder::from(list);
      let mut gc_obj = handle_builder.value();

      gc_obj[1] = Value::from(1.0);

      assert_eq!(gc_obj[0], VALUE_FALSE);
      assert_eq!(gc_obj[1], Value::from(1.0));
    }

    #[test]
    fn kind() {
      let handle_builder = GcObjectHandleBuilder::from(List::new());
      let gc_obj = handle_builder.value();

      assert_eq!(gc_obj.kind(), ObjectKind::List);
    }

    #[test]
    fn degrade() {
      let handle_builder = GcObjectHandleBuilder::from(List::new());
      let gc_obj = handle_builder.value();
      let gc_object = gc_obj.degrade();

      assert_eq!(gc_object.kind(), ObjectKind::List);
    }
  }

  mod gc_object {
    use crate::value::VALUE_NIL;

    use super::*;

    fn create_object<T: 'static + Object>(item: T) -> GcObjectHandle {
      let handle_builder = GcObjectHandleBuilder::from(item);
      handle_builder.degrade()
    }

    #[test]
    fn drop_obj() {
      let handle_list = create_object(List::<Value>::new());
      let handle_map = create_object(Map::<Value, Value>::new());
      let handle_upvalue = create_object(Upvalue::Closed(VALUE_NIL));

      drop(handle_list);
      drop(handle_map);
      drop(handle_upvalue);
    }
  }
}
