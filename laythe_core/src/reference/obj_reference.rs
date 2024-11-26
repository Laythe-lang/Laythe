use crate::{
  align_utils::{
    get_array_len_offset, get_array_offset, get_offset, get_vector_cap_offset, get_vector_offset,
    make_array_layout, make_obj_layout, make_vector_layout,
  },
  managed::{AllocObjResult, AllocateObj, DebugHeap, DebugWrap, Mark, Marked, Trace, Unmark},
  match_obj,
  object::{
    Channel, Class, Closure, Enumerator, Fun, Instance, InstanceHeader, List, LyBox, LyStr,
    Map, Method, Native, ObjHeader, ObjectKind, Tuple,
  },
  utils::strip_msb,
  value::Value,
};
use std::{
  alloc::{alloc, dealloc, handle_alloc_error},
  cmp, fmt,
  hash::{Hash, Hasher},
  io::Write,
  marker::PhantomData,
  ops::{Deref, DerefMut},
  ptr::{self, NonNull},
};

pub trait Object: Trace + DebugHeap {
  fn kind(&self) -> ObjectKind;
}

pub struct ObjRef<T: 'static + Object> {
  /// Pointer to the header of the allocate
  ptr: NonNull<T>,
}

impl<T: 'static + Object> ObjRef<T> {
  /// A const pointer to the header of this object
  #[inline]
  unsafe fn header_ptr(&self) -> *mut u8 {
    let offset = get_offset::<ObjHeader, T>();
    (self.ptr.as_ptr() as *mut u8).sub(offset)
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
    unsafe { self.ptr.as_ref() }
  }

  /// Retrieve a pointer data array
  #[inline]
  fn data_mut(&mut self) -> &mut T {
    unsafe { self.ptr.as_mut() }
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

  /// Degrade this `ObjRef<T>` into a `ObjRefect`
  #[inline]
  pub fn degrade(self) -> ObjectRef {
    let shifted = unsafe { NonNull::new_unchecked(self.header_ptr()) };
    ObjectRef::new(shifted)
  }

  /// Create a dangling ObjRef pointer
  pub fn dangling() -> ObjRef<T> {
    ObjRef {
      ptr: NonNull::dangling(),
    }
  }

  /// Return the underlying pointer as a usize. This is
  /// used by the nan boxing functionality
  #[inline]
  pub fn to_usize(self) -> usize {
    unsafe { self.header_ptr() as *const () as usize }
  }
}

impl<T: 'static + Object> Trace for ObjRef<T> {
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
        self.header(),
        DebugWrap(self, 1)
      ))
      .expect("unable to write to stdout");
    log.flush().expect("unable to flush stdout");

    self.data().trace_debug(log);
  }
}

impl<T: 'static + Object> DebugHeap for ObjRef<T> {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    if depth == 0 {
      f.write_fmt(format_args!("{:p}", self.ptr))
    } else {
      f.write_fmt(format_args!(
        "{:?}",
        DebugWrap(self.data(), depth.saturating_sub(1))
      ))
    }
  }
}

impl<T: 'static + Object> Mark for ObjRef<T> {
  #[inline]
  fn mark(&self) -> bool {
    self.header().mark()
  }
}

impl<T: 'static + Object> Marked for ObjRef<T> {
  #[inline]
  fn marked(&self) -> bool {
    self.header().marked()
  }
}

unsafe impl<T: 'static + Object> Send for ObjRef<T> {}
unsafe impl<T: 'static + Object> Sync for ObjRef<T> {}

impl<T: 'static + Object> Copy for ObjRef<T> {}
impl<T: 'static + Object> Clone for ObjRef<T> {
  #[inline]
  fn clone(&self) -> ObjRef<T> {
    *self
  }
}

impl<T: 'static + Object> Deref for ObjRef<T> {
  type Target = T;

  #[inline]
  fn deref(&self) -> &T {
    self.data()
  }
}

impl<T: 'static + Object> DerefMut for ObjRef<T> {
  #[inline]
  fn deref_mut(&mut self) -> &mut T {
    self.data_mut()
  }
}

impl<T: 'static + Object> PartialEq for ObjRef<T> {
  #[inline]
  fn eq(&self, other: &ObjRef<T>) -> bool {
    let left_inner: &T = self;
    let right_inner: &T = other;

    ptr::eq(left_inner, right_inner)
  }
}

impl<T: 'static + Object> Eq for ObjRef<T> {}

impl<T: 'static + Object> Hash for ObjRef<T> {
  #[inline]
  fn hash<H: Hasher>(&self, state: &mut H) {
    ptr::hash(self.ptr.as_ptr(), state)
  }
}

impl<T: 'static + Object> PartialOrd for ObjRef<T> {
  #[inline]
  fn partial_cmp(&self, other: &ObjRef<T>) -> Option<cmp::Ordering> {
    Some(self.cmp(other))
  }
}

impl<T: 'static + Object> Ord for ObjRef<T> {
  #[inline]
  fn cmp(&self, other: &ObjRef<T>) -> cmp::Ordering {
    self.ptr.cmp(&other.ptr)
  }
}

impl<T: 'static + Object + fmt::Display> fmt::Display for ObjRef<T> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let inner: &T = self;
    write!(f, "{inner}")
  }
}

impl<T: 'static + Object + fmt::Debug> fmt::Debug for ObjRef<T> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let inner: &T = self;

    f.debug_struct("ObjRef").field("ptr", inner).finish()
  }
}

impl<T: 'static + Object> fmt::Pointer for ObjRef<T> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    // Our value pointer pointer directly at T
    // For debugging purposes we really want to know
    // where the start of the allocation is instead
    let offset = get_offset::<ObjHeader, T>();
    let ptr = unsafe { self.ptr.as_ptr().sub(offset) };
    ptr.fmt(f)
  }
}

impl<T> AllocateObj<ObjRef<T>> for T
where
  T: Object,
{
  fn alloc(self) -> AllocObjResult<ObjRef<T>> {
    let handle = ObjectHandlerBuilder::from(self);
    let size = handle.size();
    let reference = handle.value();

    AllocObjResult {
      handle: handle.degrade(),
      size,
      reference,
    }
  }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct ObjectRef {
  /// Pointer to the header of the allocate
  ptr: NonNull<u8>,
}

impl ObjectRef {
  #[inline]
  pub fn new(ptr: NonNull<u8>) -> Self {
    ObjectRef { ptr }
  }

  #[inline]
  unsafe fn data_ptr<T>(&self) -> NonNull<T> {
    let offset = get_offset::<ObjHeader, T>();
    NonNull::new_unchecked(self.ptr.as_ptr().add(offset) as *mut T)
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
  pub fn to_str(self) -> LyStr {
    unsafe { LyStr::from_alloc_ptr(self.ptr) }
  }

  #[inline]
  pub fn to_tuple(self) -> Tuple {
    unsafe { Tuple::from_alloc_ptr(self.ptr) }
  }

  #[inline]
  pub fn to_box(self) -> ObjRef<LyBox> {
    ObjRef {
      ptr: unsafe { self.data_ptr::<LyBox>() },
    }
  }

  #[inline]
  pub fn to_channel(self) -> ObjRef<Channel> {
    ObjRef {
      ptr: unsafe { self.data_ptr::<Channel>() },
    }
  }

  #[inline]
  pub fn to_class(self) -> ObjRef<Class> {
    ObjRef {
      ptr: unsafe { self.data_ptr::<Class>() },
    }
  }

  #[inline]
  pub fn to_closure(self) -> ObjRef<Closure> {
    ObjRef {
      ptr: unsafe { self.data_ptr::<Closure>() },
    }
  }

  #[inline]
  pub fn to_fun(self) -> ObjRef<Fun> {
    ObjRef {
      ptr: unsafe { self.data_ptr::<Fun>() },
    }
  }

  #[inline]
  pub fn to_instance(self) -> Instance {
    unsafe { Instance::from_alloc_ptr(self.ptr) }
  }

  #[inline]
  pub fn to_enumerator(self) -> ObjRef<Enumerator> {
    ObjRef {
      ptr: unsafe { self.data_ptr::<Enumerator>() },
    }
  }

  #[inline]
  pub fn to_list(self) -> List {
    unsafe { List::from_alloc_ptr(self.ptr) }
  }

  #[inline]
  pub fn to_map(self) -> ObjRef<Map<Value, Value>> {
    ObjRef {
      ptr: unsafe { self.data_ptr::<Map<Value, Value>>() },
    }
  }

  #[inline]
  pub fn to_method(self) -> ObjRef<Method> {
    ObjRef {
      ptr: unsafe { self.data_ptr::<Method>() },
    }
  }

  #[inline]
  pub fn to_native(self) -> ObjRef<Native> {
    ObjRef {
      ptr: unsafe { self.data_ptr::<Native>() },
    }
  }
}

impl fmt::Display for ObjectRef {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match_obj!((self) {
      ObjectKind::String(string) => write!(f, "{string}"),
      ObjectKind::Channel(channel) => write!(f, "{channel}"),
      ObjectKind::List(list) => write!(f, "{list}"),
      ObjectKind::Map(map) => write!(f, "{map}"),
      ObjectKind::Fun(fun) => write!(f, "{fun}"),
      ObjectKind::LyBox(ly_box) => write!(f, "{ly_box}"),
      ObjectKind::Closure(closure) => write!(f, "{closure}"),
      ObjectKind::Method(method) => write!(f, "{method}"),
      ObjectKind::Class(class) => write!(f, "{class}"),
      ObjectKind::Instance(instance) => write!(f, "{instance}"),
      ObjectKind::Enumerator(enumerator) => write!(f, "{enumerator}"),
      ObjectKind::Native(native) => write!(f, "{native}"),
      ObjectKind::Tuple(tuple) => write!(f, "{tuple}"),
    })
  }
}

impl fmt::Debug for ObjectRef {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match_obj!((self) {
      ObjectKind::String(string) => write!(f, "{string:?}"),
      ObjectKind::Channel(channel) => write!(f, "{channel:?}"),
      ObjectKind::List(list) => write!(f, "{list:?}"),
      ObjectKind::Map(map) => write!(f, "{map:?}"),
      ObjectKind::Fun(fun) => write!(f, "{fun:?}"),
      ObjectKind::LyBox(ly_box) => write!(f, "{ly_box:?}"),
      ObjectKind::Closure(closure) => write!(f, "{closure:?}"),
      ObjectKind::Method(method) => write!(f, "{method:?}"),
      ObjectKind::Class(class) => write!(f, "{class:?}"),
      ObjectKind::Instance(instance) => write!(f, "{instance:?}"),
      ObjectKind::Enumerator(enumerator) => write!(f, "{:?}", enumerator.name()),
      ObjectKind::Native(native) => write!(f, "{native:?}"),
      ObjectKind::Tuple(tuple) => write!(f, "{tuple:?}"),
    })
  }
}

impl fmt::Pointer for ObjectRef {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.ptr.fmt(f)
  }
}

impl Mark for ObjectRef {
  #[inline]
  fn mark(&self) -> bool {
    self.header().mark()
  }
}

impl Marked for ObjectRef {
  #[inline]
  fn marked(&self) -> bool {
    self.header().marked()
  }
}

impl Trace for ObjectRef {
  #[inline]
  fn trace(&self) {
    if self.marked() {
      return;
    }

    match_obj!((self) {
      ObjectKind::Channel(channel) => {
        channel.trace();
      },
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
      ObjectKind::LyBox(ly_box) => {
        ly_box.trace();
      },
      ObjectKind::Tuple(tuple) => {
        tuple.trace();
      },
    });
  }

  fn trace_debug(&self, log: &mut dyn Write) {
    if self.marked() {
      return;
    }

    macro_rules! trace_debug {
      ($obj:ident) => {
        $obj.trace_debug(log)
      };
    }

    match_obj!((self) {
      ObjectKind::Channel(channel) => {
        trace_debug!(channel);
      },
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
      ObjectKind::LyBox(ly_box) => {
        trace_debug!(ly_box);
      },
      ObjectKind::Tuple(tuple) => {
        trace_debug!(tuple);
      },
    });
  }
}

impl DebugHeap for ObjectRef {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    if depth == 0 {
      return f.write_fmt(format_args!("{:p}", self.ptr));
    }

    match_obj!((self) {
      ObjectKind::Channel(channel) => {
        channel.fmt_heap(f, depth)
      },
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
      ObjectKind::LyBox(ly_box) => {
        ly_box.fmt_heap(f, depth)
      },
      ObjectKind::Tuple(tuple) => {
        tuple.fmt_heap(f, depth)
      },
    })
  }
}

unsafe impl Send for ObjectRef {}
unsafe impl Sync for ObjectRef {}

fn array_len<H>(this: &ObjectHandle) -> usize {
  #[allow(clippy::cast_ptr_alignment)]
  let count = get_array_len_offset::<H>();
  unsafe { *(this.ptr.as_ptr().add(count) as *mut usize) }
}

fn list_capacity<H>(this: &ObjectHandle) -> usize {
  #[allow(clippy::cast_ptr_alignment)]
  let count = get_vector_cap_offset::<H>();
  unsafe { strip_msb(*(this.ptr.as_ptr().add(count) as *mut usize)) }
}

pub struct ObjectHandle {
  ptr: NonNull<u8>,
}

impl ObjectHandle {
  pub fn new(ptr: NonNull<u8>) -> Self {
    Self { ptr }
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
  fn value(&self) -> ObjectRef {
    ObjectRef { ptr: self.ptr }
  }

  #[inline]
  pub fn kind(&self) -> ObjectKind {
    self.header().kind()
  }

  #[inline]
  pub fn size(&self) -> usize {
    macro_rules! kind_size {
      ($o:ty) => {{
        make_obj_layout::<ObjHeader, $o>().size()
      }};
    }

    match self.kind() {
      ObjectKind::Channel => kind_size!(Channel),
      ObjectKind::List => {
        let cap: usize = list_capacity::<ObjHeader>(self);
        make_vector_layout::<ObjHeader, Value>(cap).size()
      },
      ObjectKind::Map => kind_size!(Map<Value, Value>),
      ObjectKind::Fun => kind_size!(Fun),
      ObjectKind::Closure => kind_size!(Closure),
      ObjectKind::Class => kind_size!(Class),
      ObjectKind::Instance => {
        let len = array_len::<InstanceHeader>(self);
        make_array_layout::<InstanceHeader, Value>(len).size()
      },
      ObjectKind::Enumerator => kind_size!(Enumerator),
      ObjectKind::Method => kind_size!(Method),
      ObjectKind::Native => kind_size!(Native),
      ObjectKind::LyBox => kind_size!(LyBox),
      ObjectKind::String => {
        let len = array_len::<ObjHeader>(self);
        make_array_layout::<ObjHeader, u8>(len).size()
      },
      ObjectKind::Tuple => {
        let len = array_len::<ObjHeader>(self);
        make_array_layout::<ObjHeader, Value>(len).size()
      },
    }
  }
}

impl Drop for ObjectHandle {
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
          dealloc(self.ptr.as_ptr(), make_obj_layout::<ObjHeader, $o>());
        }};
      }

      match kind {
        ObjectKind::List => {
          let cap = list_capacity::<ObjHeader>(self);
          let count = get_vector_offset::<ObjHeader, Value>();
          let data_ptr = self.ptr.as_ptr().add(count) as *mut Value;

          for i in 0..cap {
            ptr::read(data_ptr.add(i));
          }

          dealloc(
            self.ptr.as_ptr(),
            make_vector_layout::<ObjHeader, Value>(cap),
          );
        },
        ObjectKind::Map => drop_kind!(Map<Value, Value>),
        ObjectKind::Channel => drop_kind!(Channel),
        ObjectKind::Fun => drop_kind!(Fun),
        ObjectKind::Closure => drop_kind!(Closure),
        ObjectKind::Class => drop_kind!(Class),
        ObjectKind::Instance => {
          let len = array_len::<InstanceHeader>(self);
          let count = get_array_offset::<InstanceHeader, Value>();
          let data_ptr = self.ptr.as_ptr().add(count) as *mut Value;

          for i in 0..len {
            ptr::read(data_ptr.add(i));
          }

          dealloc(
            self.ptr.as_ptr(),
            make_array_layout::<InstanceHeader, Value>(len),
          );
        },
        ObjectKind::Enumerator => drop_kind!(Enumerator),
        ObjectKind::Method => drop_kind!(Method),
        ObjectKind::Native => drop_kind!(Native),
        ObjectKind::LyBox => drop_kind!(LyBox),
        ObjectKind::String => {
          let len = array_len::<ObjHeader>(self);

          dealloc(
            self.ptr.as_ptr(),
            make_array_layout::<ObjHeader, Value>(len),
          );
        },
        ObjectKind::Tuple => {
          let len = array_len::<ObjHeader>(self);

          let count = get_array_offset::<ObjHeader, Value>();
          let data_ptr = self.ptr.as_ptr().add(count) as *mut Value;

          for i in 0..len {
            ptr::read(data_ptr.add(i));
          }

          dealloc(
            self.ptr.as_ptr(),
            make_array_layout::<ObjHeader, Value>(len),
          );
        },
      }
    }
  }
}

impl Marked for ObjectHandle {
  #[inline]
  fn marked(&self) -> bool {
    self.header().marked()
  }
}

impl Unmark for ObjectHandle {
  #[inline]
  fn unmark(&self) -> bool {
    self.header().unmark()
  }
}

impl fmt::Pointer for ObjectHandle {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.ptr.fmt(f)
  }
}

impl DebugHeap for ObjectHandle {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    self.value().fmt_heap(f, depth)
  }
}

pub struct ObjectHandlerBuilder<T> {
  ptr: NonNull<u8>,

  /// Phantom data to hold the type parameter
  phantom: PhantomData<T>,
}

impl<T: 'static + Object> ObjectHandlerBuilder<T> {
  #[inline]
  pub fn value(&self) -> ObjRef<T> {
    ObjRef {
      ptr: unsafe { self.data_ptr::<T>() },
    }
  }
}

impl<T> ObjectHandlerBuilder<T> {
  unsafe fn data_ptr<U>(&self) -> NonNull<U> {
    let offset = get_offset::<ObjHeader, U>();
    NonNull::new_unchecked(self.ptr.as_ptr().add(offset) as *mut U)
  }

  #[inline]
  pub fn degrade(self) -> ObjectHandle {
    ObjectHandle { ptr: self.ptr }
  }

  #[inline]
  pub fn size(&self) -> usize {
    make_obj_layout::<ObjHeader, T>().size()
  }
}

impl<T: Object> From<T> for ObjectHandlerBuilder<T> {
  #[inline]
  fn from(item: T) -> Self {
    let new_layout = make_obj_layout::<ObjHeader, T>();
    let buf = unsafe { alloc(new_layout) };

    if buf.is_null() {
      handle_alloc_error(new_layout);
    }

    let header = ObjHeader::new(item.kind());

    #[allow(clippy::cast_ptr_alignment)]
    unsafe {
      ptr::write(buf as *mut ObjHeader, header);
      ptr::write(buf.add(get_offset::<ObjHeader, T>()) as *mut T, item);

      ObjectHandlerBuilder {
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
      let handle_builder = ObjectHandlerBuilder::from(Map::new());
      let gc_value = handle_builder.value();

      assert_eq!(gc_value.kind(), ObjectKind::Map);
    }

    #[test]
    pub fn degrade() {
      let handle_builder = ObjectHandlerBuilder::from(Map::new());
      let handle = handle_builder.degrade();

      assert_eq!(handle.kind(), ObjectKind::Map);
    }
  }

  mod gc_obj {
    use super::*;
    use crate::{
      val,
      value::{VALUE_FALSE, VALUE_TRUE},
    };

    #[test]
    fn deref() {
      let mut map = Map::new();
      map.insert(VALUE_FALSE, VALUE_FALSE);
      map.insert(VALUE_TRUE, VALUE_TRUE);

      let handle_builder = ObjectHandlerBuilder::from(map);
      let gc_obj = handle_builder.value();

      assert_eq!(gc_obj.get(&VALUE_FALSE), Some(&VALUE_FALSE));
      assert_eq!(gc_obj.get(&VALUE_TRUE), Some(&VALUE_TRUE));
    }

    #[test]
    fn deref_mut() {
      let mut map = Map::new();
      map.insert(VALUE_FALSE, VALUE_FALSE);
      map.insert(VALUE_TRUE, VALUE_TRUE);

      let handle_builder = ObjectHandlerBuilder::from(map);
      let mut gc_obj = handle_builder.value();

      gc_obj.insert(VALUE_TRUE, val!(1.0));

      assert_eq!(gc_obj.get(&VALUE_FALSE), Some(&VALUE_FALSE));
      assert_eq!(gc_obj.get(&VALUE_TRUE), Some(&val!(1.0)));
    }

    #[test]
    fn kind() {
      let handle_builder = ObjectHandlerBuilder::from(Map::new());
      let gc_obj = handle_builder.value();

      assert_eq!(gc_obj.kind(), ObjectKind::Map);
    }

    #[test]
    fn degrade() {
      let handle_builder = ObjectHandlerBuilder::from(Map::new());
      let gc_obj = handle_builder.value();
      let gc_object = gc_obj.degrade();

      assert_eq!(gc_object.kind(), ObjectKind::Map);
    }
  }

  mod gc_object {
    use crate::value::VALUE_NIL;

    use super::*;

    fn create_object<T: 'static + Object>(item: T) -> ObjectHandle {
      let handle_builder = ObjectHandlerBuilder::from(item);
      handle_builder.degrade()
    }

    #[test]
    fn drop_obj() {
      let handle_map = create_object(Map::<Value, Value>::new());
      let handle_box = create_object(LyBox::new(VALUE_NIL));

      drop(handle_map);
      drop(handle_box);
    }
  }
}
