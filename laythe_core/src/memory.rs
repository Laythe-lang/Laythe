use crate::managed::{
  instance_handle, tuple_handle, Allocate, DebugHeapRef, GcObj, GcObjectHandle,
  GcObjectHandleBuilder, GcStr, GcStrHandle, Instance, Manage, Marked, Object, Trace, TraceRoot,
  Tuple, Unmark,
};
use crate::object::Class;
use crate::value::Value;
use hashbrown::HashMap;
use laythe_env::stdio::Stdio;
use std::{cell::RefCell, io::Write};

#[cfg(any(feature = "gc_log_free", feature = "gc_log_alloc"))]
use crate::managed::DebugWrap;

#[cfg(feature = "gc_log_free")]
use crate::managed::DebugWrapDyn;

/// The garbage collector and memory manager for laythe. Currently this is implemented a very crude
/// generation mark and sweep collector. As of now the key areas for improvements are better allocation
/// strategy and better tuning of the interaction between the nursery and regular heap
pub struct Allocator {
  /// Io in the given environment
  #[allow(dead_code)]
  stdio: RefCell<Stdio>,

  /// The regular heap where objects that have survived a gc reside
  heap: Vec<Box<dyn Manage>>,

  /// The nursery obj heap for objects
  nursery_obj_heap: Vec<GcObjectHandle>,

  /// The regular object heap for objects
  obj_heap: Vec<GcObjectHandle>,

  /// A collection of temporary roots in the gc
  temp_roots: Vec<Box<dyn Trace>>,

  /// The total byte allocated in both heaps
  bytes_allocated: usize,

  /// The intern string cache
  intern_cache: HashMap<&'static str, GcStr>,

  /// The size in bytes of the gc before the next collection
  next_gc: usize,

  /// The total number of garbage collections that have occured
  gc_count: u128,
}

const GC_HEAP_GROW_FACTOR: usize = 2;

impl Allocator {
  /// Create a new manged heap for laythe for objects.
  ///
  /// # Examples
  /// ```
  /// use laythe_core::memory::Allocator;
  /// use laythe_env::stdio::Stdio;
  ///
  /// let gc = Allocator::new(Stdio::default());
  /// ```
  pub fn new(stdio: Stdio) -> Self {
    Self {
      stdio: RefCell::new(stdio),
      heap: vec![],
      obj_heap: vec![],
      nursery_obj_heap: vec![],
      bytes_allocated: 0,
      temp_roots: vec![],
      intern_cache: HashMap::new(),
      next_gc: 1024 * 1024,
      gc_count: 0,
    }
  }

  /// Get the number of bytes allocated
  pub fn allocated(&self) -> usize {
    self.bytes_allocated
  }

  /// How many temporary roots are present
  pub fn temp_roots(&self) -> usize {
    self.temp_roots.len()
  }

  /// Create a `Managed<T>` from the provided `data`. This method will allocate space
  /// for `data` and return a pointer to it. In case of a gc the provided `context` is
  /// used to annotate active roots
  pub fn manage<R, T, C>(&mut self, data: T, context: &C) -> R
  where
    R: 'static + Trace + Copy + DebugHeapRef,
    T: Allocate<R>,
    C: TraceRoot + ?Sized,
  {
    self.allocate(data, context)
  }

  /// Create a `Managed<T>` from the provided `data`. This method will allocate space
  /// for `data` and return a pointer to it. In case of a gc the provided `context` is
  /// used to annotate active roots
  ///
  /// # Examples
  /// ```
  /// use laythe_core::memory::{Allocator, NO_GC};
  /// use laythe_core::object::LyBox;
  /// use laythe_core::value::Value;
  ///
  /// let mut gc = Allocator::default();
  /// let ly_box = gc.manage_obj(LyBox::new(Value::from(10.0)), &NO_GC);
  ///
  /// assert_eq!(ly_box.value, Value::from(10.0));
  /// ```
  pub fn manage_obj<T, C>(&mut self, data: T, context: &C) -> GcObj<T>
  where
    T: Object,
    C: TraceRoot + ?Sized,
  {
    self.allocate_obj(data, context)
  }

  /// Create a `GcStr` from a str slice. This creates
  /// or returns an interned string and allocates a pointer to the intern
  /// cache.
  ///
  /// # Examples
  /// ```
  /// use laythe_core::memory::{Allocator, NO_GC};
  ///
  /// let mut gc = Allocator::default();
  /// let str = gc.manage_str("hi!", &NO_GC);
  ///
  /// assert_eq!(&*str, "hi!");
  /// ```
  pub fn manage_str<S, C>(&mut self, src: S, context: &C) -> GcStr
  where
    S: AsRef<str>,
    C: TraceRoot + ?Sized,
  {
    let string = src.as_ref();
    if let Some(cached) = self.intern_cache.get(string) {
      return *cached;
    }

    let managed = self.allocate_str(string, context);
    let static_str: &'static str = unsafe { &*(&*managed as *const str) };
    self.intern_cache.insert(static_str, managed);
    managed
  }

  /// Create a `Tuple` from a slice. This
  /// allocates a new tuple and copies the slice into it
  ///
  /// # Examples
  /// ```
  /// use laythe_core::{
  ///   memory::{Allocator, NO_GC},
  ///   val,
  ///   value::Value,
  /// };
  ///
  /// let mut gc = Allocator::default();
  /// let tuple = gc.manage_tuple(&[val!(false), val!(1.0)], &NO_GC);
  ///
  /// assert_eq!(tuple.len(), 2);
  /// assert_eq!(tuple[0], val!(false));
  /// assert_eq!(tuple[1], val!(1.0));
  /// ```
  pub fn manage_tuple<C: TraceRoot + ?Sized>(&mut self, slice: &[Value], context: &C) -> Tuple {
    self.allocate_tuple(slice, context)
  }

  /// Create a `Instance` from a class. This
  /// allocates a new instance with all nil fields
  ///
  /// # Examples
  /// ```
  /// use laythe_core::{
  ///   memory::{Allocator, NO_GC},
  ///   val,
  ///   object::Class,
  ///   value::{Value, VALUE_NIL},
  /// };
  ///
  /// let mut gc = Allocator::default();
  /// let name = gc.manage_str("someClass", &NO_GC);
  /// let field = gc.manage_str("someField", &NO_GC);
  ///
  /// let mut class = gc.manage_obj(Class::bare(name), &NO_GC);
  /// class.add_field(field);
  ///
  /// let instance = gc.manage_instance(class, &NO_GC);
  ///
  /// assert_eq!(instance.len(), 1);
  /// assert_eq!(instance[0], VALUE_NIL);
  /// ```
  pub fn manage_instance<C: TraceRoot + ?Sized>(
    &mut self,
    class: GcObj<Class>,
    context: &C,
  ) -> Instance {
    self.allocate_instance(class, context)
  }

  /// Checks if a string is in the gc's intern cache.
  ///
  /// # Examples
  /// ```
  /// use laythe_core::memory::{Allocator, NO_GC};
  ///
  /// let mut gc = Allocator::default();
  /// gc.manage_str("hi!", &NO_GC);
  ///
  /// assert!(gc.has_str("hi!").is_some());
  /// ```
  pub fn has_str<S: AsRef<str>>(&self, src: S) -> Option<GcStr> {
    let string = src.as_ref();
    self.intern_cache.get(string).copied()
  }

  /// Push a new temporary root onto the gc to avoid collection
  pub fn push_root<T: 'static + Trace>(&mut self, managed: T) {
    self.temp_roots.push(Box::new(managed));
  }

  /// Pop a temporary root again allowing gc to occur normally
  pub fn pop_roots(&mut self, count: usize) {
    debug_assert!(
      count <= self.temp_roots.len(),
      "Attempted to pop more temp roots than are present. Currently there are {} roots and tried to pop {}.",
      count,
      self.temp_roots.len()
    );
    self.temp_roots.truncate(self.temp_roots.len() - count);
  }

  /// Allocate `data` on the gc's heap. If conditions are met
  /// a garbage collection can be triggered. When triggered will use the
  /// context to determine the active roots.
  fn allocate<R, T, C>(&mut self, data: T, context: &C) -> R
  where
    R: 'static + Trace + Copy + DebugHeapRef,
    T: Allocate<R>,
    C: TraceRoot + ?Sized,
  {
    // create own store of allocation
    let result = data.alloc();
    let handle = result.handle;
    let reference = result.reference;

    let size = handle.size();

    // push onto heap
    self.bytes_allocated += size;
    self.heap.push(handle);

    #[cfg(feature = "gc_log_alloc")]
    self.debug_allocate(reference, size);

    #[cfg(feature = "gc_stress")]
    self.collect_garbage_with_value(context, reference);

    if self.bytes_allocated > self.next_gc {
      self.collect_garbage_with_value(context, reference);
    }

    reference
  }

  fn allocate_obj<T, C>(&mut self, data: T, context: &C) -> GcObj<T>
  where
    T: Object,
    C: TraceRoot + ?Sized,
  {
    // create own store of allocation
    let object_handle = GcObjectHandleBuilder::from(data);
    let size = object_handle.size();

    let obj = object_handle.value();

    // push onto heap
    self.bytes_allocated += size;
    self.nursery_obj_heap.push(object_handle.degrade());

    #[cfg(feature = "gc_log_alloc")]
    self.debug_allocate_obj(obj, size);

    #[cfg(feature = "gc_stress")]
    self.collect_garbage_with_value(context, obj);

    if self.bytes_allocated > self.next_gc {
      self.collect_garbage_with_value(context, obj);
    }

    obj
  }

  /// Allocate a string on the gc's heap. If conditions are met a garbage
  /// collection can be trigger. When trigger the context is used to determine
  /// the current live roots.
  fn allocate_str<C: TraceRoot + ?Sized>(&mut self, string: &str, context: &C) -> GcStr {
    // create own store of allocation
    let gc_string_handle = GcStrHandle::from(string);
    let size = gc_string_handle.size();

    let gc_string = gc_string_handle.value();

    // push onto heap
    self.bytes_allocated += size;
    self.nursery_obj_heap.push(gc_string_handle.degrade());

    #[cfg(feature = "gc_log_alloc")]
    self.debug_allocate_str(gc_string, size);

    #[cfg(feature = "gc_stress")]
    self.collect_garbage_with_value(context, gc_string);

    if self.bytes_allocated > self.next_gc {
      self.collect_garbage_with_value(context, gc_string);
    }

    gc_string
  }

  /// Allocate a tuple on the gc's heap. If conditions are met a garbage
  /// collection can be trigger. When trigger the context is used to determine
  /// the current live roots.
  fn allocate_tuple<C: TraceRoot + ?Sized>(&mut self, slice: &[Value], context: &C) -> Tuple {
    // create own store of allocation
    let gc_tuple_handle = tuple_handle(slice);
    let size = gc_tuple_handle.size();

    let tuple = gc_tuple_handle.value();

    // push onto heap
    self.bytes_allocated += size;
    self.obj_heap.push(gc_tuple_handle.degrade());

    #[cfg(feature = "gc_log_alloc")]
    self.debug_allocate_tuple(tuple, size);

    #[cfg(feature = "gc_stress")]
    self.collect_garbage_with_value(context, tuple);

    if self.bytes_allocated > self.next_gc {
      self.collect_garbage_with_value(context, tuple);
    }

    tuple
  }

  /// Allocate a instance on the gc's heap. If conditions are met a garbage
  /// collection can be trigger. When trigger the context is used to determine
  /// the current live roots.
  fn allocate_instance<C: TraceRoot + ?Sized>(
    &mut self,
    class: GcObj<Class>,
    context: &C,
  ) -> Instance {
    // create own store of allocation
    let gc_instance_handle = instance_handle(class);
    let size = gc_instance_handle.size();

    let tuple = gc_instance_handle.value();

    // push onto heap
    self.bytes_allocated += size;
    self.obj_heap.push(gc_instance_handle.degrade());

    #[cfg(feature = "gc_log_alloc")]
    self.debug_allocate_tuple(tuple, size);

    #[cfg(feature = "gc_stress")]
    self.collect_garbage_with_value(context, tuple);

    if self.bytes_allocated > self.next_gc {
      self.collect_garbage_with_value(context, tuple);
    }

    tuple
  }

  /// Collect garbage present in the heap for unreach objects. Use the provided context
  /// to mark a set of initial roots into the heap. Also temporarily root a value provided
  fn collect_garbage_with_value<C: TraceRoot + ?Sized, T: 'static + Trace>(
    &mut self,
    context: &C,
    item: T,
  ) {
    self.push_root(item);
    self.collect_garbage(context);
    self.pop_roots(1)
  }

  /// Collect garbage present in the heap for unreachable objects. Use the provided context
  /// to mark a set of initial roots into the vm.
  fn collect_garbage<C: TraceRoot + ?Sized>(&mut self, context: &C) {
    #[cfg(any(
      feature = "gc_log_mark",
      feature = "gc_log_free",
      feature = "gc_log_alloc"
    ))]
    let before = self.bytes_allocated;
    self.gc_count += 1;

    #[cfg(any(
      feature = "gc_log_mark",
      feature = "gc_log_free",
      feature = "gc_log_alloc"
    ))]
    {
      let mut stdio = self.stdio.borrow_mut();
      let stdout = stdio.stdout();
      writeln!(stdout, "-- gc begin {} --", self.gc_count).expect("could not write to stdout");
    }

    if context.can_collect() {
      self.trace_root(context);
      self.temp_roots.iter().for_each(|root| {
        self.trace(&**root);
      });

      self.sweep_intern_cache();
      let obj_heap_size = self.sweep_obj_heap();
      let heap_size = self.sweep_heap();

      self.bytes_allocated = heap_size + obj_heap_size;
      self.next_gc = self.bytes_allocated * GC_HEAP_GROW_FACTOR
    }

    #[cfg(any(
      feature = "gc_log_mark",
      feature = "gc_log_free",
      feature = "gc_log_alloc"
    ))]
    {
      let mut stdio = self.stdio.borrow_mut();
      let stdout = stdio.stdout();
      let now = self.bytes_allocated;

      writeln!(stdout, "-- gc end --").expect("unable to write to stdout");
      debug_assert!(
        before >= now,
        "Heap was incorrectly calculated before: {} now {}",
        before,
        now
      );

      writeln!(
        stdout,
        "   collected {} bytes (from {} to {}) next at {}",
        before.saturating_sub(now),
        before,
        now,
        self.next_gc
      )
      .expect("unable to write to stdout");
    }
  }

  /// wrapper around a roots trace method to select either normal
  /// or debug trace at compile time.
  fn trace_root<C: TraceRoot + ?Sized>(&self, context: &C) {
    #[cfg(not(feature = "gc_log_mark"))]
    return context.trace();

    #[cfg(feature = "gc_log_mark")]
    {
      let mut stdio = self.stdio.borrow_mut();
      let stdout = stdio.stdout();

      return context.trace_debug(stdout);
    }
  }

  /// wrapper around an entities trace method to select either normal
  /// or debug trace at compile time.
  fn trace(&self, entity: &dyn Trace) {
    #[cfg(not(feature = "gc_log_mark"))]
    entity.trace();

    #[cfg(feature = "gc_log_mark")]
    {
      let mut stdio = self.stdio.borrow_mut();
      let stdout = stdio.stdout();
      entity.trace_debug(stdout);
    }
  }

  /// Remove unmarked objects from the heap. This calculates the remaining
  /// memory present in the heap
  fn sweep_obj_heap(&mut self) -> usize {
    #[cfg(feature = "gc_stress")]
    return self.sweep_obj_full();

    #[cfg(not(feature = "gc_stress"))]
    if self.gc_count % 10 == 0 {
      self.sweep_obj_full()
    } else {
      self.sweep_obj_nursery()
    }
  }

  /// Remove unmarked objects from the nursery heap. Promoting surviving objects
  /// to the normal heap
  #[cfg(not(feature = "gc_stress"))]
  fn sweep_obj_nursery(&mut self) -> usize {
    let mut remaining: usize = 0;

    self.obj_heap.iter().for_each(|obj| {
      (*obj).unmark();
      remaining += obj.size();
    });

    self
      .obj_heap
      .extend(self.nursery_obj_heap.drain(..).filter(|obj| {
        let retain = (*obj).unmark();

        #[cfg(feature = "gc_log_free")]
        debug_free_obj(&obj, !retain);

        if retain {
          remaining += obj.size();
        }

        retain
      }));

    remaining
  }

  /// Remove unmarked objects from the both heaps. Promoting surviving objects
  /// to the normal heap
  fn sweep_obj_full(&mut self) -> usize {
    let mut remaining: usize = 0;

    self.obj_heap.retain(|obj| {
      let retain = (*obj).unmark();

      #[cfg(feature = "gc_log_free")]
      debug_free_obj(&obj, !retain);

      if retain {
        remaining += obj.size();
      }

      retain
    });

    self
      .obj_heap
      .extend(self.nursery_obj_heap.drain(..).filter(|obj| {
        let retain = (*obj).unmark();

        #[cfg(feature = "gc_log_free")]
        debug_free_obj(&obj, !retain);

        if retain {
          remaining += obj.size();
        }

        retain
      }));

    remaining
  }

  /// Remove unmarked objects from the heap. This calculates the remaining
  /// memory present in the heap
  fn sweep_heap(&mut self) -> usize {
    let mut remaining: usize = 0;

    self.heap.retain(|item| {
      let retain = item.unmark();

      #[cfg(feature = "gc_log_free")]
      debug_free(item, !retain);

      if retain {
        remaining += item.size();
      }

      retain
    });

    remaining
  }

  /// Remove strings from the cache that no longer have any references
  /// in the heap
  fn sweep_intern_cache(&mut self) {
    self.intern_cache.retain(|_, &mut string| string.marked());
  }

  /// Debug logging for allocating an object.
  #[cfg(feature = "gc_log_alloc")]
  fn debug_allocate<R: DebugHeapRef>(&self, reference: R, size: usize) {
    let mut stdio = self.stdio.borrow_mut();
    let stdout = stdio.stdout();

    writeln!(
      stdout,
      "{:p} allocated {} bytes for {:?}",
      reference,
      size,
      DebugWrap(&reference, 1)
    )
    .expect("unable to write to stdout");
  }

  /// Debug logging for allocating an object.
  #[cfg(feature = "gc_log_alloc")]
  fn debug_allocate_obj<T: Object>(&self, obj: GcObj<T>, size: usize) {
    let mut stdio = self.stdio.borrow_mut();
    let stdout = stdio.stdout();

    writeln!(
      stdout,
      "{:p} allocated {} bytes for {:?}",
      obj,
      size,
      DebugWrap(&obj, 1)
    )
    .expect("unable to write to stdout");
  }

  /// Debug logging for allocating an object.
  #[cfg(feature = "gc_log_alloc")]
  fn debug_allocate_str(&self, string: GcStr, size: usize) {
    let mut stdio = self.stdio.borrow_mut();
    let stdout = stdio.stdout();

    writeln!(
      stdout,
      "{:p} allocated {} bytes for {}",
      string, size, string,
    )
    .expect("unable to write to stdout");
  }

  /// Debug logging for allocating an object.
  #[cfg(feature = "gc_log_alloc")]
  fn debug_allocate_tuple(&self, tuple: Tuple, size: usize) {
    let mut stdio = self.stdio.borrow_mut();
    let stdout = stdio.stdout();

    writeln!(
      stdout,
      "{:p} allocated {} bytes for {:?}",
      tuple,
      size,
      DebugWrap(&tuple, 1),
    )
    .expect("unable to write to stdout");
  }
}

/// Debug logging for free an object.
#[cfg(feature = "gc_log_free")]
fn debug_free(obj: &Box<dyn Manage>, free: bool) {
  if free {
    println!(
      "{:p} free {} bytes from {:?}",
      obj.loc(),
      obj.size(),
      DebugWrapDyn((*obj).as_debug(), 1)
    )
  }
}

/// Debug logging for free an object.
#[cfg(feature = "gc_log_free")]
fn debug_free_obj(obj: &GcObjectHandle, free: bool) {
  if free {
    println!(
      "{:p} free {} bytes from {:?}",
      *obj,
      obj.size(),
      DebugWrapDyn(obj, 1)
    )
  }
}

impl Default for Allocator {
  fn default() -> Self {
    Allocator::new(Stdio::default())
  }
}
pub struct NoGc();

impl TraceRoot for NoGc {
  fn trace(&self) {}

  fn trace_debug(&self, _: &mut dyn Write) {}

  fn can_collect(&self) -> bool {
    false
  }
}

pub static NO_GC: NoGc = NoGc();
