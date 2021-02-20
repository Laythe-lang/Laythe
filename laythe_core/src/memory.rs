use crate::managed::{
  Allocation, Gc, GcObj, GcObjectHandle, GcObjectHandleBuilder, GcStr, GcStrHandle, Manage, Marked,
  Object, Trace, TraceRoot, Unmark,
};
use hashbrown::HashMap;
use laythe_env::stdio::Stdio;
use std::ptr::NonNull;
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
  heap: Vec<Box<Allocation<dyn Manage>>>,

  nursery_obj_heap: Vec<GcObjectHandle>,

  obj_heap: Vec<GcObjectHandle>,

  /// The heap specifically for strings
  str_heap: Vec<GcStrHandle>,

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

impl<'a> Allocator {
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
      str_heap: vec![],
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
  pub fn manage<T: 'static + Manage, C: TraceRoot + ?Sized>(
    &mut self,
    data: T,
    context: &C,
  ) -> Gc<T> {
    self.allocate(data, context)
  }

  /// Create a `Managed<T>` from the provided `data`. This method will allocate space
  /// for `data` and return a pointer to it. In case of a gc the provided `context` is
  /// used to annotate active roots
  ///
  /// # Examples
  /// ```
  /// use laythe_core::memory::{Allocator, NO_GC};
  /// use laythe_core::object::Upvalue;
  /// use laythe_core::value::Value;
  ///
  /// let mut gc = Allocator::default();
  /// let upvalue = gc.manage_obj(Upvalue::Closed(Value::from(10.0)), &NO_GC);
  ///
  /// assert_eq!(upvalue.value(&[]), Value::from(10.0));
  /// ```
  pub fn manage_obj<T: 'static + Object, C: TraceRoot + ?Sized>(
    &mut self,
    data: T,
    context: &C,
  ) -> GcObj<T> {
    self.allocate_obj(data, context)
  }

  /// Create a `Managed<String>` from a str slice. This creates
  /// or returns an interned string and allocates a pointer to the intern
  /// cache. A Managed<String> can be created from `.manage` but will
  /// not intern the string.
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
  pub fn manage_str<S: AsRef<str>, C: TraceRoot + ?Sized>(&mut self, src: S, context: &C) -> GcStr {
    let string = src.as_ref();
    if let Some(cached) = self.intern_cache.get(string) {
      return *cached;
    }

    let managed = self.allocate_str(string, context);
    let static_str: &'static str = unsafe { &*(&*managed as *const str) };
    self.intern_cache.insert(&static_str, managed);
    managed
  }

  /// track events that may grow the size of the heap. If
  /// a heap grows beyond the current threshold will trigger a gc
  pub fn grow<T: 'static + Manage, R, F: FnOnce(&mut T) -> R, C: TraceRoot + ?Sized>(
    &mut self,
    managed: &mut T,
    context: &C,
    action: F,
  ) -> R {
    let before = managed.size();
    let result = action(managed);
    let after = managed.size();

    // get the size delta before and after the action
    // this would occur because of some resize
    self.bytes_allocated += after - before;

    // collect if need be
    #[cfg(feature = "debug_stress_gc")]
    {
      self.collect_garbage(context);
    }

    if self.bytes_allocated > self.next_gc {
      self.collect_garbage(context);
    }

    result
  }

  /// track events that may shrink the size of the heap.
  pub fn shrink<T: 'static + Manage, R, F: FnOnce(&mut T) -> R>(
    &mut self,
    managed: &mut T,
    action: F,
  ) -> R {
    let before = managed.size();
    let result = action(managed);
    let after = managed.size();

    // get the size delta before and after the action
    // this would occur because of some resize
    self.bytes_allocated += before - after;

    result
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
  fn allocate<T: 'static + Manage, C: TraceRoot + ?Sized>(
    &mut self,
    data: T,
    context: &C,
  ) -> Gc<T> {
    // create own store of allocation
    let mut alloc = Box::new(Allocation::new(data));
    let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };

    let size = alloc.size();

    // push onto heap
    self.bytes_allocated += size;
    self.heap.push(alloc);

    let managed = Gc::from(ptr);

    #[cfg(feature = "gc_log_alloc")]
    self.debug_allocate(ptr, size);

    #[cfg(feature = "gc_stress")]
    {
      self.push_root(managed);
      self.collect_garbage(context);
      self.pop_roots(1)
    }

    if self.bytes_allocated > self.next_gc {
      self.push_root(managed);
      self.collect_garbage(context);
      self.pop_roots(1)
    }

    managed
  }

  fn allocate_obj<T: 'static + Object, C: TraceRoot + ?Sized>(
    &mut self,
    data: T,
    context: &C,
  ) -> GcObj<T> {
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
    {
      self.push_root(obj);
      self.collect_garbage(context);
      self.pop_roots(1)
    }

    if self.bytes_allocated > self.next_gc {
      self.push_root(obj);
      self.collect_garbage(context);
      self.pop_roots(1)
    }

    obj
  }

  /// Allocate a strong on the gc's heap. If conditions are met a garbage
  /// collection can be trigger. When trigger the context is used to determine
  /// the current live roots.
  fn allocate_str<C: TraceRoot + ?Sized>(&mut self, string: &str, context: &C) -> GcStr {
    // create own store of allocation
    let gc_string_handle = GcStrHandle::from(string);
    let size = gc_string_handle.size();

    let gc_string = gc_string_handle.value();

    // push onto heap
    self.bytes_allocated += size;
    self.str_heap.push(gc_string_handle);

    #[cfg(feature = "gc_log_alloc")]
    self.debug_allocate_str(gc_string, size);

    #[cfg(feature = "gc_stress")]
    {
      self.push_root(gc_string);
      self.collect_garbage(context);
      self.pop_roots(1)
    }

    if self.bytes_allocated > self.next_gc {
      self.push_root(gc_string);
      self.collect_garbage(context);
      self.pop_roots(1)
    }

    gc_string
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

      let string_heap_size = self.sweep_string_heap();
      let obj_heap_size = self.sweep_obj_heap();
      let heap_size = self.sweep_heap();

      self.bytes_allocated = string_heap_size + heap_size + obj_heap_size;
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
      #[allow(clippy::let_and_return)]
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
  fn sweep_string_heap(&mut self) -> usize {
    self.intern_cache.retain(|_, &mut string| string.marked());

    let mut remaining: usize = 0;

    self.str_heap.retain(|string| {
      #[allow(clippy::let_and_return)]
      let retain = string.unmark();

      #[cfg(feature = "gc_log_free")]
      debug_string_remove(string, !retain);

      if retain {
        remaining += string.size();
      }

      retain
    });

    remaining
  }

  /// Debug logging for allocating an object.
  #[cfg(feature = "gc_log_alloc")]
  fn debug_allocate<T: 'static + Manage>(&self, ptr: NonNull<Allocation<T>>, size: usize) {
    let mut stdio = self.stdio.borrow_mut();
    let stdout = stdio.stdout();

    writeln!(
      stdout,
      "{:p} allocated {} bytes for {:?}",
      ptr.as_ptr(),
      size,
      DebugWrap(unsafe { ptr.as_ref() }, 1)
    )
    .expect("unable to write to stdout");
  }

  /// Debug logging for allocating an object.
  #[cfg(feature = "gc_log_alloc")]
  fn debug_allocate_obj<T: 'static + Object>(&self, obj: GcObj<T>, size: usize) {
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
}

/// Debug logging for removing a string from the cache.
#[cfg(feature = "gc_log_free")]
fn debug_string_remove(string: &GcStrHandle, free: bool) {
  if free {
    println!(
      "{:p} remove string from cache {:?}",
      string,
      DebugWrap(string, 1)
    )
  }
}

/// Debug logging for free an object.
#[cfg(feature = "gc_log_free")]
fn debug_free(obj: &Box<Allocation<dyn Manage>>, free: bool) {
  if free {
    println!(
      "{:p} free {} bytes from {:?}",
      &**obj,
      obj.size(),
      DebugWrapDyn((*obj).as_debug(), 0)
    )
  }
}

/// Debug logging for free an object.
#[cfg(feature = "gc_log_free")]
fn debug_free_obj(obj: &GcObjectHandle, free: bool) {
  if free {
    println!(
      "{:p} free {} bytes from {:?}",
      obj,
      obj.size(),
      DebugWrapDyn(obj, 1)
    )
  }
}

impl<'a> Default for Allocator {
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
