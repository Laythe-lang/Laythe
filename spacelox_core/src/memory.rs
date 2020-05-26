use crate::io::{NativeStdIo, StdIo};
use crate::managed::{Allocation, Manage, Managed, Trace};
use hashbrown::HashMap;
use std::cell::{Cell, RefCell};
use std::fmt;
use std::ptr::NonNull;

/// The garbage collector and memory manager for spacelox. Currently this is implemented a very crude
/// generation mark and sweep collector. As of now the key areas for improvements are better allocation
/// strategy and better tuning of the interaction between the nursery and regular heap
pub struct Gc {
  /// Io in the given environment
  #[allow(dead_code)]
  stdio: Box<dyn StdIo>,

  /// The nursery heap for new objects initially allocated into this gc
  nursery_heap: RefCell<Vec<Box<Allocation<dyn Manage>>>>,

  /// The regular heap where objects that have survived a gc reside
  heap: RefCell<Vec<Box<Allocation<dyn Manage>>>>,

  /// A collection of temporary roots in the gc
  temp_roots: RefCell<Vec<Box<dyn Trace>>>,

  /// The total byte allocated in both heaps
  bytes_allocated: Cell<usize>,

  /// The intern string cache
  intern_cache: RefCell<HashMap<&'static str, Managed<String>>>,

  /// The size in bytes of the gc before the next collection
  next_gc: Cell<usize>,

  /// The total number of garbage collections that have occured
  gc_count: Cell<u128>,
}

const GC_HEAP_GROW_FACTOR: usize = 2;

impl<'a> Gc {
  /// Create a new manged heap for spacelox for objects.
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::memory::Gc;
  /// use spacelox_core::io::NativeStdIo;
  ///
  /// let gc = Gc::new(Box::new(NativeStdIo::new()));
  /// ```
  pub fn new(stdio: Box<dyn StdIo>) -> Self {
    Gc {
      stdio,
      nursery_heap: RefCell::new(Vec::with_capacity(1000)),
      heap: RefCell::new(Vec::with_capacity(0)),
      bytes_allocated: Cell::new(0),
      temp_roots: RefCell::new(Vec::new()),
      intern_cache: RefCell::new(HashMap::new()),
      next_gc: Cell::new(1024 * 1024),
      gc_count: Cell::new(0),
    }
  }

  pub fn allocated(&self) -> usize {
    self.bytes_allocated.get()
  }

  /// Create a `Managed<T>` from the provided `data`. This method will allocate space
  /// for `data` and return a pointer to it. In case of a gc the provided `context` is
  /// used to annotate active roots
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::memory::{Gc, NO_GC};
  /// use spacelox_core::module::Module;
  /// use spacelox_core::object::Fun;
  /// use spacelox_core::io::NativeStdIo;
  /// use spacelox_core::managed::Managed;
  ///
  /// let gc = Gc::new(Box::new(NativeStdIo::new()));
  /// let module = gc.manage(Module::new(gc.manage_str("module".to_string(), &NO_GC)), &NO_GC);
  /// let fun: Fun = Fun::new(gc.manage_str("fun".to_string(), &NO_GC), module);
  ///
  /// let managed_fun = gc.manage(fun, &NO_GC);
  ///
  /// assert_eq!(&*managed_fun.name, "fun");
  /// ```
  pub fn manage<T: 'static + Manage, C: Trace + ?Sized>(&self, data: T, context: &C) -> Managed<T> {
    self.allocate(data, context)
  }

  /// Create a `Managed<String>` from a str slice. This creates
  /// or returns an interned string and allocates a pointer to the intern
  /// cache. A Managed<String> can be created from `.manage` but will
  /// not intern the string.
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::memory::{Gc, NO_GC};
  /// use spacelox_core::value::Value;
  /// use spacelox_core::managed::Managed;
  /// use spacelox_core::io::NativeStdIo;
  /// use std::ptr;
  ///
  /// let gc = Gc::new(Box::new(NativeStdIo::new()));
  /// let str = gc.manage_str("hi!".to_string(), &NO_GC);
  ///
  /// assert_eq!(&*str, "hi!");
  /// ```
  pub fn manage_str<C: Trace + ?Sized>(&self, string: String, context: &C) -> Managed<String> {
    if let Some(cached) = self.intern_cache.borrow_mut().get(&*string) {
      return *cached;
    }

    let managed = self.allocate(string, context);
    let static_str: &'static str = unsafe { &*(&**managed as *const str) };
    self.intern_cache.borrow_mut().insert(&static_str, managed);
    managed
  }

  /// clone the the `Managed` data as a new heap allocation.
  /// A `Managed` clone will simply create a new pointer to the data.
  /// In case of a gc the `context` is used to annotate roots
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::memory::{Gc, NO_GC};
  /// use spacelox_core::value::{Value, VALUE_NIL};
  /// use spacelox_core::object::Upvalue;
  /// use spacelox_core::managed::Managed;
  /// use spacelox_core::io::NativeStdIo;
  /// use std::ptr;
  ///
  /// let gc = Gc::new(Box::new(NativeStdIo::new()));
  /// let value = VALUE_NIL;
  ///
  /// let up1: Managed<Upvalue> = gc.manage(Upvalue::Open(ptr::NonNull::from(&value)), &NO_GC);
  /// let up2 = gc.clone_managed(up1, &NO_GC);
  ///
  /// assert!(!ptr::eq(&*up1, &*up2));
  /// match (&*up1, &*up2) {
  ///   (Upvalue::Open(o1), Upvalue::Open(o2)) => assert_eq!(o1, o2),
  ///   _ => panic!("No equal!"),
  /// }
  /// ```
  pub fn clone_managed<T: 'static + Manage + Clone, C: Trace + ?Sized>(
    &self,
    managed: Managed<T>,
    context: &C,
  ) -> Managed<T> {
    let cloned = (*managed).clone();
    self.allocate(cloned, context)
  }

  /// track events that may grow the size of the heap. If
  /// a heap grows beyond the current threshold will trigger a gc
  pub fn grow<T: 'static + Manage, R, F: Fn(&mut T) -> R, C: Trace + ?Sized>(
    &self,
    managed: &mut T,
    context: &C,
    action: F,
  ) -> R {
    let before = managed.size();
    let result = action(managed);
    let after = managed.size();

    // get the size delta before and after the action
    // this would occur because of some resize
    let delta = after - before;

    let allocated = self
      .bytes_allocated
      .replace(self.bytes_allocated.get() + delta);

    // collect if need be
    #[cfg(feature = "debug_stress_gc")]
    {
      self.collect_garbage::<T, C>(context, None);
    }

    if allocated + delta > self.next_gc.get() {
      self.collect_garbage::<T, C>(context, None);
    }

    result
  }

  /// track events that may shrink the size of the heap.
  pub fn shrink<T: 'static + Manage, R, F: Fn(&mut T) -> R>(
    &self,
    managed: &mut T,
    action: F,
  ) -> R {
    let before = managed.size();
    let result = action(managed);
    let after = managed.size();

    // get the size delta before and after the action
    // this would occur because of some resize
    let delta = before - after;

    self
      .bytes_allocated
      .replace(self.bytes_allocated.get() - delta);

    result
  }

  /// Push a new temporary root onto the gc to avoid collection
  pub fn push_root<T: 'static + Manage>(&self, managed: T) {
    self.temp_roots.borrow_mut().push(Box::new(managed));
  }

  /// Pop a temporary root again allowing gc to occur normally
  pub fn pop_roots(&self, count: usize) {
    let mut temp_roots = self.temp_roots.borrow_mut();
    let len = temp_roots.len();

    temp_roots.truncate(len - count);
  }

  /// Allocate `data` on the gc's heap. If conditions are met
  /// a garbage collection can be triggered. When triggered will use the
  /// context to determine the active roots.
  fn allocate<T: 'static + Manage, C: Trace + ?Sized>(&self, data: T, context: &C) -> Managed<T> {
    // create own store of allocation
    let mut alloc = Box::new(Allocation::new(data));
    let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };

    // push onto heap
    let size = alloc.size();
    let allocated = self
      .bytes_allocated
      .replace(self.bytes_allocated.get() + size);
    self.nursery_heap.borrow_mut().push(alloc);

    let managed = Managed::from(ptr);

    #[cfg(feature = "debug_stress_gc")]
    {
      self.collect_garbage(context, Some(managed));
    }

    if allocated + size > self.next_gc.get() {
      self.collect_garbage(context, Some(managed));
    }

    #[cfg(feature = "debug_gc")]
    self.debug_allocate(ptr, size);

    managed
  }

  /// Collect garbage present in the heap for unreachable objects. Use the provided context
  /// to mark a set of initial roots into the vm.
  fn collect_garbage<T: 'static + Manage, C: Trace + ?Sized>(
    &self,
    context: &C,
    last: Option<Managed<T>>,
  ) {
    let mut _before = self.bytes_allocated.get();
    self.gc_count.set(self.gc_count.get() + 1);

    #[cfg(feature = "debug_gc")]
    self.stdio.println("-- gc begin");

    if self.trace(context) {
      self.temp_roots.borrow().iter().for_each(|root| {
        root.trace();
      });

      if let Some(obj) = last {
        self.trace(&obj);
      }

      self.sweep_string_cache();
      let remaining = self.sweep();

      self.bytes_allocated.set(remaining);

      self
        .next_gc
        .set(self.bytes_allocated.get() * GC_HEAP_GROW_FACTOR);
    }

    #[cfg(feature = "debug_gc")]
    {
      let now = self.bytes_allocated.get();
      self.stdio.println("-- gc end");
      self.stdio.println(&format!(
        "   collected {} bytes (from {} to {}) next at {}",
        _before - now,
        _before,
        now,
        self.next_gc.get()
      ));
    }
  }

  /// wrapper around an entities trace method to select either normal
  /// or debug trace at compile time.
  fn trace<T: Trace + ?Sized>(&self, entity: &T) -> bool {
    #[cfg(not(feature = "debug_gc"))]
    return entity.trace();

    #[cfg(feature = "debug_gc")]
    return entity.trace_debug(&*self.stdio);
  }

  /// Remove unmarked objects from the heap. This calculates the remaining
  /// memory present in the heap
  fn sweep(&self) -> usize {
    if self.gc_count.get() % 10 == 0 {
      self.sweep_full()
    } else {
      self.sweep_nursery()
    }
  }

  /// Remove unmarked objects from the nursery heap. Promoting surviving objects
  /// to the normal heap
  fn sweep_nursery(&self) -> usize {
    let mut remaining: usize = 0;
    let mut heap = self.heap.borrow_mut();

    self.nursery_heap.borrow_mut().drain(..).for_each(|obj| {
      let retain = (*obj).marked();

      #[cfg(feature = "debug_gc")]
      self.debug_free(&obj, !retain);

      if retain {
        heap.push(obj);
      }
    });

    heap.iter().for_each(|obj| {
      (*obj).unmark();
      remaining += obj.size();
    });

    remaining
  }

  /// Remove unmarked objects from the both heaps. Promoting surviving objects
  /// to the normal heap
  fn sweep_full(&self) -> usize {
    let mut remaining: usize = 0;
    let mut heap = self.heap.borrow_mut();

    self.nursery_heap.borrow_mut().drain(..).for_each(|obj| {
      let retain = (*obj).marked();

      #[cfg(feature = "debug_gc")]
      self.debug_free(&obj, !retain);

      if retain {
        heap.push(obj);
      }
    });

    heap.retain(|obj| {
      let retain = (*obj).unmark();

      #[cfg(feature = "debug_gc")]
      self.debug_free(&obj, !retain);

      if retain {
        remaining += obj.size();
        return true;
      }

      false
    });

    remaining
  }

  /// Remove strings from the cache that no longer have any references
  /// in the heap
  fn sweep_string_cache(&self) {
    self.intern_cache.borrow_mut().retain(|_, &mut string| {
      let retain = string.obj().marked();

      #[cfg(feature = "debug_gc")]
      self.debug_string_remove(string, !retain);

      #[allow(clippy::let_and_return)]
      retain
    });
  }

  /// Debug logging for allocating an object.
  #[cfg(feature = "debug_gc")]
  fn debug_allocate<T: 'static + Manage>(&self, ptr: NonNull<Allocation<T>>, size: usize) {
    #[cfg(feature = "debug_gc")]
    {
      self.stdio.println(&format!(
        "{:p} allocate {} for {}",
        ptr.as_ptr(),
        size,
        unsafe { ptr.as_ref() }.debug()
      ));
    }
  }

  /// Debug logging for removing a string from the cache.
  #[cfg(feature = "debug_gc")]
  fn debug_string_remove(&self, string: Managed<String>, free: bool) {
    if free {
      self.stdio.println(&format!(
        "{:p} remove string from cache {}",
        &**string,
        (*string).debug(),
      ));
    }
  }

  /// Debug logging for free an object.
  #[cfg(feature = "debug_gc")]
  fn debug_free(&self, obj: &Box<Allocation<dyn Manage>>, free: bool) {
    if free {
      self
        .stdio
        .println(&format!("{:p} free {}", &**obj, (**obj).debug_free()));
    }
  }
}

impl<'a> Default for Gc {
  fn default() -> Self {
    Gc::new(Box::new(NativeStdIo::new()))
  }
}
pub struct NoGc();

impl fmt::Display for NoGc {
  fn fmt(&self, _: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    unreachable!()
  }
}

impl Trace for NoGc {
  fn trace(&self) -> bool {
    false
  }

  fn trace_debug(&self, _: &dyn StdIo) -> bool {
    false
  }
}

pub static NO_GC: NoGc = NoGc();

#[cfg(test)]
mod test {
  use super::*;
  use crate::io::NativeStdIo;

  #[test]
  fn dyn_manage() {
    let dyn_trace: Box<dyn Trace> = Box::new(NoGc());
    let gc = Gc::new(Box::new(NativeStdIo()));

    let dyn_manged_str = gc.manage("managed".to_string(), &*dyn_trace);
    assert_eq!(*dyn_manged_str, "managed".to_string());
  }
}
