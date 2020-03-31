use spacelox_core::managed::{Allocation, Manage, Managed, Trace};
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::fmt;
use std::ptr::NonNull;

pub struct Gc {
  nursery_heap: RefCell<Vec<Box<Allocation<dyn Manage>>>>,
  heap: RefCell<Vec<Box<Allocation<dyn Manage>>>>,
  bytes_allocated: Cell<usize>,
  intern_cache: RefCell<HashMap<&'static str, Managed<String>>>,
  next_gc: Cell<usize>,
  gc_count: Cell<u128>,
}

const GC_HEAP_GROW_FACTOR: usize = 2;

impl<'a> Gc {
  /// Create a new manged heap for spacelox for objects.
  ///
  /// # Examples
  /// ```
  /// use spacelox_vm::memory::Gc;
  ///
  /// let gc = Gc::new();
  /// ```
  pub fn new() -> Self {
    Gc {
      nursery_heap: RefCell::new(Vec::with_capacity(1000)),
      heap: RefCell::new(Vec::with_capacity(0)),
      bytes_allocated: Cell::new(0),
      intern_cache: RefCell::new(HashMap::new()),
      next_gc: Cell::new(1024 * 1024),
      gc_count: Cell::new(0),
    }
  }

  /// Create a `Managed<T>` from the provided `data`. This method will allocate space
  /// for `data` and return a pointer to it. In case of a gc the provided `context` is
  /// used to annotate active roots
  ///
  /// # Examples
  /// ```
  /// use spacelox_vm::memory::{Gc, NO_GC};
  /// use spacelox_core::value::{Value, Fun};
  /// use spacelox_core::chunk::Chunk;
  /// use spacelox_core::managed::Managed;
  ///
  /// let gc = Gc::new();
  /// let fun = Fun {
  ///   arity: 3,
  ///   upvalue_count: 0,
  ///   chunk: Chunk::default(),
  ///   name: Some("fun".to_string()),
  /// };
  ///
  /// let managed_fun = gc.manage(fun, &NO_GC);
  ///
  /// assert_eq!(managed_fun.name, Some("fun".to_string()));
  /// ```
  pub fn manage<T: 'static + Manage, C: Trace>(&self, data: T, context: &C) -> Managed<T> {
    self.allocate(data, context)
  }

  /// Create a `Managed<String>` from a str slice. This creates
  /// or returns an interned string and allocates a pointer to the intern
  /// cache. A Managed<String> can be created from `.manage` but will
  /// not intern the string.
  ///
  /// # Examples
  /// ```
  /// use spacelox_vm::memory::{Gc, NO_GC};
  /// use spacelox_core::value::Value;
  /// use spacelox_core::managed::Managed;
  /// use std::ptr;
  ///
  /// let gc = Gc::new();
  /// let str = gc.manage_str("hi!".to_string(), &NO_GC);
  ///
  /// assert_eq!(&*str, "hi!");
  /// ```
  pub fn manage_str<C: Trace>(&self, string: String, context: &C) -> Managed<String> {
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
  /// use spacelox_vm::memory::{Gc, NO_GC};
  /// use spacelox_core::value::{Value, Upvalue};
  /// use spacelox_core::managed::Managed;
  /// use std::ptr;
  ///
  /// let gc = Gc::new();
  /// let up1 = gc.manage(Upvalue::Open(0), &NO_GC);
  /// let up2 = gc.clone_managed(up1, &NO_GC);
  ///
  /// assert!(!ptr::eq(&*up1, &*up2));
  /// match (&*up1, &*up2) {
  ///   (Upvalue::Open(o1), Upvalue::Open(o2)) => assert_eq!(o1, o2),
  ///   _ => panic!("No equal!"),
  /// }
  /// ```
  pub fn clone_managed<T: 'static + Manage + Clone, C: Trace>(
    &self,
    managed: Managed<T>,
    context: &C,
  ) -> Managed<T> {
    let cloned = (*managed).clone();
    self.allocate(cloned, context)
  }

  /// Allocate `data` on the gc's heap. If conditions are met
  /// a garbage collection can be triggered. When triggered
  /// will use the roots provided by the `context`
  fn allocate<T: 'static + Manage, C: Trace>(&self, data: T, context: &C) -> Managed<T> {
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
      self.collect_garbage(context, managed);
    }

    if allocated + size > self.next_gc.get() {
      self.collect_garbage(context, managed);
    }

    #[cfg(feature = "debug_gc")]
    self.debug_allocate(ptr, size);

    managed
  }

  /// Collect garbage present in the heap for unreachable objects. Use the provided context
  /// to mark a set of initial roots into the vm.
  fn collect_garbage<T: 'static + Manage, C: Trace>(&self, context: &C, last: Managed<T>) {
    let mut _before = self.bytes_allocated.get();
    self.gc_count.set(self.gc_count.get() + 1);

    #[cfg(feature = "debug_gc")]
    {
      println!("-- gc begin");
    }

    if context.trace() {
      last.trace();

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
      println!("-- gc end");
      println!(
        "   collected {} bytes (from {} to {}) next at {}",
        _before - now,
        _before,
        now,
        self.next_gc.get()
      );
    }
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
      remaining = remaining + obj.size();
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
        remaining = remaining + obj.size();
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

      retain
    });
  }

  /// Debug logging for allocating an object.
  #[cfg(feature = "debug_gc")]
  fn debug_allocate<T: 'static + Manage>(&self, ptr: NonNull<Allocation<T>>, size: usize) {
    #[cfg(feature = "debug_gc")]
    {
      println!(
        "{:p} allocate {} for {}",
        ptr.as_ptr(),
        size,
        unsafe { ptr.as_ref() }.debug()
      );
    }
  }

  /// Debug logging for removing a string from the cache.
  #[cfg(feature = "debug_gc")]
  fn debug_string_remove(&self, string: Managed<String>, free: bool) {
    if free {
      println!(
        "{:p} remove string from cache {}",
        &**string,
        (*string).debug()
      );
    }
  }

  /// Debug logging for free an object.
  #[cfg(feature = "debug_gc")]
  fn debug_free(&self, obj: &Box<Allocation<dyn Manage>>, free: bool) {
    if free {
      println!("{:p} free {}", &**obj, (**obj).debug());
    }
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
}

pub static NO_GC: NoGc = NoGc {};
