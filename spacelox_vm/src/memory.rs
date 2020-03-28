use spacelox_core::managed::{Allocation, Manage, Managed, Trace};
use std::cell::{Cell, RefCell};
use std::collections::{hash_map::Entry, HashMap};
use std::fmt;
use std::ptr::NonNull;

pub struct Gc {
  heap: RefCell<Vec<Box<Allocation<dyn Manage>>>>,
  bytes_allocated: Cell<usize>,
  intern_cache: RefCell<HashMap<String, Managed<String>>>,
  next_gc: Cell<usize>,
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
      heap: RefCell::new(Vec::with_capacity(100)),
      bytes_allocated: Cell::new(0),
      intern_cache: RefCell::new(HashMap::new()),
      next_gc: Cell::new(1024 * 1024),
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
    let mut cache = self.intern_cache.borrow_mut();
    match cache.entry(string) {
      Entry::Vacant(vacant) => {
        let managed = self.allocate(vacant.key().to_string(), context);
        *vacant.insert(managed)
      }
      Entry::Occupied(occupied) => *occupied.get(),
    }
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
    self.heap.borrow_mut().push(alloc);

    let managed = Managed::from(ptr);

    #[cfg(feature = "debug_stress_gc")]
    {
      self.collect_garbage(context, managed);
    }

    if allocated + size > self.next_gc.get() {
      self.collect_garbage(context, managed);
    }

    #[cfg(feature = "debug_gc")]
    {
      println!(
        "{:p} allocate {} for {}",
        ptr.as_ptr(),
        size,
        unsafe { ptr.as_ref() }.debug()
      );
    }

    managed
  }

  /// Collect garbage present in the heap for unreachable objects. Use the provided context
  /// to mark a set of initial roots into the vm.
  fn collect_garbage<T: 'static + Manage, C: Trace>(&self, context: &C, last: Managed<T>) {
    let mut _before = self.bytes_allocated.get();
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

  /// Mark an initial set of roots are reachable filling the gray_stack
  // fn mark<T: Trace>(&self, root: &T, gray_stack: &mut Vec<Managed<dyn Manage>>) -> bool {
  //   root.trace(&mut |obj| (self.mark_obj(obj, gray_stack)))
  // }
  // fn mark<T: Trace>(&self, root: &T) -> bool {
  //   root.trace()
  // }

  /// trace all objects in the heap marking each one that is reachable from the
  /// vm roots
  // fn trace(&self, gray_stack: &mut Vec<Managed<dyn Manage>>) {
  //   let mut obj_buffer: Vec<Managed<dyn Manage>> = Vec::with_capacity(60);

  //   while let Some(gray) = gray_stack.pop() {
  //     gray.trace(&mut |obj| obj_buffer.push(obj));

  //     // drain the temp buffer into the gray stack
  //     obj_buffer.drain(..).for_each(|obj| {
  //       self.mark_obj(obj, gray_stack);
  //     })
  //   }
  // }

  /// Remove unmarked objects from the heap. This calculates the remaining
  /// memory present in the heap
  fn sweep(&self) -> usize {
    let mut remaining: usize = 0;

    self.heap.borrow_mut().retain(|obj| {
      let retain = (**obj).unmark();

      if retain {
        remaining = remaining + obj.size();
      }

      #[cfg(feature = "debug_gc")]
      {
        if !retain {
          println!("{:p} free {}", &**obj, (**obj).debug());
        }
      }

      retain
    });

    remaining
  }

  /// Remove strings from the cache that no longer have any references
  /// in the heap
  fn sweep_string_cache(&self) {
    self.intern_cache.borrow_mut().retain(|_, &mut string| {
      let retain = string.obj().marked();

      #[cfg(feature = "debug_gc")]
      {
        if !retain {
          println!(
            "{:p} remove string from cache {}",
            &**string,
            (*string).debug()
          );
        }
      }

      retain
    });
  }

  // /// mark an `Managed` as reachable from some root. This method returns
  // /// early if the object is already marked. If the object isn't marked
  // /// adds the object to the the `gray_stack`
  // fn mark_obj(&self, managed: Managed<dyn Manage>, gray_stack: &mut Vec<Managed<dyn Manage>>) {
  //   if managed.obj().mark() {
  //     return;
  //   }

  //   #[cfg(feature = "debug_gc")]
  //   {
  //     println!("{:p} mark {}", &*managed.obj(), managed.debug());
  //   }

  //   gray_stack.push(managed);
  // }
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
