use spacelox_core::managed::{Allocation, Manage, Managed, Trace};
use spacelox_interner::IStr;
use std::cell::{RefCell, Cell};
use std::fmt;
use std::ptr::NonNull;

pub struct Gc {
  heap: RefCell<Vec<Box<Allocation<dyn Manage>>>>,
  bytes_allocated: Cell<usize>,
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
  /// use spacelox_interner::IStr;
  /// 
  /// let gc = Gc::new();
  /// let fun = Fun {
  ///   arity: 3,
  ///   upvalue_count: 0,
  ///   chunk: Chunk::default(),
  ///   name: Some(IStr::new("fun")),
  /// };
  /// 
  /// let managed_fun = gc.manage(fun, &NO_GC);
  /// 
  /// assert_eq!(managed_fun.name, Some(IStr::new("fun")));
  pub fn manage<T: 'static + Manage, C: Trace>(&self, data: T, context: &C) -> Managed<T> {
    Managed::from(self.allocate(data, context))
  }

  /// Create a `Managed<IStr>` from a str slice. This creates
  /// or returns an interned string and allocates a pointer to the intern
  /// cache. A Managed<IStr> can be created from `.manage` but will 
  /// not intern the string.
  /// 
  /// # Examples
  /// ```
  /// use spacelox_vm::memory::{Gc, NO_GC};
  /// use spacelox_core::value::Value;
  /// use spacelox_core::managed::Managed;
  /// use spacelox_interner::IStr;
  /// use std::ptr;
  /// 
  /// let gc = Gc::new();
  /// let str = gc.manage_str(&"hi!", &NO_GC);
  /// 
  /// assert_eq!(str.as_str(), "hi!");
  /// ```
  pub fn manage_str<C: Trace>(&self, string: &str, context: &C) -> Managed<IStr> {
    let interned = IStr::new(string);
    self.manage(interned, context)
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
  /// let up2 = gc.clone_managed(&up1, &NO_GC);
  /// 
  /// assert!(!ptr::eq(&*up1, &*up2));
  /// match (&*up1, &*up2) {
  ///   (Upvalue::Open(o1), Upvalue::Open(o2)) => assert_eq!(o1, o2),
  ///   _ => panic!("No equal!"),
  /// }
  /// ```
  pub fn clone_managed<T: 'static + Manage + Clone, C: Trace>(
    &self,
    managed: &Managed<T>,
    context: &C,
  ) -> Managed<T> {
    let cloned = (**managed).clone();
    Managed::from(self.allocate(cloned, context))
  }

  /// Allocate `data` on the gc's heap. If conditions are met
  /// a garbage collection can be triggered. When triggered 
  /// will use the roots provided by the `context`
  fn allocate<T: 'static + Manage, C: Trace>(
    &self,
    data: T,
    context: &C,
  ) -> NonNull<Allocation<T>> {
    // create own store of allocation
    let mut alloc = Box::new(Allocation::new(data));
    let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };

    // push onto heap
    let size = alloc.size();
    self.heap.borrow_mut().push(alloc);

    #[cfg(feature = "debug_stress_gc")]
    self.collect_garbage(context);
    
    if self.bytes_allocated.replace(self.bytes_allocated.get() + size) > self.next_gc.get() {
      self.collect_garbage(context);
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

    ptr
  }

  /// Collect garbage present in the heap for unreachable objects. Use the provided context
  /// to mark a set of initial roots into the vm.
  fn collect_garbage<C: Trace>(&self, context: &C) {
    let mut _before = self.bytes_allocated.get();
    #[cfg(feature = "debug_gc")]
    {
      println!("-- gc begin");
    }

    let mut gray_stack = Vec::with_capacity(40);
    if self.mark(context, &mut gray_stack) {
      self.trace(&mut gray_stack);

      self.bytes_allocated.set(self.sweep());

      self.next_gc.set(self.bytes_allocated.get() * GC_HEAP_GROW_FACTOR);
    }

    #[cfg(feature = "debug_gc")]
    {
      let now = self.bytes_allocated.get();
      println!("-- gc end");
      println!("   collected {} bytes (from {} to {}) next at {}", _before - now, _before, now, self.next_gc.get());
    }
  }

  /// Mark an initial set of roots are reachable filling the gray_stack
  fn mark<T: Trace>(&self, root: &T, gray_stack: &mut Vec<Managed<dyn Manage>>) -> bool {
    root.trace(&mut |obj| (self.mark_obj(obj, gray_stack)))
  }

  /// trace all objects in the heap marking each one that is reachable from the
  /// vm roots
  fn trace(&self, gray_stack: &mut Vec<Managed<dyn Manage>>) {
    let mut obj_buffer: Vec<Managed<dyn Manage>> = Vec::with_capacity(60);

    while let Some(gray) = gray_stack.pop() {
      gray.trace(&mut |obj| obj_buffer.push(obj));

      // drain the temp buffer into the gray stack
      obj_buffer.drain(..).for_each(|obj| {
        self.mark_obj(obj, gray_stack);
      })
    }
  }

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
          println!("{:p} free {}", obj, (**obj).debug());
        }
      }

      retain
    });

    remaining
  }

  /// mark an `Managed` as reachable from some root. This method returns
  /// early if the object is already marked. If the object isn't marked
  /// adds the object to the the `gray_stack`
  fn mark_obj(
    &self,
    managed: Managed<dyn Manage>,
    gray_stack: &mut Vec<Managed<dyn Manage>>,
  ) {
    if managed.obj().mark() {
      return;
    }

    #[cfg(feature = "debug_gc")]
    {
      println!("{:p} mark {}", &*managed, managed.debug())
    }

    gray_stack.push(managed);
  }
}

pub struct NoGc();

impl fmt::Display for NoGc {
  fn fmt(&self, _: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    unreachable!()
  }
}

impl Trace for NoGc {
  fn trace(&self, _: &mut dyn FnMut(Managed<(dyn Manage)>)) -> bool {
    false
  }
}

pub static NO_GC: NoGc = NoGc {};
