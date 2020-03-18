use spacelox_core::managed::{Allocation, Manageable, Managed, Trace};
use spacelox_interner::IStr;
use std::cell::RefCell;
use std::fmt;
use std::ptr::NonNull;

#[cfg(feature = "debug_gc")]
use std::mem;

pub struct Gc {
  objects: RefCell<Vec<Box<Allocation<dyn Manageable>>>>,
}

impl<'a> Gc {
  pub fn new() -> Self {
    Gc {
      objects: RefCell::new(Vec::with_capacity(100)),
    }
  }

  pub fn manage<T: 'static + Manageable, C: Trace>(&self, data: T, context: &C) -> Managed<T> {
    Managed::from(self.allocate(data, context))
  }

  pub fn manage_string<C: Trace>(&self, string: &str, context: &C) -> Managed<IStr> {
    let interned = IStr::new(string);
    self.manage(interned, context)
  }

  pub fn copy_string<C: Trace>(&self, string_ptr: &str, context: &C) -> Managed<IStr> {
    let interned = IStr::new(string_ptr);
    self.manage(interned, context)
  }

  pub fn copy_managed<T: 'static + Manageable + Clone, C: Trace>(
    &self,
    managed: &Managed<T>,
    context: &C,
  ) -> Managed<T> {
    let cloned = (**managed).clone();
    Managed::from(self.allocate(cloned, context))
  }

  fn allocate<T: 'static + Manageable, C: Trace>(
    &self,
    data: T,
    context: &C,
  ) -> NonNull<Allocation<T>> {
    // #[cfg(feature = "debug_stress_gc")]
    self.collect_garbage(context);

    let mut alloc = Box::new(Allocation::new(data));
    let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };
    self.objects.borrow_mut().push(alloc);

    #[cfg(feature = "debug_gc")]
    {
      println!(
        "{:p} allocate {} for {}",
        ptr.as_ptr(),
        mem::size_of::<T>(),
        unsafe { ptr.as_ref() }.alloc_type()
      );
    }

    ptr
  }

  pub fn collect_garbage<C: Trace>(&self, context: &C) {
    #[cfg(feature = "debug_gc")]
    {
      println!("-- gc begin");
    }

    let mut gray_stack = Vec::with_capacity(40);
    if self.mark(context, &mut gray_stack) {
      self.trace(&mut gray_stack);
      self.sweep();
    }

    #[cfg(feature = "debug_gc")]
    {
      println!("-- gc end");
    }
  }

  fn mark<T: Trace>(&self, root: &T, gray_stack: &mut Vec<Managed<dyn Manageable>>) -> bool {
    root.trace(&mut |obj| (self.mark_obj(obj, gray_stack)))
  }

  fn trace(&self, gray_stack: &mut Vec<Managed<dyn Manageable>>) {
    let mut obj_buffer: Vec<Managed<dyn Manageable>> = Vec::with_capacity(60);

    while let Some(gray) = gray_stack.pop() {
      gray.trace(&mut |obj| obj_buffer.push(obj));

      obj_buffer.drain(..).for_each(|obj| {
        self.mark_obj(obj, gray_stack);
      })
    }
  }

  pub fn sweep(&self) {
    self.objects.borrow_mut().retain(|obj| {
      let retain = (**obj).unmark();

      #[cfg(feature = "debug_gc")]
      {
        if !retain {
          println!("{:p} free {}", obj, (**obj).debug());
        }
      }

      retain
    });
  }

  fn mark_obj(
    &self,
    managed: Managed<dyn Manageable>,
    gray_stack: &mut Vec<Managed<dyn Manageable>>,
  ) {
    if managed.obj().mark() {
      return;
    }

    #[cfg(feature = "debug_gc")]
    {
      println!("{:p} mark {}", &managed, managed.debug())
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
  fn trace(&self, _: &mut dyn FnMut(Managed<(dyn Manageable)>)) -> bool {
    false
  }
}

pub static NO_GC: NoGc = NoGc {};
