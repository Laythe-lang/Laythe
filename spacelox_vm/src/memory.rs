use crate::vm::VmExecutor;
use spacelox_core::value::{Allocation, Managed, Trace, Value};
use spacelox_interner::IStr;
use std::collections::HashMap;
use std::ptr::NonNull;

#[cfg(feature = "debug_gc")]
use std::mem;

pub struct Gc {
  objects: Vec<Box<Allocation<dyn Trace>>>,
}

impl<'a> Gc {
  pub fn new() -> Self {
    Gc {
      objects: Vec::new(),
    }
  }

  pub fn allocate<T: 'static + Trace>(&mut self, data: T) -> NonNull<Allocation<T>> {
    // self.reallocate();
    let mut alloc = Box::new(Allocation::new(data));
    let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };
    self.objects.push(alloc);

    #[cfg(feature = "debug_gc")]
    {
      println!(
        "{:p} allocate {} for {}",
        &**value,
        mem::size_of::<T>(),
        obj.obj_type()
      );
    }

    ptr
  }

  pub fn allocate_string(&mut self, string: &str) -> NonNull<Allocation<IStr>> {
    let interned = IStr::new(string);
    self.allocate(interned)
  }

  pub fn copy_string(&mut self, string_ptr: &str) -> NonNull<Allocation<IStr>> {
    let interned = IStr::new(string_ptr);
    self.allocate(interned)
  }

  pub fn copy_managed<T: 'static + Trace + Clone>(
    &mut self,
    managed: &Managed<T>,
  ) -> NonNull<Allocation<T>> {
    let cloned = (**managed).clone();
    self.allocate(cloned)
  }

  pub fn collect_garbage(&mut self, vm: &mut VmExecutor) {
    #[cfg(feature = "debug_gc")]
    {
      println!("-- gc begin");
    }

    self.mark_roots(vm);

    #[cfg(feature = "debug_gc")]
    {
      println!("-- gc end");
    }
  }

  pub fn mark_roots(&mut self, vm: &VmExecutor) {
    vm.stack[0..vm.stack_top].iter().for_each(|value| {
      mark_value(value);
    });

    vm.frames[0..vm.frame_count]
      .iter()
      .for_each(|frame| mark_object(&frame.closure));

    vm.open_upvalues
      .iter()
      .for_each(|upvalue| mark_object(upvalue));

    mark_table(&vm.globals);
  }

  pub fn free_objects(&mut self) {
    self.objects.retain(|obj| {
      #[cfg(feature = "debug_gc")]
      {
        println!("{:p} free {}", obj, obj.obj_type());
      }
      !(**obj).marked()
    });
  }

  pub fn reallocate(&mut self) {
    #[cfg(feature = "debug_gc")]
    {
      self.collect_garbage();
    }
  }
}

fn mark_table(table: &HashMap<Managed<IStr>, Value>) {
  table.iter().for_each(|(key, val)| {
    mark_object(key);
    mark_value(val);
  });
}

fn mark_value(value: &Value) {
  match value {
    Value::String(string) => mark_object(string),
    Value::Fun(fun) => mark_object(fun),
    Value::Closure(closure) => mark_object(closure),
    Value::Native(native) => mark_object(native),
    Value::Upvalue(upvalue) => mark_object(upvalue),
    _ => (),
  }
}

fn mark_object<'a, T: 'a + Trace>(managed: &Managed<T>) {
  managed.obj().mark();

  #[cfg(feature = "debug_gc")]
  {
    println!("{:p} mark {}", managed, managed)
  }
}
