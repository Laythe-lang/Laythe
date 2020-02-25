use crate::vm::VmExecutor;
use spacelox_core::object::{Obj, ObjValue};
use spacelox_core::value::Value;
use std::cell::Cell;
use std::collections::{HashMap, HashSet};
use std::ptr::NonNull;

#[cfg(feature = "debug_gc")]
use std::mem;

pub struct Allocator<'a> {
  objects: Cell<Option<&'a Obj<'a>>>,
  string_store: HashSet<String>,
}

impl<'a> Allocator<'a> {
  pub fn new() -> Self {
    Allocator {
      objects: Cell::new(None),
      string_store: HashSet::new(),
    }
  }

  pub fn allocate(&mut self, value: ObjValue<'a>) -> Obj<'a> {
    self.reallocate();

    let obj = Obj::new(value);
    obj.next.set(self.objects.get());
    self.objects.set(obj.next.get());

    #[cfg(feature = "debug_gc")]
    {
      println!(
        "{:p} allocate {} for {}",
        &obj,
        mem::size_of::<Obj<'a>>(),
        obj.obj_type()
      );
    }

    obj
  }

  pub fn allocate_string(&mut self, string: String) -> Obj<'a> {
    let str_ptr = NonNull::from(self.intern_string(string));
    self.allocate(ObjValue::String(str_ptr))
  }

  pub fn copy_string(&mut self, string_ptr: NonNull<str>) -> Obj<'a> {
    self.allocate(ObjValue::String(string_ptr))
  }

  fn intern_string(&mut self, string: String) -> &str {
    &**self.string_store.get_or_insert(string)
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

    mark_table(&vm.globals);
  }

  pub fn free_objects(&mut self) {
    loop {
      match self.objects.get() {
        Some(obj) => {
          #[cfg(feature = "debug_gc")]
          {
            println!("{:p} free {}", obj, obj.obj_type());
          }

          self.objects.replace(obj.next.get());
        }
        None => {
          return;
        }
      }
    }
  }

  pub fn reallocate(&mut self) {
    #[cfg(feature = "debug_gc")]
    {
      self.collect_garbage();
    }
  }
}

fn mark_table(table: &HashMap<Obj, Value>) {
  table.iter().for_each(|(key, val)| {
    mark_object(key);
    mark_value(val);
  });
}

fn mark_value(value: &Value) {
  match value {
    Value::Obj(obj) => mark_object(obj),
    _ => (),
  }
}

fn mark_object(obj: &Obj) {
  obj.is_marked.replace(true);

  #[cfg(feature = "debug_gc")]
  {
    println!("{:p} mark {}", obj, obj)
  }
}
