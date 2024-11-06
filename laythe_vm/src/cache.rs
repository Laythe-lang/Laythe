use core::panic;
use std::vec;

use laythe_core::{managed::GcObj, object::Class, utils::IdEmitter, value::Value};

/// The cache for property access and setting
#[derive(Clone, Debug)]
struct PropertyCache {
  /// The cached class
  class: GcObj<Class>,

  /// The expected index given the class
  property_index: usize,
}

/// The cache for invoking a method
#[derive(Clone, Debug)]
struct InvokeCache {
  /// The cached class
  class: GcObj<Class>,

  /// The expected method given the class
  method: Value,
}

/// The inline cache for a module in Laythe
/// This cache is meant to reduce the total
/// number of hash lookups need in the vm
#[derive(Debug)]
pub struct InlineCache {
  /// A set of property access caches. There
  /// is one for each location a property is
  /// accessed or set
  property: Vec<Option<PropertyCache>>,

  /// A set of invoke caches. There is
  /// one for each location a method is
  /// invoke
  invoke: Vec<Option<InvokeCache>>,
}

impl InlineCache {
  /// Create a new inline cache for this module, supplying
  /// the number of slot requires
  pub fn new(property_slots: usize, invoke_slots: usize) -> Self {
    Self {
      property: vec![None; property_slots],
      invoke: vec![None; invoke_slots],
    }
  }

  /// Attempt to retrieve the property cache at a given slot
  /// for the provided class
  pub fn get_property_cache(&self, inline_slot: usize, class: GcObj<Class>) -> Option<usize> {
    debug_assert!(inline_slot < self.property.len());
    match unsafe { self.property.get_unchecked(inline_slot) } {
      Some(cache) => {
        if cache.class == class {
          Some(cache.property_index)
        } else {
          None
        }
      },
      None => None,
    }
  }

  /// Set the property cache at a given slot for the provided
  /// class and property index
  pub fn set_property_cache(
    &mut self,
    inline_slot: usize,
    class: GcObj<Class>,
    property_index: usize,
  ) {
    self.set_property(
      inline_slot,
      Some(PropertyCache {
        class,
        property_index,
      }),
    );
  }

  /// Clear the property cache at a given slot
  pub fn clear_property_cache(&mut self, inline_slot: usize) {
    self.set_property(inline_slot, None);
  }

  /// Attempt to retrieve the invoke cache at a given slot
  /// for the provided class
  pub fn get_invoke_cache(&self, inline_slot: usize, class: GcObj<Class>) -> Option<Value> {
    debug_assert!(inline_slot < self.invoke.len());
    match unsafe { self.invoke.get_unchecked(inline_slot) } {
      Some(cache) => {
        if cache.class == class {
          Some(cache.method)
        } else {
          None
        }
      },
      None => None,
    }
  }

  /// Set the invoke cache at a given slot for the provided
  /// class and method
  pub fn set_invoke_cache(&mut self, inline_slot: usize, class: GcObj<Class>, method: Value) {
    self.set_invoke(inline_slot, Some(InvokeCache { class, method }));
  }

  /// Clear the invoke cache at a given slot
  pub fn clear_invoke_cache(&mut self, inline_slot: usize) {
    self.set_invoke(inline_slot, None);
  }

  fn set_property(&mut self, inline_slot: usize, value: Option<PropertyCache>) {
    debug_assert!(inline_slot < self.property.len());
    unsafe { *self.property.get_unchecked_mut(inline_slot) = value };
  }

  fn set_invoke(&mut self, inline_slot: usize, value: Option<InvokeCache>) {
    debug_assert!(inline_slot < self.invoke.len());
    unsafe { *self.invoke.get_unchecked_mut(inline_slot) = value };
  }
}

#[derive(Default, Debug)]
/// An id emitter used for inline caching. This struct
/// simply emits new ideas to use in AlignedByteCode::Slot
/// to give each cachable instruction an id
pub struct CacheIdEmitter {
  /// The id emitter for the property cache
  property: IdEmitter,

  /// The id emitter for the invoke cache
  invoke: IdEmitter,
}

impl CacheIdEmitter {
  /// Emit a new property id
  pub fn emit_property(&mut self) -> u32 {
    if self.property_count() > u32::MAX as usize {
      panic!("Laythe cannot handle more than u32 property access");
    }

    self.property.emit() as u32
  }

  /// Get the current total of property ids
  pub fn property_count(&self) -> usize {
    self.property.id_count()
  }

  /// Emit a new invoke id
  pub fn emit_invoke(&mut self) -> u32 {
    if self.invoke_count() > u32::MAX as usize {
      panic!("Laythe cannot handle more than u32 property access");
    }

    self.invoke.emit() as u32
  }

  /// Get the current total of invoke ids
  pub fn invoke_count(&self) -> usize {
    self.invoke.id_count()
  }
}

#[cfg(test)]
mod test {
  mod cache_id_emitter {
    use crate::cache::CacheIdEmitter;

    #[test]
    pub fn property() {
      let mut cache_emitter = CacheIdEmitter::default();

      assert_eq!(cache_emitter.emit_property(), 0);
      assert_eq!(cache_emitter.emit_property(), 1);
      assert_eq!(cache_emitter.emit_property(), 2);
      assert_eq!(cache_emitter.property_count(), 3);
    }

    #[test]
    pub fn invoke() {
      let mut cache_emitter = CacheIdEmitter::default();

      assert_eq!(cache_emitter.emit_invoke(), 0);
      assert_eq!(cache_emitter.emit_invoke(), 1);
      assert_eq!(cache_emitter.emit_invoke(), 2);
      assert_eq!(cache_emitter.invoke_count(), 3);
    }
  }

  mod inline_cache {
    use crate::cache::InlineCache;
    use laythe_core::{
      hooks::{GcHooks, NoContext},
      managed::{Allocator, NO_GC},
      module::Module,
      object::{Class, Fun},
      val,
      value::Value,
    };

    #[test]
    pub fn property() {
      let mut inline_cache = InlineCache::new(3, 0);
      let mut alloc = Allocator::default();

      let class_name = alloc.manage_str("example", &NO_GC);
      let class = alloc.manage_obj(Class::bare(class_name), &NO_GC);

      assert_eq!(inline_cache.get_property_cache(0, class), None);
      assert_eq!(inline_cache.get_property_cache(1, class), None);
      assert_eq!(inline_cache.get_property_cache(2, class), None);

      inline_cache.set_property_cache(0, class, 3);
      inline_cache.set_property_cache(1, class, 1);

      assert_eq!(inline_cache.get_property_cache(0, class), Some(3));
      assert_eq!(inline_cache.get_property_cache(1, class), Some(1));
      assert_eq!(inline_cache.get_property_cache(2, class), None);

      inline_cache.clear_property_cache(1);

      assert_eq!(inline_cache.get_property_cache(0, class), Some(3));
      assert_eq!(inline_cache.get_property_cache(1, class), None);
      assert_eq!(inline_cache.get_property_cache(2, class), None);
    }

    #[test]
    pub fn invoke() {
      let mut inline_cache = InlineCache::new(0, 3);
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let class_name = hooks.manage_str("class_example");
      let fun_name = hooks.manage_str("fn_example");

      let class = hooks.manage_obj(Class::bare(class_name));
      let module = hooks.manage(Module::new(&hooks, class, "example", 0));

      let fun = Fun::stub(&hooks, fun_name, module);
      let fun = val!(hooks.manage_obj(fun));

      assert_eq!(inline_cache.get_invoke_cache(0, class), None);
      assert_eq!(inline_cache.get_invoke_cache(1, class), None);
      assert_eq!(inline_cache.get_invoke_cache(2, class), None);

      inline_cache.set_invoke_cache(0, class, fun);
      inline_cache.set_invoke_cache(2, class, fun);

      assert_eq!(inline_cache.get_invoke_cache(0, class), Some(fun));
      assert_eq!(inline_cache.get_invoke_cache(1, class), None);
      assert_eq!(inline_cache.get_invoke_cache(2, class), Some(fun));

      inline_cache.clear_invoke_cache(2);

      assert_eq!(inline_cache.get_invoke_cache(0, class), Some(fun));
      assert_eq!(inline_cache.get_invoke_cache(1, class), None);
      assert_eq!(inline_cache.get_invoke_cache(2, class), None);
    }
  }
}
