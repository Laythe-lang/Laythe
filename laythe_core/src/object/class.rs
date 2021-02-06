use crate::{constants::{INDEX_GET, INDEX_SET, INIT}, managed::{DebugHeap, DebugWrap, Gc, GcStr, Manage, Trace}};
use crate::{hooks::GcHooks, value::Value};
use fnv::FnvBuildHasher;
use hashbrown::HashMap;
use std::{fmt, io::Write, mem};

#[derive(PartialEq, Clone)]
pub struct Class {
  name: GcStr,
  init: Option<Value>,
  index_get: Option<Value>,
  index_set: Option<Value>,
  methods: HashMap<GcStr, Value, FnvBuildHasher>,
  fields: HashMap<GcStr, u16, FnvBuildHasher>,
  meta_class: Option<Gc<Class>>,
  super_class: Option<Gc<Class>>,
}

impl fmt::Display for Class {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<cls {}>", self.name)
  }
}

impl Class {
  pub fn with_inheritance(hooks: &GcHooks, name: GcStr, super_class: Gc<Class>) -> Gc<Self> {
    let mut class = hooks.manage(Self {
      name,
      init: None,
      index_get: None,
      index_set: None,
      methods: HashMap::default(),
      fields: HashMap::default(),
      meta_class: None,
      super_class: None,
    });

    hooks.push_root(class);
    class.inherit(hooks, super_class);
    class.meta_from_super(hooks);
    hooks.pop_roots(1);

    class
  }

  pub fn bare(name: GcStr) -> Self {
    Self {
      name,
      init: None,
      index_get: None,
      index_set: None,
      methods: HashMap::default(),
      fields: HashMap::default(),
      meta_class: None,
      super_class: None,
    }
  }

  #[inline]
  pub fn name(&self) -> GcStr {
    self.name
  }

  #[inline]
  pub fn fields(&self) -> usize {
    self.fields.len()
  }

  #[inline]
  pub fn init(&self) -> Option<Value> {
    self.init
  }

  #[inline]
  pub fn index_get(&self) -> Option<Value> {
    self.index_get
  }

  #[inline]
  pub fn index_set(&self) -> Option<Value> {
    self.index_set
  }

  pub fn meta_class(&self) -> &Option<Gc<Class>> {
    &self.meta_class
  }

  pub fn super_class(&mut self) -> &Option<Gc<Class>> {
    &self.super_class
  }

  pub fn is_subclass(&self, class: Gc<Class>) -> bool {
    if self == &*class {
      return true;
    }

    match self.super_class {
      Some(super_class) => super_class.is_subclass(class),
      None => false,
    }
  }

  pub fn set_meta(&mut self, meta_class: Gc<Class>) -> &mut Self {
    if self.meta_class.is_some() {
      panic!("Meta class already set!");
    }

    self.meta_class = Some(meta_class);
    self
  }

  pub fn add_field(&mut self, hooks: &GcHooks, name: GcStr) -> Option<u16> {
    let len = self.fields.len();

    hooks.grow(self, |class| class.fields.insert(name, len as u16))
  }

  pub fn add_method(&mut self, hooks: &GcHooks, name: GcStr, method: Value) -> Option<Value> {
    match &*name {
      INIT => self.init = Some(method),
      INDEX_GET => self.index_get = Some(method),
      INDEX_SET => self.index_set = Some(method),
      _ => (),
    }

    hooks.grow(self, |class| class.methods.insert(name, method))
  }

  pub fn get_method(&self, name: &GcStr) -> Option<Value> {
    self.methods.get(name).copied()
  }

  #[inline]
  pub fn get_field_index(&self, name: &GcStr) -> Option<u16> {
    self.fields.get(name).copied()
  }

  pub fn inherit(&mut self, hooks: &GcHooks, super_class: Gc<Class>) {
    debug_assert!(self.methods.is_empty());
    debug_assert!(self.fields.is_empty());

    hooks.grow(self, |class| {
      class.methods.reserve(super_class.methods.len());
      super_class.methods.iter().for_each(|(key, value)| {
        if class.methods.get(&*key).is_none() {
          class.methods.insert(*key, *value);
        }
      });

      class.fields.reserve(super_class.fields.len());
      super_class.fields.iter().for_each(|(field, _index)| {
        if class.fields.get(&field).is_none() {
          let len = class.fields.len();
          class.fields.insert(*field, len as u16);
        }
      })
    });

    debug_assert!(self
      .super_class
      .map(|super_class| &*super_class.name == "Object")
      .unwrap_or(true));

    self.super_class = Some(super_class);
    self.init = self.init.or(super_class.init);
  }

  pub fn meta_from_super(&mut self, hooks: &GcHooks) {
    let super_meta_class = self
      .super_class
      .expect("Expected super class.")
      .meta_class()
      .expect("Expected super class to have meta class.");
    let super_meta_meta_class = super_meta_class
      .meta_class()
      .expect("Expected super meta class to have meta class.");

    let mut meta_class = hooks.manage(Self {
      name: hooks.manage_str(format!("{} metaClass", self.name)),
      init: None,
      index_get: None,
      index_set: None,
      methods: HashMap::default(),
      fields: HashMap::default(),
      meta_class: Some(super_meta_meta_class),
      super_class: None,
    });

    hooks.push_root(meta_class);
    meta_class.inherit(hooks, super_meta_meta_class);
    hooks.pop_roots(1);

    self.set_meta(meta_class);
  }
}

impl fmt::Debug for Class {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.fmt_heap(f, 1)
  }
}

impl Trace for Class {
  fn trace(&self) {
    self.name.trace();

    self.methods.iter().for_each(|(key, val)| {
      key.trace();
      val.trace();
    });
    self.fields.iter().for_each(|(key, _)| {
      key.trace();
    });

    if let Some(super_class) = self.super_class {
      super_class.trace();
    }
    if let Some(meta_class) = self.meta_class {
      meta_class.trace();
    }
  }

  fn trace_debug(&self, stdio: &mut dyn Write) {
    self.name.trace_debug(stdio);

    self.methods.iter().for_each(|(key, val)| {
      key.trace_debug(stdio);
      val.trace_debug(stdio);
    });
    self.fields.iter().for_each(|(key, _)| {
      key.trace_debug(stdio);
    });

    if let Some(super_class) = self.super_class {
      super_class.trace_debug(stdio);
    }
    if let Some(meta_class) = self.meta_class {
      meta_class.trace_debug(stdio);
    }
  }
}

impl DebugHeap for Class {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    let depth = depth.saturating_sub(1);

    f.debug_struct("Class")
      .field("name", &DebugWrap(&self.name, depth))
      .field("methods", &DebugWrap(&self.methods, depth))
      .field("init", &DebugWrap(&self.init, depth))
      .finish()
  }
}

impl Manage for Class {
  fn size(&self) -> usize {
    mem::size_of::<Class>()
      + (mem::size_of::<GcStr>() + mem::size_of::<Value>()) * self.methods.capacity()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

#[cfg(test)]
pub fn test_class(hooks: &GcHooks, name: &str) -> Gc<Class> {
  let mut object_class = hooks.manage(Class::bare(hooks.manage_str("Object")));
  let mut class_class = hooks.manage(Class::bare(hooks.manage_str("Object")));
  class_class.inherit(hooks, object_class);

  let class_copy = class_class;
  class_class.set_meta(class_copy);

  let object_meta_class = Class::with_inheritance(
    hooks,
    hooks.manage_str(format!("{} metaClass", object_class.name())),
    class_class,
  );

  object_class.set_meta(object_meta_class);
  Class::with_inheritance(hooks, hooks.manage_str(name), object_class)
}
