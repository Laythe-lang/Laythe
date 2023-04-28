use crate::{
  constants::INIT,
  managed::{DebugHeap, DebugWrap, GcObj, GcStr, Object, Trace},
};
use crate::{hooks::GcHooks, value::Value};
use fnv::FnvBuildHasher;
use hashbrown::HashMap;
use std::{fmt, io::Write};

use super::ObjectKind;

#[derive(PartialEq, Eq, Clone)]
pub struct Class {
  name: GcStr,
  init: Option<Value>,
  methods: HashMap<GcStr, Value, FnvBuildHasher>,
  fields: HashMap<GcStr, u16, FnvBuildHasher>,
  meta_class: Option<GcObj<Class>>,
  super_class: Option<GcObj<Class>>,
}

impl Class {
  pub fn with_inheritance(hooks: &GcHooks, name: GcStr, super_class: GcObj<Class>) -> GcObj<Self> {
    let mut class = hooks.manage_obj(Self {
      name,
      init: None,
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

  pub fn meta_class(&self) -> &Option<GcObj<Class>> {
    &self.meta_class
  }

  pub fn super_class(&self) -> &Option<GcObj<Class>> {
    &self.super_class
  }

  pub fn is_subclass(&self, class: GcObj<Class>) -> bool {
    if self == &*class {
      return true;
    }

    match self.super_class {
      Some(super_class) => super_class.is_subclass(class),
      None => false,
    }
  }

  pub fn set_meta(&mut self, meta_class: GcObj<Class>) -> &mut Self {
    if self.meta_class.is_some() {
      panic!("Meta class already set!");
    }

    self.meta_class = Some(meta_class);
    self
  }

  pub fn add_field(&mut self, name: GcStr) {
    if !self.fields.contains_key(&name) {
      let len = self.fields.len();

      self.fields.insert(name, len as u16);
    }
  }

  pub fn add_method(&mut self, name: GcStr, method: Value) {
    if &*name == INIT {
      self.init = Some(method)
    }

    self.methods.insert(name, method);
  }

  pub fn get_method(&self, name: &GcStr) -> Option<Value> {
    self.methods.get(name).copied()
  }

  #[inline]
  pub fn get_field_index(&self, name: &GcStr) -> Option<u16> {
    self.fields.get(name).copied()
  }

  pub fn inherit(&mut self, _hooks: &GcHooks, super_class: GcObj<Class>) {
    debug_assert!(self.methods.is_empty());
    debug_assert!(self.fields.is_empty());

    self.methods.reserve(super_class.methods.len());
    super_class.methods.iter().for_each(|(key, value)| {
      if self.methods.get(key).is_none() {
        self.methods.insert(*key, *value);
      }
    });

    self.fields.reserve(super_class.fields.len());
    super_class.fields.iter().for_each(|(field, index)| {
      self.fields.insert(*field, *index);
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
    let class_class = super_meta_class
      .meta_class()
      .expect("Expected super meta class to have meta class.");

    debug_assert!(
      &*class_class.name() == "Class",
      "self: {}, super: {}, class: {}",
      &*self.name(),
      &*self.super_class.unwrap().name(),
      &*class_class.name()
    );

    let mut meta_class = hooks.manage_obj(Self {
      name: hooks.manage_str(format!("{} metaClass", &*self.name)),
      init: None,
      methods: HashMap::default(),
      fields: HashMap::default(),
      meta_class: Some(class_class),
      super_class: None,
    });

    hooks.push_root(meta_class);
    meta_class.inherit(hooks, class_class);
    hooks.pop_roots(1);

    self.set_meta(meta_class);
  }
}

impl fmt::Display for Class {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<class {} {:p}>", self.name(), self)
  }
}

impl fmt::Debug for Class {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.fmt_heap(f, 2)
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
    f.debug_struct("Class")
      .field("name", &DebugWrap(&self.name, depth))
      .field("methods", &DebugWrap(&self.methods, depth))
      .field("init", &DebugWrap(&self.init, depth))
      .finish()
  }
}

impl Object for Class {
  fn kind(&self) -> ObjectKind {
    ObjectKind::Class
  }
}
