use fnv::FnvBuildHasher;
use hashbrown::HashMap;

use crate::{
  captures::Captures,
  chunk::Encode,
  hooks::GcHooks,
  managed::{Gc, GcObj, GcStr},
  module::{module_class, Module},
  object::{Class, Fiber, FiberResult, Fun, FunBuilder},
  signature::Arity,
  value::Value,
};

pub struct FiberBuilder<T = u8>
where
  T: Default + Encode,
{
  name: String,
  parent: Option<GcObj<Fiber>>,
  module_name: String,
  instructions: Vec<T>,
  max_slots: i32,
}

impl<T> Default for FiberBuilder<T>
where
  T: Default + Encode,
{
  fn default() -> Self {
    Self {
      name: "Fiber".to_string(),
      parent: None,
      module_name: "Fiber Module".to_string(),
      instructions: vec![T::default()],
      max_slots: 0,
    }
  }
}

impl<T> FiberBuilder<T>
where
  T: Default + Encode,
{
  pub fn name(mut self, name: &str) -> Self {
    self.name = name.to_string();
    self
  }

  pub fn module_name(mut self, name: &str) -> Self {
    self.module_name = name.to_string();
    self
  }

  pub fn instructions(mut self, instructions: Vec<T>) -> Self {
    self.instructions = instructions;
    self
  }

  pub fn parent(mut self, parent: GcObj<Fiber>) -> Self {
    self.parent = Some(parent);
    self
  }

  pub fn max_slots(mut self, max_slots: i32) -> Self {
    self.max_slots = max_slots;
    self
  }

  pub fn build(self, hooks: &GcHooks) -> FiberResult<GcObj<Fiber>> {
    let mut fun = test_fun_builder(hooks, &self.name, &self.module_name);

    for instruction in self.instructions {
      fun.write_instruction(instruction, 0);
    }
    fun.update_max_slots(self.max_slots);

    let fun = hooks.manage_obj(fun.build(hooks));
    hooks.push_root(fun);

    let captures = Captures::new(hooks, &[]);
    hooks.pop_roots(1);

    Fiber::new(self.parent, fun, captures).map(|fiber| hooks.manage_obj(fiber))
  }
}

impl Encode for u8 {
  fn encode(self, buf: &mut Vec<u8>) -> u32 {
    buf.push(self);
    1
  }
}

#[derive(Default)]
pub struct ClassBuilder {
  name: String,
  super_cls: Option<GcObj<Class>>,
  methods: HashMap<GcStr, Value, FnvBuildHasher>,
  fields: Vec<GcStr>,
}

impl ClassBuilder {
  pub fn name(mut self, name: &str) -> Self {
    self.name = name.to_string();
    self
  }

  pub fn super_cls(mut self, super_cls: GcObj<Class>) -> Self {
    self.super_cls = Some(super_cls);
    self
  }

  pub fn methods(mut self, methods: HashMap<GcStr, Value, FnvBuildHasher>) -> Self {
    self.methods = methods;
    self
  }

  pub fn fields(mut self, fields: Vec<GcStr>) -> Self {
    self.fields = fields;
    self
  }

  pub fn build(self, hooks: &GcHooks) -> GcObj<Class> {
    let super_cls = self.super_cls.unwrap_or_else(|| test_object_class(hooks));
    let mut class = Class::with_inheritance(hooks, hooks.manage_str(self.name), super_cls);

    for (name, method) in self.methods {
      class.add_method(name, method);
    }

    for field in self.fields {
      class.add_field(field);
    }

    class
  }
}

pub fn test_module(hooks: &GcHooks, name: &str) -> Gc<Module> {
  let base_class = test_class(hooks, "Module");
  hooks.manage(Module::new(module_class(hooks, name, base_class), 0))
}

pub fn test_class(hooks: &GcHooks, name: &str) -> GcObj<Class> {
  let object_class = test_object_class(hooks);
  Class::with_inheritance(hooks, hooks.manage_str(name), object_class)
}

pub fn test_fun(hooks: &GcHooks, name: &str, module_name: &str) -> GcObj<Fun> {
  let module = test_module(hooks, module_name);

  let builder = FunBuilder::<u8>::new(hooks.manage_str(name), module, Arity::default());

  hooks.manage_obj(builder.build(hooks))
}

pub fn test_fun_builder<T: Default>(
  hooks: &GcHooks,
  name: &str,
  module_name: &str,
) -> FunBuilder<T> {
  let module = test_module(hooks, module_name);
  FunBuilder::new(hooks.manage_str(name), module, Arity::default())
}

pub fn test_object_class(hooks: &GcHooks) -> GcObj<Class> {
  let mut object_class = hooks.manage_obj(Class::bare(hooks.manage_str("Object")));
  let mut class_class = hooks.manage_obj(Class::bare(hooks.manage_str("Class")));
  class_class.inherit(hooks, object_class);

  let class_copy = class_class;
  class_class.set_meta(class_copy);

  // create object's meta class
  let mut object_meta_class = hooks.manage_obj(Class::bare(
    hooks.manage_str(format!("{} metaClass", &*object_class.name())),
  ));

  object_meta_class.inherit(hooks, class_class);
  object_meta_class.set_meta(class_class);

  object_class.set_meta(object_meta_class);
  object_class
}
