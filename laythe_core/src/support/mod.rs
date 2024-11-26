use fnv::FnvBuildHasher;
use hashbrown::HashMap;

use crate::{
  chunk::Chunk, hooks::GcHooks, module::{module_class, Module}, object::{Class, Fun, FunBuilder, LyStr}, reference::{ObjRef, Ref}, signature::Arity, value::Value
};


#[derive(Default)]
pub struct ClassBuilder {
  name: String,
  super_cls: Option<ObjRef<Class>>,
  methods: HashMap<LyStr, Value, FnvBuildHasher>,
  fields: Vec<LyStr>,
}

impl ClassBuilder {
  pub fn name(mut self, name: &str) -> Self {
    self.name = name.to_string();
    self
  }

  pub fn super_cls(mut self, super_cls: ObjRef<Class>) -> Self {
    self.super_cls = Some(super_cls);
    self
  }

  pub fn methods(mut self, methods: HashMap<LyStr, Value, FnvBuildHasher>) -> Self {
    self.methods = methods;
    self
  }

  pub fn fields(mut self, fields: Vec<LyStr>) -> Self {
    self.fields = fields;
    self
  }

  pub fn build(self, hooks: &GcHooks) -> ObjRef<Class> {
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

pub fn test_module(hooks: &GcHooks, name: &str) -> Ref<Module> {
  let base_class = test_class(hooks, "Module");
  hooks.manage(Module::new(hooks, module_class(hooks, name, base_class), "example", 0))
}

pub fn test_class(hooks: &GcHooks, name: &str) -> ObjRef<Class> {
  let object_class = test_object_class(hooks);
  Class::with_inheritance(hooks, hooks.manage_str(name), object_class)
}

pub fn test_fun(hooks: &GcHooks, name: &str, module_name: &str) -> ObjRef<Fun> {
  let module = test_module(hooks, module_name);

  let builder = FunBuilder::new(hooks.manage_str(name), module, Arity::default());

  hooks.manage_obj(builder.build(Chunk::stub(hooks)))
}

pub fn test_fun_builder(hooks: &GcHooks, name: &str, module_name: &str) -> FunBuilder {
  let module = test_module(hooks, module_name);
  FunBuilder::new(hooks.manage_str(name), module, Arity::default())
}

pub fn test_object_class(hooks: &GcHooks) -> ObjRef<Class> {
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
