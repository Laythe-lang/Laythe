use spacelox_core::{
  arity::ArityKind,
  hooks::Hooks,
  io::StdIo,
  managed::Trace,
  module::Module,
  native::{NativeMeta, NativeMethod},
  package::Package,
  value::{Class, Value},
  CallResult, ModuleResult,
};

pub const ITER_CLASS_NAME: &'static str = "Iter";
const ITER_STR: NativeMeta = NativeMeta::new("str", ArityKind::Fixed(0));

pub fn declare_iter_class(hooks: &Hooks, self_module: &mut Module) -> ModuleResult<()> {
  let name = hooks.manage_str(String::from(ITER_CLASS_NAME));
  let class = hooks.manage(Class::new(name));

  self_module.add_export(hooks, name, Value::Class(class))
}

pub fn define_iter_class(hooks: &Hooks, self_module: &Module, _: &Package) {
  let name = hooks.manage_str(String::from(ITER_CLASS_NAME));
  let mut class = self_module.get_symbol(hooks, name).unwrap().to_class();

  class.add_method(
    hooks,
    hooks.manage_str(String::from(ITER_STR.name)),
    Value::NativeMethod(hooks.manage(Box::new(IterStr::new()))),
  );
}

#[derive(Clone, Debug, Trace)]
struct IterStr {
  meta: Box<NativeMeta>,
}

impl IterStr {
  fn new() -> Self {
    Self {
      meta: Box::new(ITER_STR),
    }
  }
}

impl NativeMethod for IterStr {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, hooks: &mut Hooks, _this: Value, _args: &[Value]) -> CallResult {
    Ok(Value::String(hooks.manage_str(String::from("Iter"))))
  }
}
