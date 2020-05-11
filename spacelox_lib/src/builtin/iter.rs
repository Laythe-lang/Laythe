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
const ITER_NEXT: NativeMeta = NativeMeta::new("next", ArityKind::Fixed(0));

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

  class.add_method(
    hooks,
    hooks.manage_str(String::from(ITER_NEXT.name)),
    Value::NativeMethod(hooks.manage(Box::new(IterNext::new()))),
  );
}

#[derive(Clone, Debug, Trace)]
struct IterStr {
  meta: &'static NativeMeta,
}

impl IterStr {
  fn new() -> Self {
    Self { meta: &ITER_STR }
  }
}

impl NativeMethod for IterStr {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    Ok(this.to_iter().name(hooks))
  }
}

#[derive(Clone, Debug, Trace)]
struct IterNext {
  meta: &'static NativeMeta,
}

impl IterNext {
  fn new() -> Self {
    Self { meta: &ITER_NEXT }
  }
}

impl NativeMethod for IterNext {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    Ok(this.to_iter().next(hooks))
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use spacelox_core::{iterator::SlIter, managed::Managed};

  #[derive(Trace, Debug)]
  struct TestIterator {
    current: usize,
  }

  impl TestIterator {
    fn new() -> Self {
      Self { current: 0 }
    }
  }

  impl SlIter for TestIterator {
    fn name(&self) -> &str {
      "Test Iterator"
    }

    fn current(&self) -> Value {
      Value::Number(self.current as f64)
    }

    fn next(&mut self, _hooks: &Hooks) -> Value {
      if self.current > 4 {
        return Value::Nil;
      }

      self.current += 1;
      Value::Number(self.current as f64 - 1.0)
    }

    fn size(&self) -> usize {
      8
    }
  }

  fn test_input(hooks: &Hooks) -> (Box<dyn SlIter>, Managed<Class>) {
    (
      Box::new(TestIterator::new()),
      hooks.manage(Class::new(hooks.manage_str(String::from("test")))),
    )
  }

  #[cfg(test)]
  mod str {
    use super::*;
    use crate::support::{test_native_dependencies, TestContext};
    use spacelox_core::iterator::SlIterator;

    #[test]
    fn new() {
      let iter_str = IterStr::new();

      assert_eq!(iter_str.meta.name, "str");
      assert_eq!(iter_str.meta.arity, ArityKind::Fixed(0));
    }

    #[test]
    fn call() {
      let iter_str = IterStr::new();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let (iter, class) = test_input(&hooks);
      let this = hooks.manage(SlIterator::new(iter, class));

      let result = iter_str.call(&mut hooks, Value::Iter(this), &[]);
      match result {
        Ok(r) => assert_eq!(&*r.to_str(), "Test Iterator"),
        Err(_) => assert!(false),
      }
    }
  }
}
