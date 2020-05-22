use spacelox_core::{
  arity::ArityKind,
  hooks::Hooks,
  io::StdIo,
  managed::Trace,
  module::Module,
  native::{NativeMeta, NativeMethod},
  package::Package,
  value::Value,
  object::Class,
  CallResult, ModuleResult,
};
use crate::support::to_dyn_method;

pub const CLOSURE_CLASS_NAME: &'static str = "Fun";

const CLOSURE_NAME: NativeMeta = NativeMeta::new("name", ArityKind::Fixed(0));
const CLOSURE_SIZE: NativeMeta = NativeMeta::new("size", ArityKind::Fixed(0));
const CLOSURE_CALL: NativeMeta = NativeMeta::new("call", ArityKind::Variadic(0));

pub fn declare_closure_class(hooks: &Hooks, self_module: &mut Module) -> ModuleResult<()> {
  let name = hooks.manage_str(String::from(CLOSURE_CLASS_NAME));
  let class = hooks.manage(Class::new(name));

  self_module.add_export(hooks, name, Value::from(class))
}

pub fn define_closure_class(hooks: &Hooks, self_module: &Module, _: &Package) {
  let name = hooks.manage_str(String::from(CLOSURE_CLASS_NAME));
  let mut class = self_module.get_symbol(hooks, name).unwrap().to_class();

  class.add_method(
    hooks,
    hooks.manage_str(String::from(CLOSURE_NAME.name)),
    Value::from(to_dyn_method(hooks, ClosureName::new())),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(CLOSURE_SIZE.name)),
    Value::from(to_dyn_method(hooks, ClosureSize::new())),
  );

  class.add_method(
    hooks,
    hooks.manage_str(String::from(CLOSURE_CALL.name)),
    Value::from(to_dyn_method(hooks, ClosureCall::new())),
  );
}

#[derive(Clone, Debug, Trace)]
struct ClosureName {
  meta: &'static NativeMeta,
}

impl ClosureName {
  fn new() -> Self {
    Self {
      meta: &CLOSURE_NAME,
    }
  }
}

impl NativeMethod for ClosureName {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, _hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    Ok(Value::from(this.to_closure().fun.name))
  }
}

#[derive(Clone, Debug, Trace)]
struct ClosureSize {
  meta: &'static NativeMeta,
}

impl ClosureSize {
  fn new() -> Self {
    Self {
      meta: &CLOSURE_SIZE,
    }
  }
}

impl NativeMethod for ClosureSize {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, _hooks: &mut Hooks, this: Value, _args: &[Value]) -> CallResult {
    let req = match this.to_closure().fun.arity {
      ArityKind::Default(req, _) => req,
      ArityKind::Fixed(req) => req,
      ArityKind::Variadic(req) => req,
    };

    Ok(Value::from(req as f64))
  }
}

#[derive(Clone, Debug, Trace)]
struct ClosureCall {
  meta: &'static NativeMeta,
}

impl ClosureCall {
  fn new() -> Self {
    Self {
      meta: &CLOSURE_CALL,
    }
  }
}

impl NativeMethod for ClosureCall {
  fn meta(&self) -> &NativeMeta {
    &self.meta
  }

  fn call(&self, hooks: &mut Hooks, this: Value, args: &[Value]) -> CallResult {
    hooks.call(this, args)
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod name {
    use super::*;
    use crate::support::{test_native_dependencies, TestContext};
    use spacelox_core::object::{Closure, Fun};

    #[test]
    fn new() {
      let closure_name = ClosureName::new();

      assert_eq!(closure_name.meta.name, "name");
      assert_eq!(closure_name.meta.arity, ArityKind::Fixed(0));
    }

    #[test]
    fn call() {
      let closure_name = ClosureName::new();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let fun = hooks.manage(Fun::new(hooks.manage_str(String::from("example"))));
      let closure = hooks.manage(Closure::new(fun));

      let result1 = closure_name.call(&mut hooks, Value::from(closure), &[]);

      match result1 {
        Ok(r) => assert_eq!(&*r.to_str(), "example"),
        Err(_) => assert!(false),
      }
    }
  }

  mod size {
    use super::*;
    use crate::support::{test_native_dependencies, TestContext};
    use spacelox_core::object::{Closure, Fun};

    #[test]
    fn new() {
      let closure_name = ClosureSize::new();

      assert_eq!(closure_name.meta.name, "size");
      assert_eq!(closure_name.meta.arity, ArityKind::Fixed(0));
    }

    #[test]
    fn call() {
      let closure_name = ClosureSize::new();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[]);
      let mut hooks = Hooks::new(&mut context);

      let mut fun = hooks.manage(Fun::new(hooks.manage_str(String::from("example"))));
      fun.arity = ArityKind::Fixed(4);

      let closure = hooks.manage(Closure::new(fun));

      let result = closure_name.call(&mut hooks, Value::from(closure), &[]);
      match result {
        Ok(r) => assert_eq!(r.to_num(), 4.0),
        Err(_) => assert!(false),
      }

      fun.arity = ArityKind::Default(2, 2);
      let result = closure_name.call(&mut hooks, Value::from(closure), &[]);
      match result {
        Ok(r) => assert_eq!(r.to_num(), 2.0),
        Err(_) => assert!(false),
      }

      fun.arity = ArityKind::Variadic(5);
      let result = closure_name.call(&mut hooks, Value::from(closure), &[]);
      match result {
        Ok(r) => assert_eq!(r.to_num(), 5.0),
        Err(_) => assert!(false),
      }
    }
  }

  mod call {
    use super::*;
    use crate::support::{test_native_dependencies, TestContext};
    use spacelox_core::object::{Closure, Fun};

    #[test]
    fn new() {
      let closure_call = ClosureCall::new();

      assert_eq!(closure_call.meta.name, "call");
      assert_eq!(closure_call.meta.arity, ArityKind::Variadic(0));
    }

    #[test]
    fn call() {
      let closure_call = ClosureCall::new();
      let gc = test_native_dependencies();
      let mut context = TestContext::new(&gc, &[Value::from(4.3)]);
      let mut hooks = Hooks::new(&mut context);

      let mut fun = hooks.manage(Fun::new(hooks.manage_str(String::from("example"))));
      fun.arity = ArityKind::Fixed(1);

      let closure = hooks.manage(Closure::new(fun));

      let args = &[Value::from(hooks.manage_str(String::from("input")))];
      let result1 = closure_call.call(&mut hooks, Value::from(closure), args);

      match result1 {
        Ok(r) => assert_eq!(r.to_num(), 4.3),
        Err(_) => assert!(false),
      }
    }
  }
}
