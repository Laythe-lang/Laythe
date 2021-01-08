use laythe_core::{
  hooks::GcHooks,
  module::Module,
  native::Native,
  object::{Class, Instance},
  package::{Import, Package},
  value::Value,
};
use laythe_env::managed::{Gc, GcStr};

pub fn to_dyn_native<T: 'static + Native>(hooks: &GcHooks, method: T) -> Gc<Box<dyn Native>> {
  hooks.manage(Box::new(method) as Box<dyn Native>)
}

pub fn default_class_inheritance(
  hooks: &GcHooks,
  package: &Package,
  class_name: &str,
) -> Result<Gc<Class>, GcStr> {
  let name = hooks.manage_str(class_name);

  let import = Import::from_str(hooks, GLOBAL_PATH);
  let module = package.import(hooks, import)?;

  let object_class = load_class_from_module(hooks, &*module, "Object")?;

  Ok(Class::with_inheritance(hooks, name, object_class))
}

pub fn default_error_inheritance(
  hooks: &GcHooks,
  package: &Package,
  class_name: &str,
) -> Result<Gc<Class>, GcStr> {
  let name = hooks.manage_str(class_name);

  let import = Import::from_str(hooks, GLOBAL_PATH);
  let module = package.import(hooks, import)?;

  let error_class = load_class_from_module(hooks, &*module, "Error")?;
  Ok(Class::with_inheritance(hooks, name, error_class))
}

pub fn load_class_from_package(
  hooks: &GcHooks,
  package: &Package,
  path: &str,
  name: &str,
) -> Result<Gc<Class>, GcStr> {
  let name = hooks.manage_str(name);
  let import: Import = Import::from_str(hooks, path);

  let module = package.import(hooks, import)?;
  match module.import(hooks).get_field(&name) {
    Some(symbol) => {
      if symbol.is_class() {
        Ok(symbol.to_class())
      } else {
        Err(hooks.manage_str(format!("Symbol {} is not a class.", name)))
      }
    }
    None => Err(hooks.manage_str(format!(
      "Could not find symbol {} in module {}.",
      name,
      module.name()
    ))),
  }
}

pub fn load_class_from_module(
  hooks: &GcHooks,
  module: &Module,
  name: &str,
) -> Result<Gc<Class>, GcStr> {
  let name = hooks.manage_str(name);
  match module.import(hooks).get_field(&name) {
    Some(symbol) => {
      if symbol.is_class() {
        Ok(symbol.to_class())
      } else {
        Err(hooks.manage_str(format!("Symbol {} is not a class.", name)))
      }
    }
    None => Err(hooks.manage_str(format!(
      "Could not find symbol {} in module {}.",
      name,
      module.name()
    ))),
  }
}

pub fn load_instance_from_module(
  hooks: &GcHooks,
  module: &Module,
  name: &str,
) -> Result<Gc<Instance>, GcStr> {
  let name = hooks.manage_str(name);
  match module.import(hooks).get_field(&name) {
    Some(symbol) => {
      if symbol.is_instance() {
        Ok(symbol.to_instance())
      } else {
        Err(hooks.manage_str(format!("Symbol {} is not a instance.", name)))
      }
    }
    None => Err(hooks.manage_str(format!(
      "Could not find symbol {} in module {}.",
      name,
      module.name()
    ))),
  }
}

pub fn export_and_insert(
  hooks: &GcHooks,
  module: &mut Module,
  name: GcStr,
  symbol: Value,
) -> Result<(), GcStr> {
  module.insert_symbol(hooks, name, symbol);
  module.export_symbol(hooks, name)
}

#[cfg(test)]
pub use self::test::*;
use crate::GLOBAL_PATH;

#[cfg(test)]
mod test {
  use crate::{
    builtin::{builtin_from_module, BuiltIn},
    create_std_lib, native, GLOBAL_PATH,
  };
  use laythe_core::{
    hooks::{GcContext, GcHooks, HookContext, Hooks, ValueContext},
    iterator::LyIter,
    module::Module,
    native::Native,
    native::{MetaData, NativeMeta, NativeMetaBuilder},
    object::Class,
    object::Fun,
    object::List,
    package::Import,
    signature::Arity,
    signature::{ParameterBuilder, ParameterKind},
    val,
    value::{Value, ValueKind, VALUE_NIL},
    Call,
  };
  use laythe_env::{
    io::Io,
    managed::{Gc, GcStr, Trace, TraceRoot},
    memory::{Allocator, NoGc},
    stdio::support::{IoStdioTest, StdioTestContainer},
  };
  use std::{cell::RefCell, io::Write, path::PathBuf, sync::Arc};

  use super::to_dyn_native;

  pub struct MockedContext {
    pub gc: RefCell<Allocator>,
    pub responses: Vec<Value>,
    io: Io,
    no_gc: NoGc,
    builtin: Option<BuiltIn>,
    response_count: usize,
  }

  impl Default for MockedContext {
    fn default() -> Self {
      Self {
        gc: RefCell::default(),
        no_gc: NoGc(),
        responses: vec![],
        io: Io::default(),
        builtin: None,
        response_count: 0,
      }
    }
  }

  impl MockedContext {
    pub fn new(responses: &[Value]) -> Self {
      Self {
        gc: RefCell::default(),
        no_gc: NoGc(),
        responses: Vec::from(responses),
        io: Io::default(),
        builtin: None,
        response_count: 0,
      }
    }

    pub fn with_std(responses: &[Value]) -> Self {
      let mut context = Self {
        gc: RefCell::default(),
        no_gc: NoGc(),
        responses: Vec::from(responses),
        io: Io::default(),
        builtin: None,
        response_count: 0,
      };

      let hooks = GcHooks::new(&mut context);
      let std = create_std_lib(&hooks).unwrap();
      let global = std
        .import(&hooks, Import::from_str(&hooks, GLOBAL_PATH))
        .expect("Could not retrieve global module");

      let builtin = builtin_from_module(&hooks, &global);

      context.builtin = builtin;
      context
    }

    pub fn with_test_stdio(stdio_container: &Arc<StdioTestContainer>) -> Self {
      Self {
        gc: RefCell::default(),
        no_gc: NoGc(),
        responses: Vec::from(vec![]),
        io: Io::default().with_stdio(Arc::new(IoStdioTest::new(stdio_container))),
        builtin: None,
        response_count: 0,
      }
    }

    pub fn add_responses(&mut self, responses: &[Value]) {
      self.responses.extend_from_slice(responses);
    }
  }

  impl HookContext for MockedContext {
    fn gc_context(&self) -> &dyn GcContext {
      self
    }

    fn value_context(&mut self) -> &mut dyn ValueContext {
      self
    }

    fn io(&mut self) -> Io {
      self.io.clone()
    }
  }

  impl GcContext for MockedContext {
    fn gc(&self) -> std::cell::RefMut<'_, Allocator> {
      self.gc.borrow_mut()
    }
  }

  impl ValueContext for MockedContext {
    fn call(&mut self, callable: Value, args: &[Value]) -> Call {
      let arity = match callable.kind() {
        ValueKind::Closure => callable.to_closure().fun.arity,
        ValueKind::Method => callable.to_method().method.to_closure().fun.arity,
        ValueKind::Native => callable.to_native().meta().signature.arity,
        _ => return Call::Exit(1),
      };

      match arity.check(args.len() as u8) {
        Ok(_) => (),
        Err(_) => {
          return Call::Exit(1);
        }
      }

      if self.response_count < self.responses.len() {
        let response = self.responses[self.response_count];
        self.response_count += 1;
        return Call::Ok(response);
      }

      Call::Exit(1)
    }

    fn call_method(&mut self, _this: Value, method: Value, args: &[Value]) -> Call {
      let arity = match method.kind() {
        ValueKind::Closure => method.to_closure().fun.arity,
        ValueKind::Method => method.to_method().method.to_closure().fun.arity,
        ValueKind::Native => method.to_native().meta().signature.arity,
        _ => {
          return Call::Exit(1);
        }
      };

      match arity.check(args.len() as u8) {
        Ok(_) => (),
        Err(_) => {
          return Call::Exit(1);
        }
      }

      if self.response_count < self.responses.len() {
        let response = self.responses[self.response_count];
        self.response_count += 1;
        return Call::Ok(response);
      }

      Call::Exit(1)
    }

    fn get_method(&mut self, this: Value, method_name: GcStr) -> Call {
      let b = match &self.builtin {
        Some(b) => b,
        None => return Call::Exit(1),
      };

      let class = b.primitives.for_value(this, this.kind());
      match class.get_method(&method_name) {
        Some(method) => Call::Ok(method),
        None => Call::Exit(1),
      }
    }

    fn get_class(&mut self, this: Value) -> Value {
      let b = match &self.builtin {
        Some(b) => b,
        None => return VALUE_NIL,
      };

      val!(b.primitives.for_value(this, this.kind()))
    }
  }

  impl TraceRoot for MockedContext {
    fn trace(&self) {
      self.no_gc.trace()
    }

    fn trace_debug(&self, log: &mut dyn Write) {
      self.no_gc.trace_debug(log)
    }

    fn can_collect(&self) -> bool {
      false
    }
  }

  pub fn test_native_dependencies() -> Box<Allocator> {
    Box::new(Allocator::default())
  }

  #[derive(Debug)]
  pub struct TestIterator {
    current: usize,
  }

  impl TestIterator {
    fn new() -> Self {
      Self { current: 0 }
    }
  }

  impl LyIter for TestIterator {
    fn name(&self) -> &str {
      "Test Iterator"
    }

    fn current(&self) -> Value {
      val!(self.current as f64)
    }

    fn next(&mut self, _hooks: &mut Hooks) -> Call {
      if self.current > 3 {
        return Call::Ok(val!(false));
      }

      self.current += 1;
      Call::Ok(val!(true))
    }

    fn size_hint(&self) -> Option<usize> {
      Some(4)
    }

    fn size(&self) -> usize {
      8
    }
  }

  impl Trace for TestIterator {}

  pub fn test_iter() -> Box<dyn LyIter> {
    Box::new(TestIterator::new())
  }

  pub fn fun_from_hooks(hooks: &GcHooks, name: &str, module_name: &str) -> Gc<Fun> {
    let module = Module::from_path(
      &hooks,
      hooks.manage(PathBuf::from(format!("path/{}.ly", module_name))),
    )
    .expect("TODO");

    let module = hooks.manage(module);
    hooks.manage(Fun::new(hooks.manage_str(name), module))
  }

  pub fn test_error_class(hooks: &GcHooks) -> Gc<Class> {
    let mut error_class = Class::bare(hooks.manage_str("Error"));

    error_class.add_method(
      hooks,
      hooks.manage_str("init"),
      val!(to_dyn_native(hooks, TestInit::from(hooks))),
    );

    hooks.manage(error_class)
  }

  const ERROR_INIT: NativeMetaBuilder = NativeMetaBuilder::method("init", Arity::Default(1, 2))
    .with_params(&[
      ParameterBuilder::new("message", ParameterKind::String),
      ParameterBuilder::new("inner", ParameterKind::Instance),
    ]);

  native!(TestInit, ERROR_INIT);

  impl Native for TestInit {
    fn call(&self, _hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
      let mut this = this.unwrap().to_instance();
      this[0] = args[0];
      this[1] = val!(_hooks.manage(List::new()));

      if args.len() > 1 {
        this[2] = args[1];
      }

      Call::Ok(val!(this))
    }
  }
}
