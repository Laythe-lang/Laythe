use crate::{
  global::{ERROR_CLASS_NAME, OBJECT_CLASS_NAME},
  StdError, StdResult, STD,
};
use laythe_core::{
  hooks::GcHooks,
  if_let_obj,
  module::{Import, Module, Package},
  object::{Class, Instance, Native, ObjectKind},
  to_obj_kind,
  value::Value,
  ObjRef, Ref,
};

pub fn default_class_inheritance(
  hooks: &GcHooks,
  package: Ref<Package>,
  class_name: &str,
) -> StdResult<ObjRef<Class>> {
  let name = hooks.manage_str(class_name);

  let import = Import::from_str(hooks, STD)?;
  let module = package.import(import)?;
  let object_class = match module.get_exported_symbol_by_name(hooks.manage_str(OBJECT_CLASS_NAME)) {
    Some(class) => class,
    None => return Err(StdError::SymbolNotFound),
  };

  if_let_obj!(ObjectKind::Class(class) = (object_class) {
    Ok(Class::with_inheritance(
      hooks,
      name,
      class,
    ))
  } else {
    Err(StdError::SymbolNotClass)
  })
}

pub fn default_error_inheritance(
  hooks: &GcHooks,
  package: Ref<Package>,
  class_name: &str,
) -> StdResult<ObjRef<Class>> {
  let name = hooks.manage_str(class_name);
  let error_name = hooks.manage_str(ERROR_CLASS_NAME);

  let import = Import::from_str(hooks, STD)?;
  let module = package.import(import)?;
  let object_class = match module.get_exported_symbol_by_name(hooks.manage_str(error_name)) {
    Some(class) => class,
    None => return Err(StdError::SymbolNotFound),
  };

  if_let_obj!(ObjectKind::Class(class) = (object_class) {
    Ok(Class::with_inheritance(
      hooks,
      name,
      class,
    ))
  } else {
    Err(StdError::SymbolNotClass)
  })
}

pub fn load_class_from_package(
  hooks: &GcHooks,
  package: Ref<Package>,
  path: &str,
  name: &str,
) -> StdResult<ObjRef<Class>> {
  let name = hooks.manage_str(name);
  let import = Import::from_str(hooks, path)?;
  let module = package.import(import)?;
  let symbol = match module.get_exported_symbol_by_name(hooks.manage_str(name)) {
    Some(class) => class,
    None => return Err(StdError::SymbolNotFound),
  };

  if_let_obj!(ObjectKind::Class(class) = (symbol) {
    Ok(class)
  } else {
    Err(StdError::SymbolNotClass)
  })
}

pub fn load_class_from_module(
  hooks: &GcHooks,
  module: Ref<Module>,
  name: &str,
) -> StdResult<ObjRef<Class>> {
  let name = hooks.manage_str(name);
  let symbol = match module.get_exported_symbol_by_name(name) {
    Some(symbol) => symbol,
    None => return Err(StdError::SymbolNotFound),
  };

  if_let_obj!(ObjectKind::Class(class) = (symbol) {
    Ok(class)
  } else {
    Err(StdError::SymbolNotClass)
  })
}

pub fn load_instance_from_module(
  hooks: &GcHooks,
  module: Ref<Module>,
  name: &str,
) -> StdResult<Instance> {
  let name = hooks.manage_str(name);
  match module.get_exported_symbol_by_name(name) {
    Some(symbol) => {
      if_let_obj!(ObjectKind::Instance(instance) = (symbol) {
        Ok(instance)
      } else {
        Err(StdError::SymbolNotInstance)
      })
    },
    None => Err(StdError::SymbolNotFound),
  }
}

pub fn export_and_insert_native(
  hooks: &GcHooks,
  mut module: Ref<Module>,
  native: ObjRef<Native>,
) -> StdResult<()> {
  let name = native.name();
  module.insert_symbol(hooks, name, Value::from(native))?;
  module.export_symbol(name).map_err(StdError::from)
}

pub fn export_and_insert<S: AsRef<str>>(
  hooks: &GcHooks,
  mut module: Ref<Module>,
  name: S,
  symbol: Value,
) -> StdResult<()> {
  let name = hooks.manage_str(name);
  module.insert_symbol(hooks, name, symbol)?;
  module.export_symbol(name).map_err(StdError::from)
}

#[cfg(test)]
pub use self::test::*;

#[cfg(test)]
mod test {
  use super::*;
  use crate::{
    builtin::{builtin_from_module, BuiltIn},
    create_std_lib, native,
  };
  use laythe_core::{
    hooks::{GcContext, GcHooks, HookContext, Hooks, ValueContext}, list, managed::{DebugHeap, Trace, TraceRoot}, match_obj, module::{module_class, ImportResult, Module}, object::{Class, Enumerate, Fun, FunBuilder, LyNative, LyStr, Native, NativeMetaBuilder}, signature::{Arity, ParameterBuilder, ParameterKind}, to_obj_kind, utils::IdEmitter, val, value::Value, Allocator, Call, Chunk, LyError
  };
  use laythe_env::{
    io::Io,
    stdio::support::{IoStdioTest, StdioTestContainer},
  };
  use std::{cell::RefCell, io::Write, sync::Arc};

  pub struct MockedContext {
    pub gc: RefCell<Allocator>,
    pub responses: Vec<Value>,
    io: Io,
    pub builtin: Option<BuiltIn>,
    response_count: usize,
  }

  impl Default for MockedContext {
    fn default() -> Self {
      Self {
        gc: RefCell::default(),
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
        responses: Vec::from(responses),
        io: Io::default(),
        builtin: None,
        response_count: 0,
      }
    }

    pub fn with_std(responses: &[Value]) -> ImportResult<Self> {
      let mut context = Self {
        gc: RefCell::default(),
        responses: Vec::from(responses),
        io: Io::default(),
        builtin: None,
        response_count: 0,
      };

      let hooks = GcHooks::new(&context);
      let mut emitter = IdEmitter::default();
      let std = create_std_lib(&hooks, &mut emitter).unwrap();

      let builtin = builtin_from_module(&hooks, &std.root_module());

      context.builtin = builtin;
      Ok(context)
    }

    pub fn with_test_stdio(stdio_container: &Arc<StdioTestContainer>) -> Self {
      Self {
        gc: RefCell::default(),
        responses: vec![],
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
      if !callable.is_obj() {
        return Err(LyError::Exit(1));
      }

      let result = match_obj!((&callable.to_obj()) {
        ObjectKind::Fun(fun) => {
          fun.check_if_valid_call(|| GcHooks::new(self), args.len() as u8)
        },
        ObjectKind::Closure(closure) => {
          closure.fun().check_if_valid_call(|| GcHooks::new(self), args.len() as u8)
        },
        ObjectKind::Method(method) => {
          method.method().to_obj().to_closure().fun().check_if_valid_call(|| GcHooks::new(self), args.len() as u8)
        },
        ObjectKind::Native(native) => {
          native.check_if_valid_call(|| GcHooks::new(self), args)
        },
        _ => return Err(LyError::Exit(1)),
      });

      match result {
        Ok(_) => (),
        Err(_) => {
          return Err(LyError::Exit(1));
        },
      }

      if self.response_count < self.responses.len() {
        let response = self.responses[self.response_count];
        self.response_count += 1;
        return Call::Ok(response);
      }

      Err(LyError::Exit(1))
    }

    fn call_method(&mut self, this: Value, method: Value, args: &[Value]) -> Call {
      if !method.is_obj() {
        return Err(LyError::Exit(1));
      }

      let result = match_obj!((&method.to_obj()) {
        ObjectKind::Fun(fun) => {
          fun.check_if_valid_call(|| GcHooks::new(self), args.len() as u8)
        },
        ObjectKind::Closure(closure) => {
          closure.fun().check_if_valid_call(|| GcHooks::new(self), args.len() as u8)
        },
        ObjectKind::Method(method) => {
          method.method().to_obj().to_closure().fun().check_if_valid_call(|| GcHooks::new(self), args.len() as u8)
        },
        ObjectKind::Native(native) => {
          let mut augmented_args = vec![this];
          augmented_args.extend_from_slice(args);

          native.check_if_valid_call(|| GcHooks::new(self), &augmented_args)
        },
        _ => return Err(LyError::Exit(1)),
      });

      match result {
        Ok(_) => (),
        Err(_) => {
          return Err(LyError::Exit(1));
        },
      }

      if self.response_count < self.responses.len() {
        let response = self.responses[self.response_count];
        self.response_count += 1;
        return Call::Ok(response);
      }

      Err(LyError::Exit(1))
    }

    fn get_method(&mut self, this: Value, method_name: LyStr) -> Call {
      let b = match &self.builtin {
        Some(b) => b,
        None => return Err(LyError::Exit(1)),
      };

      let class = b.primitives.for_value(this);
      match class.get_method(&method_name) {
        Some(method) => Call::Ok(method),
        None => Err(LyError::Exit(1)),
      }
    }

    fn get_class(&mut self, this: Value) -> ObjRef<Class> {
      let b = self
        .builtin
        .as_ref()
        .expect("Expected built in class to be defined");

      b.primitives.for_value(this)
    }

    fn scan_roots(&mut self) {}
  }

  impl TraceRoot for MockedContext {
    fn trace(&self) {}

    fn trace_debug(&self, log: &mut dyn Write) {}

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

  impl Enumerate for TestIterator {
    fn name(&self) -> &str {
      "Test"
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

    fn as_debug(&self) -> &dyn DebugHeap {
      self
    }
  }

  impl Trace for TestIterator {}

  impl DebugHeap for TestIterator {
    fn fmt_heap(&self, f: &mut std::fmt::Formatter, _: usize) -> std::fmt::Result {
      f.write_fmt(format_args!("{:?}", self))
    }
  }

  pub fn test_iter() -> Box<dyn Enumerate> {
    Box::new(TestIterator::new())
  }

  pub fn test_class(hooks: &GcHooks, name: &str) -> ObjRef<Class> {
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
    Class::with_inheritance(hooks, hooks.manage_str(name), object_class)
  }

  pub fn test_module(hooks: &GcHooks, name: &str) -> Ref<Module> {
    let base_class = test_class(hooks, "Module");
    hooks.manage(Module::new(
      hooks,
      module_class(hooks, name, base_class),
      "example",
      0,
    ))
  }

  pub fn test_fun(hooks: &GcHooks, name: &str, module_name: &str) -> ObjRef<Fun> {
    let module = test_module(hooks, module_name);

    let builder = FunBuilder::new(hooks.manage_str(name), module, Arity::default());

    hooks.manage_obj(builder.build(Chunk::stub(hooks)))
  }

  pub fn test_fun_builder(
    hooks: &GcHooks,
    name: &str,
    module_name: &str,
    arity: Arity,
  ) -> FunBuilder {
    let module = test_module(hooks, module_name);
    FunBuilder::new(hooks.manage_str(name), module, arity)
  }

  pub fn test_error_class(hooks: &GcHooks) -> ObjRef<Class> {
    let mut error_class = Class::bare(hooks.manage_str("Error"));

    error_class.add_method(hooks.manage_str("init"), val!(TestInit::native(hooks)));

    hooks.manage_obj(error_class)
  }

  const ERROR_INIT: NativeMetaBuilder = NativeMetaBuilder::method("init", Arity::Default(1, 2))
    .with_params(&[
      ParameterBuilder::new("message", ParameterKind::String),
      ParameterBuilder::new("inner", ParameterKind::Object),
    ]);

  native!(TestInit, ERROR_INIT);

  impl LyNative for TestInit {
    fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
      let mut this = args[0].to_obj().to_instance();
      this[0] = args[0];
      this[1] = val!(_hooks.manage_obj(list!()));

      if args.len() > 1 {
        this[2] = args[1];
      }

      Call::Ok(val!(this))
    }
  }
}
