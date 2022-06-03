use crate::{
  global::{ERROR_CLASS_NAME, OBJECT_CLASS_NAME},
  StdError, StdResult, STD,
};
use laythe_core::{
  hooks::GcHooks,
  if_let_obj,
  managed::{Gc, GcObj, GcStr, Instance},
  module::{Import, Module, Package},
  object::{Class, ObjectKind},
  to_obj_kind,
  value::Value,
};

pub fn default_class_inheritance(
  hooks: &GcHooks,
  package: Gc<Package>,
  class_name: &str,
) -> StdResult<GcObj<Class>> {
  let name = hooks.manage_str(class_name);

  let import = Import::from_str(hooks, STD)?;
  let module = package.import(hooks, import)?;
  let object_class = match module.get_exported_symbol(hooks.manage_str(OBJECT_CLASS_NAME)) {
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
  package: Gc<Package>,
  class_name: &str,
) -> StdResult<GcObj<Class>> {
  let name = hooks.manage_str(class_name);
  let error_name = hooks.manage_str(ERROR_CLASS_NAME);

  let import = Import::from_str(hooks, STD)?;
  let module = package.import(hooks, import)?;
  let object_class = match module.get_exported_symbol(hooks.manage_str(error_name)) {
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
  package: Gc<Package>,
  path: &str,
  name: &str,
) -> StdResult<GcObj<Class>> {
  let name = hooks.manage_str(name);
  let import = Import::from_str(hooks, path)?;
  let module = package.import(hooks, import)?;
  let symbol = match module.get_exported_symbol(hooks.manage_str(name)) {
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
  module: Gc<Module>,
  name: &str,
) -> StdResult<GcObj<Class>> {
  let name = hooks.manage_str(name);
  let symbol = match module.get_exported_symbol(name) {
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
  module: Gc<Module>,
  name: &str,
) -> StdResult<Instance> {
  let name = hooks.manage_str(name);
  match module.get_exported_symbol(name) {
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

pub fn export_and_insert(mut module: Gc<Module>, name: GcStr, symbol: Value) -> StdResult<()> {
  module.insert_symbol(name, symbol)?;
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
    hooks::{GcContext, GcHooks, HookContext, Hooks, ValueContext},
    managed::{DebugHeap, GcObj, GcObject, GcStr, Trace, TraceRoot},
    match_obj,
    memory::{Allocator, NoGc},
    module::{module_class, ImportResult, Module},
    object::{Class, Enumerate, Fun, FunBuilder, List, LyNative, Native, NativeMetaBuilder},
    signature::Arity,
    signature::{ParameterBuilder, ParameterKind},
    to_obj_kind,
    utils::IdEmitter,
    val,
    value::{Value, VALUE_NIL},
    Call, LyError, chunk::Chunk,
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

    pub fn with_std(responses: &[Value]) -> ImportResult<Self> {
      let mut context = Self {
        gc: RefCell::default(),
        no_gc: NoGc(),
        responses: Vec::from(responses),
        io: Io::default(),
        builtin: None,
        response_count: 0,
      };

      let hooks = GcHooks::new(&mut context);
      let mut emitter = IdEmitter::default();
      let std = create_std_lib(&hooks, &mut emitter).unwrap();

      let builtin = builtin_from_module(&hooks, &std.root_module());

      context.builtin = builtin;
      Ok(context)
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
      if !callable.is_obj() {
        return Err(LyError::Exit(1));
      }

      let arity = match_obj!((&callable.to_obj()) {
        ObjectKind::Fun(fun) => {
          *fun.arity()
        },
        ObjectKind::Closure(closure) => {
          *closure.fun().arity()
        },
        ObjectKind::Method(method) => {
          *method.method().to_obj().to_closure().fun().arity()
        },
        ObjectKind::Native(native) => {
          native.meta().signature.arity
        },
        _ => return Err(LyError::Exit(1)),
      });

      match arity.check(args.len() as u8) {
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

    fn call_method(&mut self, _this: Value, method: Value, args: &[Value]) -> Call {
      if !method.is_obj() {
        return Err(LyError::Exit(1));
      }

      let arity = match_obj!((&method.to_obj()) {
        ObjectKind::Closure(closure) => {
          *closure.fun().arity()
        },
        ObjectKind::Method(method) => {
          *method.method().to_obj().to_closure().fun().arity()
        },
        ObjectKind::Native(native) => {
          native.meta().signature.arity
        },
        _ => return Err(LyError::Exit(1)),
      });

      match arity.check(args.len() as u8) {
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

    fn get_method(&mut self, this: Value, method_name: GcStr) -> Call {
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

    fn get_class(&mut self, this: Value) -> Value {
      let b = match &self.builtin {
        Some(b) => b,
        None => return VALUE_NIL,
      };

      val!(b.primitives.for_value(this))
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

  pub fn test_class(hooks: &GcHooks, name: &str) -> GcObj<Class> {
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

  pub fn test_module(hooks: &GcHooks, name: &str) -> Gc<Module> {
    let base_class = test_class(hooks, "Module");
    hooks.manage(Module::new(module_class(hooks, name, base_class), 0))
  }

  pub fn test_fun(hooks: &GcHooks, name: &str, module_name: &str) -> GcObj<Fun> {
    let module = test_module(hooks, module_name);

    let builder = FunBuilder::new(hooks.manage_str(name), module, Arity::default());

    hooks.manage_obj(builder.build(Chunk::stub(&hooks)))
  }

  pub fn test_fun_builder<T: Default>(
    hooks: &GcHooks,
    name: &str,
    module_name: &str,
    arity: Arity,
  ) -> FunBuilder {
    let module = test_module(&hooks, module_name);
    FunBuilder::new(hooks.manage_str(name), module, arity)
  }

  pub fn test_error_class(hooks: &GcHooks) -> GcObj<Class> {
    let mut error_class = Class::bare(hooks.manage_str("Error"));

    error_class.add_method(hooks.manage_str("init"), val!(TestInit::native(hooks)));

    hooks.manage_obj(error_class)
  }

  const ERROR_INIT: NativeMetaBuilder = NativeMetaBuilder::method("init", Arity::Default(1, 2))
    .with_params(&[
      ParameterBuilder::new("message", ParameterKind::String),
      ParameterBuilder::new("inner", ParameterKind::Instance),
    ]);

  native!(TestInit, ERROR_INIT);

  impl LyNative for TestInit {
    fn call(&self, _hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
      let mut this = this.unwrap().to_obj().to_instance();
      this[0] = args[0];
      this[1] = val!(_hooks.manage_obj(List::new()));

      if args.len() > 1 {
        this[2] = args[1];
      }

      Call::Ok(val!(this))
    }
  }
}
