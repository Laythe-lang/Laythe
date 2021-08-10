use std::io::Write;

use laythe_core::{
  hooks::GcHooks,
  managed::{GcObj, Trace},
  module::Module,
  object::{Class, ObjectKind},
  value::{Value, ValueKind},
};

use crate::global::{BOOL_CLASS_NAME, CHANNEL_CLASS_NAME, CLASS_CLASS_NAME, CLOSURE_CLASS_NAME, EXPORT_ERROR_NAME, FIBER_CLASS_NAME, IMPORT_ERROR_NAME, ITER_CLASS_NAME, LIST_CLASS_NAME, MAP_CLASS_NAME, METHOD_CLASS_NAME, METHOD_NOT_FOUND_ERROR_NAME, MODULE_CLASS_NAME, NATIVE_CLASS_NAME, NIL_CLASS_NAME, NUMBER_CLASS_NAME, OBJECT_CLASS_NAME, PROPERTY_ERROR_NAME, RUNTIME_ERROR_NAME, STRING_CLASS_NAME, TYPE_ERROR_NAME, DEADLOCK_ERROR_NAME, VALUE_ERROR_NAME};

pub struct BuiltIn {
  /// built in classes related to dependencies
  pub dependencies: BuiltInDependencies,

  /// built in classes related to primitives
  pub primitives: BuiltInPrimitives,

  /// built in error classes
  pub errors: BuiltInErrors,
}

impl Trace for BuiltIn {
  fn trace(&self) {
    self.primitives.trace();
    self.dependencies.trace();
    self.errors.trace();
  }

  fn trace_debug(&self, stdio: &mut dyn Write) {
    self.primitives.trace_debug(stdio);
    self.dependencies.trace_debug(stdio);
    self.errors.trace_debug(stdio);
  }
}

pub struct BuiltInDependencies {
  /// The
  pub module: GcObj<Class>,
}

impl Trace for BuiltInDependencies {
  fn trace(&self) {
    self.module.trace()
  }

  fn trace_debug(&self, log: &mut dyn Write) {
    self.module.trace_debug(log)
  }
}

pub struct BuiltInPrimitives {
  /// The base Object class
  pub object: GcObj<Class>,

  /// the Nil class
  pub nil: GcObj<Class>,

  /// the Bool class
  pub bool: GcObj<Class>,

  /// The Channel class
  pub channel: GcObj<Class>,

  /// the Class class
  pub class: GcObj<Class>,

  /// the Fiber class
  pub fiber: GcObj<Class>,

  /// the Number class
  pub number: GcObj<Class>,

  /// the String class
  pub string: GcObj<Class>,

  /// the List class
  pub list: GcObj<Class>,

  /// the Map class
  pub map: GcObj<Class>,

  /// the Iter class
  pub iter: GcObj<Class>,

  /// the Closure class
  pub closure: GcObj<Class>,

  /// the method class
  pub method: GcObj<Class>,

  /// the NativeFun class
  pub native_fun: GcObj<Class>,
}

impl BuiltInPrimitives {
  pub fn for_value(&self, value: Value, stack: &[Value]) -> GcObj<Class> {
    match value.kind() {
      ValueKind::Bool => self.bool,
      ValueKind::Nil => self.nil,
      ValueKind::Number => self.number,
      ValueKind::Obj => {
        let obj = value.to_obj();

        match obj.kind() {
          ObjectKind::Channel => self.channel,
          ObjectKind::Class => obj.to_class().meta_class().expect("Meta class not set."),
          ObjectKind::Closure => self.closure,
          ObjectKind::Enumerator => self.iter,
          ObjectKind::Fun => panic!("Function should not be directly accessible"),
          ObjectKind::Fiber => self.fiber,
          ObjectKind::Instance => obj.to_instance().class(),
          ObjectKind::List => self.list,
          ObjectKind::Map => self.map,
          ObjectKind::Method => self.method,
          ObjectKind::Native => self.native_fun,
          ObjectKind::String => self.string,
          ObjectKind::Upvalue => {
            let value = obj.to_upvalue().value(stack);
            self.for_value(value, stack)
          },
        }
      },
    }
  }
}

impl Trace for BuiltInPrimitives {
  fn trace(&self) {
    self.bool.trace();
    self.nil.trace();
    self.class.trace();
    self.fiber.trace();
    self.number.trace();
    self.string.trace();
    self.list.trace();
    self.iter.trace();
    self.map.trace();
    self.closure.trace();
    self.method.trace();
    self.native_fun.trace();
  }

  fn trace_debug(&self, stdio: &mut dyn Write) {
    self.bool.trace_debug(stdio);
    self.nil.trace_debug(stdio);
    self.class.trace_debug(stdio);
    self.fiber.trace_debug(stdio);
    self.number.trace_debug(stdio);
    self.string.trace_debug(stdio);
    self.list.trace_debug(stdio);
    self.iter.trace_debug(stdio);
    self.map.trace_debug(stdio);
    self.closure.trace_debug(stdio);
    self.method.trace_debug(stdio);
    self.native_fun.trace_debug(stdio);
  }
}

pub struct BuiltInErrors {
  pub runtime: GcObj<Class>,

  pub method_not_found: GcObj<Class>,

  pub type_: GcObj<Class>,

  pub value: GcObj<Class>,

  pub deadlock: GcObj<Class>,

  pub property: GcObj<Class>,

  pub import: GcObj<Class>,

  pub export: GcObj<Class>,
}

impl Trace for BuiltInErrors {
  fn trace(&self) {
    self.method_not_found.trace()
  }

  fn trace_debug(&self, log: &mut dyn Write) {
    self.method_not_found.trace_debug(log)
  }
}

pub fn builtin_from_module(hooks: &GcHooks, module: &Module) -> Option<BuiltIn> {
  Some(BuiltIn {
    primitives: BuiltInPrimitives {
      object: module
        .get_symbol(hooks.manage_str(OBJECT_CLASS_NAME))?
        .to_obj()
        .to_class(),
      nil: module
        .get_symbol(hooks.manage_str(NIL_CLASS_NAME))?
        .to_obj()
        .to_class(),
      bool: module
        .get_symbol(hooks.manage_str(BOOL_CLASS_NAME))?
        .to_obj()
        .to_class(),
      channel: module
        .get_symbol(hooks.manage_str(CHANNEL_CLASS_NAME))?
        .to_obj()
        .to_class(),
      class: module
        .get_symbol(hooks.manage_str(CLASS_CLASS_NAME))?
        .to_obj()
        .to_class(),
      fiber: module
        .get_symbol(hooks.manage_str(FIBER_CLASS_NAME))?
        .to_obj()
        .to_class(),
      number: module
        .get_symbol(hooks.manage_str(NUMBER_CLASS_NAME))?
        .to_obj()
        .to_class(),
      string: module
        .get_symbol(hooks.manage_str(STRING_CLASS_NAME))?
        .to_obj()
        .to_class(),
      list: module
        .get_symbol(hooks.manage_str(LIST_CLASS_NAME))?
        .to_obj()
        .to_class(),
      map: module
        .get_symbol(hooks.manage_str(MAP_CLASS_NAME))?
        .to_obj()
        .to_class(),
      iter: module
        .get_symbol(hooks.manage_str(ITER_CLASS_NAME))?
        .to_obj()
        .to_class(),
      closure: module
        .get_symbol(hooks.manage_str(CLOSURE_CLASS_NAME))?
        .to_obj()
        .to_class(),
      method: module
        .get_symbol(hooks.manage_str(METHOD_CLASS_NAME))?
        .to_obj()
        .to_class(),
      native_fun: module
        .get_symbol(hooks.manage_str(NATIVE_CLASS_NAME))?
        .to_obj()
        .to_class(),
    },
    dependencies: BuiltInDependencies {
      module: module
        .get_symbol(hooks.manage_str(MODULE_CLASS_NAME))?
        .to_obj()
        .to_class(),
    },
    errors: BuiltInErrors {
      runtime: module
        .get_symbol(hooks.manage_str(RUNTIME_ERROR_NAME))?
        .to_obj()
        .to_class(),
      method_not_found: module
        .get_symbol(hooks.manage_str(METHOD_NOT_FOUND_ERROR_NAME))?
        .to_obj()
        .to_class(),
      type_: module
        .get_symbol(hooks.manage_str(TYPE_ERROR_NAME))?
        .to_obj()
        .to_class(),
        deadlock: module
        .get_symbol(hooks.manage_str(DEADLOCK_ERROR_NAME))?
        .to_obj()
        .to_class(),
      value: module
        .get_symbol(hooks.manage_str(VALUE_ERROR_NAME))?
        .to_obj()
        .to_class(),
      property: module
        .get_symbol(hooks.manage_str(PROPERTY_ERROR_NAME))?
        .to_obj()
        .to_class(),
      import: module
        .get_symbol(hooks.manage_str(IMPORT_ERROR_NAME))?
        .to_obj()
        .to_class(),
      export: module
        .get_symbol(hooks.manage_str(EXPORT_ERROR_NAME))?
        .to_obj()
        .to_class(),
    },
  })
}
