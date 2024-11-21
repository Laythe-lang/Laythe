use std::io::Write;

use laythe_core::{
  hooks::GcHooks,
  managed::Trace,
  module::Module,
  object::{Class, ObjectKind},
  value::{Value, ValueKind}, ObjRef,
};

use crate::global::{
  BOOL_CLASS_NAME, CHANNEL_CLASS_NAME, CLASS_CLASS_NAME, CLOSURE_CLASS_NAME, DEADLOCK_ERROR_NAME,
  ERROR_CLASS_NAME, EXPORT_ERROR_NAME, FUN_CLASS_NAME, IMPORT_ERROR_NAME,
  ITER_CLASS_NAME, LIST_CLASS_NAME, MAP_CLASS_NAME, METHOD_CLASS_NAME, METHOD_NOT_FOUND_ERROR_NAME,
  MODULE_CLASS_NAME, NATIVE_CLASS_NAME, NIL_CLASS_NAME, NUMBER_CLASS_NAME, OBJECT_CLASS_NAME,
  PROPERTY_ERROR_NAME, RUNTIME_ERROR_NAME, STRING_CLASS_NAME, TUPLE_CLASS_NAME, TYPE_ERROR_NAME,
  VALUE_ERROR_NAME,
};

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
  pub module: ObjRef<Class>,
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
  pub object: ObjRef<Class>,

  /// the Nil class
  pub nil: ObjRef<Class>,

  /// the Bool class
  pub bool: ObjRef<Class>,

  /// The Channel class
  pub channel: ObjRef<Class>,

  /// the Class class
  pub class: ObjRef<Class>,

  /// the Fun class
  pub fun: ObjRef<Class>,

  /// the Number class
  pub number: ObjRef<Class>,

  /// the String class
  pub string: ObjRef<Class>,

  /// the List class
  pub list: ObjRef<Class>,

  /// the Tuple class
  pub tuple: ObjRef<Class>,

  /// the Map class
  pub map: ObjRef<Class>,

  /// the Iter class
  pub iter: ObjRef<Class>,

  /// the Closure class
  pub closure: ObjRef<Class>,

  /// the method class
  pub method: ObjRef<Class>,

  /// the NativeFun class
  pub native_fun: ObjRef<Class>,
}

impl BuiltInPrimitives {
  pub fn for_value(&self, value: Value) -> ObjRef<Class> {
    match value.kind() {
      ValueKind::Bool => self.bool,
      ValueKind::Nil => self.nil,
      ValueKind::Undefined => panic!("Undefined should not be accesses"),
      ValueKind::Number => self.number,
      ValueKind::Obj => {
        let obj = value.to_obj();

        match obj.kind() {
          ObjectKind::Channel => self.channel,
          ObjectKind::Class => obj.to_class().meta_class().expect("Meta class not set."),
          ObjectKind::Closure => self.closure,
          ObjectKind::Enumerator => self.iter,
          ObjectKind::Fun => self.fun,
          ObjectKind::Instance => obj.to_instance().class(),
          ObjectKind::List => self.list,
          ObjectKind::Map => self.map,
          ObjectKind::Method => self.method,
          ObjectKind::Native => self.native_fun,
          ObjectKind::String => self.string,
          ObjectKind::Tuple => self.tuple,
          ObjectKind::LyBox => self.for_value(obj.to_box().value),
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
  pub error: ObjRef<Class>,

  pub runtime: ObjRef<Class>,

  pub method_not_found: ObjRef<Class>,

  pub type_: ObjRef<Class>,

  pub value: ObjRef<Class>,

  pub deadlock: ObjRef<Class>,

  pub property: ObjRef<Class>,

  pub import: ObjRef<Class>,

  pub export: ObjRef<Class>,
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
        .get_symbol_by_name(hooks.manage_str(OBJECT_CLASS_NAME))?
        .to_obj()
        .to_class(),
      nil: module
        .get_symbol_by_name(hooks.manage_str(NIL_CLASS_NAME))?
        .to_obj()
        .to_class(),
      bool: module
        .get_symbol_by_name(hooks.manage_str(BOOL_CLASS_NAME))?
        .to_obj()
        .to_class(),
      channel: module
        .get_symbol_by_name(hooks.manage_str(CHANNEL_CLASS_NAME))?
        .to_obj()
        .to_class(),
      class: module
        .get_symbol_by_name(hooks.manage_str(CLASS_CLASS_NAME))?
        .to_obj()
        .to_class(),
      fun: module
        .get_symbol_by_name(hooks.manage_str(FUN_CLASS_NAME))?
        .to_obj()
        .to_class(),
      number: module
        .get_symbol_by_name(hooks.manage_str(NUMBER_CLASS_NAME))?
        .to_obj()
        .to_class(),
      string: module
        .get_symbol_by_name(hooks.manage_str(STRING_CLASS_NAME))?
        .to_obj()
        .to_class(),
      list: module
        .get_symbol_by_name(hooks.manage_str(LIST_CLASS_NAME))?
        .to_obj()
        .to_class(),
      tuple: module
        .get_symbol_by_name(hooks.manage_str(TUPLE_CLASS_NAME))?
        .to_obj()
        .to_class(),
      map: module
        .get_symbol_by_name(hooks.manage_str(MAP_CLASS_NAME))?
        .to_obj()
        .to_class(),
      iter: module
        .get_symbol_by_name(hooks.manage_str(ITER_CLASS_NAME))?
        .to_obj()
        .to_class(),
      closure: module
        .get_symbol_by_name(hooks.manage_str(CLOSURE_CLASS_NAME))?
        .to_obj()
        .to_class(),
      method: module
        .get_symbol_by_name(hooks.manage_str(METHOD_CLASS_NAME))?
        .to_obj()
        .to_class(),
      native_fun: module
        .get_symbol_by_name(hooks.manage_str(NATIVE_CLASS_NAME))?
        .to_obj()
        .to_class(),
    },
    dependencies: BuiltInDependencies {
      module: module
        .get_symbol_by_name(hooks.manage_str(MODULE_CLASS_NAME))?
        .to_obj()
        .to_class(),
    },
    errors: BuiltInErrors {
      error: module
        .get_symbol_by_name(hooks.manage_str(ERROR_CLASS_NAME))?
        .to_obj()
        .to_class(),
      runtime: module
        .get_symbol_by_name(hooks.manage_str(RUNTIME_ERROR_NAME))?
        .to_obj()
        .to_class(),
      method_not_found: module
        .get_symbol_by_name(hooks.manage_str(METHOD_NOT_FOUND_ERROR_NAME))?
        .to_obj()
        .to_class(),
      type_: module
        .get_symbol_by_name(hooks.manage_str(TYPE_ERROR_NAME))?
        .to_obj()
        .to_class(),
      deadlock: module
        .get_symbol_by_name(hooks.manage_str(DEADLOCK_ERROR_NAME))?
        .to_obj()
        .to_class(),
      value: module
        .get_symbol_by_name(hooks.manage_str(VALUE_ERROR_NAME))?
        .to_obj()
        .to_class(),
      property: module
        .get_symbol_by_name(hooks.manage_str(PROPERTY_ERROR_NAME))?
        .to_obj()
        .to_class(),
      import: module
        .get_symbol_by_name(hooks.manage_str(IMPORT_ERROR_NAME))?
        .to_obj()
        .to_class(),
      export: module
        .get_symbol_by_name(hooks.manage_str(EXPORT_ERROR_NAME))?
        .to_obj()
        .to_class(),
    },
  })
}
