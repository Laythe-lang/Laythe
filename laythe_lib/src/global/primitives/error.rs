use crate::{
  native,
  support::{export_and_insert, to_dyn_native},
  StdError, StdResult,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{MetaData, Native, NativeMeta, NativeMetaBuilder},
  object::{Class, List},
  signature::{Arity, ParameterBuilder, ParameterKind},
  val,
  value::Value,
  Call,
  managed::{Gc, Trace}
};
use std::io::Write;

use super::error_inheritance;

pub const ERROR_CLASS_NAME: &str = "Error";
const ERROR_FIELD_MESSAGE: &str = "message";
const ERROR_FIELD_STACK: &str = "stack";
const ERROR_FIELD_INNER: &str = "inner";

pub const TYPE_ERROR_NAME: &str = "TypeError";
pub const FORMAT_CLASS_NAME: &str = "FormatError";
pub const VALUE_ERROR_NAME: &str = "ValueError";
pub const INDEX_ERROR_NAME: &str = "IndexError";
pub const SYNTAX_ERROR_NAME: &str = "SyntaxError";
pub const IMPORT_ERROR_NAME: &str = "ImportError";
pub const EXPORT_ERROR_NAME: &str = "ExportError";
pub const RUNTIME_ERROR_NAME: &str = "RuntimeError";
pub const PROPERTY_ERROR_NAME: &str = "PropertyError";
pub const METHOD_NOT_FOUND_ERROR_NAME: &str = "MethodNotFoundError";

const ERROR_INIT: NativeMetaBuilder = NativeMetaBuilder::method("init", Arity::Default(1, 2))
  .with_params(&[
    ParameterBuilder::new("message", ParameterKind::String),
    ParameterBuilder::new("inner", ParameterKind::Instance),
  ]);

pub fn create_error_class(hooks: &GcHooks, object: Gc<Class>) -> Gc<Class> {
  let mut class = Class::with_inheritance(hooks, hooks.manage_str(ERROR_CLASS_NAME), object);

  class.add_field(hooks, hooks.manage_str(ERROR_FIELD_MESSAGE));
  class.add_field(hooks, hooks.manage_str(ERROR_FIELD_STACK));
  class.add_field(hooks, hooks.manage_str(ERROR_FIELD_INNER));

  class.add_method(
    hooks,
    hooks.manage_str(ERROR_INIT.name),
    val!(to_dyn_native(hooks, ErrorInit::from(hooks))),
  );

  class
}

pub fn declare_global_errors(hooks: &GcHooks, module: &mut Module) -> StdResult<()> {
  let type_error = error_inheritance(hooks, module, TYPE_ERROR_NAME)?;
  let format_error = error_inheritance(hooks, module, FORMAT_CLASS_NAME)?;
  let value_error = error_inheritance(hooks, module, VALUE_ERROR_NAME)?;
  let index_error = error_inheritance(hooks, module, INDEX_ERROR_NAME)?;
  let syntax_error = error_inheritance(hooks, module, SYNTAX_ERROR_NAME)?;
  let import_error = error_inheritance(hooks, module, IMPORT_ERROR_NAME)?;
  let runtime_error = error_inheritance(hooks, module, RUNTIME_ERROR_NAME)?;
  let export_error = error_inheritance(hooks, module, EXPORT_ERROR_NAME)?;
  let property_error = error_inheritance(hooks, module, PROPERTY_ERROR_NAME)?;
  let method_not_found_error = error_inheritance(hooks, module, METHOD_NOT_FOUND_ERROR_NAME)?;

  export_and_insert(hooks, module, type_error.name(), val!(type_error))?;
  export_and_insert(hooks, module, format_error.name(), val!(format_error))?;
  export_and_insert(hooks, module, value_error.name(), val!(value_error))?;
  export_and_insert(hooks, module, index_error.name(), val!(index_error))?;
  export_and_insert(hooks, module, syntax_error.name(), val!(syntax_error))?;
  export_and_insert(hooks, module, import_error.name(), val!(import_error))?;
  export_and_insert(hooks, module, export_error.name(), val!(export_error))?;
  export_and_insert(hooks, module, runtime_error.name(), val!(runtime_error))?;
  export_and_insert(hooks, module, property_error.name(), val!(property_error))?;
  export_and_insert(
    hooks,
    module,
    method_not_found_error.name(),
    val!(method_not_found_error),
  )
  .map_err(StdError::from)
}

pub fn define_global_errors(_hooks: &GcHooks, _module: &Module) -> StdResult<()> {
  Ok(())
}

native!(ErrorInit, ERROR_INIT);

impl Native for ErrorInit {
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

#[cfg(test)]
mod test {
  use super::*;

  mod init {
    use laythe_core::object::{Class, Instance};

    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let bool_str = ErrorInit::from(&hooks);

      assert_eq!(bool_str.meta().name, "init");
      assert_eq!(bool_str.meta().signature.arity, Arity::Default(1, 2));
      assert_eq!(
        bool_str.meta().signature.parameters[0].kind,
        ParameterKind::String
      );
      assert_eq!(
        bool_str.meta().signature.parameters[1].kind,
        ParameterKind::Instance
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let error_init = ErrorInit::from(&hooks);
      let mut test_class = hooks.manage(Class::bare(hooks.manage_str("test")));
      test_class.add_field(&hooks.as_gc(), hooks.manage_str(ERROR_FIELD_MESSAGE));
      test_class.add_field(&hooks.as_gc(), hooks.manage_str(ERROR_FIELD_STACK));
      test_class.add_field(&hooks.as_gc(), hooks.manage_str(ERROR_FIELD_INNER));

      let instance = hooks.manage(Instance::new(test_class));

      let name = val!(hooks.manage_str("test"));
      let args = [name];

      let result = error_init
        .call(&mut hooks, Some(val!(instance)), &args)
        .unwrap();

      assert!(result.is_instance());
      assert_eq!(result.to_instance()[0], val!(hooks.manage_str("test")));
      assert!(result.to_instance()[1].is_list());
      assert!(result.to_instance()[2].is_nil());
    }
  }
}
