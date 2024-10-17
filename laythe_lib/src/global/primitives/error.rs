use crate::{native, support::export_and_insert, StdError, StdResult};
use laythe_core::{
  hooks::{GcHooks, Hooks}, list, managed::{Gc, GcObj, Trace}, module::Module, object::{Class, LyNative, Native, NativeMetaBuilder}, signature::{Arity, ParameterBuilder, ParameterKind}, val, value::Value, Call
};
use std::io::Write;

use super::error_inheritance;

pub const ERROR_CLASS_NAME: &str = "Error";
const ERROR_FIELD_MESSAGE: &str = "message";
const ERROR_FIELD_BACK_TRACE: &str = "backTrace";
const ERROR_FIELD_INNER: &str = "inner";

pub const TYPE_ERROR_NAME: &str = "TypeError";
pub const FORMAT_ERROR_NAME: &str = "FormatError";
pub const DEADLOCK_ERROR_NAME: &str = "DeadLockError";
pub const VALUE_ERROR_NAME: &str = "ValueError";
pub const INDEX_ERROR_NAME: &str = "IndexError";
pub const CHANNEL_ERROR_NAME: &str = "ChannelError";
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

pub fn create_error_class(hooks: &GcHooks, object: GcObj<Class>) -> GcObj<Class> {
  let mut class = Class::with_inheritance(hooks, hooks.manage_str(ERROR_CLASS_NAME), object);

  class.add_field(hooks.manage_str(ERROR_FIELD_MESSAGE));
  class.add_field(hooks.manage_str(ERROR_FIELD_BACK_TRACE));
  class.add_field(hooks.manage_str(ERROR_FIELD_INNER));

  class.add_method(
    hooks.manage_str(ERROR_INIT.name),
    val!(ErrorInit::native(hooks)),
  );

  class
}

pub fn declare_global_errors(hooks: &GcHooks, module: Gc<Module>) -> StdResult<()> {
  let type_error = error_inheritance(hooks, module, TYPE_ERROR_NAME)?;
  let format_error = error_inheritance(hooks, module, FORMAT_ERROR_NAME)?;
  let value_error = error_inheritance(hooks, module, VALUE_ERROR_NAME)?;
  let index_error = error_inheritance(hooks, module, INDEX_ERROR_NAME)?;
  let deadlock_error = error_inheritance(hooks, module, DEADLOCK_ERROR_NAME)?;
  let channel_error = error_inheritance(hooks, module, CHANNEL_ERROR_NAME)?;
  let syntax_error = error_inheritance(hooks, module, SYNTAX_ERROR_NAME)?;
  let import_error = error_inheritance(hooks, module, IMPORT_ERROR_NAME)?;
  let runtime_error = error_inheritance(hooks, module, RUNTIME_ERROR_NAME)?;
  let export_error = error_inheritance(hooks, module, EXPORT_ERROR_NAME)?;
  let property_error = error_inheritance(hooks, module, PROPERTY_ERROR_NAME)?;
  let method_not_found_error = error_inheritance(hooks, module, METHOD_NOT_FOUND_ERROR_NAME)?;

  export_and_insert(module, type_error.name(), val!(type_error))?;
  export_and_insert(module, format_error.name(), val!(format_error))?;
  export_and_insert(module, value_error.name(), val!(value_error))?;
  export_and_insert(module, index_error.name(), val!(index_error))?;
  export_and_insert(module, deadlock_error.name(), val!(deadlock_error))?;
  export_and_insert(module, channel_error.name(), val!(channel_error))?;
  export_and_insert(module, syntax_error.name(), val!(syntax_error))?;
  export_and_insert(module, import_error.name(), val!(import_error))?;
  export_and_insert(module, export_error.name(), val!(export_error))?;
  export_and_insert(module, runtime_error.name(), val!(runtime_error))?;
  export_and_insert(module, property_error.name(), val!(property_error))?;
  export_and_insert(
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

impl LyNative for ErrorInit {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> Call {
    let mut this = this.unwrap().to_obj().to_instance();
    this[0] = args[0];
    this[1] = val!(_hooks.manage_obj(list!()));

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
    use laythe_core::object::{Class, ObjectKind};

    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let bool_str = ErrorInit::native(&hooks);

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

      let error_init = ErrorInit::native(&hooks.as_gc());
      let mut test_class = hooks.manage_obj(Class::bare(hooks.manage_str("test")));
      test_class.add_field(hooks.manage_str(ERROR_FIELD_MESSAGE));
      test_class.add_field(hooks.manage_str(ERROR_FIELD_BACK_TRACE));
      test_class.add_field(hooks.manage_str(ERROR_FIELD_INNER));

      let instance = hooks.manage_obj(test_class);

      let name = val!(hooks.manage_str("test"));
      let args = [name];

      let result = error_init
        .call(&mut hooks, Some(val!(instance)), &args)
        .unwrap();

      assert!(result.is_obj_kind(ObjectKind::Instance));
      let result = result.to_obj();
      assert_eq!(result.to_instance()[0], val!(hooks.manage_str("test")));
      assert!(result.to_instance()[1].is_obj_kind(ObjectKind::List));
      assert!(result.to_instance()[2].is_nil());
    }
  }
}
