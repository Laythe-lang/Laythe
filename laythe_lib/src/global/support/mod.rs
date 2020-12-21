use crate::native;
use laythe_core::{
  hooks::{GcHooks, Hooks},
  native::{MetaData, Native, NativeMeta, NativeMetaBuilder},
  signature::Arity,
  value::{Value, VALUE_NIL},
  Call,
};
use laythe_env::managed::Trace;
use std::io::Write;

const TEST_META: NativeMetaBuilder = NativeMetaBuilder::fun("test", Arity::Fixed(0));

native!(TestNative, TEST_META);

impl Native for TestNative {
  fn call(&self, _: &mut Hooks, _this: Option<Value>, _: &[Value]) -> Call {
    Call::Ok(VALUE_NIL)
  }
}
