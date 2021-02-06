use crate::native;
use laythe_core::{
  hooks::{GcHooks, Hooks},
  managed::Trace,
  object::{LyNative, Native, NativeMetaBuilder},
  signature::Arity,
  value::{Value, VALUE_NIL},
  managed::Gc,
  Call,
};
use std::io::Write;

const TEST_META: NativeMetaBuilder = NativeMetaBuilder::fun("test", Arity::Fixed(0));

native!(TestNative, TEST_META);

impl LyNative for TestNative {
  fn call(&self, _: &mut Hooks, _this: Option<Value>, _: &[Value]) -> Call {
    Call::Ok(VALUE_NIL)
  }
}
