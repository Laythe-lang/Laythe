use laythe_core::{
  hooks::Hooks,
  native::{NativeFun, NativeMeta},
  signature::Arity,
  value::{Value, VALUE_NIL},
  CallResult,
};
use laythe_env::{managed::Trace, stdio::Stdio};

const TEST_META: NativeMeta = NativeMeta::new("test", Arity::Fixed(0), &[]);

#[derive(Clone, Debug, Trace)]
pub struct TestNative();

impl NativeFun for TestNative {
  fn meta(&self) -> &NativeMeta {
    &TEST_META
  }

  fn call(&self, _: &mut Hooks, _: &[Value]) -> CallResult {
    Ok(VALUE_NIL)
  }
}
