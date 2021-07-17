use super::class_inheritance;
use crate::{
  native,
  support::{export_and_insert, load_class_from_module},
  StdError, StdResult,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  managed::GcObj,
  managed::Trace,
  module::Module,
  object::{LyNative, Native, NativeMetaBuilder},
  signature::Arity,
  val,
  value::Value,
  Call,
};
use std::io::Write;

pub const CHANNEL_CLASS_NAME: &str = "Channel";
const CHANNEL_STR: NativeMetaBuilder = NativeMetaBuilder::method("str", Arity::Fixed(0));
const CHANNEL_LEN: NativeMetaBuilder = NativeMetaBuilder::method("len", Arity::Fixed(0));
const CHANNEL_CAPACITY: NativeMetaBuilder = NativeMetaBuilder::method("capacity", Arity::Fixed(0));

pub fn declare_channel_class(hooks: &GcHooks, module: &mut Module) -> StdResult<()> {
  let channel_class = class_inheritance(hooks, module, CHANNEL_CLASS_NAME)?;
  export_and_insert(hooks, module, channel_class.name(), val!(channel_class))
    .map_err(StdError::from)
}

pub fn define_channel_class(hooks: &GcHooks, module: &Module) -> StdResult<()> {
  let mut channel_class = load_class_from_module(hooks, module, CHANNEL_CLASS_NAME)?;

  channel_class.add_method(
    &hooks,
    hooks.manage_str(String::from(CHANNEL_STR.name)),
    val!(ChannelStr::native(hooks)),
  );

  channel_class.add_method(
    &hooks,
    hooks.manage_str(String::from(CHANNEL_LEN.name)),
    val!(ChannelLen::native(hooks)),
  );

  channel_class.add_method(
    &hooks,
    hooks.manage_str(String::from(CHANNEL_CAPACITY.name)),
    val!(ChannelCapacity::native(hooks)),
  );

  Ok(())
}

native!(ChannelStr, CHANNEL_STR);

impl LyNative for ChannelStr {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    let this = this.unwrap();
    let class = hooks.get_class(this).to_obj().to_class();
    let channel = this.to_obj().to_channel();

    Call::Ok(val!(hooks.manage_str(&format!(
      "<{} {:p}>",
      &*class.name(),
      &*channel
    ))))
  }
}

native!(ChannelLen, CHANNEL_LEN);

impl LyNative for ChannelLen {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    Call::Ok(val!(this.unwrap().to_obj().to_channel().len() as f64))
  }
}

native!(ChannelCapacity, CHANNEL_CAPACITY);

impl LyNative for ChannelCapacity {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, _args: &[Value]) -> Call {
    Call::Ok(val!(this.unwrap().to_obj().to_channel().capacity() as f64))
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod str {
    use laythe_core::object::Channel;

    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let channel_str = ChannelStr::native(&hooks);

      assert_eq!(channel_str.meta().name, "str");
      assert_eq!(channel_str.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::with_std(&[]).expect("std lib failure");
      let mut hooks = Hooks::new(&mut context);

      let channel_str = ChannelStr::native(&hooks.as_gc());
      let channel = hooks.manage_obj(Channel::with_capacity(&hooks.as_gc(), 1));

      let result = channel_str
        .call(&mut hooks, Some(val!(channel)), &[])
        .unwrap();
      assert!(result.to_obj().to_str().contains("<Channel "));
    }
  }

  mod len {
    use laythe_core::{object::Channel, support::FiberBuilder};

    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let channel_len = ChannelLen::native(&hooks);

      assert_eq!(channel_len.meta().name, "len");
      assert_eq!(channel_len.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::with_std(&[]).expect("std lib failure");
      let mut hooks = Hooks::new(&mut context);

      let channel_len = ChannelLen::native(&hooks.as_gc());
      let mut channel = hooks.manage_obj(Channel::with_capacity(&hooks.as_gc(), 1));

      let fiber = FiberBuilder::<u8>::default()
        .instructions(vec![0])
        .build(&hooks.as_gc())
        .unwrap();

      let result = channel_len
        .call(&mut hooks, Some(val!(channel)), &[])
        .unwrap();
      assert_eq!(result, val!(0.0));

      channel.enqueue(fiber, val!(1.0));

      let result = channel_len
        .call(&mut hooks, Some(val!(channel)), &[])
        .unwrap();
      assert_eq!(result, val!(1.0));
    }
  }

  mod capacity {
    use laythe_core::object::Channel;

    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let channel_capacity = ChannelCapacity::native(&hooks);

      assert_eq!(channel_capacity.meta().name, "capacity");
      assert_eq!(channel_capacity.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::with_std(&[]).expect("std lib failure");
      let mut hooks = Hooks::new(&mut context);

      let channel_capacity = ChannelCapacity::native(&hooks.as_gc());
      let channel = hooks.manage_obj(Channel::with_capacity(&hooks.as_gc(), 4));

      let result = channel_capacity
        .call(&mut hooks, Some(val!(channel)), &[])
        .unwrap();

      assert_eq!(result, val!(4.0));
    }
  }
}
