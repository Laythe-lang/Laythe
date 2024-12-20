use super::class_inheritance;
use crate::{
  global::primitives::error::CHANNEL_ERROR_NAME,
  native, native_with_error,
  support::{export_and_insert, load_class_from_module},
  StdResult,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  managed::Trace,
  module::Module,
  object::{CloseResult, LyNative, Native, NativeMetaBuilder, ObjectKind},
  signature::Arity,
  val,
  value::{Value, VALUE_NIL},
  Call, LyError, Ref,
};
use std::io::Write;

pub const CHANNEL_CLASS_NAME: &str = "Channel";
const CHANNEL_STR: NativeMetaBuilder = NativeMetaBuilder::method("str", Arity::Fixed(0));
const CHANNEL_LEN: NativeMetaBuilder = NativeMetaBuilder::method("len", Arity::Fixed(0));
const CHANNEL_CLOSE: NativeMetaBuilder =
  NativeMetaBuilder::method("close", Arity::Fixed(0)).with_stack();
const CHANNEL_CAPACITY: NativeMetaBuilder = NativeMetaBuilder::method("capacity", Arity::Fixed(0));

pub fn declare_channel_class(hooks: &GcHooks, module: Ref<Module>) -> StdResult<()> {
  let channel_class = class_inheritance(hooks, module, CHANNEL_CLASS_NAME)?;
  export_and_insert(hooks, module, CHANNEL_CLASS_NAME, val!(channel_class))
}

pub fn define_channel_class(hooks: &GcHooks, module: Ref<Module>) -> StdResult<()> {
  let mut channel_class = load_class_from_module(hooks, module, CHANNEL_CLASS_NAME)?;
  let channel_error = val!(load_class_from_module(hooks, module, CHANNEL_ERROR_NAME)?);

  channel_class.add_method(
    hooks.manage_str(String::from(CHANNEL_STR.name)),
    val!(ChannelStr::native(hooks)),
  );

  channel_class.add_method(
    hooks.manage_str(String::from(CHANNEL_LEN.name)),
    val!(ChannelLen::native(hooks)),
  );

  channel_class.add_method(
    hooks.manage_str(String::from(CHANNEL_CAPACITY.name)),
    val!(ChannelCapacity::native(hooks)),
  );

  channel_class.add_method(
    hooks.manage_str(String::from(CHANNEL_CLOSE.name)),
    val!(ChannelClose::native(hooks, channel_error)),
  );

  Ok(())
}

native!(ChannelStr, CHANNEL_STR);

impl LyNative for ChannelStr {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let this = args[0];
    let class = hooks.get_class(this);
    let channel = this.to_obj().to_channel();

    Call::Ok(val!(hooks.manage_str(format!(
      "<{} {:p}>",
      &*class.name(),
      &*channel
    ))))
  }
}

native!(ChannelLen, CHANNEL_LEN);

impl LyNative for ChannelLen {
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
    Call::Ok(val!(args[0].to_obj().to_channel().len() as f64))
  }
}

native!(ChannelCapacity, CHANNEL_CAPACITY);

impl LyNative for ChannelCapacity {
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
    Call::Ok(val!(args[0].to_obj().to_channel().capacity() as f64))
  }
}

native_with_error!(ChannelClose, CHANNEL_CLOSE);

impl LyNative for ChannelClose {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let mut channel = args[0].to_obj().to_channel();
    match channel.close() {
      CloseResult::Ok => Call::Ok(VALUE_NIL),
      CloseResult::AlreadyClosed => self.call_error(hooks, "Channel already closed."),
    }
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
    fn call() {
      let mut context = MockedContext::with_std(&[]).expect("std lib failure");
      let mut hooks = Hooks::new(&mut context);

      let channel_str = ChannelStr::native(&hooks.as_gc());
      let channel = hooks.manage_obj(Channel::with_capacity(&hooks.as_gc(), 1));

      let result = channel_str.call(&mut hooks, &[val!(channel)]).unwrap();
      assert!(result.to_obj().to_str().contains("<Channel "));
    }
  }

  mod len {
    use laythe_core::object::{Channel, ChannelWaiter};

    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn call() {
      let mut context = MockedContext::with_std(&[]).expect("std lib failure");
      let mut hooks = Hooks::new(&mut context);

      let channel_len = ChannelLen::native(&hooks.as_gc());
      let mut channel = hooks.manage_obj(Channel::with_capacity(&hooks.as_gc(), 1));

      let waiter = hooks.manage(ChannelWaiter::new(true));

      let result = channel_len.call(&mut hooks, &[val!(channel)]).unwrap();
      assert_eq!(result, val!(0.0));

      channel.send(waiter, val!(1.0));

      let result = channel_len.call(&mut hooks, &[val!(channel)]).unwrap();
      assert_eq!(result, val!(1.0));
    }
  }

  mod capacity {
    use laythe_core::object::Channel;

    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn call() {
      let mut context = MockedContext::with_std(&[]).expect("std lib failure");
      let mut hooks = Hooks::new(&mut context);

      let channel_capacity = ChannelCapacity::native(&hooks.as_gc());
      let channel = hooks.manage_obj(Channel::with_capacity(&hooks.as_gc(), 4));

      let result = channel_capacity.call(&mut hooks, &[val!(channel)]).unwrap();

      assert_eq!(result, val!(4.0));
    }
  }

  mod close {
    use laythe_core::object::Channel;

    use super::*;
    use crate::support::{test_error_class, MockedContext};

    #[test]
    fn call() {
      let mut context = MockedContext::with_std(&[]).expect("std lib failure");
      let mut hooks = Hooks::new(&mut context);
      let error = val!(test_error_class(&hooks.as_gc()));

      let channel_close = ChannelClose::native(&hooks.as_gc(), error);
      let channel = hooks.manage_obj(Channel::with_capacity(&hooks.as_gc(), 4));

      let result = channel_close.call(&mut hooks, &[val!(channel)]).unwrap();

      assert_eq!(result, VALUE_NIL);
      assert!(channel.is_closed());
    }
  }
}
