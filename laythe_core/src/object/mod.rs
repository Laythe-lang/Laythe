mod channel;
mod class;
mod closure;
mod enumerator;
mod fiber;
mod fun;
mod instance;
mod list;
mod map;
mod method;
mod native;
mod upvalue;

pub use channel::{Channel, ReceiveResult, SendResult, CloseResult};
pub use class::Class;
pub use closure::Closure;
pub use enumerator::{Enumerate, Enumerator};
pub use fiber::{Fiber, FiberResult};
pub use fun::{Fun, FunBuilder, FunKind, TryBlock};
pub use instance::Instance;
pub use list::List;
pub use map::{Map, MapEntry};
pub use method::Method;
pub use native::{LyNative, Native, NativeMeta, NativeMetaBuilder};
pub use upvalue::Upvalue;

#[cfg(test)]
pub use class::test_class;

/// Enum of value types in laythe
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum ObjectKind {
  Channel,
  Class,
  Closure,
  Enumerator,
  Fun,
  Instance,
  List,
  Map,
  Method,
  Native,
  String,
  Upvalue,
  Fiber,
}
