mod channel;
mod class;
mod closure;
mod enumerator;
mod fiber;
mod fun;
mod instance;
mod list;
mod ly_box;
mod map;
mod method;
mod native;

pub use channel::{Channel, CloseResult, ReceiveResult, SendResult};
pub use class::Class;
pub use closure::Closure;
pub use enumerator::{Enumerate, Enumerator};
pub use fiber::{Fiber, FiberResult, FiberState, UnwindResult};
pub use fun::{Fun, FunBuilder, FunKind};
// pub use instance::Instance;
pub use list::List;
pub use ly_box::LyBox;
pub use map::{Map, MapEntry};
pub use method::Method;
pub use native::{LyNative, Native, NativeMeta, NativeMetaBuilder};

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
  LyBox,
  Fiber,
  Tuple,
}
