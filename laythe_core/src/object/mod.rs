mod channel;
mod class;
mod closure;
mod enumerator;
mod fiber;
mod fun;
mod header;
mod instance;
mod list;
mod ly_box;
mod ly_str;
mod map;
mod method;
mod native;
mod tuple;

pub use channel::{Channel, CloseResult, ReceiveResult, SendResult};
pub use class::Class;
pub use closure::Closure;
pub use enumerator::{Enumerate, Enumerator};
pub use fiber::{Fiber, FiberPopResult, FiberResult, UnwindResult};
pub use fun::{Fun, FunBuilder, FunKind};
pub use header::Header as ObjHeader;
pub use instance::{Header as InstanceHeader, Instance};
pub use ly_box::LyBox;
pub use ly_str::LyStr;
pub use list::{List, ListLocation};
pub use map::{Map, MapEntry};
pub use method::Method;
pub use native::{LyNative, Native, NativeMeta, NativeMetaBuilder};
pub use tuple::Tuple;

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
