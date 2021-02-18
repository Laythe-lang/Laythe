mod class;
mod closure;
mod enumerator;
mod fun;
mod instance;
mod list;
mod map;
mod method;
mod native;
mod upvalue;

pub use class::Class;
pub use closure::Closure;
pub use enumerator::{Enumerate, Enumerator};
pub use fun::{Fun, FunBuilder, FunKind, TryBlock};
pub use instance::Instance;
pub use list::List;
pub use map::Map;
pub use method::Method;
pub use native::{LyNative, Native, NativeMeta, NativeMetaBuilder};
pub use upvalue::Upvalue;

#[cfg(test)]
pub use class::test_class;

/// Enum of value types in laythe
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum ObjectKind {
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
}