#![deny(clippy::all)]
pub mod captures;
pub mod chunk;
pub mod constants;
pub mod hooks;
pub mod impls;
pub mod managed;
pub mod memory;
pub mod module;
pub mod object;
pub mod signature;
pub mod support;
pub mod utils;
pub mod value;

pub type Call = LyResult<value::Value>;
pub type LyResult<T> = Result<T, LyError>;

#[derive(Debug, PartialEq, Eq)]
pub enum LyError {
  Err(Instance),
  Exit(u16),
}

pub type LyHashSet<K> = HashSet<K, FnvBuildHasher>;

use fnv::FnvBuildHasher;
use hashbrown::HashSet;
use managed::Instance;

#[macro_export]
macro_rules! impl_trace {
  ( $x:ty ) => {
    impl Trace for $x {
      fn trace(&self) {}
      fn trace_debug(&self, _log: &mut dyn std::io::Write) {}
    }
  };
}

#[macro_export]
macro_rules! impl_debug_heap {
  ( $x:ty ) => {
    impl DebugHeap for $x {
      fn fmt_heap(&self, f: &mut std::fmt::Formatter, _depth: usize) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
      }
    }
  };
}
