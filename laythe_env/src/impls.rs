use crate::managed::{DebugHeap, DebugWrap, Manage, Trace};
use fnv::FnvBuildHasher;
use hashbrown::{HashMap, HashSet};
use std::{
  fmt::{self, Debug},
  mem,
};

impl Trace for u128 {
  fn trace(&self) {}
  fn trace_debug(&self, _log: &mut dyn std::io::Write) {}
}

impl Manage for u128 {
  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}

impl<K: DebugHeap, V: DebugHeap> DebugHeap for HashMap<K, V, FnvBuildHasher> {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    let depth = depth.saturating_sub(1);

    f.debug_map()
      .entries(
        self
          .iter()
          .map(|(k, v)| (DebugWrap(k, depth), DebugWrap(v, depth))),
      )
      .finish()
  }
}

impl DebugHeap for u128 {
  fn fmt_heap(&self, f: &mut fmt::Formatter, _depth: usize) -> fmt::Result {
    self.fmt(f)
  }
}

impl<T: DebugHeap> DebugHeap for Option<T> {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    let depth = depth.saturating_sub(1);

    match self {
      Some(v) => f.write_fmt(format_args!("Some({:?})", DebugWrap(v, depth))),
      None => f.write_str("None"),
    }
  }
}

impl<'a, T: DebugHeap> DebugHeap for &'a [T] {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    let depth = depth.saturating_sub(1);

    f.debug_list()
      .entries(self.iter().map(|x| DebugWrap(x, depth)))
      .finish()
  }
}

impl<K: DebugHeap> DebugHeap for HashSet<K, FnvBuildHasher> {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    let depth = depth.saturating_sub(1);

    f.debug_set()
      .entries(self.iter().map(|x| DebugWrap(x, depth)))
      .finish()
  }
}
