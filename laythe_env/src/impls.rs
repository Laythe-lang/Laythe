use crate::managed::{DebugHeap, DebugWrap, Manage, Trace};
use fnv::FnvBuildHasher;
use hashbrown::HashSet;
use std::{fmt, io::Write, mem, path::PathBuf};

impl Trace for PathBuf {
  fn trace(&self) {}
  fn trace_debug(&self, _: &mut dyn Write) {}
}

impl DebugHeap for PathBuf {
  fn fmt_heap(&self, f: &mut std::fmt::Formatter, _: usize) -> std::fmt::Result {
    f.write_fmt(format_args!("{:?}", self))
  }
}

impl Manage for PathBuf {
  fn size(&self) -> usize {
    mem::size_of::<Self>() // TODO add capacity once stabilized? + self.capacity()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
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
