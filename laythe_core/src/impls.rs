use crate::{
  impl_debug_heap,
  managed::{DebugHeap, DebugWrap, Trace},
};
use fnv::FnvBuildHasher;
use hashbrown::{HashMap, HashSet};
use std::{
  collections::VecDeque,
  fmt::{self},
};

impl Trace for u8 {}
impl_debug_heap!(u8);

impl Trace for u16 {}
impl_debug_heap!(u16);

impl Trace for usize {}
impl_debug_heap!(usize);

impl Trace for *const u8 {}
impl_debug_heap!(*const u8);

impl<K: DebugHeap, V: DebugHeap> DebugHeap for HashMap<K, V, FnvBuildHasher> {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    f.debug_map()
      .entries(
        self
          .iter()
          .map(|(k, v)| (DebugWrap(k, depth), DebugWrap(v, depth))),
      )
      .finish()
  }
}

impl<T: DebugHeap> DebugHeap for Option<T> {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    match self {
      Some(v) => f.write_fmt(format_args!("Some({:?})", DebugWrap(v, depth))),
      None => f.write_str("None"),
    }
  }
}

impl<'a, T: DebugHeap> DebugHeap for &'a [T] {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    f.debug_list()
      .entries(self.iter().map(|x| DebugWrap(x, depth)))
      .finish()
  }
}

impl<K: DebugHeap> DebugHeap for HashSet<K, FnvBuildHasher> {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    f.debug_set()
      .entries(self.iter().map(|x| DebugWrap(x, depth)))
      .finish()
  }
}

impl<T: DebugHeap> DebugHeap for VecDeque<T> {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    let (head, tail) = self.as_slices();

    f.debug_list()
      .entries(head.iter().chain(tail.iter()).map(|x| DebugWrap(x, depth)))
      .finish()
  }
}
