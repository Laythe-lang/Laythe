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

impl<T1, T2> Trace for (T1, T2)
where
  T1: Trace,
  T2: Trace,
{
  fn trace(&self) {
    self.0.trace();
    self.1.trace();
  }

  fn trace_debug(&self, log: &mut dyn std::io::Write) {
    self.0.trace_debug(log);
    self.1.trace_debug(log);
  }
}

impl<T1, T2> DebugHeap for (T1, T2)
where
  T1: DebugHeap,
  T2: DebugHeap,
{
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    f.debug_tuple(&"")
      .field(&DebugWrap(&self.0, depth))
      .field(&DebugWrap(&self.1, depth))
      .finish()
  }
}

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

impl<T: DebugHeap> DebugHeap for &[T] {
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
