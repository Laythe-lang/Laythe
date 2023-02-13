use crate::{
  managed::{DebugHeap, DebugWrap, Object, Trace},
  value::Value,
};
use fmt::Display;
use fnv::FnvBuildHasher;
use hashbrown::{hash_map, HashMap};
use std::{fmt, hash::Hash, io::Write};

use super::ObjectKind;

pub type MapEntry<'a, K, V> = hash_map::Entry<'a, K, V, FnvBuildHasher>;

#[derive(Clone, Debug)]
pub struct Map<K, V>(HashMap<K, V, FnvBuildHasher>);

impl<K, V> Map<K, V> {
  pub fn new() -> Self {
    Self(HashMap::<K, V, FnvBuildHasher>::with_hasher(
      FnvBuildHasher::default(),
    ))
  }

  #[inline]
  pub fn with_capacity(capacity: usize) -> Self {
    Self(HashMap::<K, V, FnvBuildHasher>::with_capacity_and_hasher(
      capacity,
      FnvBuildHasher::default(),
    ))
  }

  pub fn len(&self) -> usize {
    self.0.len()
  }

  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }

  pub fn capacity(&self) -> usize {
    self.0.capacity()
  }

  pub fn iter(&self) -> hash_map::Iter<'_, K, V> {
    self.0.iter()
  }

  pub fn keys(&self) -> hash_map::Keys<'_, K, V> {
    self.0.keys()
  }

  pub fn values(&self) -> hash_map::Values<'_, K, V> {
    self.0.values()
  }
}

impl<K, V> Map<K, V>
where
  K: Eq + Hash,
{
  pub fn reserve(&mut self, additional: usize) {
    self.0.reserve(additional)
  }

  pub fn get(&self, key: &K) -> Option<&V> {
    self.0.get(key)
  }

  pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
    self.0.get_mut(key)
  }

  pub fn contains_key(&self, key: &K) -> bool {
    self.0.contains_key(key)
  }

  pub fn remove(&mut self, key: &K) -> Option<V> {
    self.0.remove(key)
  }

  pub fn entry(&mut self, key: K) -> MapEntry<'_, K, V> {
    self.0.entry(key)
  }

  pub fn insert(&mut self, key: K, value: V) -> Option<V> {
    self.0.insert(key, value)
  }
}

impl<K: Display, V: Display> Display for Map<K, V> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{{")?;

    for (key, val) in self.iter() {
      write!(f, "{key}: {val}")?;
    }

    write!(f, "}}")
  }
}

impl<K, V> Default for Map<K, V> {
  fn default() -> Self {
    Map(HashMap::default())
  }
}

impl<K: 'static + Trace, V: 'static + Trace> Trace for Map<K, V> {
  fn trace(&self) {
    self.iter().for_each(|(key, value)| {
      key.trace();
      value.trace();
    });
  }

  fn trace_debug(&self, stdio: &mut dyn Write) {
    self.iter().for_each(|(key, value)| {
      key.trace_debug(stdio);
      value.trace_debug(stdio);
    });
  }
}

impl<K: 'static + DebugHeap, V: 'static + DebugHeap> DebugHeap for Map<K, V> {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    f.debug_map()
      .entries(
        self
          .0
          .iter()
          .map(|(k, v)| (DebugWrap(k, depth), DebugWrap(v, depth))),
      )
      .finish()
  }
}

impl Object for Map<Value, Value> {
  fn kind(&self) -> ObjectKind {
    ObjectKind::Map
  }
}
