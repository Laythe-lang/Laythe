use fnv::FnvBuildHasher;
use hashbrown::HashMap;
use laythe_env::managed::{DebugHeap, DebugWrap};
use linear_map::LinearMap;
use std::hash::Hash;

#[derive(PartialEq, Clone, Debug)]
pub enum DynamicMap<K: Ord + Hash, V> {
  Linear(LinearMap<K, V>),
  Hash(HashMap<K, V, FnvBuildHasher>),
}

const SIZE_THRESHOLD: usize = 12;

impl<K: Ord + Hash, V> DynamicMap<K, V> {
  pub fn new() -> Self {
    Self::Linear(LinearMap::new())
  }

  pub fn get(&self, key: &K) -> Option<&V> {
    match self {
      Self::Linear(linear) => linear.get(key),
      Self::Hash(hash) => hash.get(key),
    }
  }

  pub fn insert(&mut self, key: K, value: V) -> Option<V> {
    match self {
      Self::Linear(linear) => {
        if linear.len() > SIZE_THRESHOLD {
          let cap = linear.capacity();

          let mut hash = Self::Hash(HashMap::with_capacity_and_hasher(
            std::cmp::max(SIZE_THRESHOLD * 2, cap),
            Default::default(),
          ));
          linear.drain().for_each(|(k, v)| {
            hash.insert(k, v);
          });

          let result = hash.insert(key, value);
          *self = hash;
          result
        } else {
          linear.insert(key, value)
        }
      }
      Self::Hash(hash) => hash.insert(key, value),
    }
  }

  pub fn len(&self) -> usize {
    match self {
      Self::Linear(linear) => linear.len(),
      Self::Hash(hash) => hash.len(),
    }
  }

  pub fn is_empty(&self) -> bool {
    match self {
      Self::Linear(linear) => linear.is_empty(),
      Self::Hash(hash) => hash.is_empty(),
    }
  }

  pub fn capacity(&self) -> usize {
    match self {
      Self::Linear(linear) => linear.capacity(),
      Self::Hash(hash) => hash.capacity(),
    }
  }

  pub fn for_each<F>(&self, closure: F)
  where
    F: FnMut((&K, &V)),
  {
    match self {
      Self::Linear(linear) => linear.iter().for_each(closure),
      Self::Hash(hash) => hash.iter().for_each(closure),
    }
  }

  fn iter<'a>(&'a self) -> Box<dyn Iterator<Item = (&'a K, &'a V)> + 'a> {
    match self {
      Self::Linear(linear) => Box::new(linear.iter()),
      Self::Hash(hash) => Box::new(hash.iter()),
    }
  }
}

impl<K: Ord + Hash, V> Default for DynamicMap<K, V> {
  fn default() -> Self {
    Self::new()
  }
}

impl<K: DebugHeap + Ord + Hash, V: DebugHeap> DebugHeap for DynamicMap<K, V> {
  fn fmt_heap(&self, f: &mut std::fmt::Formatter, depth: usize) -> std::fmt::Result {
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
