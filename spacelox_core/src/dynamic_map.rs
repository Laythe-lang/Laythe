use fnv::FnvHashMap;
use linear_map::LinearMap;
use std::hash::Hash;

#[derive(PartialEq, Clone, Debug)]
pub enum DynamicMap<K: Ord + Hash, V> {
  Linear(LinearMap<K, V>),
  Hash(FnvHashMap<K, V>),
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
          let mut hash = Self::Hash(FnvHashMap::with_capacity_and_hasher(
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
}
