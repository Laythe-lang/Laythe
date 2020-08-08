use std::alloc::{self, handle_alloc_error, Layout};
use std::mem;
use std::{
  borrow::Borrow,
  fmt,
  marker::PhantomData,
  ptr::{self, NonNull},
};

struct RawMapVec<K, V> {
  keys_ptr: NonNull<K>,
  vals_ptr: NonNull<V>,
  cap: usize,
}

impl<K, V> RawMapVec<K, V> {
  fn new() -> Self {
    // !0 is usize::MAX. This branch should be stripped at compile time.
    // TODO: do we need to check V as well?
    let cap = if mem::size_of::<K>() == 0 { !0 } else { 0 };

    // NonNull::dangling() doubles as "unallocated" and "zero-sized allocation"
    Self {
      keys_ptr: NonNull::dangling(),
      vals_ptr: NonNull::dangling(),
      cap,
    }
  }

  fn grow(&mut self) {
    unsafe {
      let key_size = mem::size_of::<K>();
      let val_size = mem::size_of::<V>();

      // since we set the capacity to usize::MAX when elem_size is
      // 0, getting to here necessarily means the Vec is overfull.
      assert!(key_size != 0, "capacity overflow");
      assert!(val_size != 0, "capacity overflow");

      let (new_cap, keys_ptr, vals_ptr) = if self.cap == 0 {
        let min_non_zero_cap = if key_size <= 512 && val_size <= 512 {
          4
        } else {
          1
        };

        let keys_buffer =
          alloc::alloc(Layout::array::<K>(min_non_zero_cap).expect("Bad layout")) as *mut K;
        let vals_buffer =
          alloc::alloc(Layout::array::<V>(min_non_zero_cap).expect("Bad layout")) as *mut V;

        (
          min_non_zero_cap,
          NonNull::new(keys_buffer),
          NonNull::new(vals_buffer),
        )
      } else {
        let new_cap = 2 * self.cap;

        let keys_buffer = alloc::alloc(Layout::array::<K>(new_cap).expect("Bad layout")) as *mut K;
        let vals_buffer = alloc::alloc(Layout::array::<V>(new_cap).expect("Bad layout")) as *mut V;

        ptr::copy_nonoverlapping(self.keys_ptr.as_ptr(), keys_buffer, self.cap);
        ptr::copy_nonoverlapping(self.vals_ptr.as_ptr(), vals_buffer, self.cap);

        (
          new_cap,
          NonNull::new(keys_buffer),
          NonNull::new(vals_buffer),
        )
      };

      // If allocate or reallocate fail, oom
      match (keys_ptr, vals_ptr) {
        (Some(keys_ptr), Some(vals_ptr)) => {
          let mut raw_map_vec = RawMapVec {
            keys_ptr,
            vals_ptr,
            cap: new_cap,
          };

          mem::swap(self, &mut raw_map_vec)
        }
        (None, _) => handle_alloc_error(Layout::from_size_align_unchecked(
          new_cap * key_size,
          mem::align_of::<K>(),
        )),
        (_, None) => handle_alloc_error(Layout::from_size_align_unchecked(
          new_cap * key_size,
          mem::align_of::<V>(),
        )),
      }
    }
  }
}

impl<K, V> Drop for RawMapVec<K, V> {
  fn drop(&mut self) {
    let key_size = mem::size_of::<K>();
    let val_size = mem::size_of::<V>();

    if self.cap != 0 && key_size != 0 {
      unsafe {
        alloc::dealloc(
          self.keys_ptr.as_ptr() as *mut _,
          Layout::array::<K>(self.cap).expect("Bad layout"),
        );
      }
    }

    if self.cap != 0 && val_size != 0 {
      unsafe {
        alloc::dealloc(
          self.vals_ptr.as_ptr() as *mut _,
          Layout::array::<V>(self.cap).expect("Bad layout"),
        );
      }
    }
  }
}
pub struct LinMap<K, V> {
  buf: RawMapVec<K, V>,
  len: usize,
}

impl<K, V> LinMap<K, V> {
  fn ptr_keys(&self) -> *mut K {
    self.buf.keys_ptr.as_ptr()
  }

  fn ptr_vals(&self) -> *mut V {
    self.buf.vals_ptr.as_ptr()
  }

  pub fn len(&self) -> usize {
    self.len
  }

  pub fn capacity(&self) -> usize {
    self.buf.cap
  }

  pub fn new() -> Self {
    Self {
      buf: RawMapVec::new(),
      len: 0,
    }
  }

  pub fn iter(&self) -> Iter<K, V> {
    unsafe {
      let key_slice = std::slice::from_raw_parts(self.ptr_keys(), self.len);
      let val_slice = std::slice::from_raw_parts(self.ptr_vals(), self.len);

      let iter = RawMapIter::new(key_slice, val_slice);

      Iter {
        iter: iter,
        vec: PhantomData,
      }
    }
  }

  pub fn drain(&mut self) -> Drain<K, V> {
    unsafe {
      let key_slice = std::slice::from_raw_parts(self.ptr_keys(), self.len);
      let val_slice = std::slice::from_raw_parts(self.ptr_vals(), self.len);

      let iter = RawMapIter::new(key_slice, val_slice);

      // this is a mem::forget safety thing. If Drain is forgotten, we just
      // leak the whole Vec's contents. Also we need to do this *eventually*
      // anyway, so why not do it now?
      self.len = 0;

      Drain {
        iter,
        vec: PhantomData,
      }
    }
  }
}

impl<K: Eq, V> LinMap<K, V> {
  pub fn get<Q: ?Sized>(&self, key: &Q) -> Option<&V>
  where
    K: Borrow<Q>,
    Q: Eq,
  {
    let key_slice = unsafe { std::slice::from_raw_parts(self.buf.keys_ptr.as_ptr(), self.len) };

    key_slice
      .iter()
      .position(|k| key.eq(k.borrow()))
      .map(|index| unsafe { &*self.ptr_vals().offset(index as isize) })
  }

  pub fn insert(&mut self, key: K, value: V) -> Option<V> {
    let key_slice = unsafe { std::slice::from_raw_parts(self.buf.keys_ptr.as_ptr(), self.len) };

    match key_slice.iter().position(|k| k == &key) {
      Some(index) => unsafe {
        Some(mem::replace(
          &mut *self.ptr_vals().offset(index as isize),
          value,
        ))
      },
      None => {
        if self.len == self.capacity() {
          self.buf.grow();
        }

        unsafe {
          ptr::write(self.ptr_keys().offset(self.len as isize), key);
          ptr::write(self.ptr_vals().offset(self.len as isize), value);
        }

        // Can't fail, we'll OOM first.
        self.len += 1;
        None
      }
    }
  }

  pub fn remove<Q: ?Sized>(&mut self, key: &Q) -> Option<V>
  where
    K: Borrow<Q>,
    Q: Eq,
  {
    let key_slice = unsafe { std::slice::from_raw_parts(self.buf.keys_ptr.as_ptr(), self.len) };

    match key_slice.iter().position(|k| key.eq(k.borrow())) {
      Some(index) => unsafe {
        self.len -= 1;
        let result = ptr::read(self.ptr_vals().offset(index as isize));

        ptr::copy(
          self.ptr_vals().offset(index as isize + 1),
          self.ptr_vals().offset(index as isize),
          self.len - index,
        );

        ptr::copy(
          self.ptr_keys().offset(index as isize + 1),
          self.ptr_keys().offset(index as isize),
          self.len - index,
        );

        Some(result)
      },
      None => None,
    }
  }
}

impl<K: fmt::Debug, V: fmt::Debug> fmt::Debug for LinMap<K, V> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_map()
      .entries(self.iter().map(|(k, v)| (k, v)))
      .finish()
  }
}

impl<K: Clone + Eq, V: Clone> Clone for LinMap<K, V> {
  fn clone(&self) -> Self {
    let mut cloned = Self::new();

    for (key, val) in self.iter() {
      cloned.insert(key.clone(), val.clone());
    }

    cloned
  }
}

impl<K, V> PartialEq for LinMap<K, V>
where
  K: Eq,
  V: PartialEq,
{
  fn eq(&self, other: &LinMap<K, V>) -> bool {
    if self.len() != other.len() {
      return false;
    }

    self
      .iter()
      .all(|(key, value)| other.get(key).map_or(false, |v| *value == *v))
  }
}

pub struct Iter<'a, K: 'a, V: 'a> {
  vec: PhantomData<&'a mut LinMap<K, V>>,
  iter: RawMapIter<'a, K, V>,
}

impl<'a, K: 'a, V: 'a> Iterator for Iter<'a, K, V> {
  type Item = (&'a K, &'a V);

  fn next(&mut self) -> Option<(&'a K, &'a V)> {
    self.iter.next()
  }

  fn size_hint(&self) -> (usize, Option<usize>) {
    self.iter.size_hint()
  }
}

impl<'a, K: 'a, V: 'a> DoubleEndedIterator for Iter<'a, K, V> {
  fn next_back(&mut self) -> Option<(&'a K, &'a V)> {
    self.iter.next_back()
  }
}

pub struct Drain<'a, K: 'a, V: 'a> {
  vec: PhantomData<&'a mut LinMap<K, V>>,
  iter: RawMapIter<'a, K, V>,
}

impl<'a, K: 'a, V: 'a> Iterator for Drain<'a, K, V> {
  type Item = (K, V);

  fn next(&mut self) -> Option<(K, V)> {
    self
      .iter
      .next()
      .map(|(k, v)| unsafe { (ptr::read(k as *const _), ptr::read(v as *const _)) })
  }

  fn size_hint(&self) -> (usize, Option<usize>) {
    self.iter.size_hint()
  }
}

impl<'a, K: 'a, V: 'a> DoubleEndedIterator for Drain<'a, K, V> {
  fn next_back(&mut self) -> Option<(K, V)> {
    self
      .iter
      .next_back()
      .map(|(k, v)| unsafe { (ptr::read(k as *const _), ptr::read(v as *const _)) })
  }
}

impl<'a, K, V> Drop for Drain<'a, K, V> {
  fn drop(&mut self) {
    // pre-drain the iter
    for _ in &mut self.iter {}
  }
}

struct RawMapIter<'a, K, V> {
  keys: RawIter<'a, K>,
  vals: RawIter<'a, V>,
}

impl<'a, K, V> RawMapIter<'a, K, V> {
  unsafe fn new(keys: &'a [K], vals: &'a [V]) -> Self {
    Self {
      keys: RawIter::new(keys),
      vals: RawIter::new(vals),
    }
  }
}

impl<'a, K, V> Iterator for RawMapIter<'a, K, V> {
  type Item = (&'a K, &'a V);
  fn next(&mut self) -> Option<(&'a K, &'a V)> {
    self
      .keys
      .next()
      .and_then(|key| self.vals.next().map(|val| (key, val)))
  }

  fn size_hint(&self) -> (usize, Option<usize>) {
    self.keys.size_hint()
  }
}

impl<'a, K, V> DoubleEndedIterator for RawMapIter<'a, K, V> {
  fn next_back(&mut self) -> Option<(&'a K, &'a V)> {
    self
      .keys
      .next_back()
      .and_then(|key| self.vals.next_back().map(|val| (key, val)))
  }
}

struct RawIter<'a, T> {
  _slice: PhantomData<&'a [T]>,
  start: *const T,
  end: *const T,
}

impl<'a, T> RawIter<'a, T> {
  unsafe fn new(slice: &[T]) -> Self {
    Self {
      _slice: PhantomData,
      start: slice.as_ptr(),
      end: if mem::size_of::<T>() == 0 {
        ((slice.as_ptr() as usize) + slice.len()) as *const _
      } else if slice.len() == 0 {
        slice.as_ptr()
      } else {
        slice.as_ptr().offset(slice.len() as isize)
      },
    }
  }
}

impl<'a, T> Iterator for RawIter<'a, T> {
  type Item = &'a T;
  fn next(&mut self) -> Option<&'a T> {
    if self.start == self.end {
      None
    } else {
      unsafe {
        let result = &*self.start;
        self.start = if mem::size_of::<T>() == 0 {
          (self.start as usize + 1) as *const _
        } else {
          self.start.offset(1)
        };
        Some(result)
      }
    }
  }

  fn size_hint(&self) -> (usize, Option<usize>) {
    let elem_size = mem::size_of::<T>();
    let len =
      (self.end as usize - self.start as usize) / if elem_size == 0 { 1 } else { elem_size };
    (len, Some(len))
  }
}

impl<'a, T> DoubleEndedIterator for RawIter<'a, T> {
  fn next_back(&mut self) -> Option<&'a T> {
    if self.start == self.end {
      None
    } else {
      unsafe {
        self.end = if mem::size_of::<T>() == 0 {
          (self.end as usize - 1) as *const _
        } else {
          self.end.offset(-1)
        };
        Some(&*self.end)
      }
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn new() {
    let map: LinMap<usize, usize> = LinMap::new();

    assert_eq!(map.len(), 0);
    assert_eq!(map.capacity(), 0);

    assert_eq!(map.buf.keys_ptr, NonNull::dangling());
    assert_eq!(map.buf.vals_ptr, NonNull::dangling());
    assert_eq!(mem::size_of::<LinMap<usize, usize>>(), 32);
  }

  #[test]
  fn insert() {
    let mut map: LinMap<usize, usize> = LinMap::new();

    assert!(map.insert(10, 5).is_none());

    assert_eq!(map.len(), 1);
    assert_eq!(map.capacity(), 4);

    assert_ne!(map.buf.keys_ptr, NonNull::dangling());
    assert_ne!(map.buf.vals_ptr, NonNull::dangling());

    assert_eq!(map.insert(10, 6), Some(5));

    assert_eq!(map.len(), 1);
    assert_eq!(map.capacity(), 4);

    assert_ne!(map.buf.keys_ptr, NonNull::dangling());
    assert_ne!(map.buf.vals_ptr, NonNull::dangling());

    assert!(map.insert(8, 6).is_none());

    assert_eq!(map.len(), 2);
    assert_eq!(map.capacity(), 4);

    assert_ne!(map.buf.keys_ptr, NonNull::dangling());
    assert_ne!(map.buf.vals_ptr, NonNull::dangling());
  }

  #[test]
  fn get() {
    let mut map: LinMap<usize, usize> = LinMap::new();

    assert_eq!(map.get(&5), None);

    map.insert(5, 10);

    assert_eq!(map.get(&5), Some(&10));
  }

  #[test]
  fn iter() {
    let mut map: LinMap<usize, usize> = LinMap::new();

    map.insert(1, 1);
    map.insert(2, 2);
    map.insert(3, 3);

    let mut iter = map.iter();
    assert_eq!(iter.next(), Some((&1, &1)));
    assert_eq!(iter.next(), Some((&2, &2)));
    assert_eq!(iter.next(), Some((&3, &3)));
    assert_eq!(iter.next(), None);
  }

  #[test]
  fn remove() {
    let mut map: LinMap<usize, usize> = LinMap::new();

    map.insert(1, 1);
    map.insert(2, 2);
    map.insert(3, 3);

    assert_eq!(map.remove(&2), Some(2));
    assert_eq!(map.len(), 2);
    assert_eq!(map.get(&2), None);

    assert_eq!(map.remove(&4), None);
    assert_eq!(map.len(), 2);
  }
}
