//! Handle type
//!
//! Internal implemation of interned byte array.
//! All public types are built on it.
//! As a optimization, small arrays are stored inline, without heap allocation.
//! Max length of inlined array is `size_of::<usize>() * 2 - 1`.

use std::cell::RefCell;
use std::cmp::{Ordering, PartialOrd};
use std::collections::HashSet;
use std::fmt;
use std::mem;
use std::ops::Drop;
use std::ptr::NonNull;
use std::rc::Rc;
use std::slice;
use std::u8;

#[cfg(target_endian = "little")]
#[repr(C)]
#[derive(PartialEq, Eq, Ord)]
pub struct Handle {
  ptr: NonNull<u8>,
  len: usize,
}

#[cfg(target_endian = "big")]
#[repr(C)]
#[derive(PartialEq, Eq, Ord)]
pub struct Handle {
  len: usize,
  ptr: NonNull<u8>,
}

const INLINE_MASK: usize = 1;
const INLINE_TRUE: usize = 1;
const INLINE_FALSE: usize = 0;

const INLINE_ARRAY_SIZE: usize = mem::size_of::<Handle>();
const INLINE_MAX_LEN: usize = INLINE_ARRAY_SIZE - 1;

#[cfg(target_endian = "little")]
const INLINE_META: usize = 0;
#[cfg(target_endian = "little")]
const INLINE_START: usize = 1;

#[cfg(target_endian = "big")]
const INLINE_META: usize = 7;
#[cfg(target_endian = "big")]
const INLINE_START: usize = 0;

thread_local! {
    static POOL: RefCell<HashSet<Rc<[u8]>>> = Default::default();
}

impl Handle {
  #[inline]
  pub fn new(slice: &[u8]) -> Self {
    if slice.len() > INLINE_MAX_LEN {
      Handle::new_heap(slice)
    } else {
      Handle::new_inline(slice)
    }
  }

  #[inline]
  fn new_heap(slice: &[u8]) -> Self {
    let rc = POOL.with(|pool| {
      let cached = pool.borrow().get(slice).cloned();

      match cached {
        Some(rc) => rc,
        None => {
          let rc = Rc::from(slice);
          pool.borrow_mut().insert(Rc::clone(&rc));
          rc
        }
      }
    });

    let len = rc.len();
    let ptr = NonNull::new(rc.as_ptr() as *mut u8).unwrap();

    let rc_ptr = Rc::into_raw(rc);
    debug_assert_eq!(
      rc_ptr as *const u8,
      ptr.as_ptr(),
      "slice ptr does not match with ptr from Rc::into_raw"
    );
    // At the time this code is written, RcBox (heap-allocated part of Rc) contains
    // 2 usize fields to store strong/weak ref counter, which makes it to aligned for them.
    // This is not guaranteed by spec though, It's very unlikely to change to be unaligned.
    debug_assert_eq!(
      ptr.as_ptr() as usize & INLINE_MASK,
      INLINE_FALSE,
      "It seems like Rc ptr is not alligned with at least 2 bytes, {:p}",
      ptr
    );

    Handle { ptr, len }
  }

  #[inline]
  fn new_inline(slice: &[u8]) -> Self {
    debug_assert!(
      slice.len() <= INLINE_MAX_LEN,
      "Size is larger then INLINE_MAX_LEN"
    );
    debug_assert!(
      slice.len() <= (u8::MAX >> 1) as usize,
      "What a large, really?"
    );

    let length = slice.len() as u8;
    let meta = length << 1 | INLINE_TRUE as u8;

    let mut array = [0 as u8; INLINE_ARRAY_SIZE];
    array[INLINE_META] = meta;
    array[INLINE_START..INLINE_START + slice.len()].copy_from_slice(slice);

    unsafe { mem::transmute(array) }
  }

  #[inline]
  pub fn is_inline(&self) -> bool {
    match self.ptr.as_ptr() as usize & INLINE_MASK {
      INLINE_TRUE => true,
      INLINE_FALSE => false,
      _ => unreachable!(),
    }
  }

  #[inline]
  pub fn get(&self) -> &[u8] {
    if self.is_inline() {
      self.get_inline()
    } else {
      self.get_heap()
    }
  }

  #[inline]
  fn get_heap<'a>(&'a self) -> &'a [u8] {
    unsafe { slice::from_raw_parts(self.ptr.as_ptr(), self.len) }
  }

  #[inline]
  fn get_inline<'a>(&'a self) -> &'a [u8] {
    let bytes: &'a [u8; INLINE_ARRAY_SIZE] = unsafe { mem::transmute(self) };

    let len = bytes[INLINE_META] as usize >> 1;
    &bytes[INLINE_START..INLINE_START + len]
  }

  #[inline]
  fn get_rc(&self) -> mem::ManuallyDrop<Rc<[u8]>> {
    unsafe {
      let slice_ptr = slice::from_raw_parts(self.ptr.as_ptr(), self.len) as *const [u8];
      mem::ManuallyDrop::new(Rc::from_raw(slice_ptr))
    }
  }
}

impl Drop for Handle {
  #[inline]
  fn drop(&mut self) {
    if self.is_inline() {
      return;
    }

    let mut rc = self.get_rc();

    if Rc::strong_count(&rc) == 2 {
      POOL.with(|pool| {
        pool.borrow_mut().remove(&*rc);
      });
    }

    unsafe {
      mem::ManuallyDrop::drop(&mut rc);
    }
  }
}

impl Clone for Handle {
  #[inline]
  fn clone(&self) -> Self {
    if !self.is_inline() {
      let rc = self.get_rc();
      mem::forget(rc.clone());
    }

    unsafe { mem::transmute_copy(self) }
  }
}

impl PartialOrd for Handle {
  #[inline]
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    PartialOrd::partial_cmp(self.get(), other.get())
  }
}

impl fmt::Debug for Handle {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    if self.is_inline() {
      f.write_str("Handle::Inline")?;
    } else {
      f.write_str("Handle::Heap")?;
    }

    f.debug_list().entries(self.get()).finish()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_eq_short() {
    assert_eq!(Handle::new(&b"foo"[..]), Handle::new(&b"foo"[..]));
    assert_eq!(Handle::new(&b"bar"[..]).get(), &b"bar"[..]);
  }

  #[test]
  fn test_eq_long() {
    let data = &b"Lorem ipsum dolor sit amet, consectetur adipiscing elit"[..];

    assert_eq!(Handle::new(data), Handle::new(data));
    assert_eq!(Handle::new(data).get(), data);
  }

  #[test]
  fn test_pool_and_rc_count() {
    use std::mem::replace;

    let prev_pool = POOL.with(|pool| replace(&mut *pool.borrow_mut(), Default::default()));

    let pool_size = || POOL.with(|pool| pool.borrow().len());

    let data1 = &b"Lorem ipsum dolor sit amet, consectetur adipiscing elit"[..];
    let data2 = &b"sed do eiusmod tempor incididunt ut labore et dolore magna aliqua"[..];

    assert_eq!(pool_size(), 0);
    let _b0 = Handle::new(&b"foo"[..]);
    assert_eq!(pool_size(), 0);
    let b1 = Handle::new(data1);
    assert_eq!(pool_size(), 1);
    assert_eq!(Rc::strong_count(&b1.get_rc()), 2);
    let b2 = Handle::new(data2);
    assert_eq!(pool_size(), 2);
    let b3 = Handle::new(data1);
    assert_eq!(pool_size(), 2);
    assert_eq!(Rc::strong_count(&b1.get_rc()), 3);
    let b4 = Handle::clone(&b3);
    assert_eq!(Rc::strong_count(&b1.get_rc()), 4);
    drop(b2);
    assert_eq!(pool_size(), 1);
    drop(b1);
    drop(b4);
    assert_eq!(pool_size(), 1);
    assert_eq!(Rc::strong_count(&b3.get_rc()), 2);
    drop(b3);
    assert_eq!(pool_size(), 0);

    POOL.with(|pool| {
      replace(&mut *pool.borrow_mut(), prev_pool);
    })
  }
}
