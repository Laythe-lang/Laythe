use std::{alloc::Layout, cmp, mem};

/// For a given offset determine the total offset until the next alignment
pub const fn next_aligned(num_bytes: usize, alignment: usize) -> usize {
  let remaining = num_bytes % alignment;
  if remaining == 0 {
    num_bytes
  } else {
    num_bytes + (alignment - remaining)
  }
}

/// Get the offset to the start of the `GcArray<T>` data
pub const fn get_offset<H, T>() -> usize {
  next_aligned(mem::size_of::<H>(), mem::align_of::<T>())
}

/// Determine the max alignment between the item `T`
/// and the `GcArray` header `Header`
pub fn max_align<H, T>() -> usize {
  let align_t = mem::align_of::<T>();
  let header_align = mem::align_of::<H>();
  cmp::max(align_t, header_align)
}

/// Create a rust `Layout` for a `GcArray` of `len` length.
pub fn make_array_layout<H, T>(len: usize) -> Layout {
  let alignment = max_align::<H, T>();

  let header_size = mem::size_of::<H>();
  let num_bytes = if len == 0 {
    header_size
  } else {
    next_aligned(header_size, mem::align_of::<T>()) + len * mem::size_of::<T>()
  };
  Layout::from_size_align(num_bytes, alignment).unwrap()
}

/// Create a rust `Layout` for a `GcArray` of `len` length.
pub fn make_layout<H, T>() -> Layout {
  let alignment = max_align::<H, T>();

  let header_size = mem::size_of::<H>();
  let num_bytes = next_aligned(header_size, mem::align_of::<T>()) + mem::size_of::<T>();
  Layout::from_size_align(num_bytes, alignment).unwrap()
}

#[cfg(test)]
mod tests {
  use crate::managed::utils::next_aligned;

  use super::*;
  #[test]
  fn next_aligned_test() {
    assert_eq!(next_aligned(9, 4), 12);
    assert_eq!(next_aligned(13, 4), 16);
    assert_eq!(next_aligned(12, 4), 12);
    assert_eq!(next_aligned(13, 1), 13);
    assert_eq!(next_aligned(8, 8), 8);
    assert_eq!(next_aligned(16, 32), 32);
    assert_eq!(next_aligned(16, 512), 512);
  }

  #[repr(align(512))]
  struct OverAligned {
    _data: [u8; 512],
  }

  #[test]
  fn max_align_test() {
    let header_alignment = mem::align_of::<u64>();

    assert!(mem::align_of::<i32>() <= mem::align_of::<u64>());
    assert_eq!(max_align::<u64, i32>(), header_alignment);

    assert!(mem::align_of::<u8>() <= mem::align_of::<u64>());
    assert_eq!(max_align::<u64, u8>(), header_alignment);

    assert!(mem::align_of::<OverAligned>() > mem::align_of::<u64>());
    assert_eq!(
      max_align::<u64, OverAligned>(),
      mem::align_of::<OverAligned>()
    );
  }

  #[test]
  fn make_array_layout_test() {
    // empty
    //
    let layout = make_array_layout::<u64, i32>(0);

    assert_eq!(layout.align(), mem::align_of::<u64>());
    assert_eq!(layout.size(), mem::size_of::<u64>());

    // non-empty, less than
    //
    let layout = make_array_layout::<u64, i32>(512);
    assert!(mem::align_of::<i32>() < mem::align_of::<u64>());
    assert_eq!(layout.align(), mem::align_of::<u64>());
    assert_eq!(
      layout.size(),
      mem::size_of::<u64>() + 512 * mem::size_of::<i32>()
    );

    // non-empty, equal
    //
    let layout = make_array_layout::<u64, i64>(512);
    assert_eq!(mem::align_of::<i64>(), mem::align_of::<u64>());
    assert_eq!(layout.align(), mem::align_of::<u64>());
    assert_eq!(
      layout.size(),
      mem::size_of::<u64>() + 512 * mem::size_of::<i64>()
    );

    // non-empty, greater
    let layout = make_array_layout::<u64, OverAligned>(512);
    assert!(mem::align_of::<OverAligned>() > mem::align_of::<u64>());
    assert_eq!(layout.align(), mem::align_of::<OverAligned>());
    assert_eq!(
      layout.size(),
      next_aligned(mem::size_of::<u64>(), mem::align_of::<OverAligned>())
        + 512 * mem::size_of::<OverAligned>()
    );
  }


  #[test]
  fn make_layout_test() {
    // non-empty, less than
    //
    let layout = make_layout::<u64, i32>();
    assert!(mem::align_of::<i32>() < mem::align_of::<u64>());
    assert_eq!(layout.align(), mem::align_of::<u64>());
    assert_eq!(
      layout.size(),
      mem::size_of::<u64>() + mem::size_of::<i32>()
    );

    // non-empty, equal
    //
    let layout = make_layout::<u64, i64>();
    assert_eq!(mem::align_of::<i64>(), mem::align_of::<u64>());
    assert_eq!(layout.align(), mem::align_of::<u64>());
    assert_eq!(
      layout.size(),
      mem::size_of::<u64>() + mem::size_of::<i64>()
    );

    // non-empty, greater
    let layout = make_layout::<u64, OverAligned>();
    assert!(mem::align_of::<OverAligned>() > mem::align_of::<u64>());
    assert_eq!(layout.align(), mem::align_of::<OverAligned>());
    assert_eq!(
      layout.size(),
      next_aligned(mem::size_of::<u64>(), mem::align_of::<OverAligned>())
        + mem::size_of::<OverAligned>()
    );
  }
}
