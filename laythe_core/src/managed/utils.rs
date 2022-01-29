use std::{alloc::Layout, mem};

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

pub const fn get_array_len_offset<H>() -> usize {
  next_aligned(mem::size_of::<H>(), mem::align_of::<usize>())
}

pub const fn get_array_offset<H, T>() -> usize {
  let len_offset = get_array_len_offset::<H>();
  next_aligned(len_offset + mem::size_of::<usize>(), mem::align_of::<T>())
}

/// Determine the max alignment between the item `T`
/// and the `GcArray` header `Header`
pub const fn max_align<H, T>() -> usize {
  let t_align = mem::align_of::<T>();
  let header_align = mem::align_of::<H>();

  if t_align >= header_align {
    t_align
  } else {
    header_align
  }
}

/// Determine the max alignment between the item `T`
/// and the `GcArray` header `Header`
pub const fn max_array_align<H, T>() -> usize {
  let max_align = max_align::<H, T>();
  let len_align = mem::align_of::<usize>();

  if len_align >= max_align {
    len_align
  } else {
    max_align
  }
}

/// Create a rust `Layout` for a `GcArray` of `len` length.
pub fn make_array_layout<H, T>(len: usize) -> Layout {
  let alignment = max_array_align::<H, T>();

  let num_bytes = if len == 0 {
    get_array_offset::<H, T>()
  } else {
    get_array_offset::<H, T>() + len * mem::size_of::<T>()
  };

  Layout::from_size_align(num_bytes, alignment).unwrap()
}

/// Create a rust `Layout` for a `GcArray` of `len` length.
pub fn make_obj_layout<H, T>() -> Layout {
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
  fn max_array_align_test() {
    assert_eq!(max_array_align::<u16, u8>(), mem::align_of::<usize>());
    assert_eq!(max_align::<u128, u8>(), mem::align_of::<u128>());
    assert_eq!(
      max_align::<u128, OverAligned>(),
      mem::align_of::<OverAligned>()
    );
  }

  #[test]
  fn get_offset_test() {
    assert_eq!(get_offset::<u32, u8>(), 4);
    assert_eq!(get_offset::<u32, u32>(), 4);
    assert_eq!(get_offset::<u32, u64>(), 8);
  }

  #[test]
  fn get_array_offset_test() {
    assert_eq!(get_array_offset::<u32, u8>(), mem::size_of::<usize>() * 2);
    assert_eq!(get_array_offset::<u32, u32>(), mem::size_of::<usize>() * 2);
    assert_eq!(get_array_offset::<u32, u128>(), mem::size_of::<usize>() * 2);
  }

  #[test]
  fn get_array_len_offset_test() {
    assert_eq!(get_array_len_offset::<u8>(), mem::size_of::<usize>());
    assert_eq!(get_array_len_offset::<usize>(), mem::size_of::<usize>());
    assert_eq!(get_array_len_offset::<u128>(), mem::size_of::<u128>());
  }

  #[test]
  fn make_array_layout_test() {
    // empty
    //
    let layout = make_array_layout::<u64, i32>(0);

    assert_eq!(layout.align(), mem::align_of::<u64>());
    assert_eq!(
      layout.size(),
      mem::size_of::<u64>() + mem::size_of::<usize>()
    );

    // non-empty, less than
    //
    let layout = make_array_layout::<u64, i32>(512);
    assert!(mem::align_of::<i32>() < mem::align_of::<u64>());
    assert_eq!(layout.align(), mem::align_of::<u64>());
    assert_eq!(
      layout.size(),
      mem::size_of::<u64>() + mem::size_of::<usize>() + 512 * mem::size_of::<i32>()
    );

    // non-empty, equal
    //
    let layout = make_array_layout::<u64, i64>(512);
    assert_eq!(mem::align_of::<i64>(), mem::align_of::<u64>());
    assert_eq!(layout.align(), mem::align_of::<u64>());
    assert_eq!(
      layout.size(),
      mem::size_of::<u64>() + mem::size_of::<usize>() + 512 * mem::size_of::<i64>()
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
    let layout = make_obj_layout::<u64, i32>();
    assert!(mem::align_of::<i32>() < mem::align_of::<u64>());
    assert_eq!(layout.align(), mem::align_of::<u64>());
    assert_eq!(layout.size(), mem::size_of::<u64>() + mem::size_of::<i32>());

    // non-empty, equal
    //
    let layout = make_obj_layout::<u64, i64>();
    assert_eq!(mem::align_of::<i64>(), mem::align_of::<u64>());
    assert_eq!(layout.align(), mem::align_of::<u64>());
    assert_eq!(layout.size(), mem::size_of::<u64>() + mem::size_of::<i64>());

    // non-empty, greater
    let layout = make_obj_layout::<u64, OverAligned>();
    assert!(mem::align_of::<OverAligned>() > mem::align_of::<u64>());
    assert_eq!(layout.align(), mem::align_of::<OverAligned>());
    assert_eq!(
      layout.size(),
      next_aligned(mem::size_of::<u64>(), mem::align_of::<OverAligned>())
        + mem::size_of::<OverAligned>()
    );
  }
}
