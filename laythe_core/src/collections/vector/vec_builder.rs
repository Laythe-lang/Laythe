
/// A structure used to construct a list via the allocator
pub struct VecBuilder<'a, T> {
  /// The source slice from which the list is copied from
  slice: &'a [T],

  /// The requested capacity of the list
  cap: usize,
}


impl<'a, T> VecBuilder<'a, T> {
  /// Create a list builder with a dummy slice and capacity
  pub fn cap_only(cap: usize) -> Self {
    let empty_slice: &'a [T] = &[];

    Self {
      slice: empty_slice,
      cap,
    }
  }

  /// Create a list builder with a slice and capacity
  pub fn new(slice: &'a [T], cap: usize) -> Self {
    assert!(slice.len() <= cap);

    Self { slice, cap }
  }

  /// The underlying slice for the builder
  pub fn slice(&self) -> &[T] {
    self.slice
  }

  /// The requested capacity
  pub fn cap(&self) -> usize {
    self.cap
  }
}