use crate::object::Bucket;


/// A structure used to construct a list via the allocator
pub struct MapBuilder {
  /// The requested capacity of the list
  cap: usize,
}


impl MapBuilder {
  /// Create a list builder with a dummy slice and capacity
  pub fn new(cap: usize) -> Self {
    Self {
      cap,
    }
  }

  /// The requested capacity
  pub fn cap(&self) -> usize {
    self.cap
  }
}