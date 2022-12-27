/// An exception handler in Laythe
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ExceptionHandler {
  /// The ip offset from the start call frame
  offset: usize,

  /// The calldepth of this handler
  call_frame_depth: usize,

  /// The slot depth at this handler
  slot_depth: usize,
}

impl ExceptionHandler {
  pub fn new(offset: usize, call_frame_depth: usize, slot_depth: usize) -> Self {
    Self {
      offset,
      call_frame_depth,
      slot_depth,
    }
  }

  pub fn offset(&self) -> usize {
    self.offset
  }

  pub fn call_frame_depth(&self) -> usize {
    self.call_frame_depth
  }

  pub fn slot_depth(&self) -> usize {
    self.slot_depth
  }
}
