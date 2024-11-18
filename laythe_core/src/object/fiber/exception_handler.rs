use crate::managed::{DebugHeap, Trace};

/// An exception handler in Laythe
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ExceptionHandler {
  /// The ip offset from the start call frame
  offset: usize,

  /// The call depth of this handler
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

impl Trace for ExceptionHandler {}
impl DebugHeap for ExceptionHandler {
  fn fmt_heap(&self, f: &mut std::fmt::Formatter, _: usize) -> std::fmt::Result {
    f.debug_struct("ExceptionHandler")
      .field("offset", &self.offset)
      .field("call_frame_depth", &self.call_frame_depth)
      .field("slot_depth", &self.slot_depth)
      .finish()
  }
}
