use crate::hooks::GcHooks;
use crate::managed::{Array, DebugWrap};
use crate::{impl_debug_heap, impl_trace};
use crate::{managed::DebugHeap, managed::Trace, value::Value};
use std::cmp;
use std::fmt::Debug;

/// An object that can be encoded into a byte buffer
pub trait Encode: Sized {
  type Error: Debug;

  /// consume the object and return the number of bytes written to the buffer
  fn encode(data: Vec<Self>, lines: Vec<Line>) -> Result<(Vec<u8>, Vec<Line>), Self::Error>;
}

/// Represent tokens on a line
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Line {
  /// Line number
  pub line: u32,

  /// Count of tokens on the line
  pub offset: u32,
}

impl Line {
  /// Create a new line
  pub fn new(line: u32, offset: u32) -> Line {
    Line { line, offset }
  }
}

impl_trace!(u8);
impl_debug_heap!(u8);

impl_trace!(Line);
impl_debug_heap!(Line);
/// An immutable chunk of code
#[derive(Clone, PartialEq, Eq)]
pub struct Chunk {
  /// instruction in this code chunk
  instructions: Array<u8>,

  /// constants in this code chunk
  constants: Array<Value>,

  /// debug line information
  lines: Array<Line>,
}

impl Chunk {
  /// Create a new chunk
  pub fn new(instructions: Array<u8>, constants: Array<Value>, lines: Array<Line>) -> Self {
    Self {
      instructions,
      constants,
      lines,
    }
  }

  /// Create a stubbed chunk
  pub fn stub(hooks: &GcHooks) -> Self {
    Self::stub_with_instructions(hooks, &[0])
  }

  /// Create a stubbed chunk
  pub fn stub_with_instructions(hooks: &GcHooks, instructions: &[u8]) -> Self {
    Self {
      instructions: hooks.manage::<_, &[u8]>(instructions),
      constants: hooks.manage::<_, &[Value]>(&[]),
      lines: hooks.manage::<_, &[Line]>(&[Line::new(0, instructions.len() as u32)]),
    }
  }

  /// instruction in this code chunk
  #[inline]
  pub fn instructions(&self) -> &[u8] {
    &self.instructions
  }

  /// Retrieve a constant in the constants table at
  /// the provided offset
  #[inline]
  pub fn get_constant(&self, offset: usize) -> Value {
    self.constants[offset]
  }

  /// Retrieve a constant in the constants table at
  /// the provided offset without bounds checks
  ///
  /// # Safety
  /// This method assumes the index comes from a trusted
  /// source that is inbounds.
  #[inline]
  pub unsafe fn get_constant_unchecked(&self, offset: usize) -> Value {
    *self.constants.get_unchecked(offset)
  }

  /// Get the line number at a token offset
  ///
  /// # Panics
  ///
  /// This method panics if an offset is past the last instruction
  ///
  /// ```rust
  /// use laythe_core::chunk::Chunk;
  /// use laythe_core::hooks::{NoContext, GcHooks};
  ///
  /// let mut context = NoContext::default();
  /// let hooks = GcHooks::new(&mut context);
  ///
  /// let chunk = Chunk::stub(&hooks);
  /// chunk.get_line(3);
  /// ```
  pub fn get_line(&self, offset: usize) -> u32 {
    let result = self
      .lines
      .binary_search_by_key(&(offset), |line| line.offset as usize);

    match result {
      Ok(index) => self.lines[index].line,
      Err(index) => self.lines[cmp::min(index, self.lines.len() - 1)].line,
    }
  }
}

impl Trace for Chunk {
  fn trace(&self) {
    self.instructions.trace();
    self.constants.trace();
    self.lines.trace();
  }

  fn trace_debug(&self, log: &mut dyn std::io::Write) {
    self.instructions.trace_debug(log);
    self.constants.trace_debug(log);
    self.lines.trace_debug(log);
  }
}

impl DebugHeap for Chunk {
  fn fmt_heap(&self, f: &mut std::fmt::Formatter, depth: usize) -> std::fmt::Result {
    f.debug_struct("Chunk")
      .field("instructions", &DebugWrap(&self.instructions, depth))
      .field("constants", &DebugWrap(&self.constants, depth))
      .field("lines", &DebugWrap(&self.lines, depth))
      .finish()
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[derive(Default)]
  struct Encodable();

  impl Encode for Encodable {
    type Error = ();

    fn encode(data: Vec<Self>, lines: Vec<Line>) -> Result<(Vec<u8>, Vec<Line>), Self::Error> {
      let mut encoded = Vec::with_capacity(data.len());
      for _ in data {
        encoded.push(7)
      }

      Ok((encoded, lines))
    }
  }

  #[cfg(test)]
  mod line {
    use super::*;

    #[test]
    fn line_new() {
      let line = Line::new(10, 5);
      assert_eq!(line.line, 10);
      assert_eq!(line.offset, 5);
    }
  }

  #[cfg(test)]
  mod chunk {
    use crate::{
      chunk::{Chunk, Line},
      hooks::{GcHooks, NoContext}, value::Value,
    };

    #[test]
    fn get_line() {
      let mut context = NoContext::default();
      let hooks = GcHooks::new(&mut context);

      let chunk = Chunk::new(
        hooks.manage::<_, &[u8]>(&[0]),
        hooks.manage::<_, &[Value]>(&[]),
        hooks.manage::<_, &[Line]>(&[Line::new(0, 0)])
      );

      assert_eq!(chunk.get_line(0), 0);
    }
  }
}
