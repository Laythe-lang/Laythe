use crate::hooks::GcHooks;
use crate::managed::{Array, DebugWrap};
use crate::{impl_debug_heap, impl_trace};
use crate::{managed::DebugHeap, managed::Trace, value::Value};
use std::cmp;

/// An object that can be encoded into a byte buffer
pub trait Encode {
  /// consume the object and return the number of bytes written to the buffer
  fn encode(self, buf: &mut Vec<u8>) -> u32;
}

/// Represent tokens on a line
#[derive(Debug, Clone, Copy, PartialEq)]
struct Line {
  /// Line number
  pub line: u32,

  /// Count of tokens on the line
  pub offset: u32,
}

impl Line {
  /// Create a new line
  fn new(line: u32, offset: u32) -> Line {
    Line { line, offset }
  }
}

impl_trace!(u8);
impl_debug_heap!(u8);

impl_trace!(Line);
impl_debug_heap!(Line);

/// Represents a chunk of code
/// A mutable builder for a final immutable chunk
#[derive(Default)]
pub struct ChunkBuilder {
  /// instruction in this code chunk
  instructions: Vec<u8>,

  /// constants in this code chunk
  constants: Vec<Value>,

  /// debug line information
  lines: Vec<Line>,
}

impl ChunkBuilder {
  /// instruction in this code chunk
  #[inline]
  pub fn instructions(&self) -> &[u8] {
    &self.instructions
  }

  /// Write an instruction to this chunk
  #[inline]
  pub fn write_instruction<T: Encode>(&mut self, item: T, line: u32) {
    let delta = item.encode(&mut self.instructions);
    let len = self.instructions.len() as u32;

    match self.lines.last_mut() {
      Some(last_line) => {
        if last_line.line == line {
          last_line.offset += delta;
        } else {
          self.lines.push(Line::new(line, len));
        }
      }
      None => self.lines.push(Line::new(line, len)),
    }
  }

  /// Patch an existing instruction in this check with
  /// a new value
  #[inline]
  pub fn patch_instruction(&mut self, index: usize, byte: u8) {
    self.instructions[index] = byte
  }

  /// Add a constant to this chunk
  ///
  /// # Examples
  /// ```
  /// use laythe_core::val;
  /// use laythe_core::chunk::ChunkBuilder;
  /// use laythe_core::value::Value;
  /// use laythe_core::hooks::{NoContext, GcHooks};
  ///
  /// let mut context = NoContext::default();
  /// let hooks = GcHooks::new(&mut context);
  ///
  /// let mut builder = ChunkBuilder::default();
  /// let index_1 = builder.add_constant(val!(10.4));
  /// let index_2 = builder.add_constant(val!(5.2));
  ///
  /// assert_eq!(index_1, 0);
  /// assert_eq!(index_2, 1);
  ///
  /// let chunk = builder.build(&hooks);
  ///
  /// assert_eq!(chunk.get_constant(index_1), val!(10.4));
  /// assert_eq!(chunk.get_constant(index_2), val!(5.2));
  /// ```
  #[inline]
  pub fn add_constant(&mut self, value: Value) -> usize {
    self.constants.push(value);
    self.constants.len() - 1
  }

  /// Build the final chunk from this builder. Consumes this
  /// chunk builder in the process
  pub fn build(self, hooks: &GcHooks) -> Chunk {
    let instructions = hooks.manage(&*self.instructions);
    hooks.push_root(instructions);
    let constants = hooks.manage(&*self.constants);
    hooks.push_root(constants);
    let lines = hooks.manage(&*self.lines);

    hooks.pop_roots(2);

    Chunk {
      instructions,
      constants,
      lines,
    }
  }
}

impl Trace for ChunkBuilder {
  fn trace(&self) {
    self.constants.iter().for_each(|constant| constant.trace());
  }

  fn trace_debug(&self, log: &mut dyn std::io::Write) {
    self
      .constants
      .iter()
      .for_each(|constant| constant.trace_debug(log));
  }
}

impl DebugHeap for ChunkBuilder {
  fn fmt_heap(&self, f: &mut std::fmt::Formatter, depth: usize) -> std::fmt::Result {
    f.debug_struct("ChunkBuilder")
      .field("instructions", &self.instructions)
      .field("constants", &DebugWrap(&&*self.constants, depth))
      .field("lines", &self.lines)
      .finish()
  }
}

/// An immutable chunk of code
#[derive(Clone, PartialEq)]
pub struct Chunk {
  /// instruction in this code chunk
  instructions: Array<u8>,

  /// constants in this code chunk
  constants: Array<Value>,

  /// debug line information
  lines: Array<Line>,
}

impl Chunk {
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
  /// ```rust,should_panic
  /// use laythe_core::chunk::ChunkBuilder;
  /// use laythe_core::hooks::{NoContext, GcHooks};
  ///
  /// let mut context = NoContext::default();
  /// let hooks = GcHooks::new(&mut context);
  ///
  /// let builder = ChunkBuilder::default();
  /// let chunk = builder.build(&hooks);
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
  struct Encodable();

  impl Encode for Encodable {
    fn encode(self, buf: &mut Vec<u8>) -> u32 {
      buf.push(7);
      1
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
  mod chunk_builder {
    use super::*;

    #[test]
    fn default() {
      let chunk = ChunkBuilder::default();
      assert_eq!(chunk.instructions.len(), 00);
      assert_eq!(chunk.constants.len(), 0);
    }

    #[test]
    fn write_instruction() {
      let mut chunk = ChunkBuilder::default();
      chunk.write_instruction(Encodable(), 0);

      assert_eq!(chunk.instructions.len(), 1);
      assert_eq!(chunk.instructions[0], 7)
    }

    #[test]
    fn add_constant() {
      use crate::value::VALUE_NIL;

      let mut chunk = ChunkBuilder::default();
      let index = chunk.add_constant(VALUE_NIL);

      assert_eq!(index, 0);
      assert!(chunk.constants[0].is_nil());
    }
  }

  #[cfg(test)]
  mod chunk {
    use crate::{
      chunk::ChunkBuilder,
      hooks::{GcHooks, NoContext},
    };

    use super::Encodable;

    #[test]
    fn get_line() {
      let mut context = NoContext::default();
      let hooks = GcHooks::new(&mut context);

      let mut builder = ChunkBuilder::default();
      builder.write_instruction(Encodable(), 0);
      assert_eq!(builder.build(&hooks).get_line(0), 0);
    }
  }
}
