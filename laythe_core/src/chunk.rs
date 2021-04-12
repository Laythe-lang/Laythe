use crate::{managed::Trace, value::Value};
use std::cmp;
use std::mem;

/// An object that can be encoded into a byte buffer
pub trait Encode {
  /// consume the object and return the number of bytes written to the buffer
  fn encode(self, buf: &mut Vec<u8>) -> u32;
}

/// Represent tokens on a line
#[derive(Debug, Clone, PartialEq)]
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

/// Represents a chunk of code
/// A mutable builder for a final immutable chunk
#[derive(Default, Debug)]
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
      },
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
  ///
  /// let mut builder = ChunkBuilder::default();
  /// let index_1 = builder.add_constant(val!(10.4));
  /// let index_2 = builder.add_constant(val!(5.2));
  ///
  /// assert_eq!(index_1, 0);
  /// assert_eq!(index_2, 1);
  ///
  /// let chunk = builder.build();
  ///
  /// assert_eq!(chunk.get_constant(index_1), val!(10.4));
  /// assert_eq!(chunk.get_constant(index_2), val!(5.2));
  /// ```
  #[inline]
  pub fn add_constant(&mut self, value: Value) -> usize {
    self.constants.push(value);
    self.constants.len() - 1
  }

  /// Get the approximate size of this chunk in bytes
  pub fn size(&self) -> usize {
    mem::size_of::<Self>()
      + mem::size_of::<u8>() * self.instructions.capacity()
      + mem::size_of::<Value>() * self.constants.capacity()
      + mem::size_of::<Line>() * self.lines.capacity()
  }

  /// Build the final chunk from this builder. Consumes this
  /// chunk builder in the process
  pub fn build(self) -> Chunk {
    Chunk {
      instructions: self.instructions.into_boxed_slice(),
      constants: self.constants.into_boxed_slice(),
      lines: self.lines.into_boxed_slice(),
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

/// An immutable chunk of code
#[derive(Clone, PartialEq, Default, Debug)]
pub struct Chunk {
  /// instruction in this code chunk
  instructions: Box<[u8]>,

  /// constants in this code chunk
  constants: Box<[Value]>,

  /// debug line information
  lines: Box<[Line]>,
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
  /// use laythe_core::chunk::Chunk;
  ///
  /// let chunk = Chunk::default();
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

  /// Get the size of this chunk in bytes
  pub fn size(&self) -> usize {
    mem::size_of::<Self>()
      + mem::size_of::<u8>() * self.instructions.len()
      + mem::size_of::<Value>() * self.constants.len()
      + mem::size_of::<Line>() * self.lines.len()
  }
}

impl Trace for Chunk {
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
      let chunk = Chunk::default();
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
    use crate::chunk::ChunkBuilder;

    use super::Encodable;

    #[test]
    fn get_line() {
      let mut builder = ChunkBuilder::default();
      builder.write_instruction(Encodable(), 0);
      assert_eq!(builder.build().get_line(0), 0);
    }
  }
}
