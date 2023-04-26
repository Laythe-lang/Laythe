use laythe_core::managed::DebugWrap;
use laythe_core::{managed::DebugHeap, managed::Trace, value::Value};

use crate::byte_code::SymbolicByteCode;

/// Represents a chunk of code
/// A mutable builder for a final immutable chunk
#[derive(Default)]
pub struct ChunkBuilder {
  /// instruction in this code chunk
  instructions: Vec<SymbolicByteCode>,

  /// constants in this code chunk
  constants: Vec<Value>,

  /// debug line information
  lines: Vec<u16>,
}

impl ChunkBuilder {
  /// instruction in this code chunk
  #[cfg(feature = "debug")]
  pub fn instructions(&self) -> &[SymbolicByteCode] {
    &self.instructions
  }

  /// Write an instruction to this chunk
  pub fn write_instruction(&mut self, instruction: SymbolicByteCode, line: u16) {
    self.instructions.push(instruction);
    self.lines.push(line);
  }

  /// Retrieve a constant in the constants table at
  /// the provided offset
  #[cfg(feature = "debug")]
  pub fn get_constant(&self, offset: usize) -> Value {
    self.constants[offset]
  }

  /// Add a constant to this chunk
  pub fn add_constant(&mut self, value: Value) -> usize {
    self.constants.push(value);
    self.constants.len() - 1
  }

  /// Takes the ownership of the internal pieces of this
  /// chunk builder
  pub fn take(self) -> (Vec<SymbolicByteCode>, Vec<Value>, Vec<u16>) {
    (self.instructions, self.constants, self.lines)
  }

  /// Get the line number at a token offset
  #[cfg(feature = "debug")]
  pub fn get_line(&self, offset: usize) -> u16 {
    if offset == self.lines.len() {
      self.lines[offset - 1]
    } else {
      self.lines[offset]
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

#[cfg(test)]
mod test {
  use super::*;

  #[cfg(test)]
  mod chunk_builder {
    use super::*;
    use laythe_core::value::VALUE_NIL;

    #[test]
    fn default() {
      let chunk = ChunkBuilder::default();
      assert_eq!(chunk.instructions.len(), 00);
      assert_eq!(chunk.constants.len(), 0);
    }

    #[test]
    fn write_instruction() {
      let mut chunk = ChunkBuilder::default();
      chunk.write_instruction(SymbolicByteCode::Add, 0);

      assert_eq!(chunk.instructions.len(), 1);
    }

    #[test]
    fn add_constant() {
      let mut chunk = ChunkBuilder::default();
      let index = chunk.add_constant(VALUE_NIL);

      assert_eq!(index, 0);
      assert!(chunk.constants[0].is_nil());
    }

    #[test]
    fn take() {
      let mut chunk = ChunkBuilder::default();
      chunk.add_constant(VALUE_NIL);

      chunk.write_instruction(SymbolicByteCode::Nil, 0);

      let (instructions, constants, lines) = chunk.take();

      assert_eq!(instructions.len(), 1);
      assert_eq!(constants.len(), 1);
      assert_eq!(lines.len(), 1);

      assert_eq!(instructions[0], SymbolicByteCode::Nil);
      assert_eq!(constants[0], VALUE_NIL);
      assert_eq!(lines[0], 0);
    }
  }
}
