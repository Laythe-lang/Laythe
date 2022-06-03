use laythe_core::chunk::Line;
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
  lines: Vec<Line>,
}

impl ChunkBuilder {
  /// instruction in this code chunk
  #[cfg(feature = "debug")]
  pub fn instructions(&self) -> &[SymbolicByteCode] {
    &self.instructions
  }

  /// Write an instruction to this chunk
  pub fn write_instruction(&mut self, instruction: SymbolicByteCode, line: u32) {
    self.instructions.push(instruction);
    let len = self.instructions.len() as u32;

    match self.lines.last_mut() {
      Some(last_line) => {
        if last_line.line == line {
          last_line.offset = len;
        } else {
          self.lines.push(Line::new(line, len));
        }
      },
      None => self.lines.push(Line::new(line, len)),
    }
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
  pub fn take(self) -> (Vec<SymbolicByteCode>, Vec<Value>, Vec<Line>) {
    (self.instructions, self.constants, self.lines)
  }

  /// Get the line number at a token offset
  #[cfg(feature = "debug")]
  pub fn get_line(&self, offset: usize) -> u32 {
    use std::cmp;

    let result = self
      .lines
      .binary_search_by_key(&(offset), |line| line.offset as usize);

    match result {
      Ok(index) => self.lines[index].line,
      Err(index) => self.lines[cmp::min(index, self.lines.len() - 1)].line,
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
      assert_eq!(lines[0].line, 0);
      assert_eq!(lines[0].offset, 1);
    }
  }
}
