use crate::value::{Value, ValueVec};

/// Space Lox virtual machine byte codes
#[derive(Debug, PartialEq, Clone)]
pub enum ByteCode {
  /// Return ByteCode
  Return,

  /// Negate ByteCode
  Negate,

  /// Print ByteCode
  Print,

  /// Add ByteCode
  Add,

  /// Subtract ByteCode
  Subtract,

  /// Multiply Opcode
  Multiply,

  /// Divide ByteCode
  Divide,

  /// Not ByteCode
  Not,

  /// Constant ByteCode
  Constant(u8),

  /// Nill ByteCode
  Nil,

  /// True ByteCode
  True,

  /// False ByteCode
  False,

  /// Pop ByteCode
  Pop,

  /// Define Global ByteCode
  DefineGlobal(u8),

  /// Get Global ByteCode
  GetGlobal(u8),

  /// Set Global ByteCode
  SetGlobal(u8),

  /// Get Local ByteCode
  GetLocal(u8),

  /// Set Local ByteCode
  SetLocal(u8),

  /// Equal ByteCode
  Equal,

  /// Greater ByteCode
  Greater,

  /// Less ByteCode
  Less,
}

/// Represent tokens on a line
#[derive(Debug, Clone)]
struct Line {
  /// Line number
  pub line: i32,

  /// Count of tokens on the line
  pub count: i16,
}

impl Line {
  /// Create a new line
  fn new(line: i32, count: i16) -> Line {
    Line { line, count }
  }
}

/// Represents a chunk of code
#[derive(Debug, Clone, Default)]
pub struct Chunk<'a> {
  /// instructions in this code chunk
  pub instructions: Vec<ByteCode>,

  /// constants in this code chunk
  pub constants: ValueVec<'a>,

  /// debug line information
  lines: Vec<Line>,
}

impl<'a> Chunk<'a> {
  /// Write an instruction to this chunk
  ///
  /// # Examples
  /// ```
  /// use lox_runtime::chunk::{Chunk, ByteCode};
  ///
  /// let mut chunk = Chunk::default();
  /// chunk.write_instruction(ByteCode::Return, 0);
  /// chunk.write_instruction(ByteCode::Add, 0);
  /// chunk.write_instruction(ByteCode::Constant(10), 1);
  ///
  /// assert_eq!(chunk.instructions.len(), 3);
  /// // assert_eq!(chunk.constants.values.len(), 1);
  /// ```
  ///
  pub fn write_instruction(&mut self, op_code: ByteCode, line: i32) {
    self.instructions.push(op_code);

    match self.lines.last_mut() {
      Some(last_line) => {
        if last_line.line == line {
          last_line.count += 1;
        } else {
          self.lines.push(Line::new(line, 1));
        }
      }
      None => self.lines.push(Line::new(line, 1)),
    }
  }

  /// Add a constant to this chunk
  ///
  /// # Examples
  /// ```
  /// use lox_runtime::chunk::Chunk;
  /// use lox_runtime::value::Value;
  ///
  /// let mut chunk = Chunk::default();
  /// let index_1 = chunk.add_constant(Value::Number(10.4));
  /// let index_2 = chunk.add_constant(Value::Number(5.2));
  ///
  /// assert_eq!(index_1, 0);
  /// assert_eq!(index_2, 1);
  ///
  /// assert_eq!(chunk.constants.values[index_1], Value::Number(10.4));
  /// assert_eq!(chunk.constants.values[index_2], Value::Number(5.2));
  /// ```
  pub fn add_constant(&mut self, value: Value<'a>) -> usize {
    self.constants.values.push(value);
    self.constants.values.len() - 1
  }

  /// Get the line number at a token offset
  ///
  /// # Example
  /// ```
  /// use lox_runtime::chunk::{Chunk, ByteCode};
  ///
  /// let mut chunk = Chunk::default();
  ///
  /// chunk.write_instruction(ByteCode::Add, 0);
  /// chunk.write_instruction(ByteCode::Divide, 0);
  /// chunk.write_instruction(ByteCode::Return, 2);
  ///
  /// assert_eq!(chunk.get_line(0), 0);
  /// assert_eq!(chunk.get_line(1), 0);
  /// assert_eq!(chunk.get_line(2), 2);
  /// ```
  ///
  /// # Panics
  ///
  /// This method panics if an offset is past the last instruction
  ///
  /// ```rust,should_panic
  /// use lox_runtime::chunk::Chunk;
  ///
  /// let chunk = Chunk::default();
  /// chunk.get_line(3);
  /// ```
  pub fn get_line(&self, offset: usize) -> i32 {
    let mut current: usize = 0;
    for line in &self.lines {
      current += line.count as usize;
      if current > offset {
        return line.line;
      }
    }

    panic!(format!("Unable to find line at offset {}", offset));
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
      assert_eq!(line.count, 5);
    }
  }

  #[cfg(test)]
  mod chunk {

  }
}
