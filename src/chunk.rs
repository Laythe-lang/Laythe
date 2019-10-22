use crate::value::ValueVec;

/// Space Lox virtual machine byte codes
#[derive(Debug)]
pub enum OpCode {

  /// Return OpCode
  Return,

  /// Negate OpCode
  Negate,

  /// Add OpCode
  Add,

  /// Subtract OpCode
  Subtract,

  /// Multiply Opcode
  Multiply,

  /// Divide OpCode
  Divide,

  /// Constant OpCode
  Constant(usize)
}

/// Represent tokens on a line
#[derive(Debug)]
struct Line {

  /// Line number
  pub line: i32,

  /// Count of tokens on the line
  pub count: i16,
}


impl Line {

  /// Create a new line
  fn new(line: i32, count: i16) -> Line {
    return Line { line, count };
  }
}

/// Represents a chunk of code
#[derive(Debug)]
pub struct Chunk {

  /// instructions in this code chunk
  pub instructions: Vec<OpCode>,

  /// constants in this code chunk
  pub constants: ValueVec,

  /// debug line information
  lines: Vec<Line>,
}

impl Chunk {

  /// Create a new chunk
  /// 
  /// # Examples
  /// ```
  /// use lox_runtime::chunk::Chunk;
  /// 
  /// let chunk = Chunk::new();
  /// assert_eq!(chunk.constants.values.len(), 0); 
  /// assert_eq!(chunk.instructions.len(), 0);
  /// ```
  pub fn new() -> Chunk {
    Chunk {
      instructions: Vec::new(),
      constants: ValueVec::new(),
      lines: Vec::new(),
    }
  }

  /// Write an instruction to this chunk
  /// 
  /// # Examples
  /// ```
  /// use lox_runtime::chunk::{Chunk, OpCode};
  /// 
  /// let mut chunk = Chunk::new();
  /// chunk.write_instruction(OpCode::Return, 0);
  /// chunk.write_instruction(OpCode::Add, 0);
  /// chunk.write_instruction(OpCode::Constant(10), 1);
  /// 
  /// assert_eq!(chunk.instructions.len(), 3);
  /// // assert_eq!(chunk.constants.values.len(), 1);
  /// ```
  /// 
  pub fn write_instruction(&mut self, op_code: OpCode, line: i32) {
    self.instructions.push(op_code);

    match self.lines.last_mut() {
      Some(last_line) => {
        if last_line.line == line {
          last_line.count += 1;
        } else {
          self.lines.push(Line::new(line, 1));
        }
      }
      None => self.lines.push(Line::new(line, 1))
    }
  }

  /// Add a constant to this chunk
  /// 
  /// # Examples
  /// ```
  /// use lox_runtime::chunk::Chunk;
  /// 
  /// let mut chunk = Chunk::new(); 
  /// let index_1 = chunk.add_constant(10.4); 
  /// let index_2 = chunk.add_constant(5.2);
  /// 
  /// assert_eq!(index_1, 0);
  /// assert_eq!(index_2, 1);
  /// 
  /// assert_eq!(chunk.constants.values[index_1], 10.4);
  /// assert_eq!(chunk.constants.values[index_2], 5.2);
  /// ```
  pub fn add_constant(&mut self, value: f64) -> usize {
    self.constants.values.push(value);
    return self.constants.values.len() - 1;
  }

  /// Get the line number at a token offset
  /// 
  /// # Example
  /// ```
  /// use lox_runtime::chunk::{Chunk, OpCode};
  /// 
  /// let mut chunk = Chunk::new();
  /// 
  /// chunk.write_instruction(OpCode::Add, 0);
  /// chunk.write_instruction(OpCode::Divide, 0);
  /// chunk.write_instruction(OpCode::Return, 2);
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
  /// let chunk = Chunk::new();
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
}