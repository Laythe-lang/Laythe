use crate::value::Value;

/// Space Lox virtual machine byte codes
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ByteCode {
  /// Return from script or function
  Return,

  /// Negate a value
  Negate,

  /// Print a value
  Print,

  /// Add the top two operands on the stack
  Add,

  /// Subtract the top two operands on the stack
  Subtract,

  /// Multiply the top two operands on the stack
  Multiply,

  /// Divide the top two operands on the stack
  Divide,

  /// Apply Not operator to top stack element
  Not,

  /// Retrieve a constant from the constants table
  Constant(u8),

  /// Nil literal
  Nil,

  /// True Literal
  True,

  /// False ByteCode
  False,

  /// Pop ByteCode
  Pop,

  /// Define a global in the globals table at a index
  DefineGlobal(u8),

  /// Retrieve a global at the given index
  GetGlobal(u8),

  /// Set a global at the given index
  SetGlobal(u8),

  /// Retrieve an upvalue at the given index
  GetUpvalue(u8),

  /// Set an upvalue at the given index
  SetUpvalue(u8),

  /// Get a local at the given index
  GetLocal(u8),

  /// Set a local at the given index
  SetLocal(u8),

  /// Get a property off a class instance
  GetProperty(u8),

  /// Set a property on a class instance
  SetProperty(u8),

  /// Jump to end of if block if false
  JumpIfFalse(u16),

  /// Jump conditionally to the ip
  Jump(u16),

  /// Jump to loop beginning
  Loop(u16),

  /// Call a function
  Call(u8),

  /// Invoke a method
  Invoke((u8, u8)),

  /// Invoke a method on a super class
  SuperInvoke((u8, u8)),

  /// Create a closure
  Closure(u8),

  /// Create a method
  Method(u8),

  /// Create a class
  Class(u8),

  /// Access this classes super 
  GetSuper(u8),

  /// Add inheritance to class
  Inherit,

  /// Close an upvalue by moving it to the stack
  CloseUpvalue,

  /// An upvalue index for a closure
  UpvalueIndex(UpvalueIndex),

  /// Temp loop placeholder
  Noop,

  /// Apply equality between the top two operands on the stack
  Equal,

  /// Apply greater between the top two operands on the stack
  Greater,

  /// Less greater between the top two operands on the stack
  Less,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UpvalueIndex {
  /// The upvalue is actually local
  Local(u8),

  /// The upvalue points to the enclosing function
  Upvalue(u8),
}

/// Contains the information to load
#[derive(Debug, PartialEq, Clone)]
pub struct ClosureLoader {
  /// The slot the closure is store
  slot: u8,

  /// The upvalues associated with this closure
  upvalues: Vec<UpvalueIndex>,
}

/// Represent tokens on a line
#[derive(Debug, Clone, PartialEq)]
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
#[derive(Clone, PartialEq, Default, Debug)]
pub struct Chunk {
  /// instructions in this code chunk
  pub instructions: Vec<ByteCode>,

  /// constants in this code chunk
  pub constants: Vec<Value>,

  /// debug line information
  lines: Vec<Line>,
}

impl Chunk {
  /// Write an instruction to this chunk
  ///
  /// # Examples
  /// ```
  /// use spacelox_core::chunk::{Chunk, ByteCode};
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
  /// use spacelox_core::chunk::Chunk;
  /// use spacelox_core::value::Value;
  ///
  /// let mut chunk = Chunk::default();
  /// let index_1 = chunk.add_constant(Value::Number(10.4));
  /// let index_2 = chunk.add_constant(Value::Number(5.2));
  ///
  /// assert_eq!(index_1, 0);
  /// assert_eq!(index_2, 1);
  ///
  /// assert_eq!(chunk.constants[index_1], Value::Number(10.4));
  /// assert_eq!(chunk.constants[index_2], Value::Number(5.2));
  /// ```
  pub fn add_constant(&mut self, value: Value) -> usize {
    self.constants.push(value);
    self.constants.len() - 1
  }

  /// Get the line number at a token offset
  ///
  /// # Example
  /// ```
  /// use spacelox_core::chunk::{Chunk, ByteCode};
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
  /// use spacelox_core::chunk::Chunk;
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
    use super::*;

    #[test]
    fn default() {
      let chunk = Chunk::default();
      assert_eq!(chunk.instructions.len(), 00);
      assert_eq!(chunk.constants.len(), 0);
    }

    #[test]
    fn write_instruction() {
      let mut chunk = Chunk::default();
      chunk.write_instruction(ByteCode::Nil, 0);

      assert_eq!(chunk.instructions.len(), 1);
      match chunk.instructions[0] {
        ByteCode::Nil => assert!(true),
        _ => assert!(false),
      }
    }

    #[test]
    fn add_constant() {
      let mut chunk = Chunk::default();
      let index = chunk.add_constant(Value::Nil);

      assert_eq!(index, 0);
      match chunk.constants[0] {
        Value::Nil => assert!(true),
        _ => assert!(false),
      }
    }

    #[test]
    fn get_line() {
      let mut chunk = Chunk::default();
      chunk.write_instruction(ByteCode::Nil, 0);
      assert_eq!(chunk.get_line(0), 0);
    }
  }
}
