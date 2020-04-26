use crate::value::Value;
use std::cmp;
use std::convert::TryInto;
use std::mem;

/// Space Lox virtual machine byte codes
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum AlignedByteCode {
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

  /// Create an empty list
  List,

  /// Initialize list from literal
  ListInit(u16),

  /// Create an empty map
  Map,

  /// Initial map from literal
  MapInit(u16),

  /// Get from an index
  GetIndex,

  /// Set to an index
  SetIndex,

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

  // An upvalue index for a closure
  UpvalueIndex(UpvalueIndex),

  /// Apply equality between the top two operands on the stack
  Equal,

  /// Apply greater between the top two operands on the stack
  Greater,

  /// Less greater between the top two operands on the stack
  Less,
}

impl AlignedByteCode {
  /// Encode aligned bytecode as unaligned bytecode for better storage / compactness
  pub fn encode(self, code: &mut Vec<u8>) {
    match self {
      Self::Return => push_op(code, ByteCode::Return),
      Self::Negate => push_op(code, ByteCode::Negate),
      Self::Print => push_op(code, ByteCode::Print),
      Self::Add => push_op(code, ByteCode::Add),
      Self::Subtract => push_op(code, ByteCode::Subtract),
      Self::Multiply => push_op(code, ByteCode::Multiply),
      Self::Divide => push_op(code, ByteCode::Divide),
      Self::Not => push_op(code, ByteCode::Not),
      Self::Nil => push_op(code, ByteCode::Nil),
      Self::True => push_op(code, ByteCode::True),
      Self::False => push_op(code, ByteCode::False),
      Self::List => push_op(code, ByteCode::List),
      Self::ListInit(slot) => push_op_u16(code, ByteCode::ListInit, slot),
      Self::Map => push_op(code, ByteCode::Map),
      Self::MapInit(slot) => push_op_u16(code, ByteCode::MapInit, slot),
      Self::GetIndex => push_op(code, ByteCode::GetIndex),
      Self::SetIndex => push_op(code, ByteCode::SetIndex),
      Self::Equal => push_op(code, ByteCode::Equal),
      Self::Greater => push_op(code, ByteCode::Greater),
      Self::Less => push_op(code, ByteCode::Less),
      Self::Pop => push_op(code, ByteCode::Pop),
      Self::Constant(slot) => push_op_u8(code, ByteCode::Constant, slot),
      Self::DefineGlobal(slot) => push_op_u8(code, ByteCode::DefineGlobal, slot),
      Self::GetGlobal(slot) => push_op_u8(code, ByteCode::GetGlobal, slot),
      Self::SetGlobal(slot) => push_op_u8(code, ByteCode::SetGlobal, slot),
      Self::GetUpvalue(slot) => push_op_u8(code, ByteCode::GetUpvalue, slot),
      Self::SetUpvalue(slot) => push_op_u8(code, ByteCode::SetUpvalue, slot),
      Self::GetLocal(slot) => push_op_u8(code, ByteCode::GetLocal, slot),
      Self::SetLocal(slot) => push_op_u8(code, ByteCode::SetLocal, slot),
      Self::GetProperty(slot) => push_op_u8(code, ByteCode::GetProperty, slot),
      Self::SetProperty(slot) => push_op_u8(code, ByteCode::SetProperty, slot),
      Self::JumpIfFalse(slot) => push_op_u16(code, ByteCode::JumpIfFalse, slot),
      Self::Jump(slot) => push_op_u16(code, ByteCode::Jump, slot),
      Self::Loop(slot) => push_op_u16(code, ByteCode::Loop, slot),
      Self::Call(slot) => push_op_u8(code, ByteCode::Call, slot),
      Self::Invoke((slot1, slot2)) => push_op_u8_tuple(code, ByteCode::Invoke, slot1, slot2),
      Self::SuperInvoke((slot1, slot2)) => {
        push_op_u8_tuple(code, ByteCode::SuperInvoke, slot1, slot2)
      }
      Self::Closure(slot) => push_op_u8(code, ByteCode::Closure, slot),
      Self::Method(slot) => push_op_u8(code, ByteCode::Method, slot),
      Self::Class(slot) => push_op_u8(code, ByteCode::Class, slot),
      Self::GetSuper(slot) => push_op_u8(code, ByteCode::GetSuper, slot),
      Self::Inherit => push_op(code, ByteCode::Inherit),
      Self::CloseUpvalue => push_op(code, ByteCode::CloseUpvalue),
      Self::UpvalueIndex(index) => {
        let encoded: u16 = unsafe { mem::transmute(index) };
        let bytes = encoded.to_ne_bytes();
        code.extend_from_slice(&bytes);
      }
    }
  }

  /// Decode unaligned bytecode to aligned bytecode. Primarily for testing purposes
  pub fn decode(store: &[u8], offset: usize) -> (AlignedByteCode, usize) {
    let byte_code = ByteCode::from(store[offset]);

    match byte_code {
      ByteCode::Return => (AlignedByteCode::Return, offset + 1),
      ByteCode::Negate => (AlignedByteCode::Negate, offset + 1),
      ByteCode::Print => (AlignedByteCode::Print, offset + 1),
      ByteCode::Add => (AlignedByteCode::Add, offset + 1),
      ByteCode::Subtract => (AlignedByteCode::Subtract, offset + 1),
      ByteCode::Multiply => (AlignedByteCode::Multiply, offset + 1),
      ByteCode::Divide => (AlignedByteCode::Divide, offset + 1),
      ByteCode::Not => (AlignedByteCode::Not, offset + 1),
      ByteCode::Constant => (AlignedByteCode::Constant(store[offset + 1]), offset + 2),
      ByteCode::Nil => (AlignedByteCode::Nil, offset + 1),
      ByteCode::True => (AlignedByteCode::True, offset + 1),
      ByteCode::False => (AlignedByteCode::False, offset + 1),
      ByteCode::List => (AlignedByteCode::List, offset + 1),
      ByteCode::ListInit => (
        AlignedByteCode::ListInit(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Map => (AlignedByteCode::Map, offset + 1),
      ByteCode::MapInit => (
        AlignedByteCode::MapInit(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::GetIndex => (AlignedByteCode::GetIndex, offset + 1),
      ByteCode::SetIndex => (AlignedByteCode::SetIndex, offset + 1),
      ByteCode::Pop => (AlignedByteCode::Pop, offset + 1),
      ByteCode::DefineGlobal => (AlignedByteCode::DefineGlobal(store[offset + 1]), offset + 2),
      ByteCode::GetGlobal => (AlignedByteCode::GetGlobal(store[offset + 1]), offset + 2),
      ByteCode::SetGlobal => (AlignedByteCode::SetGlobal(store[offset + 1]), offset + 2),
      ByteCode::GetUpvalue => (AlignedByteCode::GetUpvalue(store[offset + 1]), offset + 2),
      ByteCode::SetUpvalue => (AlignedByteCode::SetUpvalue(store[offset + 1]), offset + 2),
      ByteCode::GetLocal => (AlignedByteCode::GetLocal(store[offset + 1]), offset + 2),
      ByteCode::SetLocal => (AlignedByteCode::SetLocal(store[offset + 1]), offset + 2),
      ByteCode::GetProperty => (AlignedByteCode::GetProperty(store[offset + 1]), offset + 2),
      ByteCode::SetProperty => (AlignedByteCode::SetProperty(store[offset + 1]), offset + 2),
      ByteCode::JumpIfFalse => (
        AlignedByteCode::JumpIfFalse(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Jump => (
        AlignedByteCode::Jump(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Loop => (
        AlignedByteCode::Loop(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Call => (AlignedByteCode::Call(store[offset + 1]), offset + 2),
      ByteCode::Invoke => (
        AlignedByteCode::Invoke((store[offset + 1], store[offset + 2])),
        offset + 3,
      ),
      ByteCode::SuperInvoke => (
        AlignedByteCode::SuperInvoke((store[offset + 1], store[offset + 2])),
        offset + 3,
      ),
      ByteCode::Closure => (AlignedByteCode::Closure(store[offset + 1]), offset + 2),
      ByteCode::Method => (AlignedByteCode::Method(store[offset + 1]), offset + 2),
      ByteCode::Class => (AlignedByteCode::Class(store[offset + 1]), offset + 2),
      ByteCode::GetSuper => (AlignedByteCode::GetSuper(store[offset + 1]), offset + 2),
      ByteCode::Inherit => (AlignedByteCode::Inherit, offset + 1),
      ByteCode::CloseUpvalue => (AlignedByteCode::CloseUpvalue, offset + 1),
      ByteCode::Equal => (AlignedByteCode::Equal, offset + 1),
      ByteCode::Greater => (AlignedByteCode::Greater, offset + 1),
      ByteCode::Less => (AlignedByteCode::Less, offset + 1),
    }
  }
}

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
  Constant,

  /// Nil literal
  Nil,

  /// True Literal
  True,

  /// False ByteCode
  False,

  /// Empty list
  List,

  /// Initialize List
  ListInit,

  /// Empty Map
  Map,

  /// Initialize map
  MapInit,

  /// Get an index
  GetIndex,

  /// Set an index
  SetIndex,

  /// Pop ByteCode
  Pop,

  /// Define a global in the globals table at a index
  DefineGlobal,

  /// Retrieve a global at the given index
  GetGlobal,

  /// Set a global at the given index
  SetGlobal,

  /// Retrieve an upvalue at the given index
  GetUpvalue,

  /// Set an upvalue at the given index
  SetUpvalue,

  /// Get a local at the given index
  GetLocal,

  /// Set a local at the given index
  SetLocal,

  /// Get a property off a class instance
  GetProperty,

  /// Set a property on a class instance
  SetProperty,

  /// Jump to end of if block if false
  JumpIfFalse,

  /// Jump conditionally to the ip
  Jump,

  /// Jump to loop beginning
  Loop,

  /// Call a function
  Call,

  /// Invoke a method
  Invoke,

  /// Invoke a method on a super class
  SuperInvoke,

  /// Create a closure
  Closure,

  /// Create a method
  Method,

  /// Create a class
  Class,

  /// Access this classes super
  GetSuper,

  /// Add inheritance to class
  Inherit,

  /// Close an upvalue by moving it to the stack
  CloseUpvalue,

  /// Apply equality between the top two operands on the stack
  Equal,

  /// Apply greater between the top two operands on the stack
  Greater,

  /// Less greater between the top two operands on the stack
  Less,
}

impl ByteCode {
  /// Convert this bytecode to its underlying byte.
  fn to_byte(self) -> u8 {
    unsafe { mem::transmute(self) }
  }
}

impl From<u8> for ByteCode {
  /// Get the enum bytecode for a raw byte
  #[inline]
  fn from(byte: u8) -> Self {
    unsafe { mem::transmute(byte) }
  }
}

pub fn decode_u32(buffer: &[u8]) -> u32 {
  let arr: [u8; 4] = buffer.try_into().expect("slice of incorrect length.");
  u32::from_ne_bytes(arr)
}

pub fn decode_u16(buffer: &[u8]) -> u16 {
  let arr: [u8; 2] = buffer.try_into().expect("slice of incorrect length.");
  u16::from_ne_bytes(arr)
}

fn push_op(code: &mut Vec<u8>, byte: ByteCode) {
  code.push(byte.to_byte());
}

fn push_op_u8(code: &mut Vec<u8>, byte: ByteCode, param: u8) {
  code.push(byte.to_byte());
  code.push(param);
}

fn push_op_u8_tuple(code: &mut Vec<u8>, byte: ByteCode, param1: u8, param2: u8) {
  code.push(byte.to_byte());
  code.push(param1);
  code.push(param2);
}

fn push_op_u16(code: &mut Vec<u8>, byte: ByteCode, param: u16) {
  let param_bytes = param.to_ne_bytes();
  code.push(byte.to_byte());
  code.extend_from_slice(&param_bytes);
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
#[derive(Clone, PartialEq, Default, Debug)]
pub struct Chunk {
  /// instructions in this code chunk
  pub instructions: Vec<u8>,

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
  /// use spacelox_core::chunk::{Chunk, AlignedByteCode};
  ///
  /// let mut chunk = Chunk::default();
  /// chunk.write_instruction(AlignedByteCode::Return, 0);
  /// chunk.write_instruction(AlignedByteCode::Add, 0);
  /// chunk.write_instruction(AlignedByteCode::Constant(10), 1);
  ///
  /// assert_eq!(chunk.instructions.len(), 4);
  /// ```
  ///
  pub fn write_instruction(&mut self, op_code: AlignedByteCode, line: u32) {
    let l1 = self.instructions.len() as u32;
    op_code.encode(&mut self.instructions);
    let l2 = self.instructions.len() as u32;
    let delta = l2 - l1;

    match self.lines.last_mut() {
      Some(last_line) => {
        if last_line.line == line {
          last_line.offset += delta;
        } else {
          self.lines.push(Line::new(line, l2));
        }
      }
      None => self.lines.push(Line::new(line, l2)),
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
  /// use spacelox_core::chunk::{Chunk, AlignedByteCode};
  /// let mut chunk = Chunk::default();
  ///     
  /// chunk.write_instruction(AlignedByteCode::Add, 0);
  /// chunk.write_instruction(AlignedByteCode::Divide, 0);
  /// chunk.write_instruction(AlignedByteCode::Return, 2);
  /// chunk.write_instruction(AlignedByteCode::Constant(2), 3);
  ///
  /// assert_eq!(chunk.get_line(1), 0);
  /// assert_eq!(chunk.get_line(2), 0);
  /// assert_eq!(chunk.get_line(3), 2);
  /// assert_eq!(chunk.get_line(4), 3);
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
  pub fn get_line(&self, offset: usize) -> u32 {
    let result = self
      .lines
      .binary_search_by_key(&(offset), |line| line.offset as usize);

    match result {
      Ok(index) => self.lines[index].line,
      Err(index) => self.lines[cmp::min(index, self.lines.len() - 1)].line,
    }
  }

  /// Get the approximate size of this chunk in bytes
  pub fn size(&self) -> usize {
    mem::size_of::<Self>()
      + mem::size_of::<u8>() * self.instructions.capacity()
      + mem::size_of::<Value>() * self.constants.capacity()
      + mem::size_of::<Line>() * self.lines.capacity()
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod byte_code {
    use super::*;
    // use std::mem;

    #[test]
    fn encode_decode() {
      let code: Vec<(usize, AlignedByteCode)> = vec![
        (1, AlignedByteCode::Return),
        (1, AlignedByteCode::Negate),
        (1, AlignedByteCode::Print),
        (1, AlignedByteCode::Add),
        (1, AlignedByteCode::Subtract),
        (1, AlignedByteCode::Multiply),
        (1, AlignedByteCode::Divide),
        (1, AlignedByteCode::Not),
        (2, AlignedByteCode::Constant(173)),
        (1, AlignedByteCode::Nil),
        (1, AlignedByteCode::True),
        (1, AlignedByteCode::False),
        (3, AlignedByteCode::ListInit(13444)),
        (1, AlignedByteCode::Pop),
        (2, AlignedByteCode::DefineGlobal(173)),
        (2, AlignedByteCode::GetGlobal(173)),
        (2, AlignedByteCode::SetGlobal(173)),
        (2, AlignedByteCode::GetUpvalue(173)),
        (2, AlignedByteCode::SetUpvalue(173)),
        (2, AlignedByteCode::GetLocal(173)),
        (2, AlignedByteCode::SetLocal(173)),
        (2, AlignedByteCode::GetProperty(173)),
        (2, AlignedByteCode::SetProperty(173)),
        (3, AlignedByteCode::JumpIfFalse(13444)),
        (3, AlignedByteCode::Jump(13444)),
        (3, AlignedByteCode::Loop(13444)),
        (2, AlignedByteCode::Call(173)),
        (3, AlignedByteCode::Invoke((173, 173))),
        (3, AlignedByteCode::SuperInvoke((173, 173))),
        (2, AlignedByteCode::Closure(173)),
        (2, AlignedByteCode::Method(173)),
        (2, AlignedByteCode::Class(173)),
        (2, AlignedByteCode::GetSuper(173)),
        (1, AlignedByteCode::Inherit),
        (1, AlignedByteCode::CloseUpvalue),
        (1, AlignedByteCode::Equal),
        (1, AlignedByteCode::Greater),
        (1, AlignedByteCode::Less),
      ];

      let mut buffer: Vec<u8> = Vec::new();
      for (size1, byte_code1) in &code {
        for (size2, byte_code2) in &code {
          byte_code1.encode(&mut buffer);
          byte_code2.encode(&mut buffer);

          let (decoded1, offset1) = AlignedByteCode::decode(&buffer, 0);
          let (decoded2, offset2) = AlignedByteCode::decode(&buffer, offset1);

          assert_eq!(offset1, *size1);
          assert_eq!(offset2, *size2 + offset1);

          assert_eq!(*byte_code1, decoded1);
          assert_eq!(*byte_code2, decoded2);
          buffer.clear();
        }
      }
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
      chunk.write_instruction(AlignedByteCode::Nil, 0);

      assert_eq!(chunk.instructions.len(), 1);
      match chunk.instructions[0] {
        9 => assert!(true),
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
      chunk.write_instruction(AlignedByteCode::Nil, 0);
      assert_eq!(chunk.get_line(0), 0);
    }
  }
}
