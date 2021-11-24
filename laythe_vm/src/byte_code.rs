use laythe_core::chunk::Encode;
use std::mem;

#[cfg(any(test, feature = "debug"))]
use std::convert::TryInto;

/// Space Lox virtual machine byte codes
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum AlignedByteCode {
  /// Return from script or function
  Return,

  /// Negate a value
  Negate,

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

  /// Perform a logical and operator
  And(u16),

  /// Perform a logical or operator
  Or(u16),

  /// Retrieve a constant from the constants table
  Constant(u8),

  /// Retrieve a constant of higher number from the constants table
  ConstantLong(u16),

  /// Nil literal
  Nil,

  /// True Literal
  True,

  /// False ByteCode
  False,

  /// Initialize list from literal
  List(u16),

  /// Initialize map from literal
  Map(u16),

  /// Launch a fiber
  Launch(u8),

  /// Initialize a channel
  Channel,

  /// Initialize a channel
  BufferedChannel,

  /// Receive from a channel
  Receive,

  /// Send to a channel
  Send,

  /// Combine string interpolation
  Interpolate(u16),

  /// Get the next element from an iterator
  IterNext(u16),

  /// Get the current value from an iterator
  IterCurrent(u16),

  /// Drop a value
  Drop,

  /// Drop n values
  DropN(u8),

  /// Duplicate top of the stack
  Dup,

  /// Import all symbols
  Import(u16),

  /// Import a single symbol
  ImportSymbol((u16, u16)),

  /// Export a symbol from the current module
  Export(u16),

  /// Define a global in the globals table at a index
  DefineGlobal(u16),

  /// Retrieve a global at the given index
  GetGlobal(u16),

  /// Set a global at the given index
  SetGlobal(u16),

  /// Box a local at a given index,
  Box(u8),

  /// Create a new empty box
  EmptyBox,

  /// Move top of stack into box
  FillBox,

  /// Get a box local at the given index
  GetBox(u8),

  /// Set a box local at the given index
  SetBox(u8),

  /// Get a local at the given index
  GetLocal(u8),

  /// Set a local at the given index
  SetLocal(u8),

  /// Get a box local at the given index
  GetCapture(u8),

  /// Set a box local at the given index
  SetCapture(u8),

  /// Get a property off a class instance
  GetProperty(u16),

  /// Set a property on a class instance
  SetProperty(u16),

  /// Jump to end of if block if false
  JumpIfFalse(u16),

  /// Jump conditionally to the ip
  Jump(u16),

  /// Jump to loop beginning
  Loop(u16),

  /// Call a function
  Call(u8),

  /// Invoke a method
  Invoke((u16, u8)),

  /// Invoke a method on a super class
  SuperInvoke((u16, u8)),

  /// Create a closure
  Closure(u16),

  /// Create a method
  Method(u16),

  /// Create a field
  Field(u16),

  /// Create a static method
  StaticMethod(u16),

  /// Create a class
  Class(u16),

  /// Inherit from another class
  Inherit,

  /// Access this classes super
  GetSuper(u16),

  // An capture index for a closure
  CaptureIndex(CaptureIndex),

  /// A inline cache slot
  Slot(u32),

  /// Apply equality between the top two operands on the stack
  Equal,

  /// Check if the top two operands on the stack are not equal
  NotEqual,

  /// Apply greater between the top two operands on the stack
  Greater,

  /// Check if the 2nd from the top operand is >= the top
  GreaterEqual,

  /// Less greater between the top two operands on the stack
  Less,

  /// Check if the 2nd from the top operand is <= the top
  LessEqual,
}

impl AlignedByteCode {
  /// Decode unaligned bytecode to aligned bytecode. Primarily for testing purposes
  #[cfg(any(test, feature = "debug"))]
  pub fn decode(store: &[u8], offset: usize) -> (AlignedByteCode, usize) {
    let byte_code = ByteCode::from(store[offset]);

    match byte_code {
      ByteCode::Return => (AlignedByteCode::Return, offset + 1),
      ByteCode::Negate => (AlignedByteCode::Negate, offset + 1),
      ByteCode::Add => (AlignedByteCode::Add, offset + 1),
      ByteCode::Subtract => (AlignedByteCode::Subtract, offset + 1),
      ByteCode::Multiply => (AlignedByteCode::Multiply, offset + 1),
      ByteCode::Divide => (AlignedByteCode::Divide, offset + 1),
      ByteCode::And => (
        AlignedByteCode::And(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Or => (
        AlignedByteCode::Or(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Not => (AlignedByteCode::Not, offset + 1),
      ByteCode::Constant => (AlignedByteCode::Constant(store[offset + 1]), offset + 2),
      ByteCode::ConstantLong => (
        AlignedByteCode::ConstantLong(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Nil => (AlignedByteCode::Nil, offset + 1),
      ByteCode::True => (AlignedByteCode::True, offset + 1),
      ByteCode::False => (AlignedByteCode::False, offset + 1),
      ByteCode::List => (
        AlignedByteCode::List(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Map => (
        AlignedByteCode::Map(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Launch => (AlignedByteCode::Launch(store[offset + 1]), offset + 2),
      ByteCode::Channel => (AlignedByteCode::Channel, offset + 1),
      ByteCode::BufferedChannel => (AlignedByteCode::BufferedChannel, offset + 1),
      ByteCode::Receive => (AlignedByteCode::Receive, offset + 1),
      ByteCode::Send => (AlignedByteCode::Send, offset + 1),
      ByteCode::Interpolate => (
        AlignedByteCode::Interpolate(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::IterNext => (
        AlignedByteCode::IterNext(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::IterCurrent => (
        AlignedByteCode::IterCurrent(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Drop => (AlignedByteCode::Drop, offset + 1),
      ByteCode::DropN => (AlignedByteCode::DropN(store[offset + 1]), offset + 2),
      ByteCode::Dup => (AlignedByteCode::Dup, offset + 1),
      ByteCode::Import => (
        AlignedByteCode::Import(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::ImportSymbol => (
        AlignedByteCode::ImportSymbol((
          decode_u16(&store[offset + 1..offset + 3]),
          decode_u16(&store[offset + 3..offset + 5]),
        )),
        offset + 5,
      ),
      ByteCode::Export => (
        AlignedByteCode::Export(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::DefineGlobal => (
        AlignedByteCode::DefineGlobal(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::GetGlobal => (
        AlignedByteCode::GetGlobal(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::SetGlobal => (
        AlignedByteCode::SetGlobal(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Box => (AlignedByteCode::Box(store[offset + 1]), offset + 2),
      ByteCode::EmptyBox => (AlignedByteCode::EmptyBox, offset + 1),
      ByteCode::FillBox => (AlignedByteCode::FillBox, offset + 1),
      ByteCode::GetBox => (AlignedByteCode::GetBox(store[offset + 1]), offset + 2),
      ByteCode::SetBox => (AlignedByteCode::SetBox(store[offset + 1]), offset + 2),
      ByteCode::GetLocal => (AlignedByteCode::GetLocal(store[offset + 1]), offset + 2),
      ByteCode::SetLocal => (AlignedByteCode::SetLocal(store[offset + 1]), offset + 2),
      ByteCode::GetCapture => (AlignedByteCode::GetCapture(store[offset + 1]), offset + 2),
      ByteCode::SetCapture => (AlignedByteCode::SetCapture(store[offset + 1]), offset + 2),
      ByteCode::GetProperty => (
        AlignedByteCode::GetProperty(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::SetProperty => (
        AlignedByteCode::SetProperty(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
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
        AlignedByteCode::Invoke((
          decode_u16(&store[offset + 1..offset + 3]),
          store[offset + 3],
        )),
        offset + 4,
      ),
      ByteCode::SuperInvoke => (
        AlignedByteCode::SuperInvoke((
          decode_u16(&store[offset + 1..offset + 3]),
          store[offset + 3],
        )),
        offset + 4,
      ),
      ByteCode::Closure => (
        AlignedByteCode::Closure(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Method => (
        AlignedByteCode::Method(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Field => (
        AlignedByteCode::Field(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::StaticMethod => (
        AlignedByteCode::StaticMethod(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Class => (
        AlignedByteCode::Class(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Inherit => (AlignedByteCode::Inherit, offset + 1),
      ByteCode::GetSuper => (
        AlignedByteCode::GetSuper(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Equal => (AlignedByteCode::Equal, offset + 1),
      ByteCode::NotEqual => (AlignedByteCode::NotEqual, offset + 1),
      ByteCode::Greater => (AlignedByteCode::Greater, offset + 1),
      ByteCode::GreaterEqual => (AlignedByteCode::GreaterEqual, offset + 1),
      ByteCode::Less => (AlignedByteCode::Less, offset + 1),
      ByteCode::LessEqual => (AlignedByteCode::LessEqual, offset + 1),
    }
  }

  /// What effect will this instruction have on the stack
  pub fn stack_effect(&self) -> i32 {
    match self {
      AlignedByteCode::Return => 0,
      AlignedByteCode::Negate => 0,
      AlignedByteCode::Add => -1,
      AlignedByteCode::Subtract => -1,
      AlignedByteCode::Multiply => -1,
      AlignedByteCode::Divide => -1,
      AlignedByteCode::Not => 0,
      AlignedByteCode::And(_) => -1,
      AlignedByteCode::Or(_) => -1,
      AlignedByteCode::Constant(_) => 1,
      AlignedByteCode::ConstantLong(_) => 1,
      AlignedByteCode::Nil => 1,
      AlignedByteCode::True => 1,
      AlignedByteCode::False => 1,
      AlignedByteCode::List(cnt) => -(*cnt as i32) + 1,
      AlignedByteCode::Map(cnt) => -(*cnt as i32 * 2) + 1,
      AlignedByteCode::Launch(args) => -(*args as i32 + 1),
      AlignedByteCode::Channel => 1,
      AlignedByteCode::BufferedChannel => 0,
      AlignedByteCode::Receive => 0,
      AlignedByteCode::Send => 0,
      AlignedByteCode::Interpolate(cnt) => -(*cnt as i32) + 1,
      AlignedByteCode::IterNext(_) => 0,
      AlignedByteCode::IterCurrent(_) => 0,
      AlignedByteCode::Drop => -1,
      AlignedByteCode::DropN(cnt) => -(*cnt as i32),
      AlignedByteCode::Dup => 1,
      AlignedByteCode::Import(_) => 1,
      AlignedByteCode::ImportSymbol(_) => 1,
      AlignedByteCode::Export(_) => 0,
      AlignedByteCode::DefineGlobal(_) => -1,
      AlignedByteCode::GetGlobal(_) => 1,
      AlignedByteCode::SetGlobal(_) => 0,
      AlignedByteCode::Box(_) => 0,
      AlignedByteCode::EmptyBox => 1,
      AlignedByteCode::FillBox => -1,
      AlignedByteCode::GetBox(_) => 1,
      AlignedByteCode::SetBox(_) => 0,
      AlignedByteCode::GetLocal(_) => 1,
      AlignedByteCode::SetLocal(_) => 0,
      AlignedByteCode::GetCapture(_) => 1,
      AlignedByteCode::SetCapture(_) => 0,
      AlignedByteCode::GetProperty(_) => 0,
      AlignedByteCode::SetProperty(_) => -1,
      AlignedByteCode::JumpIfFalse(_) => -1,
      AlignedByteCode::Jump(_) => 0,
      AlignedByteCode::Loop(_) => 0,
      AlignedByteCode::Call(args) => -(*args as i32),
      AlignedByteCode::Invoke((_, args)) => -(*args as i32),
      AlignedByteCode::SuperInvoke((_, args)) => -(*args as i32 + 1),
      AlignedByteCode::Closure(_) => 1,
      AlignedByteCode::Method(_) => -1,
      AlignedByteCode::Field(_) => 0,
      AlignedByteCode::StaticMethod(_) => -1,
      AlignedByteCode::Class(_) => 1,
      AlignedByteCode::Inherit => 0,
      AlignedByteCode::GetSuper(_) => -1,
      AlignedByteCode::CaptureIndex(_) => 0,
      AlignedByteCode::Slot(_) => 0,
      AlignedByteCode::Equal => -1,
      AlignedByteCode::NotEqual => -1,
      AlignedByteCode::Greater => -1,
      AlignedByteCode::GreaterEqual => -1,
      AlignedByteCode::Less => -1,
      AlignedByteCode::LessEqual => -1,
    }
  }
}

impl Encode for AlignedByteCode {
  /// Encode aligned bytecode as unaligned bytecode for better storage / compactness
  fn encode(self, code: &mut Vec<u8>) -> u32 {
    match self {
      Self::Return => op(code, ByteCode::Return),
      Self::Negate => op(code, ByteCode::Negate),
      Self::Add => op(code, ByteCode::Add),
      Self::Subtract => op(code, ByteCode::Subtract),
      Self::Multiply => op(code, ByteCode::Multiply),
      Self::Divide => op(code, ByteCode::Divide),
      Self::And(slot) => op_short(code, ByteCode::And, slot),
      Self::Or(slot) => op_short(code, ByteCode::Or, slot),
      Self::Not => op(code, ByteCode::Not),
      Self::Nil => op(code, ByteCode::Nil),
      Self::True => op(code, ByteCode::True),
      Self::False => op(code, ByteCode::False),
      Self::List(slot) => op_short(code, ByteCode::List, slot),
      Self::Map(slot) => op_short(code, ByteCode::Map, slot),
      Self::Launch(slot) => op_byte(code, ByteCode::Launch, slot),
      Self::Channel => op(code, ByteCode::Channel),
      Self::BufferedChannel => op(code, ByteCode::BufferedChannel),
      Self::Receive => op(code, ByteCode::Receive),
      Self::Send => op(code, ByteCode::Send),
      Self::Interpolate(slot) => op_short(code, ByteCode::Interpolate, slot),
      Self::IterNext(slot) => op_short(code, ByteCode::IterNext, slot),
      Self::IterCurrent(slot) => op_short(code, ByteCode::IterCurrent, slot),
      Self::Equal => op(code, ByteCode::Equal),
      Self::NotEqual => op(code, ByteCode::NotEqual),
      Self::Greater => op(code, ByteCode::Greater),
      Self::GreaterEqual => op(code, ByteCode::GreaterEqual),
      Self::Less => op(code, ByteCode::Less),
      Self::LessEqual => op(code, ByteCode::LessEqual),
      Self::Drop => op(code, ByteCode::Drop),
      Self::DropN(slot) => op_byte(code, ByteCode::DropN, slot),
      Self::Dup => op(code, ByteCode::Dup),
      Self::Constant(slot) => op_byte(code, ByteCode::Constant, slot),
      Self::ConstantLong(slot) => op_short(code, ByteCode::ConstantLong, slot),
      Self::Import(path) => op_short(code, ByteCode::Import, path),
      Self::ImportSymbol((path, slot)) => {
        push_op_u16_tuple(code, ByteCode::ImportSymbol, path, slot);
        4
      }
      Self::Export(slot) => op_short(code, ByteCode::Export, slot),
      Self::DefineGlobal(slot) => op_short(code, ByteCode::DefineGlobal, slot),
      Self::GetGlobal(slot) => op_short(code, ByteCode::GetGlobal, slot),
      Self::SetGlobal(slot) => op_short(code, ByteCode::SetGlobal, slot),
      Self::Box(slot) => op_byte(code, ByteCode::Box, slot),
      Self::EmptyBox => op(code, ByteCode::EmptyBox),
      Self::FillBox => op(code, ByteCode::FillBox),
      Self::GetBox(slot) => op_byte(code, ByteCode::GetBox, slot),
      Self::SetBox(slot) => op_byte(code, ByteCode::SetBox, slot),
      Self::GetLocal(slot) => op_byte(code, ByteCode::GetLocal, slot),
      Self::SetLocal(slot) => op_byte(code, ByteCode::SetLocal, slot),
      Self::GetCapture(slot) => op_byte(code, ByteCode::GetCapture, slot),
      Self::SetCapture(slot) => op_byte(code, ByteCode::SetCapture, slot),
      Self::GetProperty(slot) => op_short(code, ByteCode::GetProperty, slot),
      Self::SetProperty(slot) => op_short(code, ByteCode::SetProperty, slot),
      Self::JumpIfFalse(slot) => op_short(code, ByteCode::JumpIfFalse, slot),
      Self::Jump(slot) => op_short(code, ByteCode::Jump, slot),
      Self::Loop(slot) => op_short(code, ByteCode::Loop, slot),
      Self::Call(slot) => op_byte(code, ByteCode::Call, slot),
      Self::Invoke((slot1, slot2)) => {
        push_op_u16_u8_tuple(code, ByteCode::Invoke, slot1, slot2);
        4
      }
      Self::SuperInvoke((slot1, slot2)) => {
        push_op_u16_u8_tuple(code, ByteCode::SuperInvoke, slot1, slot2);
        4
      }
      Self::Closure(slot) => op_short(code, ByteCode::Closure, slot),
      Self::Method(slot) => op_short(code, ByteCode::Method, slot),
      Self::Field(slot) => op_short(code, ByteCode::Field, slot),
      Self::StaticMethod(slot) => op_short(code, ByteCode::StaticMethod, slot),
      Self::Class(slot) => op_short(code, ByteCode::Class, slot),
      Self::Inherit => op(code, ByteCode::Inherit),
      Self::GetSuper(slot) => op_short(code, ByteCode::GetSuper, slot),
      Self::CaptureIndex(index) => {
        let encoded: u16 = unsafe { mem::transmute(index) };
        let bytes = encoded.to_ne_bytes();
        code.extend_from_slice(&bytes);
        3
      }
      Self::Slot(slot) => {
        let bytes = slot.to_ne_bytes();
        code.extend_from_slice(&bytes);
        5
      }
    }
  }
}

fn op(code: &mut Vec<u8>, byte_code: ByteCode) -> u32 {
  push_op(code, byte_code);
  1
}

fn op_byte(code: &mut Vec<u8>, byte_code: ByteCode, byte: u8) -> u32 {
  push_op_u8(code, byte_code, byte);
  2
}

fn op_short(code: &mut Vec<u8>, byte_code: ByteCode, short: u16) -> u32 {
  push_op_u16(code, byte_code, short);
  3
}

/// Space Lox virtual machine byte codes
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ByteCode {
  /// Return from script or function
  Return,

  /// Negate a value
  Negate,

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

  /// Perform a logical and operator
  And,

  /// Perform a logical or operator
  Or,

  /// Retrieve a constant from the constants table
  Constant,

  /// Retrieve a constant of higher number from the constants table
  ConstantLong,

  /// Nil literal
  Nil,

  /// True Literal
  True,

  /// False ByteCode
  False,

  /// Initialize List
  List,

  /// Initialize map
  Map,

  /// Launch a fiber
  Launch,

  /// Initialize a channel
  Channel,

  /// Initialize a buffered channel
  BufferedChannel,

  /// Receive from a  channel
  Receive,

  /// Send to a  channel
  Send,

  /// Combine string interpolation
  Interpolate,

  /// Get the next element from an iterator
  IterNext,

  /// Get the current value from an iterator
  IterCurrent,

  /// Drop a value
  Drop,

  /// Drop n values
  DropN,

  /// Duplicate top of the stack
  Dup,

  /// Import all symbols
  Import,

  /// Import a single symbol
  ImportSymbol,

  /// Export a symbol from the current module
  Export,

  /// Define a global in the globals table at a index
  DefineGlobal,

  /// Retrieve a global at the given index
  GetGlobal,

  /// Set a global at the given index
  SetGlobal,

  /// Box a local at a given index,
  Box,

  /// Create a new empty box
  EmptyBox,

  /// Move top of stack into box
  FillBox,

  /// Get a boxed local at the given index
  GetBox,

  /// Set a boxed local at the given index
  SetBox,

  /// Get a local at the given index
  GetLocal,

  /// Set a local at the given index
  SetLocal,

  /// Get a local at the given index
  GetCapture,

  /// Set a local at the given index
  SetCapture,

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

  /// Create a field
  Field,

  /// Create a static method
  StaticMethod,

  /// Create a class
  Class,

  /// Inherit from another class
  Inherit,

  /// Access this classes super
  GetSuper,

  /// Apply equality between the top two operands on the stack
  Equal,

  /// Check if the top two operands on the stack are not equal
  NotEqual,

  /// Apply greater between the top two operands on the stack
  Greater,

  /// Check if the 2nd from the top operand is >= the top
  GreaterEqual,

  /// Less greater between the top two operands on the stack
  Less,

  /// Check if the 2nd from the top operand is <= the top
  LessEqual,
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CaptureIndex {
  /// The capture is in the local function
  Local(u8),

  /// The capture points to the enclosing function
  Enclosing(u8),
}

#[cfg(any(test, feature = "debug"))]
pub fn decode_u32(buffer: &[u8]) -> u32 {
  let arr: [u8; 4] = buffer.try_into().expect("slice of incorrect length.");
  u32::from_ne_bytes(arr)
}

#[cfg(any(test, feature = "debug"))]
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

fn push_op_u16_u8_tuple(code: &mut Vec<u8>, byte: ByteCode, param1: u16, param2: u8) {
  code.push(byte.to_byte());
  let param_bytes = param1.to_ne_bytes();
  code.extend_from_slice(&param_bytes);
  code.push(param2);
}

fn push_op_u16(code: &mut Vec<u8>, byte: ByteCode, param: u16) {
  let param_bytes = param.to_ne_bytes();
  code.push(byte.to_byte());
  code.extend_from_slice(&param_bytes);
}

fn push_op_u16_tuple(code: &mut Vec<u8>, byte: ByteCode, param1: u16, param2: u16) {
  code.push(byte.to_byte());
  let param_bytes = param1.to_ne_bytes();
  code.extend_from_slice(&param_bytes);
  let param_bytes = param2.to_ne_bytes();
  code.extend_from_slice(&param_bytes);
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn encode_decode() {
    let code: Vec<(usize, AlignedByteCode)> = vec![
      (1, AlignedByteCode::Return),
      (1, AlignedByteCode::Negate),
      (1, AlignedByteCode::Add),
      (1, AlignedByteCode::Subtract),
      (1, AlignedByteCode::Multiply),
      (1, AlignedByteCode::Divide),
      (1, AlignedByteCode::Not),
      (2, AlignedByteCode::Constant(113)),
      (3, AlignedByteCode::ConstantLong(45863)),
      (3, AlignedByteCode::Import(2235)),
      (5, AlignedByteCode::ImportSymbol((2235, 113))),
      (3, AlignedByteCode::Export(7811)),
      (1, AlignedByteCode::Nil),
      (1, AlignedByteCode::True),
      (1, AlignedByteCode::False),
      (3, AlignedByteCode::List(54782)),
      (3, AlignedByteCode::Map(1923)),
      (2, AlignedByteCode::Launch(197)),
      (1, AlignedByteCode::Channel),
      (1, AlignedByteCode::BufferedChannel),
      (1, AlignedByteCode::Receive),
      (1, AlignedByteCode::Send),
      (2, AlignedByteCode::Box(66)),
      (1, AlignedByteCode::EmptyBox),
      (1, AlignedByteCode::FillBox),
      (3, AlignedByteCode::Interpolate(3389)),
      (3, AlignedByteCode::IterNext(81)),
      (3, AlignedByteCode::IterCurrent(49882)),
      (1, AlignedByteCode::Drop),
      (3, AlignedByteCode::DefineGlobal(42)),
      (3, AlignedByteCode::GetGlobal(14119)),
      (3, AlignedByteCode::SetGlobal(2043)),
      (3, AlignedByteCode::SetGlobal(38231)),
      (2, AlignedByteCode::GetBox(183)),
      (2, AlignedByteCode::SetBox(56)),
      (2, AlignedByteCode::GetLocal(96)),
      (2, AlignedByteCode::SetLocal(149)),
      (2, AlignedByteCode::GetBox(11)),
      (2, AlignedByteCode::SetBox(197)),
      (3, AlignedByteCode::GetProperty(18273)),
      (3, AlignedByteCode::SetProperty(253)),
      (3, AlignedByteCode::JumpIfFalse(8941)),
      (3, AlignedByteCode::Jump(95)),
      (3, AlignedByteCode::Loop(34590)),
      (2, AlignedByteCode::Call(77)),
      (4, AlignedByteCode::Invoke((5591, 19))),
      (4, AlignedByteCode::SuperInvoke((2105, 15))),
      (3, AlignedByteCode::Closure(3638)),
      (3, AlignedByteCode::Method(188)),
      (3, AlignedByteCode::Field(6634)),
      (3, AlignedByteCode::StaticMethod(4912)),
      (3, AlignedByteCode::Class(64136)),
      (1, AlignedByteCode::Inherit),
      (3, AlignedByteCode::GetSuper(24)),
      (1, AlignedByteCode::Equal),
      (1, AlignedByteCode::NotEqual),
      (1, AlignedByteCode::Greater),
      (1, AlignedByteCode::GreaterEqual),
      (1, AlignedByteCode::LessEqual),
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
