use laythe_core::chunk::Encode;
use std::{fmt::Display, mem};

#[cfg(any(test, feature = "debug"))]
use std::convert::TryInto;

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Label(u32);

impl Label {
  pub fn new(id: u32) -> Self {
    Self(id)
  }
}

impl Display for Label {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.0)
  }
}

/// Space Lox virtual machine byte codes
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum SymbolicByteCode {
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
  And(Label),

  /// Perform a logical or operator
  Or(Label),

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

  /// Initialize list from literal
  Tuple(u16),

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
  JumpIfFalse(Label),

  /// Jump conditionally to the ip
  Jump(Label),

  /// Jump to loop beginning
  Loop(Label),

  ///
  Label(Label),

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

impl SymbolicByteCode {
  // /// Decode unaligned bytecode to aligned bytecode. Primarily for testing purposes
  // #[cfg(any(test, feature = "debug"))]
  // pub fn decode(store: &[u8], offset: usize) -> (SymbolicByteCode, usize) {
  //   let byte_code = ByteCode::from(store[offset]);

  //   match byte_code {
  //     ByteCode::Return => (SymbolicByteCode::Return, offset + 1),
  //     ByteCode::Negate => (SymbolicByteCode::Negate, offset + 1),
  //     ByteCode::Add => (SymbolicByteCode::Add, offset + 1),
  //     ByteCode::Subtract => (SymbolicByteCode::Subtract, offset + 1),
  //     ByteCode::Multiply => (SymbolicByteCode::Multiply, offset + 1),
  //     ByteCode::Divide => (SymbolicByteCode::Divide, offset + 1),
  //     ByteCode::And => (
  //       SymbolicByteCode::And(decode_u16(&store[offset + 1..offset + 3])),
  //       offset + 3,
  //     ),
  //     ByteCode::Or => (
  //       SymbolicByteCode::Or(decode_u16(&store[offset + 1..offset + 3])),
  //       offset + 3,
  //     ),
  //     ByteCode::Not => (SymbolicByteCode::Not, offset + 1),
  //     ByteCode::Constant => (SymbolicByteCode::Constant(store[offset + 1]), offset + 2),
  //     ByteCode::ConstantLong => (
  //       SymbolicByteCode::ConstantLong(decode_u16(&store[offset + 1..offset + 3])),
  //       offset + 3,
  //     ),
  //     ByteCode::Nil => (SymbolicByteCode::Nil, offset + 1),
  //     ByteCode::True => (SymbolicByteCode::True, offset + 1),
  //     ByteCode::False => (SymbolicByteCode::False, offset + 1),
  //     ByteCode::List => (
  //       SymbolicByteCode::List(decode_u16(&store[offset + 1..offset + 3])),
  //       offset + 3,
  //     ),
  //     ByteCode::Tuple => (
  //       SymbolicByteCode::Tuple(decode_u16(&store[offset + 1..offset + 3])),
  //       offset + 3,
  //     ),
  //     ByteCode::Map => (
  //       SymbolicByteCode::Map(decode_u16(&store[offset + 1..offset + 3])),
  //       offset + 3,
  //     ),
  //     ByteCode::Launch => (SymbolicByteCode::Launch(store[offset + 1]), offset + 2),
  //     ByteCode::Channel => (SymbolicByteCode::Channel, offset + 1),
  //     ByteCode::BufferedChannel => (SymbolicByteCode::BufferedChannel, offset + 1),
  //     ByteCode::Receive => (SymbolicByteCode::Receive, offset + 1),
  //     ByteCode::Send => (SymbolicByteCode::Send, offset + 1),
  //     ByteCode::Interpolate => (
  //       SymbolicByteCode::Interpolate(decode_u16(&store[offset + 1..offset + 3])),
  //       offset + 3,
  //     ),
  //     ByteCode::IterNext => (
  //       SymbolicByteCode::IterNext(decode_u16(&store[offset + 1..offset + 3])),
  //       offset + 3,
  //     ),
  //     ByteCode::IterCurrent => (
  //       SymbolicByteCode::IterCurrent(decode_u16(&store[offset + 1..offset + 3])),
  //       offset + 3,
  //     ),
  //     ByteCode::Drop => (SymbolicByteCode::Drop, offset + 1),
  //     ByteCode::DropN => (SymbolicByteCode::DropN(store[offset + 1]), offset + 2),
  //     ByteCode::Dup => (SymbolicByteCode::Dup, offset + 1),
  //     ByteCode::Import => (
  //       SymbolicByteCode::Import(decode_u16(&store[offset + 1..offset + 3])),
  //       offset + 3,
  //     ),
  //     ByteCode::ImportSymbol => (
  //       SymbolicByteCode::ImportSymbol((
  //         decode_u16(&store[offset + 1..offset + 3]),
  //         decode_u16(&store[offset + 3..offset + 5]),
  //       )),
  //       offset + 5,
  //     ),
  //     ByteCode::Export => (
  //       SymbolicByteCode::Export(decode_u16(&store[offset + 1..offset + 3])),
  //       offset + 3,
  //     ),
  //     ByteCode::DefineGlobal => (
  //       SymbolicByteCode::DefineGlobal(decode_u16(&store[offset + 1..offset + 3])),
  //       offset + 3,
  //     ),
  //     ByteCode::GetGlobal => (
  //       SymbolicByteCode::GetGlobal(decode_u16(&store[offset + 1..offset + 3])),
  //       offset + 3,
  //     ),
  //     ByteCode::SetGlobal => (
  //       SymbolicByteCode::SetGlobal(decode_u16(&store[offset + 1..offset + 3])),
  //       offset + 3,
  //     ),
  //     ByteCode::Box => (SymbolicByteCode::Box(store[offset + 1]), offset + 2),
  //     ByteCode::EmptyBox => (SymbolicByteCode::EmptyBox, offset + 1),
  //     ByteCode::FillBox => (SymbolicByteCode::FillBox, offset + 1),
  //     ByteCode::GetBox => (SymbolicByteCode::GetBox(store[offset + 1]), offset + 2),
  //     ByteCode::SetBox => (SymbolicByteCode::SetBox(store[offset + 1]), offset + 2),
  //     ByteCode::GetLocal => (SymbolicByteCode::GetLocal(store[offset + 1]), offset + 2),
  //     ByteCode::SetLocal => (SymbolicByteCode::SetLocal(store[offset + 1]), offset + 2),
  //     ByteCode::GetCapture => (SymbolicByteCode::GetCapture(store[offset + 1]), offset + 2),
  //     ByteCode::SetCapture => (SymbolicByteCode::SetCapture(store[offset + 1]), offset + 2),
  //     ByteCode::GetProperty => (
  //       SymbolicByteCode::GetProperty(decode_u16(&store[offset + 1..offset + 3])),
  //       offset + 3,
  //     ),
  //     ByteCode::SetProperty => (
  //       SymbolicByteCode::SetProperty(decode_u16(&store[offset + 1..offset + 3])),
  //       offset + 3,
  //     ),
  //     ByteCode::JumpIfFalse => (
  //       SymbolicByteCode::JumpIfFalse(decode_u16(&store[offset + 1..offset + 3])),
  //       offset + 3,
  //     ),
  //     ByteCode::Jump => (
  //       SymbolicByteCode::Jump(decode_u16(&store[offset + 1..offset + 3])),
  //       offset + 3,
  //     ),
  //     ByteCode::Loop => (
  //       SymbolicByteCode::Loop(decode_u16(&store[offset + 1..offset + 3])),
  //       offset + 3,
  //     ),
  //     ByteCode::Call => (SymbolicByteCode::Call(store[offset + 1]), offset + 2),
  //     ByteCode::Invoke => (
  //       SymbolicByteCode::Invoke((
  //         decode_u16(&store[offset + 1..offset + 3]),
  //         store[offset + 3],
  //       )),
  //       offset + 4,
  //     ),
  //     ByteCode::SuperInvoke => (
  //       SymbolicByteCode::SuperInvoke((
  //         decode_u16(&store[offset + 1..offset + 3]),
  //         store[offset + 3],
  //       )),
  //       offset + 4,
  //     ),
  //     ByteCode::Closure => (
  //       SymbolicByteCode::Closure(decode_u16(&store[offset + 1..offset + 3])),
  //       offset + 3,
  //     ),
  //     ByteCode::Method => (
  //       SymbolicByteCode::Method(decode_u16(&store[offset + 1..offset + 3])),
  //       offset + 3,
  //     ),
  //     ByteCode::Field => (
  //       SymbolicByteCode::Field(decode_u16(&store[offset + 1..offset + 3])),
  //       offset + 3,
  //     ),
  //     ByteCode::StaticMethod => (
  //       SymbolicByteCode::StaticMethod(decode_u16(&store[offset + 1..offset + 3])),
  //       offset + 3,
  //     ),
  //     ByteCode::Class => (
  //       SymbolicByteCode::Class(decode_u16(&store[offset + 1..offset + 3])),
  //       offset + 3,
  //     ),
  //     ByteCode::Inherit => (SymbolicByteCode::Inherit, offset + 1),
  //     ByteCode::GetSuper => (
  //       SymbolicByteCode::GetSuper(decode_u16(&store[offset + 1..offset + 3])),
  //       offset + 3,
  //     ),
  //     ByteCode::Equal => (SymbolicByteCode::Equal, offset + 1),
  //     ByteCode::NotEqual => (SymbolicByteCode::NotEqual, offset + 1),
  //     ByteCode::Greater => (SymbolicByteCode::Greater, offset + 1),
  //     ByteCode::GreaterEqual => (SymbolicByteCode::GreaterEqual, offset + 1),
  //     ByteCode::Less => (SymbolicByteCode::Less, offset + 1),
  //     ByteCode::LessEqual => (SymbolicByteCode::LessEqual, offset + 1),
  //   }
  // }

  /// Encode aligned bytecode as unaligned bytecode for better storage / compactness
  fn encode(self, code: &mut Vec<u8>, label_offsets: &[usize], offset: usize) {
    dbg!(label_offsets);
    dbg!(offset);

    match self {
      Self::Return => op(code, ByteCode::Return),
      Self::Negate => op(code, ByteCode::Negate),
      Self::Add => op(code, ByteCode::Add),
      Self::Subtract => op(code, ByteCode::Subtract),
      Self::Multiply => op(code, ByteCode::Multiply),
      Self::Divide => op(code, ByteCode::Divide),
      Self::And(slot) => op_short(
        code,
        ByteCode::And,
        (offset - label_offsets[slot.0 as usize]) as u16,
      ),
      Self::Or(slot) => op_short(
        code,
        ByteCode::Or,
        (offset - label_offsets[slot.0 as usize]) as u16,
      ),
      Self::Not => op(code, ByteCode::Not),
      Self::Nil => op(code, ByteCode::Nil),
      Self::True => op(code, ByteCode::True),
      Self::False => op(code, ByteCode::False),
      Self::List(slot) => op_short(code, ByteCode::List, slot),
      Self::Tuple(slot) => op_short(code, ByteCode::Tuple, slot),
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
      },
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
      Self::JumpIfFalse(slot) => op_short(
        code,
        ByteCode::JumpIfFalse,
        (label_offsets[slot.0 as usize] - offset) as u16,
      ),
      Self::Jump(slot) => op_short(
        code,
        ByteCode::Jump,
        (label_offsets[slot.0 as usize] - offset) as u16,
      ),
      Self::Loop(slot) => op_short(
        code,
        ByteCode::Loop,
        (offset - label_offsets[slot.0 as usize]) as u16,
      ),
      Self::Call(slot) => op_byte(code, ByteCode::Call, slot),
      Self::Invoke((slot1, slot2)) => {
        push_op_u16_u8_tuple(code, ByteCode::Invoke, slot1, slot2);
      },
      Self::SuperInvoke((slot1, slot2)) => {
        push_op_u16_u8_tuple(code, ByteCode::SuperInvoke, slot1, slot2);
      },
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
      },
      Self::Slot(slot) => {
        let bytes = slot.to_ne_bytes();
        code.extend_from_slice(&bytes);
      },
      Self::Label(_) => (),
    }
  }

  /// What effect will this instruction have on the stack
  pub const fn len(&self) -> usize {
    match self {
      SymbolicByteCode::Return => 1,
      SymbolicByteCode::Negate => 1,
      SymbolicByteCode::Add => 1,
      SymbolicByteCode::Subtract => 1,
      SymbolicByteCode::Multiply => 1,
      SymbolicByteCode::Divide => 1,
      SymbolicByteCode::Not => 1,
      SymbolicByteCode::And(_) => 3,
      SymbolicByteCode::Or(_) => 3,
      SymbolicByteCode::Constant(_) => 2,
      SymbolicByteCode::ConstantLong(_) => 3,
      SymbolicByteCode::Nil => 1,
      SymbolicByteCode::True => 1,
      SymbolicByteCode::False => 1,
      SymbolicByteCode::List(_) => 3,
      SymbolicByteCode::Tuple(_) => 3,
      SymbolicByteCode::Map(_) => 3,
      SymbolicByteCode::Launch(_) => 2,
      SymbolicByteCode::Channel => 1,
      SymbolicByteCode::BufferedChannel => 0,
      SymbolicByteCode::Receive => 1,
      SymbolicByteCode::Send => 1,
      SymbolicByteCode::Interpolate(_) => 2,
      SymbolicByteCode::IterNext(_) => 3,
      SymbolicByteCode::IterCurrent(_) => 3,
      SymbolicByteCode::Drop => 1,
      SymbolicByteCode::DropN(_) => 2,
      SymbolicByteCode::Dup => 1,
      SymbolicByteCode::Import(_) => 3,
      SymbolicByteCode::ImportSymbol(_) => 5,
      SymbolicByteCode::Export(_) => 3,
      SymbolicByteCode::DefineGlobal(_) => 3,
      SymbolicByteCode::GetGlobal(_) => 1,
      SymbolicByteCode::SetGlobal(_) => 3,
      SymbolicByteCode::Box(_) => 2,
      SymbolicByteCode::EmptyBox => 1,
      SymbolicByteCode::FillBox => 1,
      SymbolicByteCode::GetBox(_) => 2,
      SymbolicByteCode::SetBox(_) => 2,
      SymbolicByteCode::GetLocal(_) => 2,
      SymbolicByteCode::SetLocal(_) => 2,
      SymbolicByteCode::GetCapture(_) => 2,
      SymbolicByteCode::SetCapture(_) => 2,
      SymbolicByteCode::GetProperty(_) => 3,
      SymbolicByteCode::SetProperty(_) => 3,
      SymbolicByteCode::JumpIfFalse(_) => 3,
      SymbolicByteCode::Jump(_) => 3,
      SymbolicByteCode::Loop(_) => 3,
      SymbolicByteCode::Call(_) => 2,
      SymbolicByteCode::Invoke((_, _)) => 4,
      SymbolicByteCode::SuperInvoke((_, _)) => 4,
      SymbolicByteCode::Closure(_) => 3,
      SymbolicByteCode::Method(_) => 3,
      SymbolicByteCode::Field(_) => 3,
      SymbolicByteCode::StaticMethod(_) => 3,
      SymbolicByteCode::Class(_) => 3,
      SymbolicByteCode::Inherit => 1,
      SymbolicByteCode::GetSuper(_) => 3,
      SymbolicByteCode::CaptureIndex(_) => 3,
      SymbolicByteCode::Slot(_) => 4,
      SymbolicByteCode::Equal => 1,
      SymbolicByteCode::NotEqual => 1,
      SymbolicByteCode::Greater => 1,
      SymbolicByteCode::GreaterEqual => 1,
      SymbolicByteCode::Less => 1,
      SymbolicByteCode::LessEqual => 1,
      SymbolicByteCode::Label(_) => 0,
    }
  }

  /// What effect will this instruction have on the stack
  pub const fn stack_effect(&self) -> i32 {
    match self {
      SymbolicByteCode::Return => 0,
      SymbolicByteCode::Negate => 0,
      SymbolicByteCode::Add => -1,
      SymbolicByteCode::Subtract => -1,
      SymbolicByteCode::Multiply => -1,
      SymbolicByteCode::Divide => -1,
      SymbolicByteCode::Not => 0,
      SymbolicByteCode::And(_) => -1,
      SymbolicByteCode::Or(_) => -1,
      SymbolicByteCode::Constant(_) => 1,
      SymbolicByteCode::ConstantLong(_) => 1,
      SymbolicByteCode::Nil => 1,
      SymbolicByteCode::True => 1,
      SymbolicByteCode::False => 1,
      SymbolicByteCode::List(cnt) => -(*cnt as i32) + 1,
      SymbolicByteCode::Tuple(cnt) => -(*cnt as i32) + 1,
      SymbolicByteCode::Map(cnt) => -(*cnt as i32 * 2) + 1,
      SymbolicByteCode::Launch(args) => -(*args as i32 + 1),
      SymbolicByteCode::Channel => 1,
      SymbolicByteCode::BufferedChannel => 0,
      SymbolicByteCode::Receive => 0,
      SymbolicByteCode::Send => 0,
      SymbolicByteCode::Interpolate(cnt) => -(*cnt as i32) + 1,
      SymbolicByteCode::IterNext(_) => 0,
      SymbolicByteCode::IterCurrent(_) => 0,
      SymbolicByteCode::Drop => -1,
      SymbolicByteCode::DropN(cnt) => -(*cnt as i32),
      SymbolicByteCode::Dup => 1,
      SymbolicByteCode::Import(_) => 1,
      SymbolicByteCode::ImportSymbol(_) => 1,
      SymbolicByteCode::Export(_) => 0,
      SymbolicByteCode::DefineGlobal(_) => -1,
      SymbolicByteCode::GetGlobal(_) => 1,
      SymbolicByteCode::SetGlobal(_) => 0,
      SymbolicByteCode::Box(_) => 0,
      SymbolicByteCode::EmptyBox => 1,
      SymbolicByteCode::FillBox => -1,
      SymbolicByteCode::GetBox(_) => 1,
      SymbolicByteCode::SetBox(_) => 0,
      SymbolicByteCode::GetLocal(_) => 1,
      SymbolicByteCode::SetLocal(_) => 0,
      SymbolicByteCode::GetCapture(_) => 1,
      SymbolicByteCode::SetCapture(_) => 0,
      SymbolicByteCode::GetProperty(_) => 0,
      SymbolicByteCode::SetProperty(_) => -1,
      SymbolicByteCode::JumpIfFalse(_) => -1,
      SymbolicByteCode::Jump(_) => 0,
      SymbolicByteCode::Loop(_) => 0,
      SymbolicByteCode::Call(args) => -(*args as i32),
      SymbolicByteCode::Invoke((_, args)) => -(*args as i32),
      SymbolicByteCode::SuperInvoke((_, args)) => -(*args as i32 + 1),
      SymbolicByteCode::Closure(_) => 1,
      SymbolicByteCode::Method(_) => -1,
      SymbolicByteCode::Field(_) => 0,
      SymbolicByteCode::StaticMethod(_) => -1,
      SymbolicByteCode::Class(_) => 1,
      SymbolicByteCode::Inherit => 0,
      SymbolicByteCode::GetSuper(_) => -1,
      SymbolicByteCode::CaptureIndex(_) => 0,
      SymbolicByteCode::Slot(_) => 0,
      SymbolicByteCode::Equal => -1,
      SymbolicByteCode::NotEqual => -1,
      SymbolicByteCode::Greater => -1,
      SymbolicByteCode::GreaterEqual => -1,
      SymbolicByteCode::Less => -1,
      SymbolicByteCode::LessEqual => -1,
      SymbolicByteCode::Label(_) => 0,
    }
  }
}

impl Encode for SymbolicByteCode {
  fn encode(instructions: Vec<Self>) -> Vec<u8> {
    let mut label_offsets: [usize; u16::MAX as usize] = [0; u16::MAX as usize];
    let label_count = label_count(&instructions);

    if label_count > u16::MAX as usize {
      todo!("Really handle this");
    }

    compute_label_offsets(&instructions, &mut label_offsets[..label_count]);

    let mut buffer = Vec::with_capacity(instructions.len());
    let mut offset: usize = 0;

    for instruction in instructions {
      instruction.encode(&mut buffer, &label_offsets[..label_count], offset);
      offset += instruction.len()
    }

    buffer
  }
}

fn label_count(instructions: &[SymbolicByteCode]) -> usize {
  let mut count = 0;

  for instruction in instructions {
    if let SymbolicByteCode::Label(_) = instruction {
      count += 1;
    }
  }

  count
}

fn compute_label_offsets(instructions: &[SymbolicByteCode], label_offsets: &mut [usize]) {
  let mut offset: usize = 0;

  for instruction in instructions {
    if let SymbolicByteCode::Label(label) = instruction {
      label_offsets[label.0 as usize] = offset;
    }

    offset += instruction.len()
  }
}

impl Default for SymbolicByteCode {
  fn default() -> Self {
    SymbolicByteCode::Nil
  }
}

fn op(code: &mut Vec<u8>, byte_code: ByteCode) {
  push_op(code, byte_code);
}

fn op_byte(code: &mut Vec<u8>, byte_code: ByteCode, byte: u8) {
  push_op_u8(code, byte_code, byte);
}

fn op_short(code: &mut Vec<u8>, byte_code: ByteCode, short: u16) {
  push_op_u16(code, byte_code, short);
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

  /// Initialize Tuple
  Tuple,

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

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn encode_len() {
    let code: Vec<(usize, SymbolicByteCode)> = vec![
      (1, SymbolicByteCode::Return),
      (1, SymbolicByteCode::Negate),
      (1, SymbolicByteCode::Add),
      (1, SymbolicByteCode::Subtract),
      (1, SymbolicByteCode::Multiply),
      (1, SymbolicByteCode::Divide),
      (1, SymbolicByteCode::Not),
      (2, SymbolicByteCode::Constant(113)),
      (3, SymbolicByteCode::ConstantLong(45863)),
      (3, SymbolicByteCode::Import(2235)),
      (5, SymbolicByteCode::ImportSymbol((2235, 113))),
      (3, SymbolicByteCode::Export(7811)),
      (1, SymbolicByteCode::Nil),
      (1, SymbolicByteCode::True),
      (1, SymbolicByteCode::False),
      (3, SymbolicByteCode::List(54782)),
      (3, SymbolicByteCode::Tuple(52782)),
      (3, SymbolicByteCode::Map(1923)),
      (2, SymbolicByteCode::Launch(197)),
      (1, SymbolicByteCode::Channel),
      (1, SymbolicByteCode::BufferedChannel),
      (1, SymbolicByteCode::Receive),
      (1, SymbolicByteCode::Send),
      (2, SymbolicByteCode::Box(66)),
      (1, SymbolicByteCode::EmptyBox),
      (1, SymbolicByteCode::FillBox),
      (3, SymbolicByteCode::Interpolate(3389)),
      (3, SymbolicByteCode::IterNext(81)),
      (3, SymbolicByteCode::IterCurrent(49882)),
      (1, SymbolicByteCode::Drop),
      (3, SymbolicByteCode::DefineGlobal(42)),
      (3, SymbolicByteCode::GetGlobal(14119)),
      (3, SymbolicByteCode::SetGlobal(2043)),
      (3, SymbolicByteCode::SetGlobal(38231)),
      (2, SymbolicByteCode::GetBox(183)),
      (2, SymbolicByteCode::SetBox(56)),
      (2, SymbolicByteCode::GetLocal(96)),
      (2, SymbolicByteCode::SetLocal(149)),
      (2, SymbolicByteCode::GetBox(11)),
      (2, SymbolicByteCode::SetBox(197)),
      (3, SymbolicByteCode::GetProperty(18273)),
      (3, SymbolicByteCode::SetProperty(253)),
      (3, SymbolicByteCode::JumpIfFalse(Label::new(0))),
      (3, SymbolicByteCode::Jump(Label::new(0))),
      (3, SymbolicByteCode::Loop(Label::new(0))),
      (2, SymbolicByteCode::Call(77)),
      (4, SymbolicByteCode::Invoke((5591, 19))),
      (4, SymbolicByteCode::SuperInvoke((2105, 15))),
      (3, SymbolicByteCode::Closure(3638)),
      (3, SymbolicByteCode::Method(188)),
      (3, SymbolicByteCode::Field(6634)),
      (3, SymbolicByteCode::StaticMethod(4912)),
      (3, SymbolicByteCode::Class(64136)),
      (1, SymbolicByteCode::Inherit),
      (3, SymbolicByteCode::GetSuper(24)),
      (1, SymbolicByteCode::Equal),
      (1, SymbolicByteCode::NotEqual),
      (1, SymbolicByteCode::Greater),
      (1, SymbolicByteCode::GreaterEqual),
      (1, SymbolicByteCode::LessEqual),
      (0, SymbolicByteCode::Label(Label::new(0))),
    ];

    let mut buffer: Vec<u8> = Vec::new();
    let mut label_offsets: Vec<usize> = vec![0];

    for (size1, byte_code1) in &code {
      for (size2, byte_code2) in &code {
        let mut offset = 0;
        byte_code1.encode(&mut buffer, &label_offsets, offset);
        offset += byte_code1.len();

        byte_code2.encode(&mut buffer, &label_offsets, offset);

        assert_eq!(buffer.len(), byte_code1.len() + byte_code2.len());
        buffer.clear();
      }
    }
  }
}
