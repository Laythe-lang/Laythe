use bumpalo::collections::Vec;
use codespan_reporting::diagnostic::Diagnostic;
use std::{cell::RefCell, fmt::Display, mem, rc::Rc};
use variant_count::VariantCount;

#[cfg(any(test, feature = "debug"))]
use std::convert::TryInto;

use crate::{cache::CacheIdEmitter, source::VmFileId};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Label(u32);

impl Label {
  pub fn new(id: u32) -> Self {
    Self(id)
  }

  pub fn val(&self) -> u32 {
    self.0
  }
}

impl Display for Label {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.0)
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CaptureIndex {
  /// The capture is in the local function
  Local(u8),

  /// The capture points to the enclosing function
  Enclosing(u8),
}

/// Space Lox virtual machine byte codes
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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

  /// Push an exception handler onto the fiber
  PushHandler((u16, Label)),

  /// Pop an exception handler off the fiber
  PopHandler,

  /// A marked location
  Label(Label),

  /// Indicates the end of an expression
  ArgumentDelimiter,

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

  /// An invoke cache slot
  InvokeSlot,

  /// A property slot
  PropertySlot,

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
  /// Encode aligned bytecode as unaligned bytecode for better storage / compactness
  pub fn encode(
    self,
    code: &mut Vec<u8>,
    label_offsets: &[usize],
    cache_id_emitter: Rc<RefCell<CacheIdEmitter>>,
    offset: usize,
  ) -> Option<Diagnostic<VmFileId>> {
    match self {
      Self::Return => op(code, ByteCode::Return),
      Self::Negate => op(code, ByteCode::Negate),
      Self::Add => op(code, ByteCode::Add),
      Self::Subtract => op(code, ByteCode::Subtract),
      Self::Multiply => op(code, ByteCode::Multiply),
      Self::Divide => op(code, ByteCode::Divide),
      Self::And(target) => {
        let jump = label_offsets[target.0 as usize] - offset - 3;
        op_jump(code, ByteCode::And, jump)
      },
      Self::Or(target) => {
        let jump = label_offsets[target.0 as usize] - offset - 3;
        op_jump(code, ByteCode::Or, jump)
      },
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
        push_op_u16_tuple(code, ByteCode::ImportSymbol, path, slot)
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
      Self::JumpIfFalse(target) => {
        let jump = label_offsets[target.0 as usize] - offset - 3;
        op_jump(code, ByteCode::JumpIfFalse, jump)
      },
      Self::Jump(target) => {
        let jump = label_offsets[target.0 as usize] - offset - 3;
        op_jump(code, ByteCode::Jump, jump)
      },
      Self::Loop(target) => {
        let jump = offset - label_offsets[target.0 as usize] + 3;
        op_jump(code, ByteCode::Loop, jump)
      },
      Self::PushHandler((slots, target)) => {
        let jump = label_offsets[target.0 as usize] - offset - 5;
        push_op_u16_tuple(code, ByteCode::PushHandler, slots, (jump) as u16);
        jump_error(jump)
      },
      Self::PopHandler => op(code, ByteCode::PopHandler),
      Self::Call(slot) => op_byte(code, ByteCode::Call, slot),
      Self::Invoke((slot1, slot2)) => push_op_u16_u8_tuple(code, ByteCode::Invoke, slot1, slot2),
      Self::SuperInvoke((slot1, slot2)) => {
        push_op_u16_u8_tuple(code, ByteCode::SuperInvoke, slot1, slot2)
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
        None
      },
      Self::PropertySlot => {
        let bytes = cache_id_emitter.borrow_mut().emit_property().to_ne_bytes();
        code.extend_from_slice(&bytes);
        None
      },
      Self::InvokeSlot => {
        let bytes = cache_id_emitter.borrow_mut().emit_invoke().to_ne_bytes();
        code.extend_from_slice(&bytes);
        None
      },
      Self::Label(_) => None,
      Self::ArgumentDelimiter => None,
    }
  }

  /// What is the len of this instruction once encoded
  pub const fn len(&self) -> usize {
    match self {
      Self::Return => 1,
      Self::Negate => 1,
      Self::Add => 1,
      Self::Subtract => 1,
      Self::Multiply => 1,
      Self::Divide => 1,
      Self::Not => 1,
      Self::And(_) => 3,
      Self::Or(_) => 3,
      Self::Constant(_) => 2,
      Self::ConstantLong(_) => 3,
      Self::Nil => 1,
      Self::True => 1,
      Self::False => 1,
      Self::List(_) => 3,
      Self::Tuple(_) => 3,
      Self::Map(_) => 3,
      Self::Launch(_) => 2,
      Self::Channel => 1,
      Self::BufferedChannel => 1,
      Self::Receive => 1,
      Self::Send => 1,
      Self::Interpolate(_) => 3,
      Self::IterNext(_) => 3,
      Self::IterCurrent(_) => 3,
      Self::Drop => 1,
      Self::DropN(_) => 2,
      Self::Dup => 1,
      Self::Import(_) => 3,
      Self::ImportSymbol(_) => 5,
      Self::Export(_) => 3,
      Self::DefineGlobal(_) => 3,
      Self::GetGlobal(_) => 3,
      Self::SetGlobal(_) => 3,
      Self::Box(_) => 2,
      Self::EmptyBox => 1,
      Self::FillBox => 1,
      Self::GetBox(_) => 2,
      Self::SetBox(_) => 2,
      Self::GetLocal(_) => 2,
      Self::SetLocal(_) => 2,
      Self::GetCapture(_) => 2,
      Self::SetCapture(_) => 2,
      Self::GetProperty(_) => 3,
      Self::SetProperty(_) => 3,
      Self::JumpIfFalse(_) => 3,
      Self::Jump(_) => 3,
      Self::Loop(_) => 3,
      Self::PushHandler(_) => 5,
      Self::PopHandler => 1,
      Self::Call(_) => 2,
      Self::Invoke((_, _)) => 4,
      Self::SuperInvoke((_, _)) => 4,
      Self::Closure(_) => 3,
      Self::Method(_) => 3,
      Self::Field(_) => 3,
      Self::StaticMethod(_) => 3,
      Self::Class(_) => 3,
      Self::Inherit => 1,
      Self::GetSuper(_) => 3,
      Self::CaptureIndex(_) => 2,
      Self::PropertySlot => 4,
      Self::InvokeSlot => 4,
      Self::Equal => 1,
      Self::NotEqual => 1,
      Self::Greater => 1,
      Self::GreaterEqual => 1,
      Self::Less => 1,
      Self::LessEqual => 1,
      Self::Label(_) => 0,
      Self::ArgumentDelimiter => 0,
    }
  }

  /// What effect will this instruction have on the stack
  pub const fn stack_effect(&self) -> i32 {
    match self {
      Self::Return => 0,
      Self::Negate => 0,
      Self::Add => -1,
      Self::Subtract => -1,
      Self::Multiply => -1,
      Self::Divide => -1,
      Self::Not => 0,
      Self::And(_) => -1,
      Self::Or(_) => -1,
      Self::Constant(_) => 1,
      Self::ConstantLong(_) => 1,
      Self::Nil => 1,
      Self::True => 1,
      Self::False => 1,
      Self::List(cnt) => -(*cnt as i32) + 1,
      Self::Tuple(cnt) => -(*cnt as i32) + 1,
      Self::Map(cnt) => -(*cnt as i32 * 2) + 1,
      Self::Launch(args) => -(*args as i32 + 1),
      Self::Channel => 1,
      Self::BufferedChannel => 0,
      Self::Receive => 0,
      Self::Send => 0,
      Self::Interpolate(cnt) => -(*cnt as i32) + 1,
      Self::IterNext(_) => 0,
      Self::IterCurrent(_) => 0,
      Self::Drop => -1,
      Self::DropN(cnt) => -(*cnt as i32),
      Self::Dup => 1,
      Self::Import(_) => 1,
      Self::ImportSymbol(_) => 1,
      Self::Export(_) => 0,
      Self::DefineGlobal(_) => -1,
      Self::GetGlobal(_) => 1,
      Self::SetGlobal(_) => 0,
      Self::Box(_) => 0,
      Self::EmptyBox => 1,
      Self::FillBox => -1,
      Self::GetBox(_) => 1,
      Self::SetBox(_) => 0,
      Self::GetLocal(_) => 1,
      Self::SetLocal(_) => 0,
      Self::GetCapture(_) => 1,
      Self::SetCapture(_) => 0,
      Self::GetProperty(_) => 0,
      Self::SetProperty(_) => -1,
      Self::JumpIfFalse(_) => -1,
      Self::Jump(_) => 0,
      Self::Loop(_) => 0,
      Self::PushHandler(_) => 0,
      Self::PopHandler => 0,
      Self::Call(args) => -(*args as i32),
      Self::Invoke((_, args)) => -(*args as i32),
      Self::SuperInvoke((_, args)) => -(*args as i32 + 1),
      Self::Closure(_) => 1,
      Self::Method(_) => -1,
      Self::Field(_) => 0,
      Self::StaticMethod(_) => -1,
      Self::Class(_) => 1,
      Self::Inherit => 0,
      Self::GetSuper(_) => -1,
      Self::CaptureIndex(_) => 0,
      Self::PropertySlot => 0,
      Self::InvokeSlot => 0,
      Self::Equal => -1,
      Self::NotEqual => -1,
      Self::Greater => -1,
      Self::GreaterEqual => -1,
      Self::Less => -1,
      Self::LessEqual => -1,
      Self::Label(_) => 0,
      Self::ArgumentDelimiter => 0,
    }
  }
}

fn jump_error(jump: usize) -> Option<Diagnostic<VmFileId>> {
  if jump > u16::MAX as usize {
    Some(Diagnostic::error().with_message("Unable to jump so far."))
  } else {
    None
  }
}

impl Default for SymbolicByteCode {
  fn default() -> Self {
    SymbolicByteCode::Nil
  }
}

fn op(code: &mut Vec<u8>, byte_code: ByteCode) -> Option<Diagnostic<VmFileId>> {
  push_op(code, byte_code);
  None
}

fn op_byte(code: &mut Vec<u8>, byte_code: ByteCode, byte: u8) -> Option<Diagnostic<VmFileId>> {
  push_op_u8(code, byte_code, byte);
  None
}

fn op_jump(code: &mut Vec<u8>, byte_code: ByteCode, jump: usize) -> Option<Diagnostic<VmFileId>> {
  op_short(code, byte_code, jump as u16);
  jump_error(jump)
}

fn op_short(code: &mut Vec<u8>, byte_code: ByteCode, short: u16) -> Option<Diagnostic<VmFileId>> {
  push_op_u16(code, byte_code, short);
  None
}

fn push_op(code: &mut Vec<u8>, byte: ByteCode) {
  code.push(byte.to_byte());
}

fn push_op_u8(code: &mut Vec<u8>, byte: ByteCode, param: u8) {
  code.push(byte.to_byte());
  code.push(param);
}

fn push_op_u16_u8_tuple(
  code: &mut Vec<u8>,
  byte: ByteCode,
  param1: u16,
  param2: u8,
) -> Option<Diagnostic<VmFileId>> {
  code.push(byte.to_byte());
  let param_bytes = param1.to_ne_bytes();
  code.extend_from_slice(&param_bytes);
  code.push(param2);
  None
}

fn push_op_u16(code: &mut Vec<u8>, byte: ByteCode, param: u16) {
  let param_bytes = param.to_ne_bytes();
  code.push(byte.to_byte());
  code.extend_from_slice(&param_bytes);
}

fn push_op_u16_tuple(
  code: &mut Vec<u8>,
  byte: ByteCode,
  param1: u16,
  param2: u16,
) -> Option<Diagnostic<VmFileId>> {
  code.push(byte.to_byte());
  let param_bytes = param1.to_ne_bytes();
  code.extend_from_slice(&param_bytes);
  let param_bytes = param2.to_ne_bytes();
  code.extend_from_slice(&param_bytes);
  None
}

/// Laythe virtual machine byte codes
#[derive(Debug, PartialEq, Eq, Clone, Copy, VariantCount)]
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

  /// Push an exception handler onto the fiber
  PushHandler,

  /// Pop an exception handler off the fiber
  PopHandler,

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

  #[cfg(any(test, feature = "debug"))]
  pub fn from_byte(byte: u8) -> Self {
    if byte as usize >= ByteCode::VARIANT_COUNT {
      panic!(
        "Value {} is out of range {} byte code variants",
        byte,
        ByteCode::VARIANT_COUNT
      );
    }

    unsafe { mem::transmute(byte) }
  }

  pub unsafe fn from_byte_unchecked(byte: u8) -> Self {
    mem::transmute(byte)
  }
}

/// Laythe virtual machine byte codes
#[cfg(any(test, feature = "debug"))]
#[derive(PartialEq, Clone, Copy, Debug)]
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
  JumpIfFalse(u16),

  /// Jump conditionally to the ip
  Jump(u16),

  /// Jump to loop beginning
  Loop(u16),

  /// Push an exception handler onto the fiber
  PushHandler((u16, u16)),

  /// Pop an exception handler off the fiber
  PopHandler,

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
  #[allow(dead_code)]
  CaptureIndex(CaptureIndex),

  /// A inline cache slot
  #[allow(dead_code)]
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

#[cfg(any(test, feature = "debug"))]
impl AlignedByteCode {
  /// Decode unaligned bytecode to aligned bytecode. Primarily for testing purposes
  pub fn decode(store: &[u8], offset: usize) -> (AlignedByteCode, usize) {
    let byte_code = ByteCode::from_byte(store[offset]);

    match byte_code {
      ByteCode::Return => (Self::Return, offset + 1),
      ByteCode::Negate => (Self::Negate, offset + 1),
      ByteCode::Add => (Self::Add, offset + 1),
      ByteCode::Subtract => (Self::Subtract, offset + 1),
      ByteCode::Multiply => (Self::Multiply, offset + 1),
      ByteCode::Divide => (Self::Divide, offset + 1),
      ByteCode::And => (
        Self::And(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Or => (
        Self::Or(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Not => (Self::Not, offset + 1),
      ByteCode::Constant => (Self::Constant(store[offset + 1]), offset + 2),
      ByteCode::ConstantLong => (
        Self::ConstantLong(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Nil => (Self::Nil, offset + 1),
      ByteCode::True => (Self::True, offset + 1),
      ByteCode::False => (Self::False, offset + 1),
      ByteCode::List => (
        Self::List(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Tuple => (
        Self::Tuple(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Map => (
        Self::Map(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Launch => (Self::Launch(store[offset + 1]), offset + 2),
      ByteCode::Channel => (Self::Channel, offset + 1),
      ByteCode::BufferedChannel => (Self::BufferedChannel, offset + 1),
      ByteCode::Receive => (Self::Receive, offset + 1),
      ByteCode::Send => (Self::Send, offset + 1),
      ByteCode::Interpolate => (
        Self::Interpolate(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::IterNext => (
        Self::IterNext(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::IterCurrent => (
        Self::IterCurrent(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Drop => (Self::Drop, offset + 1),
      ByteCode::DropN => (Self::DropN(store[offset + 1]), offset + 2),
      ByteCode::Dup => (Self::Dup, offset + 1),
      ByteCode::Import => (
        Self::Import(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::ImportSymbol => (
        Self::ImportSymbol((
          decode_u16(&store[offset + 1..offset + 3]),
          decode_u16(&store[offset + 3..offset + 5]),
        )),
        offset + 5,
      ),
      ByteCode::Export => (
        Self::Export(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::DefineGlobal => (
        Self::DefineGlobal(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::GetGlobal => (
        Self::GetGlobal(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::SetGlobal => (
        Self::SetGlobal(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Box => (Self::Box(store[offset + 1]), offset + 2),
      ByteCode::EmptyBox => (Self::EmptyBox, offset + 1),
      ByteCode::FillBox => (Self::FillBox, offset + 1),
      ByteCode::GetBox => (Self::GetBox(store[offset + 1]), offset + 2),
      ByteCode::SetBox => (Self::SetBox(store[offset + 1]), offset + 2),
      ByteCode::GetLocal => (Self::GetLocal(store[offset + 1]), offset + 2),
      ByteCode::SetLocal => (Self::SetLocal(store[offset + 1]), offset + 2),
      ByteCode::GetCapture => (Self::GetCapture(store[offset + 1]), offset + 2),
      ByteCode::SetCapture => (Self::SetCapture(store[offset + 1]), offset + 2),
      ByteCode::GetProperty => (
        Self::GetProperty(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::SetProperty => (
        Self::SetProperty(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::JumpIfFalse => (
        Self::JumpIfFalse(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Jump => (
        Self::Jump(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Loop => (
        Self::Loop(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::PushHandler => (
        Self::PushHandler((
          decode_u16(&store[offset + 1..offset + 3]),
          decode_u16(&store[offset + 3..offset + 5]),
        )),
        offset + 5,
      ),
      ByteCode::PopHandler => (Self::PopHandler, offset + 1),
      ByteCode::Call => (Self::Call(store[offset + 1]), offset + 2),
      ByteCode::Invoke => (
        Self::Invoke((
          decode_u16(&store[offset + 1..offset + 3]),
          store[offset + 3],
        )),
        offset + 4,
      ),
      ByteCode::SuperInvoke => (
        Self::SuperInvoke((
          decode_u16(&store[offset + 1..offset + 3]),
          store[offset + 3],
        )),
        offset + 4,
      ),
      ByteCode::Closure => (
        Self::Closure(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Method => (
        Self::Method(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Field => (
        Self::Field(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::StaticMethod => (
        Self::StaticMethod(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Class => (
        Self::Class(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Inherit => (Self::Inherit, offset + 1),
      ByteCode::GetSuper => (
        Self::GetSuper(decode_u16(&store[offset + 1..offset + 3])),
        offset + 3,
      ),
      ByteCode::Equal => (Self::Equal, offset + 1),
      ByteCode::NotEqual => (Self::NotEqual, offset + 1),
      ByteCode::Greater => (Self::Greater, offset + 1),
      ByteCode::GreaterEqual => (Self::GreaterEqual, offset + 1),
      ByteCode::Less => (Self::Less, offset + 1),
      ByteCode::LessEqual => (Self::LessEqual, offset + 1),
    }
  }
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
  use bumpalo::Bump;

  #[test]
  fn encode_len() {
    let bump = Bump::new();
    let code: Vec<(usize, SymbolicByteCode)> = bumpalo::vec![
      in &bump;
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
      (3, SymbolicByteCode::JumpIfFalse(Label::new(1))),
      (3, SymbolicByteCode::Jump(Label::new(1))),
      (3, SymbolicByteCode::Loop(Label::new(0))),
      (3, SymbolicByteCode::And(Label::new(1))),
      (3, SymbolicByteCode::Or(Label::new(1))),
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
    ];

    let mut buffer = Vec::<u8>::with_capacity_in(20, &bump);
    let cache_id_emitter = Rc::new(RefCell::new(CacheIdEmitter::default()));

    for (size1, byte_code1) in &code {
      for (size2, byte_code2) in &code {
        buffer.clear();
        let label_offsets = [0, 10];
        // TODO we probably need to dynamically calculate these labels so for the various jump instructions

        let err1 = byte_code1.encode(&mut buffer, &label_offsets, Rc::clone(&cache_id_emitter), 0);
        assert_eq!(
          buffer.len(),
          *size1,
          "byte {:?} expected to be {} size",
          *byte_code1,
          *size1
        );
        assert!(err1.is_none());

        let len = buffer.len();
        let err2 = byte_code2.encode(
          &mut buffer,
          &label_offsets,
          Rc::clone(&cache_id_emitter),
          len,
        );
        assert_eq!(
          buffer.len() - size1,
          *size2,
          "byte {:?} expected to be {} size",
          *byte_code2,
          *size2
        );
        assert!(err2.is_none());
      }
    }
  }
}
