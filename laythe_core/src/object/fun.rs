use laythe_env::managed::{DebugHeap, DebugWrap, Gc, GcStr, Manage, Trace};
use std::{fmt, io::Write, mem};

use crate::{
  chunk::{AlignedByteCode, Chunk},
  hooks::GcHooks,
  module::Module,
  signature::Arity,
  value::Value,
};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum FunKind {
  Fun,
  Method,
  StaticMethod,
  Initializer,
  Script,
}

impl fmt::Display for FunKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let string = match self {
      FunKind::Fun => "function",
      FunKind::Method => "method",
      FunKind::StaticMethod => "static method",
      FunKind::Initializer => "class initializer",
      FunKind::Script => "script",
    };

    f.write_str(string)
  }
}

#[derive(Clone)]
pub struct TryBlock {
  /// Start of the try block
  start: u16,

  /// End of the try block
  end: u16,
}

impl TryBlock {
  pub fn new(start: u16, end: u16) -> Self {
    TryBlock { start, end }
  }
}

#[derive(Clone)]
pub struct Fun {
  /// Name if not top-level script
  name: GcStr,

  /// Arity of this function
  pub arity: Arity,

  /// Number of upvalues
  pub upvalue_count: usize,

  /// The module this function belongs to
  module: Gc<Module>,

  /// Catch block present in this function
  try_blocks: Vec<TryBlock>,

  /// Code for the function body
  chunk: Chunk,
}

impl Fun {
  pub fn new(name: GcStr, module: Gc<Module>) -> Self {
    Self {
      arity: Arity::default(),
      upvalue_count: 0,
      chunk: Chunk::default(),
      module,
      name,
      try_blocks: Vec::new(),
    }
  }

  #[inline]
  pub fn name(&self) -> GcStr {
    self.name
  }

  #[inline]
  pub fn chunk(&self) -> &Chunk {
    &self.chunk
  }

  #[inline]
  pub fn module(&self) -> Gc<Module> {
    self.module
  }

  pub fn add_try(&mut self, hooks: &GcHooks, try_block: TryBlock) {
    hooks.grow(self, |fun| fun.try_blocks.push(try_block));
  }

  pub fn has_catch_jump(&self, ip: u16) -> Option<u16> {
    let mut min_range = std::u16::MAX;
    let mut jump = None;

    for try_block in &self.try_blocks {
      if ip >= try_block.start && ip < try_block.end {
        let len = try_block.end - try_block.start;

        if len < min_range {
          min_range = len;
          jump = Some(try_block.end);
        }
      }
    }

    jump
  }

  #[inline]
  pub fn write_instruction(&mut self, hooks: &GcHooks, op_code: AlignedByteCode, line: u32) {
    hooks.grow(self, |fun| fun.chunk.write_instruction(op_code, line));
  }

  #[inline]
  pub fn replace_instruction(&mut self, index: usize, instruction: u8) {
    self.chunk.instructions[index] = instruction;
  }

  #[inline]
  pub fn add_constant(&mut self, hooks: &GcHooks, constant: Value) -> usize {
    hooks.grow(self, |fun| fun.chunk.add_constant(constant))
  }

  pub fn shrink_to_fit(&mut self, hooks: &GcHooks) {
    hooks.shrink(self, |fun| {
      fun.chunk.shrink_to_fit();
      fun.try_blocks.shrink_to_fit();
    });
  }
}

impl fmt::Display for Fun {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<fn {}>", self.name)
  }
}

impl fmt::Debug for Fun {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.fmt_heap(f, 1)
  }
}

impl Trace for Fun {
  fn trace(&self) {
    self.name.trace();
    self.chunk.constants.iter().for_each(|constant| {
      constant.trace();
    });
    self.module.trace();
  }

  fn trace_debug(&self, stdio: &mut dyn Write) {
    self.name.trace_debug(stdio);
    self.chunk.constants.iter().for_each(|constant| {
      constant.trace_debug(stdio);
    });
    self.module.trace_debug(stdio);
  }
}

impl DebugHeap for Fun {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    let depth = depth.saturating_sub(1);

    f.debug_struct("Fun")
      .field("name", &DebugWrap(&self.name, depth))
      .field("arity", &self.arity)
      .field("upvalue_count", &self.upvalue_count)
      .field("Module", &DebugWrap(&self.module, depth))
      .field("chunk", &self.chunk)
      .finish()
  }
}

impl Manage for Fun {
  fn size(&self) -> usize {
    mem::size_of::<Self>()
      + self.chunk.size()
      + mem::size_of::<TryBlock>() * self.try_blocks.capacity()
  }

  fn as_debug(&self) -> &dyn DebugHeap {
    self
  }
}
