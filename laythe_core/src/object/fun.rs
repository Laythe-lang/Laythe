use std::{fmt, io::Write};

use crate::{
  chunk::{Chunk, ChunkBuilder, Encode},
  hooks::GcHooks,
  impl_debug_heap, impl_trace,
  managed::{Array, DebugHeap, DebugWrap, Gc, GcStr, Object, Trace},
  module::Module,
  signature::Arity,
  value::Value,
};

use super::ObjectKind;

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

#[derive(Clone, Copy, Debug)]
pub struct TryBlock {
  /// Start of the try block
  start: usize,

  /// End of the try block
  end: usize,

  /// How many slots were used at the beginning of this
  /// try catch
  slots: usize,
}

impl TryBlock {
  pub fn new(start: usize, end: usize, slots: usize) -> Self {
    TryBlock { start, end, slots }
  }
}

impl_trace!(TryBlock);
impl_debug_heap!(TryBlock);

/// A mutable builder for an immutable function
pub struct FunBuilder<T> {
  /// Name if not top-level script
  name: GcStr,

  /// Arity of this function
  arity: Arity,

  /// Number of captures
  capture_count: u8,

  /// The max number of slots in this function
  max_slots: i32,

  /// The module this function belongs to
  module: Gc<Module>,

  /// Catch block present in this function
  try_blocks: Vec<TryBlock>,

  /// Code for the function body
  chunk: ChunkBuilder<T>,
}

impl<T: Default> FunBuilder<T> {
  pub fn new(name: GcStr, module: Gc<Module>, arity: Arity) -> Self {
    Self {
      arity,
      capture_count: 0,
      max_slots: 0,
      chunk: ChunkBuilder::<T>::default(),
      module,
      name,
      try_blocks: Vec::new(),
    }
  }
}

impl<T> FunBuilder<T> {
  pub fn name(&self) -> &str {
    &self.name
  }

  /// Increment the capture count of this function
  /// builder
  #[inline]
  pub fn inc_capture(&mut self) {
    self.capture_count += 1;
  }

  /// Update max slots if new count is greater than
  /// previous
  #[inline]
  pub fn update_max_slots(&mut self, slots: i32) {
    // debug_assert!(slots >= 0);
    if slots > self.max_slots {
      self.max_slots = slots
    }
  }

  /// Retrieve the current count of captures
  #[inline]
  pub fn capture_count(&self) -> u8 {
    self.capture_count
  }

  /// Retrieve a reference to the underlying chunk builder
  #[inline]
  pub fn chunk(&self) -> &ChunkBuilder<T> {
    &self.chunk
  }

  /// Write an aligned byte code to this function
  #[inline]
  pub fn write_instruction(&mut self, item: T, line: u32) {
    self.chunk.write_instruction(item, line)
  }

  /// Patch an instruction on this function
  #[inline]
  pub fn patch_instruction(&mut self, index: usize, byte: T) {
    self.chunk.patch_instruction(index, byte);
  }

  /// Add a constant to this function
  #[inline]
  pub fn add_constant(&mut self, constant: Value) -> usize {
    self.chunk.add_constant(constant)
  }

  /// Add a try block to this function
  pub fn add_try(&mut self, try_block: TryBlock) {
    self.try_blocks.push(try_block)
  }
}

impl<T: Encode> FunBuilder<T> {
  /// Build a final immutable Fun from this builder
  pub fn build(self, hooks: &GcHooks) -> Fun {
    let try_blocks = hooks.manage(&*self.try_blocks);
    hooks.push_root(try_blocks);
    let chunk = self.chunk.build(hooks);
    hooks.pop_roots(1);

    Fun {
      name: self.name,
      arity: self.arity,
      capture_count: self.capture_count,
      max_slot: self.max_slots as u32,
      module_id: self.module.id(),
      module: self.module,
      try_blocks,
      chunk,
    }
  }
}

impl<T> Trace for FunBuilder<T> {
  fn trace(&self) {
    self.name.trace();
    self.chunk.trace();
    self.module.trace();
  }

  fn trace_debug(&self, log: &mut dyn Write) {
    self.name.trace_debug(log);
    self.chunk.trace_debug(log);
    self.module.trace_debug(log);
  }
}

#[derive(Clone)]
pub struct Fun {
  /// Name of this function
  name: GcStr,

  /// Arity of this function
  arity: Arity,

  /// Number of captures
  capture_count: u8,

  /// The max number of slots for this function
  max_slot: u32,

  /// What is the id of the associated module
  module_id: usize,

  /// The module this function belongs to
  module: Gc<Module>,

  /// Catch block present in this function
  try_blocks: Array<TryBlock>,

  /// Code for the function body
  chunk: Chunk,
}

impl Fun {
  pub fn stub<T: Encode + Default>(hooks: &GcHooks, name: GcStr, module: Gc<Module>, instruction: T) -> Fun {
    let mut builder = FunBuilder::new(name, module, Arity::Variadic(0));
    builder.write_instruction(instruction, 0);

    builder.build(hooks)
  }

  /// Name of this function
  #[inline]
  pub fn name(&self) -> GcStr {
    self.name
  }

  /// Name of this function
  pub fn set_name(&mut self, new_name: GcStr) {
    self.name = new_name;
  }

  /// Arity of this function
  #[inline]
  pub fn arity(&self) -> &Arity {
    &self.arity
  }

  /// Code for the function body
  #[inline]
  pub fn chunk(&self) -> &Chunk {
    &self.chunk
  }

  /// The module id this function belongs to
  #[inline]
  pub fn module_id(&self) -> usize {
    self.module_id
  }

  /// The module this function belongs to
  #[inline]
  pub fn module(&self) -> Gc<Module> {
    self.module
  }

  /// Number of captures
  #[inline]
  pub fn capture_count(&self) -> usize {
    self.capture_count as usize
  }

  /// Number of slots
  #[inline]
  pub fn max_slots(&self) -> usize {
    self.max_slot as usize
  }

  pub fn has_catch_jump(&self, ip: usize) -> Option<(usize, usize)> {
    let mut min_range = std::usize::MAX;
    let mut catch = None;

    for try_block in self.try_blocks.iter() {
      if ip >= try_block.start && ip < try_block.end {
        let len = try_block.end - try_block.start;

        if len < min_range {
          min_range = len;
          catch = Some((try_block.end, try_block.slots));
        }
      }
    }

    catch
  }
}

impl fmt::Display for Fun {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<fn {} {:p}>", self.name, &*self)
  }
}

impl fmt::Debug for Fun {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.fmt_heap(f, 2)
  }
}

impl Trace for Fun {
  fn trace(&self) {
    self.name.trace();
    self.module.trace();
    self.try_blocks.trace();
    self.chunk.trace();
  }

  fn trace_debug(&self, log: &mut dyn Write) {
    self.name.trace_debug(log);
    self.module.trace_debug(log);
    self.try_blocks.trace_debug(log);
    self.chunk.trace_debug(log);
  }
}

impl DebugHeap for Fun {
  fn fmt_heap(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
    f.debug_struct("Fun")
      .field("name", &DebugWrap(&self.name, depth))
      .field("arity", &self.arity)
      .field("capture_count", &self.capture_count)
      .field("max_slots", &self.max_slot)
      .field("module_id", &self.module_id)
      .field("module", &DebugWrap(&self.module, depth))
      .field("try_blocks", &DebugWrap(&self.try_blocks, depth))
      .field("chunk", &DebugWrap(&self.chunk, depth))
      .finish()
  }
}

impl Object for Fun {
  fn kind(&self) -> ObjectKind {
    ObjectKind::Fun
  }
}
