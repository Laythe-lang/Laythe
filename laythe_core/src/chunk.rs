use crate::hooks::GcHooks;
use crate::managed::{Array, DebugWrap};
use crate::{managed::DebugHeap, managed::Trace, value::Value};

/// An immutable chunk of code
#[derive(Clone, PartialEq, Eq)]
pub struct Chunk {
  /// instruction in this code chunk
  instructions: Array<u8>,

  /// constants in this code chunk
  constants: Array<Value>,

  /// debug line information
  lines: Array<u16>,
}

impl Chunk {
  /// Create a new chunk
  pub fn new(instructions: Array<u8>, constants: Array<Value>, lines: Array<u16>) -> Self {
    Self {
      instructions,
      constants,
      lines,
    }
  }

  /// Create a stubbed chunk
  pub fn stub(hooks: &GcHooks) -> Self {
    Self::stub_with_instructions(hooks, &[0])
  }

  /// Create a stubbed chunk
  pub fn stub_with_instructions(hooks: &GcHooks, instructions: &[u8]) -> Self {
    Self {
      instructions: hooks.manage::<_, &[u8]>(instructions),
      constants: hooks.manage::<_, &[Value]>(&[]),
      lines: hooks.manage::<_, &[u16]>(&[0]),
    }
  }

  /// instruction in this code chunk
  #[inline]
  pub fn instructions(&self) -> &[u8] {
    &self.instructions
  }

  /// Retrieve a constant in the constants table at
  /// the provided offset
  #[inline]
  pub fn get_constant(&self, offset: usize) -> Value {
    self.constants[offset]
  }

  /// Retrieve a constant in the constants table at
  /// the provided offset without bounds checks
  ///
  /// # Safety
  /// This method assumes the index comes from a trusted
  /// source that is inbounds.
  #[inline]
  pub unsafe fn get_constant_unchecked(&self, offset: usize) -> Value {
    *self.constants.get_unchecked(offset)
  }

  /// Get the line number at a token offset
  ///
  /// # Panics
  ///
  /// This method panics if an offset is past the last instruction
  ///
  /// ```rust
  /// use laythe_core::chunk::Chunk;
  /// use laythe_core::hooks::{NoContext, GcHooks};
  ///
  /// let mut context = NoContext::default();
  /// let hooks = GcHooks::new(&mut context);
  ///
  /// let chunk = Chunk::stub(&hooks);
  /// chunk.get_line(0);
  /// ```
  pub fn get_line(&self, offset: usize) -> u16 {
    if offset == self.lines.len() {
      self.lines[offset - 1]
    } else {
      self.lines[offset]
    }
  }
}

impl Trace for Chunk {
  #[inline]
  fn trace(&self) {
    self.instructions.trace();
    self.constants.trace();
    self.lines.trace();
  }

  fn trace_debug(&self, log: &mut dyn std::io::Write) {
    self.instructions.trace_debug(log);
    self.constants.trace_debug(log);
    self.lines.trace_debug(log);
  }
}

impl DebugHeap for Chunk {
  fn fmt_heap(&self, f: &mut std::fmt::Formatter, depth: usize) -> std::fmt::Result {
    f.debug_struct("Chunk")
      .field("instructions", &DebugWrap(&self.instructions, depth))
      .field("constants", &DebugWrap(&self.constants, depth))
      .field("lines", &DebugWrap(&self.lines, depth))
      .finish()
  }
}

#[cfg(test)]
mod test {
  #[cfg(test)]
  mod chunk {
    use crate::{
      chunk::Chunk,
      hooks::{GcHooks, NoContext},
      value::Value,
    };

    #[test]
    fn get_line() {
      let mut context = NoContext::default();
      let hooks = GcHooks::new(&mut context);

      let chunk = Chunk::new(
        hooks.manage::<_, &[u8]>(&[0]),
        hooks.manage::<_, &[Value]>(&[]),
        hooks.manage::<_, &[u16]>(&[5]),
      );

      assert_eq!(chunk.get_line(0), 5);
    }
  }
}
