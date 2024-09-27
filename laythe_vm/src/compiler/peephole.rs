use crate::{
  byte_code::{SymbolicByteCode, ByteCodeEncoder, EncodedChunk}, cache::CacheIdEmitter, chunk_builder::ChunkBuilder, source::VmFileId,
};
use bumpalo::{collections, Bump};
use codespan_reporting::diagnostic::Diagnostic;
use laythe_core::{
  chunk::Chunk,
  hooks::GcHooks,
  object::{Fun, FunBuilder},
};
use std::{cell::RefCell, rc::Rc};

struct VecCursor<T> {
  vec: Vec<T>,
  reader: usize,
  writer: usize,
}

impl<T> VecCursor<T> {
  pub fn new(vec: Vec<T>) -> Self {
    Self {
      vec,
      reader: 0,
      writer: 0,
    }
  }

  pub fn len(&self) -> usize {
    self.vec.len()
  }

  pub fn at_end(&self) -> bool {
    self.reader >= self.len()
  }

  pub fn read_slice(&self) -> &[T] {
    &self.vec[self.reader..]
  }

  pub fn write(&mut self, value: T) {
    debug_assert!(self.writer <= self.len());
    self.vec[self.writer] = value;
    self.inc_writer(1);
  }

  pub fn inc_reader(&mut self, len: usize) {
    self.reader += len;
  }

  pub fn inc_writer(&mut self, len: usize) {
    self.writer += len;
    debug_assert!(self.writer <= self.reader);
  }

  pub fn take(mut self) -> Vec<T> {
    self.vec.truncate(self.writer);
    self.vec
  }
}

impl<T: Copy> VecCursor<T> {
  pub fn read(&mut self) -> T {
    debug_assert!(self.reader <= self.len());
    let value = self.vec[self.reader];
    self.inc_reader(1);
    value
  }

  pub fn peek(&self) -> T {
    self.vec[self.reader]
  }

  pub fn peek_next(&self) -> Option<T> {
    if self.reader + 1 < self.len() {
      Some(self.vec[self.reader + 1])
    } else {
      None
    }
  }

  pub fn copy_cursors(&mut self) {
    let value = self.read();
    self.write(value);
  }
}

pub fn peephole_compile<'a>(
  hooks: &GcHooks,
  mut fun_builder: FunBuilder,
  chunk_builder: ChunkBuilder,
  alloc: &'a Bump,
  cache_id_emitter: Rc<RefCell<CacheIdEmitter>>,
) -> Result<Fun, collections::Vec<'a, Diagnostic<VmFileId>>> {
  let (instructions, constants, lines) = chunk_builder.take();

  let (mut instructions, lines) = peephole_optimize(instructions, lines);

  let label_count = label_count(&instructions);
  apply_stack_effects(&mut fun_builder, &mut instructions);

  let mut label_offsets: collections::Vec<usize> = bumpalo::vec![in alloc; 0; label_count];

  if label_count > u16::MAX as usize {
    todo!("Really handle this");
  }

  compute_label_offsets(&instructions, &mut label_offsets[..label_count]);

  let code_buffer = collections::Vec::with_capacity_in(instructions.len() * 2, alloc);
  let line_buffer = collections::Vec::with_capacity_in(instructions.len() * 2, alloc);
  let errors = collections::Vec::new_in(alloc);

  let encoder = ByteCodeEncoder::new(line_buffer, code_buffer, errors, cache_id_emitter);
  let EncodedChunk { code, lines } = encoder.encode(&instructions, &lines, &label_offsets[..label_count])?;

  let instructions = hooks.manage(&*code);
  hooks.push_root(instructions);
  let constants = hooks.manage(&*constants);
  hooks.push_root(constants);
  let lines = hooks.manage(&*lines);

  assert_eq!(lines.len(), instructions.len());

  Ok(fun_builder.build(Chunk::new(instructions, constants, lines)))
}

fn peephole_optimize(
  instructions: Vec<SymbolicByteCode>,
  lines: Vec<u16>,
) -> (Vec<SymbolicByteCode>, Vec<u16>) {
  let mut instructions_cursor = VecCursor::new(instructions);
  let mut lines_cursor = VecCursor::new(lines);

  while !instructions_cursor.at_end() {
    match instructions_cursor.read_slice() {
      [SymbolicByteCode::Drop, SymbolicByteCode::Drop, ..] => {
        drop(&mut instructions_cursor, &mut lines_cursor)
      },
      [SymbolicByteCode::GetProperty(slot), SymbolicByteCode::PropertySlot, SymbolicByteCode::Call(args), ..] =>
      {
        let slot = *slot;
        let args = *args;

        invoke(&mut instructions_cursor, &mut lines_cursor, slot, args)
      },
      [SymbolicByteCode::GetSuper(slot), SymbolicByteCode::PropertySlot, SymbolicByteCode::Call(args), ..] =>
      {
        let slot = *slot;
        let args = *args;

        invoke_super(&mut instructions_cursor, &mut lines_cursor, slot, args)
      },
      [SymbolicByteCode::Jump(_)
      | SymbolicByteCode::Loop(_)
      | SymbolicByteCode::Return
      | SymbolicByteCode::Raise, ..] => {
        remove_dead_code(&mut instructions_cursor, &mut lines_cursor)
      },
      [SymbolicByteCode::ArgumentDelimiter, ..] => {
        instructions_cursor.inc_reader(1);
        lines_cursor.inc_reader(1);
      },
      _ => {
        instructions_cursor.copy_cursors();
        lines_cursor.copy_cursors();
      },
    }
  }

  (instructions_cursor.take(), lines_cursor.take())
}

fn drop(instructions: &mut VecCursor<SymbolicByteCode>, lines: &mut VecCursor<u16>) {
  lines.copy_cursors();
  let mut drop_count: u8 = 1;

  while instructions.peek_next() == Some(SymbolicByteCode::Drop) {
    instructions.inc_reader(1);
    drop_count += 1;
  }

  instructions.write(if drop_count == 1 {
    SymbolicByteCode::Drop
  } else {
    SymbolicByteCode::DropN(drop_count)
  });

  instructions.inc_reader(1);
  lines.inc_reader(drop_count as usize - 1);
}

fn invoke(
  instructions: &mut VecCursor<SymbolicByteCode>,
  lines: &mut VecCursor<u16>,
  slot: u16,
  args: u8,
) {
  instructions.inc_reader(3);
  instructions.write(SymbolicByteCode::Invoke((slot, args)));
  instructions.write(SymbolicByteCode::InvokeSlot);

  let first_line = lines.read();
  lines.inc_reader(2);
  lines.write(first_line);
  lines.write(first_line);
}

fn invoke_super(
  instructions: &mut VecCursor<SymbolicByteCode>,
  lines: &mut VecCursor<u16>,
  slot: u16,
  args: u8,
) {
  instructions.inc_reader(3);
  instructions.write(SymbolicByteCode::SuperInvoke((slot, args)));
  instructions.write(SymbolicByteCode::InvokeSlot);

  let first_line = lines.read();
  lines.inc_reader(2);
  lines.write(first_line);
  lines.write(first_line);
}

fn remove_dead_code(instructions: &mut VecCursor<SymbolicByteCode>, lines: &mut VecCursor<u16>) {
  instructions.copy_cursors();
  lines.copy_cursors();

  while !instructions.at_end() {
    if let SymbolicByteCode::Label(_) = instructions.peek() {
      break;
    }

    instructions.inc_reader(1);
    lines.inc_reader(1);
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

fn apply_stack_effects(fun_builder: &mut FunBuilder, instructions: &mut [SymbolicByteCode]) {
  let mut slots: i32 = 1;

  for instruction in instructions {
    if let SymbolicByteCode::PushHandler((_, label)) = instruction {
      // TODO handle to many slots
      *instruction = SymbolicByteCode::PushHandler((slots as u16, *label))
    }

    slots += instruction.stack_effect();
    debug_assert!(slots >= 0);
    fun_builder.update_max_slots(slots);
  }
}

fn compute_label_offsets(instructions: &[SymbolicByteCode], label_offsets: &mut [usize]) {
  let mut offset: usize = 0;

  for instruction in instructions {
    if let SymbolicByteCode::Label(label) = instruction {
      label_offsets[label.val() as usize] = offset;
    }

    offset += instruction.len()
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod vec_cursor {
    use crate::compiler::peephole::VecCursor;

    #[test]
    fn new() {
      let cursor = VecCursor::<u8>::new(vec![0]);
      assert!(!cursor.at_end());
    }

    #[test]
    fn len() {
      let cursor = VecCursor::<u8>::new(vec![1, 2, 3]);
      assert_eq!(cursor.len(), 3);
    }

    #[test]
    fn at_end() {
      let mut cursor = VecCursor::<u8>::new(vec![1]);
      assert!(!cursor.at_end());

      cursor.read();
      assert!(cursor.at_end());
    }

    #[test]
    fn read_slice() {
      let mut cursor = VecCursor::<u8>::new(vec![1, 2]);
      assert_eq!(cursor.read_slice(), &[1, 2]);

      cursor.read();
      assert_eq!(cursor.read_slice(), &[2]);
    }

    #[test]
    fn write() {
      let mut cursor = VecCursor::<u8>::new(vec![1, 2]);

      cursor.inc_reader(2);
      cursor.write(3);
      cursor.write(4);

      assert_eq!(cursor.take(), vec![3, 4]);
    }

    #[test]
    fn inc_writer() {
      let mut cursor = VecCursor::<u8>::new(vec![1, 2]);

      cursor.inc_reader(2);
      cursor.inc_writer(1);
      cursor.write(3);

      assert_eq!(cursor.take(), vec![1, 3]);
    }

    #[test]
    fn inc_reader() {
      let mut cursor = VecCursor::<u8>::new(vec![1, 2]);

      cursor.inc_reader(1);

      assert_eq!(cursor.read(), 2);
    }

    #[test]
    fn take() {
      let mut cursor = VecCursor::<u8>::new(vec![1, 2]);

      cursor.inc_reader(2);
      cursor.inc_writer(2);

      assert_eq!(cursor.take(), vec![1, 2]);
    }

    #[test]
    fn read() {
      let mut cursor = VecCursor::<u8>::new(vec![1, 2]);

      assert_eq!(cursor.read(), 1);
      assert_eq!(cursor.read(), 2);
    }

    #[test]
    fn peek() {
      let mut cursor = VecCursor::<u8>::new(vec![1, 2]);

      assert_eq!(cursor.peek(), 1);
      cursor.inc_reader(1);
      assert_eq!(cursor.read(), 2);
    }

    #[test]
    fn peek_next() {
      let mut cursor = VecCursor::<u8>::new(vec![1, 2]);

      assert_eq!(cursor.peek_next(), Some(2));
      cursor.inc_reader(1);
      assert_eq!(cursor.peek_next(), None);
    }

    #[test]
    fn copy_cursors() {
      let mut cursor = VecCursor::<u8>::new(vec![1, 2]);
      cursor.inc_reader(1);

      cursor.copy_cursors();
      cursor.inc_writer(1);

      assert_eq!(cursor.take(), vec![2, 2]);
    }
  }

  mod labels {
    use super::*;
    use crate::byte_code::Label;

    #[test]
    fn count() {
      let instructions = [
        SymbolicByteCode::Label(Label::new(0)),
        SymbolicByteCode::Nil,
        SymbolicByteCode::GetCapture(2),
        SymbolicByteCode::Label(Label::new(1)),
      ];

      assert_eq!(label_count(&instructions), 2);
    }

    #[test]
    fn compute_offsets() {
      let instructions = [
        SymbolicByteCode::Label(Label::new(0)),
        SymbolicByteCode::Nil,
        SymbolicByteCode::GetCapture(2),
        SymbolicByteCode::Label(Label::new(1)),
      ];

      let mut label_offsets = [0, 0];

      compute_label_offsets(&instructions, &mut label_offsets);

      assert_eq!(label_offsets[0], 0);
      assert_eq!(label_offsets[1], 3);
    }

    #[test]
    #[should_panic]
    fn compute_offsets_missing_slots() {
      compute_label_offsets(&[SymbolicByteCode::Label(Label::new(0))], &mut []);
    }
  }

  mod replacements {
    use crate::byte_code::Label;

    use super::*;

    #[test]
    fn drop_replacement() {
      let mut instructions = VecCursor::new(vec![
        SymbolicByteCode::Drop,
        SymbolicByteCode::Drop,
        SymbolicByteCode::Drop,
      ]);
      let mut lines = VecCursor::new(vec![1, 2, 3]);

      drop(&mut instructions, &mut lines);

      let instructions = instructions.take();
      let lines = lines.take();

      assert_eq!(instructions.len(), 1);
      assert_eq!(instructions[0], SymbolicByteCode::DropN(3));

      assert_eq!(lines.len(), 1);
      assert_eq!(lines[0], 1);
    }

    #[test]
    fn invoke_replacement() {
      let slot = 3;
      let args = 2;

      let mut instructions = VecCursor::new(vec![
        SymbolicByteCode::GetProperty(slot),
        SymbolicByteCode::PropertySlot,
        SymbolicByteCode::Call(args),
      ]);
      let mut lines = VecCursor::new(vec![1, 2, 3]);

      invoke(
        &mut instructions,
        &mut lines,
        slot,
        args,
      );

      let instructions = instructions.take();
      let lines = lines.take();

      assert_eq!(instructions.len(), 2);
      assert_eq!(instructions[0], SymbolicByteCode::Invoke((slot, args)));
      assert_eq!(instructions[1], SymbolicByteCode::InvokeSlot);

      assert_eq!(lines.len(), 2);
      assert_eq!(lines[0], 1);
      assert_eq!(lines[1], 1);
    }

    #[test]
    fn super_invoke_replacement() {
      let slot = 3;
      let args = 2;

      let mut instructions = VecCursor::new(vec![
        SymbolicByteCode::GetSuper(slot),
        SymbolicByteCode::PropertySlot,
        SymbolicByteCode::Call(args),
      ]);
      let mut lines = VecCursor::new(vec![1, 2, 3]);

      invoke_super(
        &mut instructions,
        &mut lines,
        slot,
        args,
      );

      let instructions = instructions.take();
      let lines = lines.take();

      assert_eq!(instructions.len(), 2);
      assert_eq!(instructions[0], SymbolicByteCode::SuperInvoke((slot, args)));
      assert_eq!(instructions[1], SymbolicByteCode::InvokeSlot);

      assert_eq!(lines.len(), 2);
      assert_eq!(lines[0], 1);
      assert_eq!(lines[1], 1);
    }

    #[test]
    fn remove_dead_code_replacement() {
      let mut instructions = VecCursor::new(vec![
        SymbolicByteCode::Raise,
        SymbolicByteCode::Drop,
        SymbolicByteCode::Nil,
        SymbolicByteCode::Label(Label::new(0)),
        SymbolicByteCode::Constant(0),
      ]);
      let mut lines = VecCursor::new(vec![1, 2, 3, 4, 5]);

      remove_dead_code(&mut instructions, &mut lines);

      instructions.copy_cursors();
      instructions.copy_cursors();
      lines.copy_cursors();
      lines.copy_cursors();

      let instructions = instructions.take();
      let lines = lines.take();

      assert_eq!(instructions.len(), 3);
      assert_eq!(instructions[0], SymbolicByteCode::Raise);
      assert_eq!(instructions[1], SymbolicByteCode::Label(Label::new(0)));
      assert_eq!(instructions[2], SymbolicByteCode::Constant(0));

      assert_eq!(lines.len(), 3);
      assert_eq!(lines[0], 1);
      assert_eq!(lines[1], 4);
      assert_eq!(lines[2], 5);
    }
  }

  mod stack_effects {
    use laythe_core::{hooks::NoContext, signature::Arity, support::test_module};

    use crate::byte_code::Label;

    use super::*;

    #[test]
    fn calculate_func_max_stack() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let module = test_module(&hooks, "test module");
      let mut builder = FunBuilder::new(hooks.manage_str("test"), module, Arity::default());

      let mut instructions = vec![
        SymbolicByteCode::Constant(1),
        SymbolicByteCode::Constant(1),
        SymbolicByteCode::Constant(1),
        SymbolicByteCode::DropN(2),
        SymbolicByteCode::Constant(1),
        SymbolicByteCode::Constant(1),
      ];

      apply_stack_effects(&mut builder, &mut instructions);

      assert_eq!(instructions.len(), 6);
      assert_eq!(instructions[0], SymbolicByteCode::Constant(1));
      assert_eq!(instructions[1], SymbolicByteCode::Constant(1));
      assert_eq!(instructions[2], SymbolicByteCode::Constant(1));
      assert_eq!(instructions[3], SymbolicByteCode::DropN(2));
      assert_eq!(instructions[4], SymbolicByteCode::Constant(1));
      assert_eq!(instructions[4], SymbolicByteCode::Constant(1));

      let chunk = Chunk::stub(&hooks);
      let fun = builder.build(chunk);
      assert_eq!(fun.max_slots(), 4);
    }

    #[test]
    fn calculate_push_handler() {
      let context = NoContext::default();
      let hooks = GcHooks::new(&context);

      let module = test_module(&hooks, "test module");
      let mut builder = FunBuilder::new(hooks.manage_str("test"), module, Arity::default());

      let mut instructions = vec![
        SymbolicByteCode::Constant(1),
        SymbolicByteCode::Constant(1),
        SymbolicByteCode::Constant(1),
        SymbolicByteCode::PushHandler((0, Label::new(0)))
      ];

      apply_stack_effects(&mut builder, &mut instructions);

      assert_eq!(instructions.len(), 4);
      assert_eq!(instructions[0], SymbolicByteCode::Constant(1));
      assert_eq!(instructions[1], SymbolicByteCode::Constant(1));
      assert_eq!(instructions[2], SymbolicByteCode::Constant(1));
      assert_eq!(instructions[3], SymbolicByteCode::PushHandler((4, Label::new(0))));
    }
  }

  mod optimize {
    use super::*;

    #[test]
    fn optimize_code() {
      let instructions = vec![
        SymbolicByteCode::Drop,
        SymbolicByteCode::Drop,
        SymbolicByteCode::GetProperty(3),
        SymbolicByteCode::PropertySlot,
        SymbolicByteCode::Call(1),
        SymbolicByteCode::Nil,
        SymbolicByteCode::Drop,
        SymbolicByteCode::Drop,
        SymbolicByteCode::Drop,
        SymbolicByteCode::GetSuper(1),
        SymbolicByteCode::PropertySlot,
        SymbolicByteCode::Call(2),
        SymbolicByteCode::Raise,
        SymbolicByteCode::False,
      ];
      let lines = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14];

      let (instructions, lines) = peephole_optimize(instructions, lines);

      assert_eq!(instructions.len(), 8);
      assert_eq!(instructions[0], SymbolicByteCode::DropN(2));
      assert_eq!(instructions[1], SymbolicByteCode::Invoke((3, 1)));
      assert_eq!(instructions[2], SymbolicByteCode::InvokeSlot);
      assert_eq!(instructions[3], SymbolicByteCode::Nil);
      assert_eq!(instructions[4], SymbolicByteCode::DropN(3));
      assert_eq!(instructions[5], SymbolicByteCode::SuperInvoke((1, 2)));
      assert_eq!(instructions[6], SymbolicByteCode::InvokeSlot);
      assert_eq!(instructions[7], SymbolicByteCode::Raise);

      assert_eq!(lines.len(), 8);
      assert_eq!(lines[0], 1);
      assert_eq!(lines[1], 3);
      assert_eq!(lines[2], 3);
      assert_eq!(lines[3], 6);
      assert_eq!(lines[4], 7);
      assert_eq!(lines[5], 10);
      assert_eq!(lines[6], 10);
      assert_eq!(lines[7], 13);
    }
  }
}
