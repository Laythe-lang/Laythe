use crate::{
  byte_code::SymbolicByteCode, cache::CacheIdEmitter, chunk_builder::ChunkBuilder, source::VmFileId,
};
use bumpalo::{collections, Bump};
use codespan_reporting::diagnostic::Diagnostic;
use laythe_core::{
  chunk::Chunk,
  hooks::GcHooks,
  object::{Fun, FunBuilder},
};
use std::{cell::RefCell, rc::Rc};

pub fn peephole_compile(
  hooks: &GcHooks,
  fun_builder: FunBuilder,
  chunk_builder: ChunkBuilder,
  alloc: &Bump,
  cache_id_emitter: Rc<RefCell<CacheIdEmitter>>,
) -> Result<Fun, Vec<Diagnostic<VmFileId>>> {
  let (instructions, constants, mut lines) = chunk_builder.take();

  let instructions = peephole_optimize(instructions);

  let label_count = label_count(&instructions);

  let mut label_offsets: collections::Vec<usize> = bumpalo::vec![in alloc; 0; label_count];

  if label_count > u16::MAX as usize {
    todo!("Really handle this");
  }

  compute_label_offsets(&instructions, &mut label_offsets[..label_count]);

  let mut buffer = collections::Vec::with_capacity_in(instructions.len(), alloc);
  let mut offset: usize = 0;

  let mut lines_iter = lines.iter_mut();
  let mut line_option = lines_iter.next();
  let mut errors = vec![];

  for (index, instruction) in instructions.iter().enumerate() {
    if let Some(error) = instruction.encode(
      &mut buffer,
      &label_offsets[..label_count],
      Rc::clone(&cache_id_emitter),
      offset,
    ) {
      errors.push(error);
    }
    offset += instruction.len();

    if let Some(line) = &mut line_option {
      if index + 1 == line.offset as usize {
        line.offset = offset as u32;
        line_option = lines_iter.next();
      }
    }
  }

  if errors.is_empty() {
    let instructions = hooks.manage(&*buffer);
    hooks.push_root(instructions);
    let constants = hooks.manage(&*constants);
    hooks.push_root(constants);
    let lines = hooks.manage(&*lines);

    Ok(fun_builder.build(Chunk::new(instructions, constants, lines)))
  } else {
    Err(errors)
  }
}

fn peephole_optimize(mut instructions: Vec<SymbolicByteCode>) -> Vec<SymbolicByteCode> {
  let mut reader: usize = 0;
  let mut writer: usize = 0;

  while reader < instructions.len() {
    match &instructions[reader..] {
      [SymbolicByteCode::Drop, SymbolicByteCode::Drop, ..] => {
        drop(&mut instructions, &mut reader, &mut writer)
      },
      [SymbolicByteCode::GetProperty(slot), SymbolicByteCode::PropertySlot, SymbolicByteCode::Call(args), ..] =>
      {
        let slot = *slot;
        let args = *args;

        invoke(&mut instructions, &mut reader, &mut writer, slot, args)
      },
      [SymbolicByteCode::GetSuper(slot), SymbolicByteCode::PropertySlot, SymbolicByteCode::Call(args), ..] =>
      {
        let slot = *slot;
        let args = *args;

        invoke_super(&mut instructions, &mut reader, &mut writer, slot, args)
      },
      [SymbolicByteCode::ArgumentDelimiter, ..] => {
        reader += 1;
      },
      _ => {
        instructions[writer] = instructions[reader];
        reader += 1;
        writer += 1;
      },
    }
  }

  instructions.truncate(writer);
  instructions
}

pub fn drop(instructions: &mut [SymbolicByteCode], reader: &mut usize, writer: &mut usize) {
  let mut drop_count: u8 = 1;
  let mut local_reader = *reader;

  while local_reader + 1 < instructions.len()
    && instructions[local_reader + 1] == SymbolicByteCode::Drop
  {
    local_reader += 1;
    drop_count += 1;
  }

  instructions[*writer] = if drop_count == 1 {
    SymbolicByteCode::Drop
  } else {
    SymbolicByteCode::DropN(drop_count)
  };
  *writer += 1;
  *reader = local_reader + 1;
}

pub fn invoke(
  instructions: &mut [SymbolicByteCode],
  reader: &mut usize,
  writer: &mut usize,
  slot: u16,
  args: u8,
) {
  instructions[*writer] = SymbolicByteCode::Invoke((slot, args));
  instructions[*writer + 1] = SymbolicByteCode::InvokeSlot;

  *writer += 2;
  *reader += 3;
}

pub fn invoke_super(
  instructions: &mut [SymbolicByteCode],
  reader: &mut usize,
  writer: &mut usize,
  slot: u16,
  args: u8,
) {
  instructions[*writer] = SymbolicByteCode::SuperInvoke((slot, args));
  instructions[*writer + 1] = SymbolicByteCode::InvokeSlot;

  *writer += 2;
  *reader += 3;
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
      label_offsets[label.val() as usize] = offset;
    }

    offset += instruction.len()
  }
}

#[cfg(test)]
mod test {
  use super::*;

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
    use super::*;

    #[test]
    fn drop_replacement() {
      let mut instructions = [
        SymbolicByteCode::Drop,
        SymbolicByteCode::Drop,
        SymbolicByteCode::Nil,
        SymbolicByteCode::Drop,
        SymbolicByteCode::Drop,
        SymbolicByteCode::Drop,
      ];
      let mut reader = 0;
      let mut writer = 0;

      drop(&mut instructions, &mut reader, &mut writer);
      assert_eq!(instructions[0], SymbolicByteCode::DropN(2));
      assert_eq!(reader, 2);
      assert_eq!(writer, 1);

      reader = 3;
      writer = 1;

      drop(&mut instructions, &mut reader, &mut writer);
      assert_eq!(instructions[1], SymbolicByteCode::DropN(3));
      assert_eq!(reader, 6);
      assert_eq!(writer, 2);
    }

    #[test]
    fn invoke_replacement() {
      let slot = 3;
      let args = 2;

      let mut instructions = [
        SymbolicByteCode::GetProperty(slot),
        SymbolicByteCode::PropertySlot,
        SymbolicByteCode::Call(args),
      ];
      let mut reader = 0;
      let mut writer = 0;

      invoke(&mut instructions, &mut reader, &mut writer, slot, args);
      assert_eq!(instructions[0], SymbolicByteCode::Invoke((slot, args)));
      assert_eq!(instructions[1], SymbolicByteCode::InvokeSlot);
      assert_eq!(reader, 3);
      assert_eq!(writer, 2);
    }

    #[test]
    fn super_invoke_replacement() {
      let slot = 3;
      let args = 2;

      let mut instructions = [
        SymbolicByteCode::GetSuper(slot),
        SymbolicByteCode::PropertySlot,
        SymbolicByteCode::Call(args),
      ];
      let mut reader = 0;
      let mut writer = 0;

      invoke_super(&mut instructions, &mut reader, &mut writer, slot, args);
      assert_eq!(instructions[0], SymbolicByteCode::SuperInvoke((slot, args)));
      assert_eq!(instructions[1], SymbolicByteCode::InvokeSlot);
      assert_eq!(reader, 3);
      assert_eq!(writer, 2);
    }
  }

  mod optimize {
    use super::*;

    #[test]
    fn optimize_code() {
      let mut instructions = vec![
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
      ];

      instructions = peephole_optimize(instructions);

      assert_eq!(instructions[0], SymbolicByteCode::DropN(2));
      assert_eq!(instructions[1], SymbolicByteCode::Invoke((3, 1)));
      assert_eq!(instructions[2], SymbolicByteCode::InvokeSlot);
      assert_eq!(instructions[3], SymbolicByteCode::Nil);
      assert_eq!(instructions[4], SymbolicByteCode::DropN(3));
      assert_eq!(instructions[5], SymbolicByteCode::SuperInvoke((1, 2)));
      assert_eq!(instructions[6], SymbolicByteCode::InvokeSlot);
    }
  }
}
