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

  let mut label_offsets: collections::Vec<usize> = bumpalo::vec![in &alloc; 0; label_count];

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

