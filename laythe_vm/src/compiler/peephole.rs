use crate::byte_code::SymbolicByteCode;

pub fn peephole(mut instructions: Vec<SymbolicByteCode>) -> Vec<SymbolicByteCode> {
  let mut reader: usize = 0;
  let mut writer: usize = 0;

  while reader < instructions.len() {
    match &instructions[reader..] {
      [SymbolicByteCode::Drop, SymbolicByteCode::Drop, ..]  => drop(&mut instructions, &mut reader, &mut writer),
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

  while instructions[local_reader + 1] == SymbolicByteCode::Drop {
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
