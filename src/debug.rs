use crate::chunk::{ByteCode, Chunk};

/// Write a chunk to console
pub fn disassemble_chunk(code_chunk: &Chunk, name: &str) {
  println!("== {0} ==", name);

  for offset in 0..code_chunk.instructions.len() {
    disassemble_instruction(code_chunk, offset)
  }
}

/// Write an instruction to console
pub fn disassemble_instruction(chunk: &Chunk, offset: usize) {
  print!("{:0>4} ", offset);

  if offset > 0 && chunk.get_line(offset) == chunk.get_line(offset - 1) {
    print!("   | ")
  } else {
    print!("{:>4} ", chunk.get_line(offset))
  }

  let instruction = &chunk.instructions[offset];
  match instruction {
    ByteCode::Return => simple_instruction("Return"),
    ByteCode::Print => simple_instruction("Print"),
    ByteCode::Negate => simple_instruction("Negate"),
    ByteCode::Add => simple_instruction("Add"),
    ByteCode::Subtract => simple_instruction("Subtract"),
    ByteCode::Multiply => simple_instruction("Multiply"),
    ByteCode::Divide => simple_instruction("Divide"),
    ByteCode::Not => simple_instruction("Not"),
    ByteCode::Nil => simple_instruction("Nil"),
    ByteCode::True => simple_instruction("True"),
    ByteCode::False => simple_instruction("False"),
    ByteCode::Pop => simple_instruction("Pop"),
    ByteCode::Call(arg_count) => byte_instruction("Call", *arg_count),
    ByteCode::DefineGlobal(constant) => constant_instruction("DefineGlobal", chunk, *constant),
    ByteCode::GetGlobal(constant) => constant_instruction("GetGlobal", chunk, *constant),
    ByteCode::SetGlobal(constant) => constant_instruction("SetGlobal", chunk, *constant),
    ByteCode::GetLocal(slot) => byte_instruction("GetLocal", *slot),
    ByteCode::SetLocal(slot) => byte_instruction("SetLocal", *slot),
    ByteCode::Jump(jump) => jump_instruction("Jump", 1, *jump, offset as isize),
    ByteCode::JumpIfFalse(jump) => jump_instruction("Jump", 1, *jump, offset as isize),
    ByteCode::Loop(jump) => jump_instruction("Loop", -1, *jump, offset as isize),
    ByteCode::Noop => simple_instruction("Noop"),
    ByteCode::Equal => simple_instruction("Equal"),
    ByteCode::Greater => simple_instruction("Greater"),
    ByteCode::Less => simple_instruction("Less"),
    ByteCode::Constant(constant) => constant_instruction("Constant", chunk, *constant),
  }
}

fn jump_instruction(name: &str, sign: isize, jump: u8, offset: isize) {
  let net_jump = sign * (jump as isize);
  println!("{} {:4} -> {}", name, offset, offset + 1 + net_jump);
}

/// print a constant
fn constant_instruction(name: &str, chunk: &Chunk, constant: u8) {
  print!("{} {:4} ", name, constant);
  print!("{}", &chunk.constants.values[constant as usize]);
  println!();
}

/// print a byte instruction
fn byte_instruction(name: &str, slot: u8) {
  println!("{} {:4} ", name, slot);
}

/// print a simple instruction
fn simple_instruction(name: &str) {
  println!("{}", name);
}
