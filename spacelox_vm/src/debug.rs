#[cfg(feature = "debug")]
use spacelox_core::chunk::{ByteCode, Chunk, UpvalueIndex};

#[cfg(feature = "debug")]
use spacelox_core::value::Value;

/// Write a chunk to console
#[cfg(feature = "debug")]
pub fn disassemble_chunk(code_chunk: &Chunk, name: &str) {
  println!("== {0} ==", name);

  let mut skip_to: Option<usize> = None;

  for offset in 0..code_chunk.instructions.len() {
    if let Some(skip) = skip_to {
      if skip >= offset {
        continue;
      }
    }

    skip_to = disassemble_instruction(code_chunk, offset)
  }
}

/// Write an instruction to console
#[cfg(feature = "debug")]
pub fn disassemble_instruction(chunk: &Chunk, offset: usize) -> Option<usize> {
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
    ByteCode::Invoke((name, arg_count)) => invoke_instruction("Invoke", chunk, *name, *arg_count),
    ByteCode::Class(constant) => constant_instruction("Class", chunk, *constant),
    ByteCode::Closure(constant) => closure_instruction("Closure", chunk, *constant, offset),
    ByteCode::Method(constant) => constant_instruction("Method", chunk, *constant),
    ByteCode::CloseUpvalue => simple_instruction("CloseUpvalue"),
    ByteCode::UpvalueIndex(_) => simple_instruction("!=== UpValueIndex - Invalid ===!"),
    ByteCode::DefineGlobal(constant) => constant_instruction("DefineGlobal", chunk, *constant),
    ByteCode::GetGlobal(constant) => constant_instruction("GetGlobal", chunk, *constant),
    ByteCode::SetGlobal(constant) => constant_instruction("SetGlobal", chunk, *constant),
    ByteCode::GetLocal(slot) => byte_instruction("GetLocal", *slot),
    ByteCode::SetLocal(slot) => byte_instruction("SetLocal", *slot),
    ByteCode::GetUpvalue(slot) => byte_instruction("GetUpvalue", *slot),
    ByteCode::SetUpvalue(slot) => byte_instruction("SetUpvalue", *slot),
    ByteCode::SetProperty(slot) => byte_instruction("GetProperty", *slot),
    ByteCode::GetProperty(slot) => byte_instruction("SetProperty", *slot),
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

#[cfg(feature = "debug")]
fn jump_instruction(name: &str, sign: isize, jump: u16, offset: isize) -> Option<usize> {
  let net_jump = sign * (jump as isize);
  println!("{:16} {:4} -> {}", name, offset, offset + 1 + net_jump);
  None
}

/// print a constant
#[cfg(feature = "debug")]
fn constant_instruction(name: &str, chunk: &Chunk, constant: u8) -> Option<usize> {
  print!("{:16} {:4} ", name, constant);
  print!("{}", &chunk.constants[constant as usize]);
  println!();
  None
}

/// print a closure
#[cfg(feature = "debug")]
fn closure_instruction(name: &str, chunk: &Chunk, constant: u8, offset: usize) -> Option<usize> {
  print!("{:16} {:4} ", name, constant);
  print!("{}", &chunk.constants[constant as usize]);
  println!();

  let value = &chunk.constants[constant as usize];
  let upvalue_count = match value {
    Value::Fun(fun) => fun.upvalue_count,
    _ => {
      println!(
        "!=== Compilation failure found {} instead of function ===!",
        value.value_type()
      );
      0
    }
  };

  for i in 0..upvalue_count {
    let total_offset = offset + i + 1;
    let upvalue_index = &chunk.instructions[total_offset];

    if let ByteCode::UpvalueIndex(index) = upvalue_index {
      match index {
        UpvalueIndex::Local(local) => println!(
          "{:0>4}      |                     local {}",
          total_offset, local
        ),
        UpvalueIndex::Upvalue(upvalue) => println!(
          "{:0>4}      |                     upvalue {}",
          total_offset, upvalue
        ),
      }
    } else {
      println!("!=== Did not find upvalue ===!");
    }
  }

  Some(offset + upvalue_count)
}

#[cfg(feature = "debug")]
fn invoke_instruction(name: &str, chunk: &Chunk, constant: u8, arg_count: u8) -> Option<usize> {
  print!("{:16} ({} args) {:4} ", name, arg_count, constant);
  print!("{}", &chunk.constants[constant as usize]);
  println!();
  None
}

/// print a byte instruction
#[cfg(feature = "debug")]
fn byte_instruction(name: &str, slot: u8) -> Option<usize> {
  println!("{:16} {:4} ", name, slot);
  None
}

/// print a simple instruction
#[cfg(feature = "debug")]
fn simple_instruction(name: &str) -> Option<usize> {
  println!("{:16}", name);
  None
}
