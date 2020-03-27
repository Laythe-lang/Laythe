use spacelox_core::chunk::{decode_u16, AlignedByteCode, Chunk, UpvalueIndex};
use spacelox_core::value::Value;
use std::mem;

/// Write a chunk to console
pub fn disassemble_chunk(code_chunk: &Chunk, name: &str) {
  println!("== {0} ==", name);

  let mut offset: usize = 0;
  let mut last_offset: usize = 0;

  while offset < code_chunk.instructions.len() {
    let temp = disassemble_instruction(code_chunk, offset, last_offset);
    last_offset = offset;
    offset = temp;
  }
}

/// Write an instruction to console
pub fn disassemble_instruction(chunk: &Chunk, offset: usize, last_offset: usize) -> usize {
  print!("{:0>4} ", offset);

  if offset > 0 && chunk.get_line(offset) == chunk.get_line(last_offset) {
    print!("   | ")
  } else {
    print!("{:>4} ", chunk.get_line(offset))
  }

  let (instruction, offset) = AlignedByteCode::decode(&chunk.instructions, offset);
  match instruction {
    AlignedByteCode::Return => simple_instruction("Return", offset),
    AlignedByteCode::Print => simple_instruction("Print", offset),
    AlignedByteCode::Negate => simple_instruction("Negate", offset),
    AlignedByteCode::Add => simple_instruction("Add", offset),
    AlignedByteCode::Subtract => simple_instruction("Subtract", offset),
    AlignedByteCode::Multiply => simple_instruction("Multiply", offset),
    AlignedByteCode::Divide => simple_instruction("Divide", offset),
    AlignedByteCode::Not => simple_instruction("Not", offset),
    AlignedByteCode::Nil => simple_instruction("Nil", offset),
    AlignedByteCode::True => simple_instruction("True", offset),
    AlignedByteCode::False => simple_instruction("False", offset),
    AlignedByteCode::Pop => simple_instruction("Pop", offset),
    AlignedByteCode::Call(arg_count) => byte_instruction("Call", arg_count, offset),
    AlignedByteCode::Invoke((constant, arg_count)) => {
      invoke_instruction("Invoke", chunk, constant, arg_count, offset)
    }
    AlignedByteCode::SuperInvoke((constant, arg_count)) => {
      invoke_instruction("SuperInvoke", chunk, constant, arg_count, offset)
    }
    AlignedByteCode::Class(constant) => constant_instruction("Class", chunk, constant, offset),
    AlignedByteCode::GetSuper(constant) => {
      constant_instruction("GetSuper", chunk, constant, offset)
    }
    AlignedByteCode::Inherit => simple_instruction("Inherit", offset),
    AlignedByteCode::Closure(constant) => closure_instruction("Closure", chunk, constant, offset),
    AlignedByteCode::Method(constant) => constant_instruction("Method", chunk, constant, offset),
    AlignedByteCode::CloseUpvalue => simple_instruction("CloseUpvalue", offset),
    AlignedByteCode::UpvalueIndex(_) => {
      simple_instruction("!=== UpValueIndex - Invalid ===!", offset)
    }
    AlignedByteCode::DefineGlobal(constant) => {
      constant_instruction("DefineGlobal", chunk, constant, offset)
    }
    AlignedByteCode::GetGlobal(constant) => {
      constant_instruction("GetGlobal", chunk, constant, offset)
    }
    AlignedByteCode::SetGlobal(constant) => {
      constant_instruction("SetGlobal", chunk, constant, offset)
    }
    AlignedByteCode::GetLocal(slot) => byte_instruction("GetLocal", slot, offset),
    AlignedByteCode::SetLocal(slot) => byte_instruction("SetLocal", slot, offset),
    AlignedByteCode::GetUpvalue(slot) => byte_instruction("GetUpvalue", slot, offset),
    AlignedByteCode::SetUpvalue(slot) => byte_instruction("SetUpvalue", slot, offset),
    AlignedByteCode::SetProperty(slot) => byte_instruction("GetProperty", slot, offset),
    AlignedByteCode::GetProperty(slot) => byte_instruction("SetProperty", slot, offset),
    AlignedByteCode::Jump(jump) => jump_instruction("Jump", 1, jump, offset),
    AlignedByteCode::JumpIfFalse(jump) => jump_instruction("JumpIfFalse", 1, jump, offset),
    AlignedByteCode::Loop(jump) => jump_instruction("Loop", -1, jump, offset),
    AlignedByteCode::Equal => simple_instruction("Equal", offset),
    AlignedByteCode::Greater => simple_instruction("Greater", offset),
    AlignedByteCode::Less => simple_instruction("Less", offset),
    AlignedByteCode::Constant(constant) => {
      constant_instruction("Constant", chunk, constant, offset)
    }
  }
}

fn jump_instruction(name: &str, sign: isize, jump: u16, offset: usize) -> usize {
  let net_jump = sign * (jump as isize);
  println!(
    "{:16} {:4} -> {}",
    name,
    offset - 3,
    (offset as isize) + net_jump
  );
  offset
}

/// print a constant
fn constant_instruction(name: &str, chunk: &Chunk, constant: u8, offset: usize) -> usize {
  print!("{:16} {:4} ", name, constant);
  print!("{}", &chunk.constants[constant as usize]);
  println!();
  offset
}

/// print a closure
fn closure_instruction(name: &str, chunk: &Chunk, constant: u8, offset: usize) -> usize {
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

  let mut current_offset = offset;
  for _ in 0..upvalue_count {
    let b1 = chunk.instructions[current_offset];
    let b2 = chunk.instructions[current_offset + 1];

    let upvalue_index: UpvalueIndex = unsafe { mem::transmute(decode_u16(b1, b2)) };

    match upvalue_index {
      UpvalueIndex::Local(local) => println!(
        "{:0>4}      |                     local {}",
        current_offset, local
      ),
      UpvalueIndex::Upvalue(upvalue) => println!(
        "{:0>4}      |                     upvalue {}",
        current_offset, upvalue
      ),
    }

    current_offset = current_offset + 2;
  }

  current_offset
}

fn invoke_instruction(
  name: &str,
  chunk: &Chunk,
  constant: u8,
  arg_count: u8,
  offset: usize,
) -> usize {
  print!("{:16} ({} args) {:4} ", name, arg_count, constant);
  print!("{}", &chunk.constants[constant as usize]);
  println!();
  offset
}

/// print a byte instruction
fn byte_instruction(name: &str, slot: u8, offset: usize) -> usize {
  println!("{:16} {:4} ", name, slot);
  offset
}

/// print a simple instruction
fn simple_instruction(name: &str, offset: usize) -> usize {
  println!("{:16}", name);
  offset
}
