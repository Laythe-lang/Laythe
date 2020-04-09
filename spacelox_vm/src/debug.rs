use spacelox_core::chunk::{decode_u16, AlignedByteCode, Chunk, UpvalueIndex};
use spacelox_core::io::StdIo;
use spacelox_core::value::Value;
use std::mem;

/// Write a chunk to console
pub fn disassemble_chunk<S: StdIo>(stdio: &S, code_chunk: &Chunk, name: &str) {
  stdio.println(&format!("== {0} ==", name));

  let mut offset: usize = 0;
  let mut last_offset: usize = 0;

  while offset < code_chunk.instructions.len() {
    let temp = disassemble_instruction(stdio, code_chunk, offset, last_offset);
    last_offset = offset;
    offset = temp;
  }
}

/// Write an instruction to console
pub fn disassemble_instruction<S: StdIo>(
  stdio: &S,
  chunk: &Chunk,
  offset: usize,
  last_offset: usize,
) -> usize {
  stdio.print(&format!("{:0>4} ", offset));

  if offset > 0 && chunk.get_line(offset) == chunk.get_line(last_offset) {
    stdio.print("   | ")
  } else {
    stdio.print(&format!("{:>4} ", chunk.get_line(offset)))
  }

  let (instruction, offset) = AlignedByteCode::decode(&chunk.instructions, offset as usize);
  match instruction {
    AlignedByteCode::Return => simple_instruction(stdio, "Return", offset),
    AlignedByteCode::Print => simple_instruction(stdio, "Print", offset),
    AlignedByteCode::Negate => simple_instruction(stdio, "Negate", offset),
    AlignedByteCode::Add => simple_instruction(stdio, "Add", offset),
    AlignedByteCode::Subtract => simple_instruction(stdio, "Subtract", offset),
    AlignedByteCode::Multiply => simple_instruction(stdio, "Multiply", offset),
    AlignedByteCode::Divide => simple_instruction(stdio, "Divide", offset),
    AlignedByteCode::Not => simple_instruction(stdio, "Not", offset),
    AlignedByteCode::Nil => simple_instruction(stdio, "Nil", offset),
    AlignedByteCode::True => simple_instruction(stdio, "True", offset),
    AlignedByteCode::False => simple_instruction(stdio, "False", offset),
    AlignedByteCode::List => simple_instruction(stdio, "List", offset),
    AlignedByteCode::ListInit(arg_count) => short_instruction(stdio, "ListInit", arg_count, offset),
    AlignedByteCode::Pop => simple_instruction(stdio, "Pop", offset),
    AlignedByteCode::Call(arg_count) => byte_instruction(stdio, "Call", arg_count, offset),
    AlignedByteCode::Invoke((constant, arg_count)) => {
      invoke_instruction(stdio, "Invoke", chunk, constant, arg_count, offset)
    }
    AlignedByteCode::SuperInvoke((constant, arg_count)) => {
      invoke_instruction(stdio, "SuperInvoke", chunk, constant, arg_count, offset)
    }
    AlignedByteCode::Class(constant) => {
      constant_instruction(stdio, "Class", chunk, constant, offset)
    }
    AlignedByteCode::GetSuper(constant) => {
      constant_instruction(stdio, "GetSuper", chunk, constant, offset)
    }
    AlignedByteCode::Inherit => simple_instruction(stdio, "Inherit", offset),
    AlignedByteCode::Closure(constant) => {
      closure_instruction(stdio, "Closure", chunk, constant, offset)
    }
    AlignedByteCode::Method(constant) => {
      constant_instruction(stdio, "Method", chunk, constant, offset)
    }
    AlignedByteCode::CloseUpvalue => simple_instruction(stdio, "CloseUpvalue", offset),
    AlignedByteCode::UpvalueIndex(_) => {
      simple_instruction(stdio, "!=== UpValueIndex - Invalid ===!", offset)
    }
    AlignedByteCode::DefineGlobal(constant) => {
      constant_instruction(stdio, "DefineGlobal", chunk, constant, offset)
    }
    AlignedByteCode::GetGlobal(constant) => {
      constant_instruction(stdio, "GetGlobal", chunk, constant, offset)
    }
    AlignedByteCode::SetGlobal(constant) => {
      constant_instruction(stdio, "SetGlobal", chunk, constant, offset)
    }
    AlignedByteCode::GetLocal(slot) => byte_instruction(stdio, "GetLocal", slot, offset),
    AlignedByteCode::SetLocal(slot) => byte_instruction(stdio, "SetLocal", slot, offset),
    AlignedByteCode::GetUpvalue(slot) => byte_instruction(stdio, "GetUpvalue", slot, offset),
    AlignedByteCode::SetUpvalue(slot) => byte_instruction(stdio, "SetUpvalue", slot, offset),
    AlignedByteCode::SetProperty(slot) => byte_instruction(stdio, "SetProperty", slot, offset),
    AlignedByteCode::GetProperty(slot) => byte_instruction(stdio, "GetProperty", slot, offset),
    AlignedByteCode::Jump(jump) => jump_instruction(stdio, "Jump", 1, jump, offset),
    AlignedByteCode::JumpIfFalse(jump) => jump_instruction(stdio, "JumpIfFalse", 1, jump, offset),
    AlignedByteCode::Loop(jump) => jump_instruction(stdio, "Loop", -1, jump, offset),
    AlignedByteCode::Equal => simple_instruction(stdio, "Equal", offset),
    AlignedByteCode::Greater => simple_instruction(stdio, "Greater", offset),
    AlignedByteCode::Less => simple_instruction(stdio, "Less", offset),
    AlignedByteCode::Constant(constant) => {
      constant_instruction(stdio, "Constant", chunk, constant, offset)
    }
  }
}

fn jump_instruction(
  stdio: &impl StdIo,
  name: &str,
  sign: isize,
  jump: u16,
  offset: usize,
) -> usize {
  let net_jump = sign * (jump as isize);
  stdio.println(&format!(
    "{:16} {:4} -> {}",
    name,
    offset - 3,
    (offset as isize) + net_jump
  ));
  offset
}

/// print a constant
fn constant_instruction(
  stdio: &impl StdIo,
  name: &str,
  chunk: &Chunk,
  constant: u8,
  offset: usize,
) -> usize {
  stdio.print(&format!("{:16} {:4} ", name, constant));
  stdio.println(&format!("{}", &chunk.constants[constant as usize]));
  offset
}

/// print a closure
fn closure_instruction(
  stdio: &impl StdIo,
  name: &str,
  chunk: &Chunk,
  constant: u8,
  offset: usize,
) -> usize {
  stdio.print(&format!("{:16} {:4} ", name, constant));
  stdio.println(&format!("{}", &chunk.constants[constant as usize]));

  let value = &chunk.constants[constant as usize];
  let upvalue_count = match value {
    Value::Fun(fun) => fun.upvalue_count,
    _ => {
      stdio.eprintln(&format!(
        "!=== Compilation failure found {} instead of function ===!",
        value.value_type()
      ));
      0
    }
  };

  let mut current_offset = offset;
  for _ in 0..upvalue_count {
    let upvalue_index: UpvalueIndex = unsafe {
      mem::transmute(decode_u16(
        &chunk.instructions[current_offset..current_offset + 2],
      ))
    };

    match upvalue_index {
      UpvalueIndex::Local(local) => stdio.println(&format!(
        "{:0>4}      |                     local {}",
        current_offset, local
      )),
      UpvalueIndex::Upvalue(upvalue) => stdio.println(&format!(
        "{:0>4}      |                     upvalue {}",
        current_offset, upvalue
      )),
    }

    current_offset += 2;
  }

  current_offset
}

fn invoke_instruction(
  stdio: &impl StdIo,
  name: &str,
  chunk: &Chunk,
  constant: u8,
  arg_count: u8,
  offset: usize,
) -> usize {
  stdio.print(&format!("{:16} ({} args) {:4} ", name, arg_count, constant));
  stdio.println(&format!("{}", &chunk.constants[constant as usize]));
  offset
}

/// print a short instruction
fn short_instruction(stdio: &impl StdIo, name: &str, slot: u16, offset: usize) -> usize {
  stdio.println(&format!("{:16} {:4} ", name, slot));
  offset
}

/// print a byte instruction
fn byte_instruction(stdio: &impl StdIo, name: &str, slot: u8, offset: usize) -> usize {
  stdio.println(&format!("{:16} {:4} ", name, slot));
  offset
}

/// print a simple instruction
fn simple_instruction(stdio: &impl StdIo, name: &str, offset: usize) -> usize {
  stdio.println(&format!("{:16}", name));
  offset
}
