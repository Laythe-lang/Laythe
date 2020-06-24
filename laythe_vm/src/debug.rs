use crate::call_frame::CallFrame;
use laythe_core::chunk::{decode_u16, AlignedByteCode, Chunk, UpvalueIndex};
use laythe_env::stdio::StdIo;
use std::mem;

/// Indicate where and how an exception was caught
pub fn exception_catch<S: StdIo>(stdio: &S, frame: &CallFrame, idx: usize) {
  stdio.println(&format!(
    "Exception popped {:0>4} frames caught by: {}",
    idx, frame.closure.fun.name
  ));
}

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
  ip: usize,
  last_offset: usize,
) -> usize {
  stdio.print(&format!("{:0>4} ", ip));

  if ip > 0 && chunk.get_line(ip) == chunk.get_line(last_offset) {
    stdio.print("   | ")
  } else {
    stdio.print(&format!("{:>4} ", chunk.get_line(ip)))
  }

  let (instruction, offset) = AlignedByteCode::decode(&chunk.instructions, ip as usize);
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
    AlignedByteCode::Map => simple_instruction(stdio, "Map", offset),
    AlignedByteCode::MapInit(arg_count) => short_instruction(stdio, "MapInit", arg_count, offset),
    AlignedByteCode::IterNext(constant) => {
      invoke_instruction(stdio, "IterNext", chunk, constant, 0, offset)
    }
    AlignedByteCode::IterCurrent(constant) => {
      constant_instruction(stdio, "IterCurrent", chunk, constant, offset)
    }
    AlignedByteCode::GetIndex => simple_instruction(stdio, "GetIndex", offset),
    AlignedByteCode::SetIndex => simple_instruction(stdio, "SetIndex", offset),
    AlignedByteCode::Drop => simple_instruction(stdio, "Drop", offset),
    AlignedByteCode::Call(arg_count) => byte_instruction(stdio, "Call", arg_count, offset),
    AlignedByteCode::Import(path) => constant_instruction(stdio, "Import", chunk, path, offset),
    AlignedByteCode::Export(constant) => {
      constant_instruction(stdio, "Export", chunk, constant, offset)
    }
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
    AlignedByteCode::SetProperty(slot) => {
      constant_instruction(stdio, "SetProperty", chunk, slot, offset)
    }
    AlignedByteCode::GetProperty(slot) => {
      constant_instruction(stdio, "GetProperty", chunk, slot, offset)
    }
    AlignedByteCode::Jump(jump) => jump_instruction(stdio, "Jump", 1, jump, offset),
    AlignedByteCode::JumpIfFalse(jump) => jump_instruction(stdio, "JumpIfFalse", 1, jump, offset),
    AlignedByteCode::Loop(jump) => jump_instruction(stdio, "Loop", -1, jump, offset),
    AlignedByteCode::Equal => simple_instruction(stdio, "Equal", offset),
    AlignedByteCode::Greater => simple_instruction(stdio, "Greater", offset),
    AlignedByteCode::Less => simple_instruction(stdio, "Less", offset),
    AlignedByteCode::Constant(constant) => {
      constant_instruction(stdio, "Constant", chunk, constant as u16, offset)
    }
    AlignedByteCode::ConstantLong(constant) => {
      constant_instruction(stdio, "ConstantLong", chunk, constant, offset)
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
    "{:16} {:5} -> {}",
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
  constant: u16,
  offset: usize,
) -> usize {
  stdio.print(&format!("{:16} {:5} ", name, constant));
  stdio.println(&format!("{}", &chunk.constants[constant as usize]));
  offset
}

/// print a closure
fn closure_instruction(
  stdio: &impl StdIo,
  name: &str,
  chunk: &Chunk,
  constant: u16,
  offset: usize,
) -> usize {
  stdio.print(&format!("{:16} {:5} ", name, constant));
  stdio.println(&format!("{}", &chunk.constants[constant as usize]));

  let value = &chunk.constants[constant as usize];
  let upvalue_count = if value.is_fun() {
    value.to_fun().upvalue_count
  } else {
    stdio.eprintln(&format!(
      "!=== Compilation failure found {} instead of function ===!",
      value.value_type()
    ));
    0
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
  constant: u16,
  arg_count: u8,
  offset: usize,
) -> usize {
  stdio.print(&format!("{:16} {:5} ({} args) ", name, arg_count, constant));
  stdio.println(&format!("{}", &chunk.constants[constant as usize]));
  offset
}

/// print a short instruction
fn short_instruction(stdio: &impl StdIo, name: &str, slot: u16, offset: usize) -> usize {
  stdio.println(&format!("{:16} {:5}", name, slot));
  offset
}

/// print a byte instruction
fn byte_instruction(stdio: &impl StdIo, name: &str, slot: u8, offset: usize) -> usize {
  stdio.println(&format!("{:16} {:5}", name, slot));
  offset
}

/// print a simple instruction
fn simple_instruction(stdio: &impl StdIo, name: &str, offset: usize) -> usize {
  stdio.println(&format!("{:16}", name));
  offset
}
