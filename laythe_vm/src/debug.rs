use laythe_core::{
  chunk::{Chunk, ChunkBuilder},
  if_let_obj,
  object::ObjectKind,
  to_obj_kind,
  value::Value,
};
use laythe_env::stdio::Stdio;
use std::{io, io::Write, mem};

use crate::byte_code::{
  decode_u16, decode_u32, AlignedByteCode, CaptureIndex, Label, SymbolicByteCode,
};

pub fn print_symbolic_code(
  stdio: &mut Stdio,
  chunk_builder: &ChunkBuilder<SymbolicByteCode>,
  name: &str,
) -> io::Result<()> {
  let stdout = stdio.stdout();
  writeln!(stdout)?;
  writeln!(stdout, "{0}", name)?;

  let mut offset: usize = 0;
  let mut last_index: usize = 0;

  while offset < chunk_builder.instructions().len() {
    let show_line = chunk_builder.get_line(offset) == chunk_builder.get_line(last_index);
    let temp = print_byte_code(stdio, chunk_builder, offset, show_line);
    last_index = offset;
    offset = temp?;
  }

  Ok(())
}

// /// Write an instruction to console
pub fn print_byte_code(
  stdio: &mut Stdio,
  chunk: &ChunkBuilder<SymbolicByteCode>,
  offset: usize,
  show_line: bool,
) -> io::Result<usize> {
  let stdout = stdio.stdout();
  let instruction = chunk.instructions()[offset];
  let offset = offset + 1;

  if let SymbolicByteCode::Label(label) = instruction {
    return label_instruction(stdio.stdout(), label, offset);
  }

  write!(stdout, "  {:0>4} ", offset)?;

  if offset != 0 && show_line {
    write!(stdout, "   | ")?;
  } else {
    write!(stdout, "{:>4} ", chunk.get_line(offset))?;
  }

  match instruction {
    SymbolicByteCode::Return => simple_instruction(stdio.stdout(), "Return", offset),
    SymbolicByteCode::Negate => simple_instruction(stdio.stdout(), "Negate", offset),
    SymbolicByteCode::Add => simple_instruction(stdio.stdout(), "Add", offset),
    SymbolicByteCode::Subtract => simple_instruction(stdio.stdout(), "Subtract", offset),
    SymbolicByteCode::Multiply => simple_instruction(stdio.stdout(), "Multiply", offset),
    SymbolicByteCode::Divide => simple_instruction(stdio.stdout(), "Divide", offset),
    SymbolicByteCode::And(jump) => symbolic_jump_instruction(stdio.stdout(), "And", jump, offset),
    SymbolicByteCode::Or(jump) => symbolic_jump_instruction(stdio.stdout(), "Or", jump, offset),
    SymbolicByteCode::Not => simple_instruction(stdio.stdout(), "Not", offset),
    SymbolicByteCode::Nil => simple_instruction(stdio.stdout(), "Nil", offset),
    SymbolicByteCode::True => simple_instruction(stdio.stdout(), "True", offset),
    SymbolicByteCode::False => simple_instruction(stdio.stdout(), "False", offset),
    SymbolicByteCode::List(arg_count) => {
      short_instruction(stdio.stdout(), "List", arg_count, offset)
    },
    SymbolicByteCode::Tuple(arg_count) => {
      short_instruction(stdio.stdout(), "Tuple", arg_count, offset)
    },
    SymbolicByteCode::Map(arg_count) => short_instruction(stdio.stdout(), "Map", arg_count, offset),
    SymbolicByteCode::Launch(arg_count) => {
      byte_instruction(stdio.stdout(), "Launch", arg_count, offset)
    },
    SymbolicByteCode::Channel => simple_instruction(stdio.stdout(), "Channel", offset),
    SymbolicByteCode::BufferedChannel => {
      simple_instruction(stdio.stdout(), "BufferedChannel", offset)
    },
    SymbolicByteCode::Receive => simple_instruction(stdio.stdout(), "Receive", offset),
    SymbolicByteCode::Send => simple_instruction(stdio.stdout(), "Send", offset),
    SymbolicByteCode::Interpolate(arg_count) => {
      short_instruction(stdio.stdout(), "Interpolate", arg_count, offset)
    },
    SymbolicByteCode::IterNext(slot) => {
      symbolic_constant_instruction(stdio.stdout(), "IterNext", slot, chunk, offset)
    },
    SymbolicByteCode::IterCurrent(slot) => {
      symbolic_constant_instruction(stdio.stdout(), "IterCurrent", slot, chunk, offset)
    },
    SymbolicByteCode::Box(slot) => byte_instruction(stdio.stdout(), "Box", slot, offset),
    SymbolicByteCode::EmptyBox => simple_instruction(stdio.stdout(), "EmptyBox", offset),
    SymbolicByteCode::FillBox => simple_instruction(stdio.stdout(), "FillBox", offset),
    SymbolicByteCode::Drop => simple_instruction(stdio.stdout(), "Drop", offset),
    SymbolicByteCode::DropN(count) => byte_instruction(stdio.stdout(), "DropN", count, offset),
    SymbolicByteCode::Dup => simple_instruction(stdio.stdout(), "Dup", offset),
    SymbolicByteCode::Call(arg_count) => {
      byte_instruction(stdio.stdout(), "Call", arg_count, offset)
    },
    SymbolicByteCode::Import(slot) => {
      symbolic_constant_instruction(stdio.stdout(), "Import", slot, chunk, offset)
    },
    SymbolicByteCode::ImportSymbol((path, slot)) => symbolic_constant_pair_instruction(
      stdio.stdout(),
      "ImportSymbol",
      (path, slot),
      (
        chunk.get_constant(path as usize),
        chunk.get_constant(slot as usize),
      ),
      offset,
    ),
    SymbolicByteCode::Export(slot) => {
      symbolic_constant_instruction(stdio.stdout(), "Export", slot, chunk, offset)
    },
    SymbolicByteCode::Invoke((slot, arg_count)) => {
      symbolic_invoke_instruction(stdio.stdout(), "Invoke", slot, arg_count, chunk, offset)
    },
    SymbolicByteCode::SuperInvoke((slot, arg_count)) => symbolic_invoke_instruction(
      stdio.stdout(),
      "SuperInvoke",
      slot,
      arg_count,
      chunk,
      offset,
    ),
    SymbolicByteCode::Class(slot) => {
      symbolic_constant_instruction(stdio.stdout(), "Class", slot, chunk, offset)
    },
    SymbolicByteCode::Inherit => simple_instruction(stdio.stdout(), "Inherit", offset),
    SymbolicByteCode::GetSuper(slot) => {
      symbolic_constant_instruction(stdio.stdout(), "GetSuper", slot, chunk, offset)
    },
    SymbolicByteCode::Closure(constant) => {
      symbolic_closure_instruction(stdio, "Closure", chunk, constant, offset)
    },
    SymbolicByteCode::Method(slot) => {
      symbolic_constant_instruction(stdio.stdout(), "Method", slot, chunk, offset)
    },
    SymbolicByteCode::Field(slot) => {
      symbolic_constant_instruction(stdio.stdout(), "Field", slot, chunk, offset)
    },
    SymbolicByteCode::StaticMethod(slot) => {
      symbolic_constant_instruction(stdio.stdout(), "StaticMethod", slot, chunk, offset)
    },
    SymbolicByteCode::CaptureIndex(_) => {
      simple_instruction(stdio.stdout(), "!=== CaptureIndex - Invalid ===!", offset)
    },
    SymbolicByteCode::Slot(_) => {
      simple_instruction(stdio.stdout(), "!=== Slot - Invalid ===!", offset)
    },
    SymbolicByteCode::DefineGlobal(slot) => {
      symbolic_constant_instruction(stdio.stdout(), "DefineGlobal", slot, chunk, offset)
    },
    SymbolicByteCode::GetGlobal(slot) => {
      symbolic_constant_instruction(stdio.stdout(), "GetGlobal", slot, chunk, offset)
    },
    SymbolicByteCode::SetGlobal(slot) => {
      symbolic_constant_instruction(stdio.stdout(), "SetGlobal", slot, chunk, offset)
    },
    SymbolicByteCode::GetLocal(slot) => byte_instruction(stdio.stdout(), "GetLocal", slot, offset),
    SymbolicByteCode::SetLocal(slot) => byte_instruction(stdio.stdout(), "SetLocal", slot, offset),
    SymbolicByteCode::GetBox(slot) => byte_instruction(stdio.stdout(), "GetBox", slot, offset),
    SymbolicByteCode::SetBox(slot) => byte_instruction(stdio.stdout(), "SetBox", slot, offset),
    SymbolicByteCode::GetCapture(slot) => {
      byte_instruction(stdio.stdout(), "GetCapture", slot, offset)
    },
    SymbolicByteCode::SetCapture(slot) => {
      byte_instruction(stdio.stdout(), "SetCapture", slot, offset)
    },
    SymbolicByteCode::SetProperty(slot) => {
      symbolic_constant_instruction_with_slot(stdio.stdout(), "SetProperty", chunk, slot, offset)
    },
    SymbolicByteCode::GetProperty(slot) => {
      symbolic_constant_instruction_with_slot(stdio.stdout(), "GetProperty", chunk, slot, offset)
    },
    SymbolicByteCode::Jump(jump) => symbolic_jump_instruction(stdio.stdout(), "Jump", jump, offset),
    SymbolicByteCode::JumpIfFalse(jump) => {
      symbolic_jump_instruction(stdio.stdout(), "JumpIfFalse", jump, offset)
    },
    SymbolicByteCode::Loop(jump) => symbolic_jump_instruction(stdio.stdout(), "Loop", jump, offset),
    SymbolicByteCode::Equal => simple_instruction(stdio.stdout(), "Equal", offset),
    SymbolicByteCode::NotEqual => simple_instruction(stdio.stdout(), "NotEqual", offset),
    SymbolicByteCode::Greater => simple_instruction(stdio.stdout(), "Greater", offset),
    SymbolicByteCode::GreaterEqual => simple_instruction(stdio.stdout(), "GreaterEqual", offset),
    SymbolicByteCode::Less => simple_instruction(stdio.stdout(), "Less", offset),
    SymbolicByteCode::LessEqual => simple_instruction(stdio.stdout(), "LessEqual", offset),
    SymbolicByteCode::Constant(slot) => {
      symbolic_constant_instruction(stdio.stdout(), "Constant", slot as u16, chunk, offset)
    },
    SymbolicByteCode::ConstantLong(slot) => {
      symbolic_constant_instruction(stdio.stdout(), "ConstantLong", slot, chunk, offset)
    },
    SymbolicByteCode::Label(_) => unreachable!(),
  }
}

fn label_instruction(stdout: &mut dyn Write, label: Label, offset: usize) -> io::Result<usize> {
  writeln!(stdout, "L{}:", label)?;
  Ok(offset)
}

fn symbolic_jump_instruction(
  stdout: &mut dyn Write,
  name: &str,
  label: Label,
  offset: usize,
) -> io::Result<usize> {
  writeln!(stdout, "{:13} L{}:", name, label)?;
  Ok(offset)
}

/// print a constant
fn symbolic_constant_instruction(
  stdout: &mut dyn Write,
  name: &str,
  slot: u16,
  chunk: &ChunkBuilder<SymbolicByteCode>,
  offset: usize,
) -> io::Result<usize> {
  write!(stdout, "{:13} {:5} ", name, slot)?;
  writeln!(stdout, "{}", &chunk.get_constant(slot as usize))?;
  Ok(offset)
}

/// print a constant
fn symbolic_constant_pair_instruction(
  stdout: &mut dyn Write,
  name: &str,
  slots: (u16, u16),
  constants: (Value, Value),
  offset: usize,
) -> io::Result<usize> {
  write!(stdout, "{:13} {:5} {:5}", name, slots.0, slots.1)?;
  writeln!(stdout, "{} {}", &constants.0, &constants.1)?;
  Ok(offset)
}

// /// print a constant
fn symbolic_constant_instruction_with_slot(
  stdout: &mut dyn Write,
  name: &str,
  chunk: &ChunkBuilder<SymbolicByteCode>,
  constant: u16,
  offset: usize,
) -> io::Result<usize> {
  write!(stdout, "{:13} {:5} ", name, constant)?;
  write!(stdout, "{}", &chunk.get_constant(constant as usize))?;

  if let SymbolicByteCode::Slot(cache_slot) = chunk.instructions()[offset] {
    writeln!(stdout, " cache slot {}", &cache_slot)?;
  } else {
    panic!("Unexpected SymbolicByteCode following invoke")
  }

  Ok(offset + 1)
}

/// print a closure
fn symbolic_closure_instruction(
  stdio: &mut Stdio,
  name: &str,
  chunk: &ChunkBuilder<SymbolicByteCode>,
  constant: u16,
  offset: usize,
) -> io::Result<usize> {
  let stdout = stdio.stdout();

  write!(stdout, "{:13} {:5} ", name, constant)?;
  writeln!(stdout, "{}", &chunk.get_constant(constant as usize))?;

  let value = chunk.get_constant(constant as usize);

  let capture_count = if_let_obj!(ObjectKind::Fun(fun) = (value) {
    fun.capture_count()
  } else {
    let stderr = stdio.stderr();

    writeln!(
      stderr,
      "!=== Compilation failure found {} instead of function ===!",
      value.value_type()
    )?;
    panic!();
  });

  let mut current_offset = offset;
  for _ in 0..capture_count {
    if let SymbolicByteCode::CaptureIndex(capture_index) = chunk.instructions()[current_offset] {
      match capture_index {
        CaptureIndex::Local(local) => writeln!(
          stdout,
          "  {:0>4}      |                  local {}",
          current_offset, local
        ),
        CaptureIndex::Enclosing(capture) => writeln!(
          stdout,
          "  {:0>4}      |                  capture {}",
          current_offset, capture
        ),
      }?;

      current_offset += 1;
    }
  }

  Ok(current_offset)
}

fn symbolic_invoke_instruction(
  stdout: &mut dyn Write,
  name: &str,
  slot: u16,
  arg_count: u8,
  chunk: &ChunkBuilder<SymbolicByteCode>,
  offset: usize,
) -> io::Result<usize> {
  write!(stdout, "{:13} {:5} ({} args) ", name, slot, arg_count)?;
  write!(stdout, "{}", &chunk.get_constant(slot as usize))?;

  if let SymbolicByteCode::Slot(cache_slot) = chunk.instructions()[offset] {
    writeln!(stdout, " cache slot {}", &cache_slot)?;
  } else {
    panic!("Unexpected SymbolicByteCode following invoke")
  }

  Ok(offset + 1)
}

/// Write an instruction to console
pub fn disassemble_instruction(
  stdio: &mut Stdio,
  chunk: &Chunk,
  offset: usize,
  show_line: bool,
) -> io::Result<usize> {
  let stdout = stdio.stdout();
  write!(stdout, "  {:0>4} ", offset)?;

  if offset != 0 && show_line {
    write!(stdout, "   | ")?;
  } else {
    write!(stdout, "{:>4} ", chunk.get_line(offset))?;
  }

  let (instruction, offset) = AlignedByteCode::decode(chunk.instructions(), offset as usize);
  match instruction {
    AlignedByteCode::Return => simple_instruction(stdio.stdout(), "Return", offset),
    AlignedByteCode::Negate => simple_instruction(stdio.stdout(), "Negate", offset),
    AlignedByteCode::Add => simple_instruction(stdio.stdout(), "Add", offset),
    AlignedByteCode::Subtract => simple_instruction(stdio.stdout(), "Subtract", offset),
    AlignedByteCode::Multiply => simple_instruction(stdio.stdout(), "Multiply", offset),
    AlignedByteCode::Divide => simple_instruction(stdio.stdout(), "Divide", offset),
    AlignedByteCode::And(jump) => jump_instruction(stdio.stdout(), "And", 1, jump, offset),
    AlignedByteCode::Or(jump) => jump_instruction(stdio.stdout(), "Or", 1, jump, offset),
    AlignedByteCode::Not => simple_instruction(stdio.stdout(), "Not", offset),
    AlignedByteCode::Nil => simple_instruction(stdio.stdout(), "Nil", offset),
    AlignedByteCode::True => simple_instruction(stdio.stdout(), "True", offset),
    AlignedByteCode::False => simple_instruction(stdio.stdout(), "False", offset),
    AlignedByteCode::List(arg_count) => {
      short_instruction(stdio.stdout(), "List", arg_count, offset)
    },
    AlignedByteCode::Tuple(arg_count) => {
      short_instruction(stdio.stdout(), "Tuple", arg_count, offset)
    },
    AlignedByteCode::Map(arg_count) => short_instruction(stdio.stdout(), "Map", arg_count, offset),
    AlignedByteCode::Launch(arg_count) => {
      byte_instruction(stdio.stdout(), "Launch", arg_count, offset)
    },
    AlignedByteCode::Channel => simple_instruction(stdio.stdout(), "Channel", offset),
    AlignedByteCode::BufferedChannel => {
      simple_instruction(stdio.stdout(), "BufferedChannel", offset)
    },
    AlignedByteCode::Receive => simple_instruction(stdio.stdout(), "Receive", offset),
    AlignedByteCode::Send => simple_instruction(stdio.stdout(), "Send", offset),
    AlignedByteCode::Interpolate(arg_count) => {
      short_instruction(stdio.stdout(), "Interpolate", arg_count, offset)
    },
    AlignedByteCode::IterNext(constant) => {
      constant_instruction(stdio.stdout(), "IterNext", chunk, constant, offset)
    },
    AlignedByteCode::IterCurrent(constant) => {
      constant_instruction(stdio.stdout(), "IterCurrent", chunk, constant, offset)
    },
    AlignedByteCode::Box(slot) => byte_instruction(stdio.stdout(), "Box", slot, offset),
    AlignedByteCode::EmptyBox => simple_instruction(stdio.stdout(), "EmptyBox", offset),
    AlignedByteCode::FillBox => simple_instruction(stdio.stdout(), "FillBox", offset),
    AlignedByteCode::Drop => simple_instruction(stdio.stdout(), "Drop", offset),
    AlignedByteCode::DropN(count) => byte_instruction(stdio.stdout(), "DropN", count, offset),
    AlignedByteCode::Dup => simple_instruction(stdio.stdout(), "Dup", offset),
    AlignedByteCode::Call(arg_count) => byte_instruction(stdio.stdout(), "Call", arg_count, offset),
    AlignedByteCode::Import(path) => {
      constant_instruction(stdio.stdout(), "Import", chunk, path, offset)
    },
    AlignedByteCode::ImportSymbol((path, slot)) => {
      constant_pair_instruction(stdio.stdout(), "ImportSymbol", chunk, (path, slot), offset)
    },
    AlignedByteCode::Export(constant) => {
      constant_instruction(stdio.stdout(), "Export", chunk, constant, offset)
    },
    AlignedByteCode::Invoke((constant, arg_count)) => {
      invoke_instruction(stdio.stdout(), "Invoke", chunk, constant, arg_count, offset)
    },
    AlignedByteCode::SuperInvoke((constant, arg_count)) => invoke_instruction(
      stdio.stdout(),
      "SuperInvoke",
      chunk,
      constant,
      arg_count,
      offset,
    ),
    AlignedByteCode::Class(constant) => {
      constant_instruction(stdio.stdout(), "Class", chunk, constant, offset)
    },
    AlignedByteCode::Inherit => simple_instruction(stdio.stdout(), "Inherit", offset),
    AlignedByteCode::GetSuper(constant) => {
      constant_instruction(stdio.stdout(), "GetSuper", chunk, constant, offset)
    },
    AlignedByteCode::Closure(constant) => {
      closure_instruction(stdio, "Closure", chunk, constant, offset)
    },
    AlignedByteCode::Method(constant) => {
      constant_instruction(stdio.stdout(), "Method", chunk, constant, offset)
    },
    AlignedByteCode::Field(constant) => {
      constant_instruction(stdio.stdout(), "Field", chunk, constant, offset)
    },
    AlignedByteCode::StaticMethod(constant) => {
      constant_instruction(stdio.stdout(), "StaticMethod", chunk, constant, offset)
    },
    AlignedByteCode::DefineGlobal(constant) => {
      constant_instruction(stdio.stdout(), "DefineGlobal", chunk, constant, offset)
    },
    AlignedByteCode::GetGlobal(constant) => {
      constant_instruction(stdio.stdout(), "GetGlobal", chunk, constant, offset)
    },
    AlignedByteCode::SetGlobal(constant) => {
      constant_instruction(stdio.stdout(), "SetGlobal", chunk, constant, offset)
    },
    AlignedByteCode::GetLocal(slot) => byte_instruction(stdio.stdout(), "GetLocal", slot, offset),
    AlignedByteCode::SetLocal(slot) => byte_instruction(stdio.stdout(), "SetLocal", slot, offset),
    AlignedByteCode::GetBox(slot) => byte_instruction(stdio.stdout(), "GetBox", slot, offset),
    AlignedByteCode::SetBox(slot) => byte_instruction(stdio.stdout(), "SetBox", slot, offset),
    AlignedByteCode::GetCapture(slot) => {
      byte_instruction(stdio.stdout(), "GetCapture", slot, offset)
    },
    AlignedByteCode::SetCapture(slot) => {
      byte_instruction(stdio.stdout(), "SetCapture", slot, offset)
    },
    AlignedByteCode::SetProperty(slot) => {
      constant_instruction_with_slot(stdio.stdout(), "SetProperty", chunk, slot, offset)
    },
    AlignedByteCode::GetProperty(slot) => {
      constant_instruction_with_slot(stdio.stdout(), "GetProperty", chunk, slot, offset)
    },
    AlignedByteCode::Jump(jump) => jump_instruction(stdio.stdout(), "Jump", 1, jump, offset),
    AlignedByteCode::JumpIfFalse(jump) => {
      jump_instruction(stdio.stdout(), "JumpIfFalse", 1, jump, offset)
    },
    AlignedByteCode::Loop(jump) => jump_instruction(stdio.stdout(), "Loop", -1, jump, offset),
    AlignedByteCode::Equal => simple_instruction(stdio.stdout(), "Equal", offset),
    AlignedByteCode::NotEqual => simple_instruction(stdio.stdout(), "NotEqual", offset),
    AlignedByteCode::Greater => simple_instruction(stdio.stdout(), "Greater", offset),
    AlignedByteCode::GreaterEqual => simple_instruction(stdio.stdout(), "GreaterEqual", offset),
    AlignedByteCode::Less => simple_instruction(stdio.stdout(), "Less", offset),
    AlignedByteCode::LessEqual => simple_instruction(stdio.stdout(), "LessEqual", offset),
    AlignedByteCode::Constant(constant) => {
      constant_instruction(stdio.stdout(), "Constant", chunk, constant as u16, offset)
    },
    AlignedByteCode::ConstantLong(constant) => {
      constant_instruction(stdio.stdout(), "ConstantLong", chunk, constant, offset)
    },
  }
}

fn jump_instruction(
  stdout: &mut dyn Write,
  name: &str,
  sign: isize,
  jump: u16,
  offset: usize,
) -> io::Result<usize> {
  let net_jump = sign * (jump as isize);
  writeln!(
    stdout,
    "{:13} {:5} -> {}",
    name,
    offset - 3,
    (offset as isize) + net_jump
  )?;
  Ok(offset)
}

// /// print a constant
fn constant_instruction(
  stdout: &mut dyn Write,
  name: &str,
  chunk: &Chunk,
  constant: u16,
  offset: usize,
) -> io::Result<usize> {
  write!(stdout, "{:13} {:5} ", name, constant)?;
  writeln!(stdout, "{}", &chunk.get_constant(constant as usize))?;
  Ok(offset)
}

// /// print a constant
fn constant_pair_instruction(
  stdout: &mut dyn Write,
  name: &str,
  chunk: &Chunk,
  constants: (u16, u16),
  offset: usize,
) -> io::Result<usize> {
  write!(stdout, "{:13} {:5} {:5}", name, constants.0, constants.1)?;
  writeln!(
    stdout,
    "{} {}",
    &chunk.get_constant(constants.0 as usize),
    &chunk.get_constant(constants.1 as usize)
  )?;
  Ok(offset)
}

/// print a constant
fn constant_instruction_with_slot(
  stdout: &mut dyn Write,
  name: &str,
  chunk: &Chunk,
  constant: u16,
  offset: usize,
) -> io::Result<usize> {
  write!(stdout, "{:13} {:5} ", name, constant)?;
  write!(stdout, "{}", &chunk.get_constant(constant as usize))?;
  writeln!(
    stdout,
    " cache slot {}",
    &decode_u32(&chunk.instructions()[offset..offset + 4])
  )?;

  Ok(offset + 4)
}

/// print a closure
fn closure_instruction(
  stdio: &mut Stdio,
  name: &str,
  chunk: &Chunk,
  constant: u16,
  offset: usize,
) -> io::Result<usize> {
  let stdout = stdio.stdout();

  write!(stdout, "{:13} {:5} ", name, constant)?;
  writeln!(stdout, "{}", &chunk.get_constant(constant as usize))?;

  let value = chunk.get_constant(constant as usize);

  let capture_count = if_let_obj!(ObjectKind::Fun(fun) = (value) {
    fun.capture_count()
  } else {
    let stderr = stdio.stderr();

    writeln!(
      stderr,
      "!=== Compilation failure found {} instead of function ===!",
      value.value_type()
    )?;
    panic!();
  });

  let mut current_offset = offset;
  for _ in 0..capture_count {
    let capture_index: CaptureIndex = unsafe {
      mem::transmute(decode_u16(
        &chunk.instructions()[current_offset..current_offset + 2],
      ))
    };

    match capture_index {
      CaptureIndex::Local(local) => writeln!(
        stdout,
        "  {:0>4}      |                  local {}",
        current_offset, local
      ),
      CaptureIndex::Enclosing(capture) => writeln!(
        stdout,
        "  {:0>4}      |                  capture {}",
        current_offset, capture
      ),
    }?;

    current_offset += 2;
  }

  Ok(current_offset)
}

fn invoke_instruction(
  stdout: &mut dyn Write,
  name: &str,
  chunk: &Chunk,
  constant: u16,
  arg_count: u8,
  offset: usize,
) -> io::Result<usize> {
  write!(stdout, "{:13} {:5} ({} args) ", name, constant, arg_count)?;
  write!(stdout, "{}", &chunk.get_constant(constant as usize))?;
  writeln!(
    stdout,
    " cache slot {}",
    &decode_u32(&chunk.instructions()[offset..offset + 4])
  )?;
  Ok(offset + 4)
}

/// print a short instruction
fn short_instruction(
  stdout: &mut dyn Write,
  name: &str,
  slot: u16,
  offset: usize,
) -> io::Result<usize> {
  writeln!(stdout, "{:13} {:5}", name, slot)?;
  Ok(offset)
}

/// print a byte instruction
fn byte_instruction(
  stdout: &mut dyn Write,
  name: &str,
  slot: u8,
  offset: usize,
) -> io::Result<usize> {
  writeln!(stdout, "{:13} {:5}", name, slot)?;
  Ok(offset)
}

/// print a simple instruction
fn simple_instruction(stdout: &mut dyn Write, name: &str, offset: usize) -> io::Result<usize> {
  writeln!(stdout, "{:13}", name)?;
  Ok(offset)
}
