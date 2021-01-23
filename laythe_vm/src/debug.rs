use crate::call_frame::CallFrame;
use laythe_core::chunk::{decode_u16, AlignedByteCode, Chunk, UpvalueIndex};
use laythe_env::stdio::Stdio;
use std::{io, io::Write, mem};

/// Indicate where and how an exception was caught
pub fn exception_catch(stdout: &mut dyn Write, frame: &CallFrame, idx: usize) -> io::Result<()> {
  writeln!(
    stdout,
    "Exception popped {:0>4} frames caught by: {}",
    idx,
    frame.closure.fun().name()
  )
}

/// Write a chunk to console
pub fn disassemble_chunk(stdio: &mut Stdio, chunk: &Chunk, name: &str) -> io::Result<()> {
  let stdout = stdio.stdout();
  writeln!(stdout)?;
  writeln!(stdout, "{0}", name)?;

  let mut offset: usize = 0;
  let mut last_offset: usize = 0;

  while offset < chunk.instructions.len() {
    let show_line = chunk.get_line(offset) == chunk.get_line(last_offset);
    let temp = disassemble_instruction(stdio, chunk, offset, show_line);
    last_offset = offset;
    offset = temp?;
  }

  Ok(())
}

/// Write an instruction to console
pub fn disassemble_instruction(
  stdio: &mut Stdio,
  chunk: &Chunk,
  ip: usize,
  show_line: bool,
) -> io::Result<usize> {
  let stdout = stdio.stdout();
  write!(stdout, "  {:0>4} ", ip)?;

  if ip != 0 && show_line {
    write!(stdout, "   | ")?;
  } else {
    write!(stdout, "{:>4} ", chunk.get_line(ip))?;
  }

  let (instruction, offset) = AlignedByteCode::decode(&chunk.instructions, ip as usize);
  match instruction {
    AlignedByteCode::Return => simple_instruction(stdio.stdout(), "Return", offset),
    AlignedByteCode::Negate => simple_instruction(stdio.stdout(), "Negate", offset),
    AlignedByteCode::Add => simple_instruction(stdio.stdout(), "Add", offset),
    AlignedByteCode::Subtract => simple_instruction(stdio.stdout(), "Subtract", offset),
    AlignedByteCode::Multiply => simple_instruction(stdio.stdout(), "Multiply", offset),
    AlignedByteCode::Divide => simple_instruction(stdio.stdout(), "Divide", offset),
    AlignedByteCode::Not => simple_instruction(stdio.stdout(), "Not", offset),
    AlignedByteCode::Nil => simple_instruction(stdio.stdout(), "Nil", offset),
    AlignedByteCode::True => simple_instruction(stdio.stdout(), "True", offset),
    AlignedByteCode::False => simple_instruction(stdio.stdout(), "False", offset),
    AlignedByteCode::List(arg_count) => {
      short_instruction(stdio.stdout(), "List", arg_count, offset)
    }
    AlignedByteCode::Map(arg_count) => short_instruction(stdio.stdout(), "Map", arg_count, offset),
    AlignedByteCode::Interpolate(arg_count) => {
      short_instruction(stdio.stdout(), "Interpolate", arg_count, offset)
    }
    AlignedByteCode::IterNext(constant) => {
      invoke_instruction(stdio.stdout(), "IterNext", chunk, constant, 0, offset)
    }
    AlignedByteCode::IterCurrent(constant) => {
      constant_instruction(stdio.stdout(), "IterCurrent", chunk, constant, offset)
    }
    AlignedByteCode::GetIndex => simple_instruction(stdio.stdout(), "GetIndex", offset),
    AlignedByteCode::SetIndex => simple_instruction(stdio.stdout(), "SetIndex", offset),
    AlignedByteCode::Drop => simple_instruction(stdio.stdout(), "Drop", offset),
    AlignedByteCode::DropN(count) => byte_instruction(stdio.stdout(), "DropN", count, offset),
    AlignedByteCode::Dup => simple_instruction(stdio.stdout(), "Dup", offset),
    AlignedByteCode::Call(arg_count) => byte_instruction(stdio.stdout(), "Call", arg_count, offset),
    AlignedByteCode::Import(path) => {
      constant_instruction(stdio.stdout(), "Import", chunk, path, offset)
    }
    AlignedByteCode::Export(constant) => {
      constant_instruction(stdio.stdout(), "Export", chunk, constant, offset)
    }
    AlignedByteCode::Invoke((constant, arg_count)) => {
      invoke_instruction(stdio.stdout(), "Invoke", chunk, constant, arg_count, offset)
    }
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
    }
    AlignedByteCode::Inherit => simple_instruction(stdio.stdout(), "Inherit", offset),
    AlignedByteCode::GetSuper(constant) => {
      constant_instruction(stdio.stdout(), "GetSuper", chunk, constant, offset)
    }
    AlignedByteCode::Closure(constant) => {
      closure_instruction(stdio, "Closure", chunk, constant, offset)
    }
    AlignedByteCode::Method(constant) => {
      constant_instruction(stdio.stdout(), "Method", chunk, constant, offset)
    }
    AlignedByteCode::Field(constant) => {
      constant_instruction(stdio.stdout(), "Field", chunk, constant, offset)
    }
    AlignedByteCode::StaticMethod(constant) => {
      constant_instruction(stdio.stdout(), "StaticMethod", chunk, constant, offset)
    }
    AlignedByteCode::CloseUpvalue => simple_instruction(stdio.stdout(), "CloseUpvalue", offset),
    AlignedByteCode::UpvalueIndex(_) => {
      simple_instruction(stdio.stdout(), "!=== UpValueIndex - Invalid ===!", offset)
    }
    AlignedByteCode::DefineGlobal(constant) => {
      constant_instruction(stdio.stdout(), "DefineGlobal", chunk, constant, offset)
    }
    AlignedByteCode::GetGlobal(constant) => {
      constant_instruction(stdio.stdout(), "GetGlobal", chunk, constant, offset)
    }
    AlignedByteCode::SetGlobal(constant) => {
      constant_instruction(stdio.stdout(), "SetGlobal", chunk, constant, offset)
    }
    AlignedByteCode::GetLocal(slot) => byte_instruction(stdio.stdout(), "GetLocal", slot, offset),
    AlignedByteCode::SetLocal(slot) => byte_instruction(stdio.stdout(), "SetLocal", slot, offset),
    AlignedByteCode::GetUpvalue(slot) => {
      byte_instruction(stdio.stdout(), "GetUpvalue", slot, offset)
    }
    AlignedByteCode::SetUpvalue(slot) => {
      byte_instruction(stdio.stdout(), "SetUpvalue", slot, offset)
    }
    AlignedByteCode::SetProperty(slot) => {
      constant_instruction(stdio.stdout(), "SetProperty", chunk, slot, offset)
    }
    AlignedByteCode::GetProperty(slot) => {
      constant_instruction(stdio.stdout(), "GetProperty", chunk, slot, offset)
    }
    AlignedByteCode::Jump(jump) => jump_instruction(stdio.stdout(), "Jump", 1, jump, offset),
    AlignedByteCode::JumpIfFalse(jump) => {
      jump_instruction(stdio.stdout(), "JumpIfFalse", 1, jump, offset)
    }
    AlignedByteCode::Loop(jump) => jump_instruction(stdio.stdout(), "Loop", -1, jump, offset),
    AlignedByteCode::Equal => simple_instruction(stdio.stdout(), "Equal", offset),
    AlignedByteCode::NotEqual => simple_instruction(stdio.stdout(), "NotEqual", offset),
    AlignedByteCode::Greater => simple_instruction(stdio.stdout(), "Greater", offset),
    AlignedByteCode::GreaterEqual => simple_instruction(stdio.stdout(), "GreaterEqual", offset),
    AlignedByteCode::Less => simple_instruction(stdio.stdout(), "Less", offset),
    AlignedByteCode::LessEqual => simple_instruction(stdio.stdout(), "LessEqual", offset),
    AlignedByteCode::Constant(constant) => {
      constant_instruction(stdio.stdout(), "Constant", chunk, constant as u16, offset)
    }
    AlignedByteCode::ConstantLong(constant) => {
      constant_instruction(stdio.stdout(), "ConstantLong", chunk, constant, offset)
    }
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

/// print a constant
fn constant_instruction(
  stdout: &mut dyn Write,
  name: &str,
  chunk: &Chunk,
  constant: u16,
  offset: usize,
) -> io::Result<usize> {
  write!(stdout, "{:13} {:5} ", name, constant)?;
  writeln!(stdout, "{}", &chunk.constants[constant as usize])?;
  Ok(offset)
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
  writeln!(stdout, "{}", &chunk.constants[constant as usize])?;

  let value = &chunk.constants[constant as usize];
  let upvalue_count = if value.is_fun() {
    value.to_fun().upvalue_count
  } else {
    let stderr = stdio.stderr();

    writeln!(
      stderr,
      "!=== Compilation failure found {} instead of function ===!",
      value.value_type()
    )?;
    panic!();
  };

  let mut current_offset = offset;
  for _ in 0..upvalue_count {
    let upvalue_index: UpvalueIndex = unsafe {
      mem::transmute(decode_u16(
        &chunk.instructions[current_offset..current_offset + 2],
      ))
    };

    match upvalue_index {
      UpvalueIndex::Local(local) => writeln!(
        stdout,
        "  {:0>4}      |                  local {}",
        current_offset, local
      ),
      UpvalueIndex::Upvalue(upvalue) => writeln!(
        stdout,
        "  {:0>4}      |                  upvalue {}",
        current_offset, upvalue
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
  writeln!(stdout, "{}", &chunk.constants[constant as usize])?;
  Ok(offset)
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
