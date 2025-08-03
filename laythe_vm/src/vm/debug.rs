use crate::debug::disassemble_instruction;
use laythe_env::stdio::Stdio;
use std::io;

use super::Vm;

impl Vm {
  /// Print debugging information for the current instruction
  pub(super) unsafe fn print_state(&self, ip: *const u8) -> io::Result<usize> { unsafe {
    let mut stdio = self.io.stdio();

    self.print_stack_debug(&mut stdio)?;

    let start = self.current_fun.chunk().instructions().as_ptr();
    let offset = ip.offset_from(start) as usize;
    disassemble_instruction(&mut stdio, self.current_fun.chunk(), offset, false)
  }}

  /// Print debugging information for the current hook
  pub(super) unsafe fn print_hook_state(&self, hook_name: &str, with: &str) -> io::Result<()> { unsafe {
    let mut stdio = self.io.stdio();

    self.print_stack_debug(&mut stdio)?;

    let stdout = stdio.stdout();
    writeln!(stdout, "  Vm Hook {hook_name}: {with}")
  }}

  /// Print the current stack
  pub(super) unsafe fn print_stack_debug(&self, stdio: &mut Stdio) -> io::Result<()> {
    let stdout = stdio.stdout();

    if self.fiber.frames().len() > 1 {
      write!(stdout, "Frame Stack:  ")?;
      for frame in self.fiber.frames().iter() {
        let fun = frame.fun();
        write!(stdout, "[ {:}:{:} ]", fun.module().name(), fun.name())?;
      }
      writeln!(stdout)?;
    }

    write!(stdout, "Local Stack:  ")?;
    for value in self.fiber.frame_stack() {
      let s = value.to_string();
      write!(
        stdout,
        "[ {} ]",
        s.chars().take(60).collect::<String>()
      )?;
    }

    writeln!(stdout)
  }
}
