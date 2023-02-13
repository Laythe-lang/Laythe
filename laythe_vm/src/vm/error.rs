use super::{ExecuteMode, ExecuteResult, Signal, Vm};
use laythe_core::{
  if_let_obj,
  managed::{GcObj, Instance},
  object::{Class, ObjectKind, UnwindResult},
  to_obj_kind, val,
  value::Value,
};

impl Vm {
  /// Report an internal issue to the user
  pub(super) fn internal_error(&self, message: &str) -> ! {
    panic!("Internal Error: {message}")
  }

  /// Report a known laythe runtime error to the user
  pub(super) unsafe fn runtime_error(&mut self, error: GcObj<Class>, message: &str) -> Signal {
    let error_message = val!(self.manage_str(message));
    self.fiber.push(error_message);

    let mode = ExecuteMode::CallFunction(self.fiber.frames().len());
    let result = match self.resolve_call(val!(error), 1) {
      Signal::Ok => self.execute(mode),
      Signal::OkReturn => ExecuteResult::Ok(self.fiber.pop()),
      Signal::RuntimeError => ExecuteResult::RuntimeError,
      _ => self.internal_error("Unexpected signal in run_fun."),
    };

    match result {
      ExecuteResult::Ok(error) => {
        if_let_obj!(ObjectKind::Instance(instance) = (error) {
          self.set_error(instance)
        } else {
          self.internal_error("Failed to construct error")
        })
      },
      _ => self.internal_error("Failed to construct error"),
    }
  }

  /// Search for a catch block up the stack, printing the error if no catch is found
  pub(super) unsafe fn stack_unwind(
    &mut self,
    error: Instance,
    mode: ExecuteMode,
  ) -> Option<ExecuteResult> {
    self.store_ip();

    let bottom_frame = match mode {
      ExecuteMode::Normal => None,
      ExecuteMode::CallFunction(depth) => Some(depth),
    };

    match self.fiber.stack_unwind(bottom_frame) {
      UnwindResult::Handled(frame) => {
        self.current_fun = frame.fun();
        self.ip = frame.ip();
        None
      },
      UnwindResult::Unhandled => {
        self.print_error(error);
        Some(ExecuteResult::RuntimeError)
      },
      UnwindResult::UnwindStopped => Some(ExecuteResult::RuntimeError),
    }
  }

  /// Set the current error place the vm signal a runtime error
  pub(super) fn set_error(&mut self, error: Instance) -> Signal {
    self.fiber.set_error(error);
    Signal::RuntimeError
  }

  /// Set the current error place the vm signal a runtime error
  pub(super) fn set_exit(&mut self, code: u16) -> Signal {
    self.exit_code = code;
    Signal::Exit
  }

  /// Print an error message and the current call stack to the user
  fn print_error(&mut self, error: Instance) {
    let mut stdio = self.io.stdio();
    let stderr = stdio.stderr();

    self.fiber.print_error(stderr, error);
  }
}
