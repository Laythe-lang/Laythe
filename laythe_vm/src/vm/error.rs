use super::{ExecutionMode, ExecutionResult, ExecutionSignal, Vm};
use laythe_core::{
  if_let_obj,
  managed::{GcObj, GcStr, Instance},
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
  pub(super) unsafe fn runtime_error_from_str(
    &mut self,
    error: GcObj<Class>,
    message: &str,
  ) -> ExecutionSignal {
    self.runtime_error(error, self.manage_str(message))
  }

  /// Report a known laythe runtime error to the user
  pub(super) unsafe fn runtime_error(
    &mut self,
    error: GcObj<Class>,
    message: GcStr,
  ) -> ExecutionSignal {
    let error_message = val!(self.manage_str(message));
    // Make sure we have enough space for the error message
    // As this isn't accounted for during compilation
    self.fiber.ensure_stack(1);
    self.fiber.push(error_message);

    let mode = ExecutionMode::CallingNativeCode(self.fiber.frames().len());
    let result = match self.resolve_call(val!(error), 1) {
      ExecutionSignal::Ok => self.execute(mode),
      ExecutionSignal::OkReturn => ExecutionResult::Ok(self.fiber.pop()),
      ExecutionSignal::RuntimeError => ExecutionResult::RuntimeError,
      _ => self.internal_error("Unexpected signal in run_fun."),
    };

    match result {
      ExecutionResult::Ok(error) => {
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
    mode: ExecutionMode,
  ) -> Option<ExecutionResult> {
    self.store_ip();

    let bottom_frame = match mode {
      ExecutionMode::Normal => None,
      ExecutionMode::CallingNativeCode(depth) => Some(depth),
    };

    match self.fiber.stack_unwind(bottom_frame) {
      UnwindResult::PotentiallyHandled(frame) => {
        self.current_fun = frame.fun();
        self.ip = frame.ip();
        None
      },
      UnwindResult::Unhandled => {
        self.print_error(error);
        Some(ExecutionResult::RuntimeError)
      },
      UnwindResult::UnwindStopped => Some(ExecutionResult::RuntimeError),
    }
  }

  /// Set the current error place the vm signal a runtime error
  pub(super) fn set_error(&mut self, error: Instance) -> ExecutionSignal {
    self.fiber.set_error(error);
    ExecutionSignal::RuntimeError
  }

  /// Set the current error place the vm signal a runtime error
  pub(super) fn set_exit(&mut self, code: u16) -> ExecutionSignal {
    self.exit_code = code;
    ExecutionSignal::Exit
  }

  /// Print an error message and the current call stack to the user
  fn print_error(&mut self, error: Instance) {
    let mut stdio = self.io.stdio();
    let stderr = stdio.stderr();

    self.fiber.print_error(stderr, error);
  }
}
