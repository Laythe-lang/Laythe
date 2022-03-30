use super::{ExecuteMode, ExecuteResult, Signal, Vm};
use laythe_core::{managed::GcStr, val, value::Value, Call, LyError};

impl Vm {
  /// Run a laythe function on top of the current stack.
  /// This acts as a hook for native functions to execute laythe function
  pub(super) unsafe fn run_fun(&mut self, callable: Value, args: &[Value]) -> Call {
    self.fiber.ensure_stack(args.len() + 1);
    self.fiber.push(callable);
    for arg in args {
      self.fiber.push(*arg);
    }

    #[cfg(feature = "debug")]
    {
      self
        .print_hook_state("run_fun", &format!("{}", callable))
        .expect("Unable to print hook state");
    }

    let mode = ExecuteMode::CallFunction(self.fiber.frames().len());
    let result = match self.resolve_call(callable, args.len() as u8) {
      Signal::Ok => self.execute(mode),
      Signal::OkReturn => ExecuteResult::Ok(self.fiber.pop()),
      Signal::RuntimeError => ExecuteResult::RuntimeError,
      _ => self.internal_error("Unexpected signal in run_fun."),
    };

    self.to_call_result(result)
  }

  /// Run a laythe method on top of the current stack.
  /// This acts as a hook for native functions to execute laythe function
  pub(super) unsafe fn run_method(&mut self, this: Value, method: Value, args: &[Value]) -> Call {
    self.fiber.ensure_stack(args.len() + 1);
    self.fiber.push(this);
    for arg in args {
      self.fiber.push(*arg);
    }

    #[cfg(feature = "debug")]
    {
      self
        .print_hook_state("fun_method", &format!("{}:{}", this, method))
        .expect("Unable to print hook state");
    }

    let mode = ExecuteMode::CallFunction(self.fiber.frames().len());

    let result = match self.resolve_call(method, args.len() as u8) {
      Signal::Ok => self.execute(mode),
      Signal::OkReturn => ExecuteResult::Ok(self.fiber.pop()),
      Signal::RuntimeError => ExecuteResult::RuntimeError,
      _ => self.internal_error("Unexpected signal in run_method."),
    };

    self.to_call_result(result)
  }

  /// Get a method for this this value with a given method name
  pub(super) unsafe fn get_method(&mut self, this: Value, method_name: GcStr) -> Call {
    let class = self.value_class(this);

    match class.get_method(&method_name) {
      Some(method) => Call::Ok(method),
      None => {
        let error_message = val!(self.manage_str(format!(
          "Class {} does not have method {}",
          class.name(),
          method_name
        )));

        self.run_fun(val!(self.builtin.errors.import), &[error_message])
      },
    }
  }

  /// Convert an execute result to a call result
  fn to_call_result(&self, execute_result: ExecuteResult) -> Call {
    match execute_result {
      ExecuteResult::Ok(value) => Call::Ok(value),
      ExecuteResult::Exit(_) => self.internal_error("Accidental early exit in hook call"),
      ExecuteResult::CompileError => {
        self.internal_error("Compiler error should occur before code is executed.")
      },
      ExecuteResult::RuntimeError => match self.fiber.error() {
        Some(error) => Call::Err(LyError::Err(error)),
        None => self.internal_error("Error not set on vm executor."),
      },
    }
  }
}
