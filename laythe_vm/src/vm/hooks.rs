use super::{ExecutionMode, ExecutionResult, ExecutionSignal, Vm};
use laythe_core::{object::LyStr, val, value::Value, Call, LyError};

impl Vm {
  /// Run a laythe function on top of the current stack.
  /// This acts as a hook for native functions to execute laythe function
  pub(super) unsafe fn run_fun(&mut self, callable: Value, args: &[Value]) -> Call { unsafe {
    let mut fiber = self.fiber;
    fiber.ensure_stack(self, args.len() + 1);

    fiber.push(callable);
    for arg in args {
      fiber.push(*arg);
    }

    #[cfg(feature = "debug")]
    {
      self
        .print_hook_state("run_fun", &format!("{callable}"))
        .expect("Unable to print hook state");
    }

    let mode = ExecutionMode::CallingNativeCode(self.fiber.frames().len());
    let result = match self.resolve_call(callable, args.len() as u8) {
      ExecutionSignal::Ok => self.execute(mode),
      ExecutionSignal::OkReturn => ExecutionResult::Ok(self.fiber.pop()),
      ExecutionSignal::RuntimeError => ExecutionResult::RuntimeError,
      _ => self.internal_error("Unexpected signal in run_fun."),
    };

    self.to_call_result(result)
  }}

  /// Run a laythe method on top of the current stack.
  /// This acts as a hook for native functions to execute laythe function
  pub(super) unsafe fn run_method(&mut self, this: Value, method: Value, args: &[Value]) -> Call { unsafe {
    let mut fiber = self.fiber;

    fiber.ensure_stack(self, args.len() + 1);
    fiber.push(this);
    for arg in args {
      fiber.push(*arg);
    }

    #[cfg(feature = "debug")]
    {
      self
        .print_hook_state("run_method", &format!("{this}:{method}"))
        .expect("Unable to print hook state");
    }

    let mode = ExecutionMode::CallingNativeCode(self.fiber.frames().len());

    let result = match self.resolve_call(method, args.len() as u8) {
      ExecutionSignal::Ok => self.execute(mode),
      ExecutionSignal::OkReturn => ExecutionResult::Ok(self.fiber.pop()),
      ExecutionSignal::RuntimeError => ExecutionResult::RuntimeError,
      _ => self.internal_error("Unexpected signal in run_method."),
    };

    self.to_call_result(result)
  }}

  /// Get a method for this this value with a given method name
  pub(super) unsafe fn get_method(&mut self, this: Value, method_name: LyStr) -> Call { unsafe {
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
  }}

  pub(super) fn scan_roots(&mut self) {
    self.fiber.scan_roots();
  }

  /// Convert an execute result to a call result
  fn to_call_result(&self, execute_result: ExecutionResult) -> Call {
    match execute_result {
      ExecutionResult::Ok(value) => Call::Ok(value),
      ExecutionResult::Exit(_) => self.internal_error("Accidental early exit in hook call"),
      ExecutionResult::CompileError => {
        self.internal_error("Compiler error should occur before code is executed.")
      },
      ExecutionResult::RuntimeError => match self.fiber.error() {
        Some(error) => Call::Err(LyError::Err(error)),
        None => self.internal_error("Error not set on vm executor."),
      },
    }
  }
}
