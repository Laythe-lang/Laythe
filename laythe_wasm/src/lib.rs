extern crate wasm_bindgen;

mod stdio_wasm;
mod time_wasm;

use js_sys::Function;
use laythe_env::io::Io;
use laythe_vm::vm::{ExecuteResult, Vm};
use std::{path::PathBuf, rc::Rc};
use stdio_wasm::{IoStdioWasmConsole, IoStdioWasmJsFunction};
use time_wasm::IoTimeWasm;
use wasm_bindgen::prelude::*;

// setup the console panic hook
pub fn set_panic_hook() {
  #[cfg(feature = "console_error_panic_hook")]
  console_error_panic_hook::set_once();
}

#[wasm_bindgen]
pub struct VmWasm(Vm);

#[wasm_bindgen]
impl VmWasm {
  pub fn new() -> Self {
    set_panic_hook();
    let wasm_io = Io::default()
      .with_time(Rc::new(IoTimeWasm::default()))
      .with_stdio(Rc::new(IoStdioWasmConsole()));

    Self(Vm::new(wasm_io))
  }

  pub fn with_stdout(stdout: &Function) -> Self {
    set_panic_hook();
    let wasm_io = Io::default()
      .with_time(Rc::new(IoTimeWasm::default()))
      .with_stdio(Rc::new(IoStdioWasmJsFunction::new(Rc::new(stdout.clone()))));

    Self(Vm::new(wasm_io))
  }

  pub fn version() -> String {
    Vm::version().to_string()
  }

  pub fn run(&mut self, source: &str) -> f64 {
    let result = self.0.run(PathBuf::from("script.ly"), source);

    match result {
      ExecuteResult::Ok => 0.0,
      ExecuteResult::FunResult(_) => 1.0,
      ExecuteResult::InternalError => 2.0,
      ExecuteResult::RuntimeError => 3.0,
      ExecuteResult::CompileError => 4.0,
    }
  }
}
