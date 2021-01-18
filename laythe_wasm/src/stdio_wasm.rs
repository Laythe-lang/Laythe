use io::Write;
use js_sys::Function;
use laythe_env::{
  io::IoImpl,
  stdio::{MockRead, Stdio, StdioImpl},
};
use std::{cell::RefCell, io, sync::Arc};
use termcolor::WriteColor;
use wasm_bindgen::JsValue;
use web_sys::console::{error_1, log_1};

#[derive(Debug)]
pub struct IoStdioWasmConsole();

impl IoImpl<Stdio> for IoStdioWasmConsole {
  fn make(&self) -> Stdio {
    Stdio::new(Box::new(StdioWasm::new(&log_1, &error_1)))
  }
}

#[derive(Debug)]
pub struct IoStdioWasmJsFunction {
  fun: Arc<Function>,
  line_buffer: Arc<RefCell<String>>,
}

impl IoStdioWasmJsFunction {
  pub fn new(fun: Arc<Function>) -> Self {
    Self {
      fun,
      line_buffer: Arc::new(RefCell::new("".to_string())),
    }
  }
}

impl IoImpl<Stdio> for IoStdioWasmJsFunction {
  fn make(&self) -> Stdio {
    Stdio::new(Box::new(StdioJsFunction::new(
      Arc::clone(&self.fun),
      Arc::clone(&self.line_buffer),
    )))
  }
}

pub struct StdioWasm<'a, O, E>
where
  O: Fn(&JsValue),
  E: Fn(&JsValue),
{
  stdout: ConsoleWrapper<'a, O>,
  stderr: ConsoleWrapper<'a, E>,
  stdin: MockRead,
}

impl<'a, O, E> StdioWasm<'a, O, E>
where
  O: Fn(&JsValue),
  E: Fn(&JsValue),
{
  pub fn new(stdout_impl: &'a O, stderr_impl: &'a E) -> Self {
    Self {
      stdout: ConsoleWrapper::new(stdout_impl),
      stderr: ConsoleWrapper::new(stderr_impl),
      stdin: MockRead(),
    }
  }
}

impl<'a, O, E> StdioImpl for StdioWasm<'a, O, E>
where
  O: Fn(&JsValue) + 'static + Sync,
  E: Fn(&JsValue) + 'static + Sync,
{
  fn stdout(&mut self) -> &mut dyn io::Write {
    &mut self.stdout
  }
  fn stderr(&mut self) -> &mut dyn io::Write {
    &mut self.stderr
  }
  fn stderr_color(&mut self) -> &mut dyn WriteColor {
    &mut self.stderr
  }
  fn stdin(&mut self) -> &mut dyn io::Read {
    &mut self.stdin
  }
  fn read_line(&self, _buffer: &mut String) -> io::Result<usize> {
    Ok(0)
  }
}

struct StdioJsFunction {
  stdout: FunWrapper,
  stderr: FunWrapper,
  stdin: MockRead,
}

impl StdioJsFunction {
  pub fn new(stdout: Arc<Function>, line_buffer: Arc<RefCell<String>>) -> Self {
    Self {
      stdout: FunWrapper::new(Arc::clone(&stdout), &line_buffer),
      stderr: FunWrapper::new(stdout, &line_buffer),
      stdin: MockRead(),
    }
  }
}

impl StdioImpl for StdioJsFunction {
  fn stdout(&mut self) -> &mut dyn io::Write {
    &mut self.stdout
  }
  fn stderr(&mut self) -> &mut dyn io::Write {
    &mut self.stderr
  }
  fn stderr_color(&mut self) -> &mut dyn WriteColor {
    &mut self.stderr
  }
  fn stdin(&mut self) -> &mut dyn io::Read {
    &mut self.stdin
  }
  fn read_line(&self, _buffer: &mut String) -> io::Result<usize> {
    Ok(0)
  }
}

struct ConsoleWrapper<'a, W: Fn(&JsValue)> {
  writer: &'a W,
  line_buffer: String,
}

impl<'a, W: Fn(&JsValue)> ConsoleWrapper<'a, W> {
  pub fn new(writer: &'a W) -> Self {
    Self {
      writer,
      line_buffer: "".to_string(),
    }
  }
}

impl<'a, W: Fn(&JsValue)> WriteColor for ConsoleWrapper<'a, W> {
  fn supports_color(&self) -> bool {
    false
  }

  fn set_color(&mut self, _: &termcolor::ColorSpec) -> io::Result<()> {
    Ok(())
  }

  fn reset(&mut self) -> io::Result<()> {
    Ok(())
  }
}

impl<'a, W: Fn(&JsValue)> Write for ConsoleWrapper<'a, W> {
  fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
    let string = std::str::from_utf8(buf);

    match string {
      Ok(string) => {
        let combined = format!("{}{}", self.line_buffer, string);
        let splits: Vec<&str> = combined.split("\n").collect();

        match splits.split_last() {
          Some((last, rest)) => {
            let mut written = 0;

            for line in rest {
              (self.writer)(&format!("{}", line).into());
              written += line.len() + 1;
            }

            let existing = self.line_buffer.len();
            self.line_buffer = last.to_string();
            written += self.line_buffer.len();
            Ok(written - existing)
          }
          None => Ok(0),
        }
      }
      Err(err) => Err(io::Error::new(io::ErrorKind::InvalidData, err.to_string())),
    }
  }

  fn flush(&mut self) -> io::Result<()> {
    if self.line_buffer != "" {
      (self.writer)(&self.line_buffer.clone().into());
      self.line_buffer.truncate(0);
    }

    Ok(())
  }
}

struct FunWrapper {
  fun: Arc<Function>,
  line_buffer: Arc<RefCell<String>>,
}

impl FunWrapper {
  pub fn new(fun: Arc<Function>, line_buffer: &Arc<RefCell<String>>) -> Self {
    Self {
      fun,
      line_buffer: Arc::clone(line_buffer),
    }
  }
}

impl WriteColor for FunWrapper {
  fn supports_color(&self) -> bool {
    false
  }

  fn set_color(&mut self, _: &termcolor::ColorSpec) -> io::Result<()> {
    Ok(())
  }

  fn reset(&mut self) -> io::Result<()> {
    Ok(())
  }
}

impl Write for FunWrapper {
  fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
    let string = std::str::from_utf8(buf);

    match string {
      Ok(string) => {
        let combined = format!("{}{}", self.line_buffer.borrow(), string);
        let splits: Vec<&str> = combined.split("\n").collect();

        match splits.split_last() {
          Some((last, rest)) => {
            let mut written = 0;

            for line in rest {
              if let Err(_) = self.fun.call1(&JsValue::NULL, &format!("{}", line).into()) {
                return Err(io::Error::new(
                  io::ErrorKind::Other,
                  "Failed to write to provided callback",
                ));
              }
              written += line.len() + 1;
            }

            let existing = self.line_buffer.borrow().len();
            self.line_buffer.replace(last.to_string());
            written += self.line_buffer.borrow().len();

            Ok(written - existing)
          }
          None => Ok(0),
        }
      }
      Err(err) => Err(io::Error::new(io::ErrorKind::InvalidData, err.to_string())),
    }
  }

  fn flush(&mut self) -> io::Result<()> {
    if &*self.line_buffer.borrow_mut() != "" {
      if let Err(_) = self
        .fun
        .call1(&JsValue::NULL, &self.line_buffer.borrow().clone().into())
      {
        return Err(io::Error::new(
          io::ErrorKind::Other,
          "Failed to write to provided callback",
        ));
      }
      self.line_buffer.borrow_mut().truncate(0);
    }

    Ok(())
  }
}
