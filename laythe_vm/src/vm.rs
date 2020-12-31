use crate::{
  call_frame::CallFrame,
  compiler::{Compiler, CompilerResult},
  constants::{DEFAULT_STACK_MAX, FRAME_MAX, REPL_MODULE},
  dep_manager::DepManager,
  parser::Parser,
};
use laythe_core::{
  chunk::{AlignedByteCode, ByteCode, UpvalueIndex},
  constants::{PLACEHOLDER_NAME, SCRIPT},
  hooks::{GcContext, GcHooks, HookContext, Hooks, NoContext, ValueContext},
  module::Module,
  native::{Native, NativeMeta},
  object::Map,
  object::{Class, Closure, Fun, Instance, List, Method, Upvalue},
  package::{Import, Package},
  signature::{ArityError, Environment, ParameterKind, SignatureError},
  utils::{is_falsey, ptr_len},
  val,
  value::{Value, ValueKind, VALUE_NIL, VALUE_TRUE},
  Call, LyResult,
};
use laythe_env::{
  io::Io,
  managed::{Gc, RootTrace, Trace},
  memory::{Allocator, NO_GC},
};
use laythe_lib::{builtin_from_module, create_std_lib, GLOBAL, STD};
use laythe_native::io::io_native;
use smol_str::SmolStr;
use std::convert::TryInto;
use std::io::Write;
use std::mem;
use std::ptr;
use std::{cell::RefCell, cmp::Ordering};
use std::{path::PathBuf, ptr::NonNull};

#[cfg(feature = "debug")]
use crate::debug::{disassemble_instruction, exception_catch};

#[cfg(feature = "debug")]
use std::io;

#[cfg(feature = "debug_upvalues")]
use std::{cmp::Ordering, io};

#[cfg(feature = "debug")]
use laythe_env::stdio::Stdio;

const VERSION: &str = "0.1.0";

#[derive(Debug, Clone, PartialEq)]
enum Signal {
  Ok,
  OkReturn,
  Exit,
  RuntimeError,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExecuteResult {
  Ok,
  FunResult(Value),
  InternalError,
  RuntimeError,
  CompileError,
}

pub enum ExecuteMode {
  Normal,
  CallFunction(usize),
}

pub fn default_native_vm() -> Vm {
  Vm::new(io_native())
}

/// A set of dependencies needed by the virtual machine
pub struct VmDependencies {
  /// access to the environments io
  io: Io,

  /// The garbage collector
  gc: Allocator,

  /// The native functions
  std_lib: Gc<Package>,
}

/// The virtual machine for the laythe programming language
pub struct Vm {
  /// A stack holding all local variable currently in use
  stack: Vec<Value>,

  /// A stack holding call frames currently in use
  frames: Vec<CallFrame>,

  /// The vm's garbage collector
  gc: RefCell<Allocator>,

  /// The environments io access
  io: Io,

  /// an object to manage the current dependencies
  dep_manager: Gc<DepManager>,

  /// The global module
  global: Gc<Module>,

  /// A collection of currently available upvalues
  open_upvalues: Vec<Gc<Upvalue>>,

  /// the main script level function
  script: Option<Gc<Closure>>,

  /// The current frame's function
  current_fun: Gc<Fun>,

  /// The current frame's closure
  current_frame: *mut CallFrame,

  /// The current error if one is active
  current_error: Option<Gc<Instance>>,

  /// What exit code is currently set
  exit_code: u16,

  /// pointer to the top of the value stack
  stack_top: *mut Value,

  /// pointer to the current instruction
  ip: *const u8,

  /// TODO replace this. A fun to fill a call frame for higher order native functions
  /// may want to eventually have a function rental so native functions can set name / module
  /// for exception
  native_fun_stub: Gc<Fun>,

  /// The current frame depth of the program
  frame_count: usize,
}

impl Vm {
  pub fn new(io: Io) -> Vm {
    let gc = Allocator::new(io.stdio());
    let no_gc_context = NoContext::new(gc);
    let hooks = GcHooks::new(&no_gc_context);

    let cwd = io
      .env()
      .current_dir()
      .expect("Could not obtain the current working directory.");

    let std_lib = create_std_lib(&hooks).expect("Standard library creation failed");
    let global = std_lib
      .import(
        &hooks,
        Import::new(vec![hooks.manage_str(STD), hooks.manage_str(GLOBAL)]),
      )
      .expect("Could not retrieve global module");

    let fun = Fun::new(hooks.manage_str(PLACEHOLDER_NAME), global);

    let managed_fun = hooks.manage(fun);
    let closure = hooks.manage(Closure::new(managed_fun));

    let mut frames = vec![CallFrame::new(closure); FRAME_MAX];
    let mut stack = vec![VALUE_NIL; DEFAULT_STACK_MAX];

    let builtin = builtin_from_module(&hooks, &global)
      .expect("Failed to generate builtin class from global module");

    let dep_manager = DepManager::new(io.clone(), builtin, hooks.manage(cwd));
    let mut dep_manager = hooks.manage(dep_manager);

    dep_manager.add_package(&hooks, std_lib);

    let current_frame = &mut frames[0];
    let current_fun = current_frame.closure.fun;
    let stack_top = &mut stack[1] as *mut Value;
    let ip = ptr::null();

    let current_frame = current_frame as *mut CallFrame;
    let mut native_fun_stub = hooks.manage(Fun::new(hooks.manage_str("native"), global));
    native_fun_stub.write_instruction(&hooks, AlignedByteCode::Nil, 0);

    let gc = RefCell::new(no_gc_context.done());

    Vm {
      io,
      stack,
      frames,
      gc,
      dep_manager,
      global,
      frame_count: 1,
      script: None,
      current_fun,
      current_frame,
      current_error: None,
      exit_code: 0,
      stack_top,
      ip,
      native_fun_stub,
      open_upvalues: Vec::with_capacity(100),
    }
  }

  /// The current version of the virtual machine
  pub fn version() -> &'static str {
    VERSION
  }

  /// Start the interactive repl
  pub fn repl(&mut self) -> ExecuteResult {
    let mut stdio = self.io.stdio();

    let main_module = self
      .main_module(self.dep_manager.src_dir.join(PathBuf::from(REPL_MODULE)))
      .expect("Could not retrieve main module");

    loop {
      let mut buffer = String::new();

      if write!(stdio.stdout(), "laythe:> ").is_err() {
        return ExecuteResult::InternalError;
      }
      stdio.stdout().flush().expect("Could not write to stdout");

      match stdio.read_line(&mut buffer) {
        Ok(_) => {
          self.interpret(main_module, &buffer);
        }
        Err(error) => panic!(error),
      }
    }
  }

  /// Run the provided source file
  pub fn run(&mut self, module_path: PathBuf, source: &str) -> ExecuteResult {
    match self.io.fs().canonicalize(&module_path) {
      Ok(module_path) => {
        let mut directory = module_path.clone();
        directory.pop();

        self.dep_manager.src_dir = self.gc.borrow_mut().manage(directory, &NO_GC);
        let main_module = match self.main_module(module_path) {
          Ok(module) => module,
          Err(err) => {
            return err;
          }
        };

        self.interpret(main_module, source)
      }
      Err(err) => {
        writeln!(self.io.stdio().stderr(), "{}", &err.to_string())
          .expect("Unable to write to stderr");
        ExecuteResult::RuntimeError
      }
    }
  }

  /// Interpret the provided laythe script returning the execution result
  fn interpret(&mut self, main_module: Gc<Module>, source: &str) -> ExecuteResult {
    match self.compile(main_module, source) {
      Ok(fun) => {
        self.prepare(fun);
        self.execute(ExecuteMode::Normal)
      }
      Err(()) => ExecuteResult::CompileError,
    }
  }

  /// Compile the provided laythe source into the virtual machine's bytecode
  fn compile(&mut self, module: Gc<Module>, source: &str) -> CompilerResult {
    let ast = Parser::new(self.io.stdio(), &source).parse()?;

    let gc = self.gc.take();
    let compiler = Compiler::new(module, self, &self.io, gc);
    let (result, gc) = compiler.compile(&ast);
    self.gc.replace(gc);

    result
  }

  /// Reset the vm to execute another script
  fn prepare(&mut self, script: Gc<Fun>) {
    self.script = Some(self.gc.borrow_mut().manage(Closure::new(script), &NO_GC));
    self.reset_stack();
    let result = self.call(self.script.expect("Script removed."), 0);

    if result != Signal::Ok {
      self.internal_error("Main script call failed.");
    }

    let mut current_module = self.current_fun.module;
    self
      .global
      .transfer_exported(&GcHooks::new(self), &mut current_module)
      .expect("Transfer global module failed");
  }

  /// Prepare the main module for use
  fn main_module(&self, module_path: PathBuf) -> Result<Gc<Module>, ExecuteResult> {
    let hooks = GcHooks::new(self);

    let mut stdio = self.io.stdio();
    let stderr = stdio.stderr();

    // resolve the main module from the provided path
    let module_path = hooks.manage(module_path);
    hooks.push_root(module_path);

    let module = match Module::from_path(&hooks, module_path) {
      Ok(module) => module,
      Err(err) => {
        writeln!(stderr, "{}", err).expect("Unable to write to stderr");
        return Err(ExecuteResult::RuntimeError);
      }
    };
    let mut module = hooks.manage(module);
    hooks.push_root(module);

    // transfer the symbols from the global module into the main module
    match self.global.transfer_exported(&hooks, &mut module) {
      LyResult::Ok(_) => {}
      _ => {
        writeln!(stderr, "Transfer global module failed.").expect("Unable to write to stderr");
        return Err(ExecuteResult::RuntimeError);
      }
    }

    hooks.pop_roots(2);
    Ok(module)
  }

  /// Run a laythe function on top of the current stack.
  /// This acts as a hook for native functions to execute laythe function
  fn run_fun(&mut self, callable: Value, args: &[Value]) -> ExecuteResult {
    for arg in args {
      self.push(*arg);
    }

    let mode = ExecuteMode::CallFunction(self.frame_count);
    match self.resolve_call(callable, args.len() as u8) {
      Signal::Ok => self.execute(mode),
      Signal::OkReturn => ExecuteResult::FunResult(self.pop()),
      Signal::RuntimeError => ExecuteResult::RuntimeError,
      _ => self.internal_error("Unexpected signal in run_fun."),
    }
  }

  /// Run a laythe method on top of the current stack.
  /// This acts as a hook for native functions to execute laythe function
  fn run_method(&mut self, this: Value, method: Value, args: &[Value]) -> ExecuteResult {
    self.push(this);
    for arg in args {
      self.push(*arg);
    }

    let mode = ExecuteMode::CallFunction(self.frame_count);
    match self.resolve_call(method, args.len() as u8) {
      Signal::Ok => self.execute(mode),
      Signal::OkReturn => ExecuteResult::FunResult(self.pop()),
      Signal::RuntimeError => ExecuteResult::RuntimeError,
      _ => self.internal_error("Unexpected signal in run_method."),
    }
  }

  /// Get a method for this this value with a given method name
  fn get_method(&mut self, this: Value, method_name: Gc<SmolStr>) -> Call {
    let class = self
      .dep_manager
      .primitive_classes()
      .for_value(this, this.kind());

    match class.get_method(&method_name) {
      Some(method) => Call::Ok(method),
      None => {
        let error_message = val!(self.gc.borrow_mut().manage_str(
          format!("Class {} does not have method {}", class.name, method_name),
          self,
        ));

        let result = self.run_fun(
          val!(self.dep_manager.error_classes().import),
          &[error_message],
        );

        self.to_call_result(result)
      }
    }
  }

  /// Get the class for this value
  fn get_class(&mut self, this: Value) -> Value {
    val!(self
      .dep_manager
      .primitive_classes()
      .for_value(this, this.kind()))
  }

  /// Main virtual machine execution loop. This will run the until the program interrupts
  /// from a normal exit or from a runtime error.
  fn execute(&mut self, mode: ExecuteMode) -> ExecuteResult {
    loop {
      // get the current instruction
      let op_code: ByteCode = ByteCode::from(self.read_byte());

      #[cfg(feature = "debug")]
      {
        let ip = unsafe { self.ip.offset(-1) };
        if let Err(_) = self.print_state(ip) {
          return ExecuteResult::InternalError;
        }
      }

      // execute the decoded instruction
      let result = match op_code {
        ByteCode::Negate => self.op_negate(),
        ByteCode::Add => self.op_add(),
        ByteCode::Subtract => self.op_sub(),
        ByteCode::Multiply => self.op_mul(),
        ByteCode::Divide => self.op_div(),
        ByteCode::Not => self.op_not(),
        ByteCode::Equal => self.op_equal(),
        ByteCode::NotEqual => self.op_not_equal(),
        ByteCode::Greater => self.op_greater(),
        ByteCode::GreaterEqual => self.op_greater_equal(),
        ByteCode::Less => self.op_less(),
        ByteCode::LessEqual => self.op_less_equal(),
        ByteCode::JumpIfFalse => self.op_jump_if_false(),
        ByteCode::Jump => self.op_jump(),
        ByteCode::Loop => self.op_loop(),
        ByteCode::DefineGlobal => self.op_define_global(),
        ByteCode::GetIndex => self.op_get_index(),
        ByteCode::SetIndex => self.op_set_index(),
        ByteCode::GetGlobal => self.op_get_global(),
        ByteCode::SetGlobal => self.op_set_global(),
        ByteCode::GetLocal => self.op_get_local(),
        ByteCode::SetLocal => self.op_set_local(),
        ByteCode::GetUpvalue => self.op_get_upvalue(),
        ByteCode::SetUpvalue => self.op_set_upvalue(),
        ByteCode::GetProperty => self.op_get_property(),
        ByteCode::SetProperty => self.op_set_property(),
        ByteCode::Import => self.op_import(),
        ByteCode::Export => self.op_export(),
        ByteCode::Drop => self.op_drop(),
        ByteCode::DropN => self.op_drop_n(),
        ByteCode::Dup => self.op_dup(),
        ByteCode::Nil => self.op_literal(VALUE_NIL),
        ByteCode::True => self.op_literal(val!(true)),
        ByteCode::False => self.op_literal(val!(false)),
        ByteCode::List => self.op_list(),
        ByteCode::Map => self.op_map(),
        ByteCode::Interpolate => self.op_interpolate(),
        ByteCode::IterNext => self.op_iter_next(),
        ByteCode::IterCurrent => self.op_iter_current(),
        ByteCode::Constant => self.op_constant(),
        ByteCode::ConstantLong => self.op_constant_long(),
        ByteCode::Call => self.op_call(),
        ByteCode::Invoke => self.op_invoke(),
        ByteCode::SuperInvoke => self.op_super_invoke(),
        ByteCode::Closure => self.op_closure(),
        ByteCode::Method => self.op_method(),
        ByteCode::Field => self.op_field(),
        ByteCode::StaticMethod => self.op_static_method(),
        ByteCode::Class => self.op_class(),
        ByteCode::Inherit => self.op_inherit(),
        ByteCode::GetSuper => self.op_get_super(),
        ByteCode::CloseUpvalue => self.op_close_upvalue(),
        ByteCode::Return => self.op_return(),
      };

      match result {
        Signal::OkReturn => {
          if let ExecuteMode::CallFunction(depth) = mode {
            if depth == self.frame_count {
              return ExecuteResult::FunResult(self.get_val(-1));
            }
          }
        }
        Signal::Ok => (),
        Signal::RuntimeError => match self.current_error {
          Some(error) => {
            if let Some(signal) = self.stack_unwind(error) {
              return signal;
            }
          }
          None => self.internal_error("Runtime error was not set."),
        },
        Signal::Exit => {
          return ExecuteResult::Ok;
        }
      }
    }
  }

  /// Get an immutable reference to value on the stack
  #[inline]
  fn get_val(&self, offset: isize) -> Value {
    unsafe { *self.stack_top.offset(offset) }
  }

  /// Set a value on the stack
  #[inline]
  fn set_val(&mut self, offset: isize, val: Value) {
    unsafe { *self.stack_top.offset(offset) = val }
  }

  /// Update the current instruction pointer
  #[inline]
  fn update_ip(&mut self, offset: isize) {
    unsafe { self.ip = self.ip.offset(offset) }
  }

  /// Store the ip in the current frame
  #[inline]
  fn load_ip(&mut self) {
    unsafe { self.ip = (*self.current_frame).ip }
  }

  /// Store the ip in the current frame
  #[inline]
  fn store_ip(&mut self) {
    unsafe { (*self.current_frame).ip = self.ip }
  }

  /// Get the current frame slots
  #[inline]
  fn slots(&self) -> *mut Value {
    unsafe { (*self.current_frame).slots }
  }

  /// Get the current closure
  #[inline]
  fn closure(&self) -> Gc<Closure> {
    unsafe { (*self.current_frame).closure }
  }

  /// read a u8 out of the bytecode
  #[inline]
  #[no_mangle]
  fn read_byte(&mut self) -> u8 {
    let byte = unsafe { ptr::read(self.ip) };
    self.update_ip(1);
    byte
  }

  /// read a u16 out of the bytecode
  #[inline]
  fn read_short(&mut self) -> u16 {
    let slice = unsafe { std::slice::from_raw_parts(self.ip, 2) };
    let buffer = slice.try_into().expect("slice of incorrect length.");
    let short = u16::from_ne_bytes(buffer);
    self.update_ip(2);

    short
  }

  /// push a value onto the stack
  #[inline]
  fn push(&mut self, value: Value) {
    self.set_val(0, value);
    self.stack_top = unsafe { self.stack_top.offset(1) };
  }

  /// pop a value off the stack
  #[inline]
  fn pop(&mut self) -> Value {
    self.drop();
    self.get_val(0)
  }

  /// drop a value off the stack
  #[inline]
  fn drop(&mut self) {
    self.stack_top = unsafe { self.stack_top.offset(-1) };
  }

  /// drop a value off the stack
  #[inline]
  fn drop_n(&mut self, count: u8) {
    self.stack_top = unsafe { self.stack_top.offset(-(count as isize)) };
  }

  /// reference a value n slots from the stack head
  #[inline]
  fn peek(&self, distance: isize) -> Value {
    self.get_val(-(distance as isize) - 1)
  }

  /// read a constant from the current chunk
  #[inline]
  fn read_constant(&self, index: u16) -> Value {
    unsafe {
      *self
        .current_fun
        .chunk()
        .constants
        .get_unchecked(index as usize)
    }
  }

  /// read a constant as a string from the current chunk
  #[inline]
  fn read_string(&self, index: u16) -> Gc<SmolStr> {
    self.read_constant(index).to_str()
  }

  /// reset the stack in case of interrupt
  fn reset_stack(&mut self) {
    self.stack_top = &mut self.stack[1] as *mut Value;
    self.current_frame = &mut self.frames[0] as *mut CallFrame;
    self.frame_count = 1;
    self.open_upvalues.clear();
  }

  /// push a literal value onto the stack
  fn op_literal(&mut self, value: Value) -> Signal {
    self.push(value);
    Signal::Ok
  }

  /// drop a value off the stack
  fn op_drop(&mut self) -> Signal {
    self.drop();
    Signal::Ok
  }

  /// drop a value off the stack
  fn op_drop_n(&mut self) -> Signal {
    let count = self.read_byte();
    self.drop_n(count);
    Signal::Ok
  }

  /// duplicate the top value on the stack
  fn op_dup(&mut self) -> Signal {
    self.push(self.peek(0));
    Signal::Ok
  }

  /// create a list from a list literal
  fn op_list(&mut self) -> Signal {
    let arg_count = self.read_short();
    let args = unsafe {
      std::slice::from_raw_parts(
        self.stack_top.offset(-(arg_count as isize)),
        arg_count as usize,
      )
    };
    let list = val!(self.gc.borrow_mut().manage(List::from(args), self));
    self.stack_top = unsafe { self.stack_top.offset(-(arg_count as isize)) };
    self.push(list);

    Signal::Ok
  }

  /// create a map from a map literal
  fn op_map(&mut self) -> Signal {
    let arg_count = self.read_short();
    let mut map = self
      .gc
      .borrow_mut()
      .manage(Map::with_capacity(arg_count as usize), self);

    for i in 0..arg_count {
      let key = self.get_val(-(i as isize * 2) - 2);
      let value = self.get_val(-(i as isize * 2) - 1);

      map.insert(key, value);
    }
    self.stack_top = unsafe { self.stack_top.offset(-(arg_count as isize * 2)) };
    self.push(val!(map));

    Signal::Ok
  }

  /// create a map from a map literal
  fn op_interpolate(&mut self) -> Signal {
    let arg_count = self.read_short();
    let args = unsafe {
      std::slice::from_raw_parts(
        self.stack_top.offset(-(arg_count as isize)),
        arg_count as usize,
      )
    };

    let mut length: usize = 0;
    for arg in args {
      length += arg.to_str().len();
    }

    let mut buffers = String::with_capacity(length);
    for arg in args {
      buffers.push_str(arg.to_str().as_str())
    }

    self.stack_top = unsafe { self.stack_top.offset(-(arg_count as isize)) };
    let interpolated = val!(self.gc.borrow_mut().manage_str(buffers, self));
    self.push(interpolated);
    Signal::Ok
  }

  /// move an iterator to the next element
  fn op_iter_next(&mut self) -> Signal {
    let receiver = self.peek(0);

    if receiver.is_iter() {
      self.update_ip(2);
      match receiver.to_iter().next(&mut Hooks::new(self)) {
        Call::Ok(value) => {
          self.set_val(-1, value);
          Signal::Ok
        }
        Call::Err(error) => self.set_error(error),
        Call::Exit(code) => self.set_exit(code),
      }
    } else {
      let constant = self.read_short();
      let method_name = self.read_string(constant);
      self.invoke(receiver, method_name, 0)
    }
  }

  /// get the current value from an iterator
  fn op_iter_current(&mut self) -> Signal {
    let receiver = self.peek(0);

    if receiver.is_iter() {
      self.update_ip(2);
      let result = receiver.to_iter().current();
      self.set_val(-1, result);
      Signal::Ok
    } else {
      let constant = self.read_short();
      let method_name = self.read_string(constant);
      self.invoke(receiver, method_name, 0)
    }
  }

  /// call a function or method
  fn op_call(&mut self) -> Signal {
    let arg_count = self.read_byte();
    let callee = self.peek(arg_count as isize);

    self.resolve_call(callee, arg_count)
  }

  /// invoke a method on an instance's class
  fn op_invoke(&mut self) -> Signal {
    let constant = self.read_short();
    let arg_count = self.read_byte();

    let method_name = self.read_string(constant);
    let receiver = self.peek(arg_count as isize);

    self.invoke(receiver, method_name, arg_count)
  }

  /// invoke a method
  fn invoke(&mut self, receiver: Value, method_name: Gc<SmolStr>, arg_count: u8) -> Signal {
    if receiver.is_instance() {
      let instance = receiver.to_instance();
      if let Some(field) = instance.get_field(&method_name) {
        self.set_val(-(arg_count as isize) - 1, *field);
        return self.resolve_call(*field, arg_count);
      }
    }

    let class = self
      .dep_manager
      .primitive_classes()
      .for_value(receiver, receiver.kind());

    self.invoke_from_class(class, method_name, arg_count)
  }

  /// Invoke a method on a instance's super class
  fn op_super_invoke(&mut self) -> Signal {
    let constant = self.read_short();
    let arg_count = self.read_byte();

    let method_name = self.read_string(constant);
    let super_class = self.pop().to_class();

    self.invoke_from_class(super_class, method_name, arg_count)
  }

  /// Generate a new class
  fn op_class(&mut self) -> Signal {
    let slot = self.read_short();
    let name = self.read_string(slot);

    let class = val!(self.gc.borrow_mut().manage(Class::bare(name), self));
    self.push(class);
    Signal::Ok
  }

  fn op_inherit(&mut self) -> Signal {
    let super_class = self.peek(1);

    if !super_class.is_class() {
      return self.runtime_error(
        self.dep_manager.error_classes().runtime,
        "Superclass must be a class.",
      );
    }

    let hooks = GcHooks::new(self);
    let mut sub_class = self.peek(0).to_class();

    sub_class.inherit(&hooks, super_class.to_class());
    sub_class.meta_from_super(&hooks);

    Signal::Ok
  }

  /// Get this classes super class
  fn op_get_super(&mut self) -> Signal {
    let slot = self.read_short();
    let name = self.read_string(slot);
    let super_class = self.pop().to_class();

    self.bind_method(super_class, name)
  }

  /// Loop by performing an unconditional jump to a new instruction
  fn op_loop(&mut self) -> Signal {
    let jump = self.read_short() as isize;
    self.update_ip(-jump);
    Signal::Ok
  }

  /// Jump if the condition evaluates to a falsey value
  fn op_jump_if_false(&mut self) -> Signal {
    let jump = self.read_short();
    if is_falsey(self.peek(0)) {
      self.update_ip(jump as isize);
      return Signal::Ok;
    }

    Signal::Ok
  }

  /// Unconditionally jump to some other instruction
  fn op_jump(&mut self) -> Signal {
    let jump = self.read_short();
    self.update_ip(jump as isize);
    Signal::Ok
  }

  /// Define a global variable
  fn op_define_global(&mut self) -> Signal {
    let slot = self.read_short();
    let name = self.read_string(slot);
    let global = self.pop();
    let mut current_module = self.current_fun.module;
    current_module.insert_symbol(&GcHooks::new(self), name, global);
    Signal::Ok
  }

  fn op_set_index(&mut self) -> Signal {
    let receiver = self.peek(2_isize);

    let class = self
      .dep_manager
      .primitive_classes()
      .for_value(receiver, receiver.kind());

    match class.index_set {
      Some(index_set) => {
        let value = self.peek(1);
        let signal = self.resolve_call(index_set, 2);
        self.drop();
        self.push(value);
        signal
      }
      None => self.runtime_error(
        self.dep_manager.error_classes().method_not_found,
        &format!("No method []= on class {}.", class.name),
      ),
    }
  }

  fn op_set_global(&mut self) -> Signal {
    let slot = self.read_short();
    let string = self.read_string(slot);
    let peek = self.peek(0);

    let mut current_module = self.current_fun.module;
    if current_module
      .insert_symbol(&GcHooks::new(self), string, peek)
      .is_none()
    {
      current_module.remove_symbol(&GcHooks::new(self), string);
      return self.runtime_error(
        self.dep_manager.error_classes().property,
        &format!("Undefined variable {}", string.as_str()),
      );
    }

    Signal::Ok
  }

  fn op_set_local(&mut self) -> Signal {
    let slot = self.read_byte() as isize;
    let copy = self.peek(0);
    let slots = self.slots();

    unsafe { *slots.offset(slot) = copy }

    Signal::Ok
  }

  fn op_set_property(&mut self) -> Signal {
    let slot = self.read_short();
    let value = self.peek(1);
    let name = self.read_string(slot);

    if value.is_instance() {
      let mut instance = value.to_instance();
      let value = self.peek(0);
      instance.set_field(name, value);

      let popped = self.pop();
      self.drop();
      self.push(popped);

      return Signal::Ok;
    }

    self.runtime_error(
      self.dep_manager.error_classes().runtime,
      "Only instances have settable fields.",
    )
  }

  fn op_set_upvalue(&mut self) -> Signal {
    let slot = self.read_byte();
    let value = self.peek(0);
    let upvalue = &mut self.closure().upvalues[slot as usize];

    let open_index = match &mut *upvalue.to_upvalue() {
      Upvalue::Open(stack_ptr) => Some(*stack_ptr),
      Upvalue::Closed(store) => {
        *store = value;
        None
      }
    };

    if let Some(stack_ptr) = open_index {
      unsafe { ptr::write(stack_ptr.as_ptr(), value) }
    }

    Signal::Ok
  }

  fn op_get_index(&mut self) -> Signal {
    let receiver = self.peek(1_isize);

    let class = self
      .dep_manager
      .primitive_classes()
      .for_value(receiver, receiver.kind());

    match class.index_get {
      Some(index_get) => self.resolve_call(index_get, 1),
      None => self.runtime_error(
        self.dep_manager.error_classes().method_not_found,
        &format!("No method [] on class {}.", class.name),
      ),
    }
  }

  fn op_get_global(&mut self) -> Signal {
    let store_index = self.read_short();
    let string = self.read_string(store_index);

    match self.current_fun.module.get_symbol(string) {
      Some(gbl) => {
        let copy = *gbl;
        self.push(copy);
        Signal::Ok
      }
      None => self.runtime_error(
        self.dep_manager.error_classes().runtime,
        &format!("Undefined variable {}", string.as_str()),
      ),
    }
  }

  fn op_get_local(&mut self) -> Signal {
    let slot = self.read_byte() as isize;
    let copy = unsafe { *self.slots().offset(slot) };

    self.push(copy);
    Signal::Ok
  }

  fn op_get_upvalue(&mut self) -> Signal {
    let slot = self.read_byte();
    let upvalue_value = &self.closure().upvalues[slot as usize];

    let value = match &*upvalue_value.to_upvalue() {
      Upvalue::Open(stack_ptr) => *unsafe { stack_ptr.as_ref() },
      Upvalue::Closed(store) => *store,
    };

    self.push(value);

    Signal::Ok
  }

  fn op_get_property(&mut self) -> Signal {
    let slot = self.read_short();
    let value = self.peek(0);
    let name = self.read_string(slot);

    self.get_property(value, name)
  }

  fn get_property(&mut self, value: Value, name: Gc<SmolStr>) -> Signal {
    if value.is_instance() {
      let instance = value.to_instance();

      if let Some(value) = instance.get_field(&name) {
        self.set_val(-1, *value);
        return Signal::Ok;
      }
    }

    let class = self
      .dep_manager
      .primitive_classes()
      .for_value(value, value.kind());
    self.bind_method(class, name)
  }

  fn op_import(&mut self) -> Signal {
    let index_path = self.read_short();
    let path = self.read_string(index_path);

    let mut dep_manager = self.dep_manager;
    let current_module = self.current_fun.module;

    match dep_manager.import(&mut Hooks::new(self), current_module, path) {
      LyResult::Ok(module) => {
        self.push(val!(module.import(&GcHooks::new(self))));
        Signal::Ok
      }
      LyResult::Err(error) => self.set_error(error),
      LyResult::Exit(code) => self.set_exit(code),
    }
  }

  fn op_export(&mut self) -> Signal {
    let index = self.read_short();
    let name = self.read_string(index);
    let mut copy = self.current_fun.module;

    match copy.export_symbol(&GcHooks::new(self), name) {
      Ok(_) => Signal::Ok,
      Err(error) => self.runtime_error(self.dep_manager.error_classes().export, error.as_str()),
    }
  }

  /// return from a laythe function placing the result on top of the stack
  fn op_return(&mut self) -> Signal {
    // get the function result close upvalues and pop frame
    let result = self.pop();

    // pop a frame from the call stack return signal if provided
    if let Some(signal) = self.pop_frame() {
      return signal;
    }

    // push result onto stack
    self.push(result);
    Signal::OkReturn
  }

  fn op_negate(&mut self) -> Signal {
    let pop = self.pop();

    if pop.is_num() {
      self.push(val!(-pop.to_num()));
      Signal::Ok
    } else {
      self.runtime_error(
        self.dep_manager.error_classes().runtime,
        "Operand must be a number.",
      )
    }
  }

  fn op_not(&mut self) -> Signal {
    let value = self.pop();
    self.push(val!(is_falsey(value)));
    Signal::Ok
  }

  fn op_add(&mut self) -> Signal {
    let (right, left) = (self.pop(), self.pop());

    if right.is_num() && left.is_num() {
      self.push(val!(left.to_num() + right.to_num()));
      Signal::Ok
    } else if right.is_str() && left.is_str() {
      let left = left.to_str();
      let right = right.to_str();

      let mut buffer = String::with_capacity(left.len() + right.len());
      buffer.push_str(left.as_str());
      buffer.push_str(right.as_str());

      let string = self.gc.borrow_mut().manage_str(buffer, self);
      self.push(val!(string));
      Signal::Ok
    } else {
      self.runtime_error(
        self.dep_manager.error_classes().runtime,
        "Operands must be two numbers or two strings.",
      )
    }
  }

  fn op_sub(&mut self) -> Signal {
    let (right, left) = (self.pop(), self.pop());

    if right.is_num() && left.is_num() {
      self.push(val!(left.to_num() - right.to_num()));
      return Signal::Ok;
    }

    self.runtime_error(
      self.dep_manager.error_classes().runtime,
      "Operands must be numbers.",
    )
  }

  fn op_mul(&mut self) -> Signal {
    let (right, left) = (self.pop(), self.pop());

    if right.is_num() && left.is_num() {
      self.push(val!(left.to_num() * right.to_num()));
      return Signal::Ok;
    }

    self.runtime_error(
      self.dep_manager.error_classes().runtime,
      "Operands must be numbers.",
    )
  }

  fn op_div(&mut self) -> Signal {
    let (right, left) = (self.pop(), self.pop());

    if right.is_num() && left.is_num() {
      self.push(val!(left.to_num() / right.to_num()));
      return Signal::Ok;
    }

    self.runtime_error(
      self.dep_manager.error_classes().runtime,
      "Operands must be numbers.",
    )
  }

  fn op_less(&mut self) -> Signal {
    let (right, left) = (self.pop(), self.pop());

    if right.is_num() && left.is_num() {
      self.push(val!(left.to_num() < right.to_num()));
      return Signal::Ok;
    }

    if right.is_str() && left.is_str() {
      self.push(val!(
        (*left.to_str()).cmp(&right.to_str()) == Ordering::Less
      ));
      return Signal::Ok;
    }

    self.runtime_error(
      self.dep_manager.error_classes().runtime,
      "Operands must be numbers.",
    )
  }

  fn op_less_equal(&mut self) -> Signal {
    let (right, left) = (self.pop(), self.pop());

    if right.is_num() && left.is_num() {
      self.push(val!(left.to_num() <= right.to_num()));
      return Signal::Ok;
    }

    if right.is_str() && left.is_str() {
      if left == right {
        self.push(VALUE_TRUE);
      } else {
        self.push(val!(
          (*left.to_str()).cmp(&right.to_str()) == Ordering::Less
        ));
      }

      return Signal::Ok;
    }

    self.runtime_error(
      self.dep_manager.error_classes().runtime,
      "Operands must be numbers or strings.",
    )
  }

  fn op_greater(&mut self) -> Signal {
    let (right, left) = (self.pop(), self.pop());

    if right.is_num() && left.is_num() {
      self.push(val!(left.to_num() > right.to_num()));
      return Signal::Ok;
    }

    if right.is_str() && left.is_str() {
      self.push(val!(
        (*left.to_str()).cmp(&right.to_str()) == Ordering::Greater
      ));
      return Signal::Ok;
    }

    self.runtime_error(
      self.dep_manager.error_classes().runtime,
      "Operands must be numbers or strings.",
    )
  }

  fn op_greater_equal(&mut self) -> Signal {
    let (right, left) = (self.pop(), self.pop());

    if right.is_num() && left.is_num() {
      self.push(val!(left.to_num() >= right.to_num()));
      return Signal::Ok;
    }

    if right.is_str() && left.is_str() {
      if left == right {
        self.push(VALUE_TRUE);
      } else {
        self.push(val!(
          (*left.to_str()).cmp(&right.to_str()) == Ordering::Greater
        ));
      }

      return Signal::Ok;
    }

    self.runtime_error(
      self.dep_manager.error_classes().runtime,
      "Operands must be numbers or strings.",
    )
  }

  fn op_equal(&mut self) -> Signal {
    let right = self.pop();
    let left = self.pop();

    self.push(val!(left == right));
    Signal::Ok
  }

  fn op_not_equal(&mut self) -> Signal {
    let right = self.pop();
    let left = self.pop();

    self.push(val!(left != right));
    Signal::Ok
  }

  fn op_method(&mut self) -> Signal {
    let slot = self.read_short();
    let name = self.read_string(slot);

    let class = self.peek(1);
    let method = self.peek(0);

    if class.is_class() && method.is_closure() {
      class
        .to_class()
        .add_method(&GcHooks::new(self), name, method);
    } else {
      self.internal_error("Invalid Stack for op_method.");
    }

    self.drop();
    Signal::Ok
  }

  fn op_field(&mut self) -> Signal {
    let slot = self.read_short();
    let name = self.read_string(slot);

    let class = self.peek(0);

    if class.is_class() {
      class.to_class().add_field(&GcHooks::new(self), name);
    } else {
      self.internal_error("Invalid Stack for op_method.");
    }

    Signal::Ok
  }

  fn op_static_method(&mut self) -> Signal {
    let slot = self.read_short();
    let name = self.read_string(slot);

    let class = self.peek(1);
    let method = self.peek(0);

    if class.is_class() && method.is_closure() {
      match class.to_class().meta() {
        Some(mut meta) => {
          meta.add_method(&GcHooks::new(self), name, method);
        }
        None => self.internal_error(&format!("{} meta class not set.", class.to_class().name)),
      }
    } else {
      self.internal_error("Invalid Stack for op_static_method.");
    }

    self.drop();
    Signal::Ok
  }

  fn op_closure(&mut self) -> Signal {
    let slot = self.read_short();
    let fun = self.read_constant(slot).to_fun();
    let mut closure = self.gc.borrow_mut().manage(Closure::new(fun), self);
    self.gc.borrow_mut().push_root(closure);

    for i in 0..fun.upvalue_count {
      let upvalue_index: UpvalueIndex = unsafe { mem::transmute(self.read_short()) };

      match upvalue_index {
        UpvalueIndex::Local(index) => {
          let value = unsafe { &*self.slots().offset(index as isize) };
          closure.upvalues[i] = self.capture_upvalue(NonNull::from(value))
        }
        UpvalueIndex::Upvalue(upvalue) => {
          let upvalue = self.closure().upvalues[upvalue as usize];
          closure.upvalues[i] = upvalue
        }
      }
    }

    self.gc.borrow_mut().pop_roots(1);
    self.push(val!(closure));
    Signal::Ok
  }

  fn op_close_upvalue(&mut self) -> Signal {
    self.close_upvalues(NonNull::from(unsafe { &*self.stack_top.offset(-1) }));
    self.drop();
    Signal::Ok
  }

  fn op_constant(&mut self) -> Signal {
    let slot = self.read_byte();
    let constant = self.read_constant(slot as u16);
    self.push(constant);

    Signal::Ok
  }

  fn op_constant_long(&mut self) -> Signal {
    let slot = self.read_short();
    let constant = self.read_constant(slot);
    self.push(constant);

    Signal::Ok
  }

  fn resolve_call(&mut self, callee: Value, arg_count: u8) -> Signal {
    match callee.kind() {
      ValueKind::Closure => self.call(callee.to_closure(), arg_count),
      ValueKind::Method => self.call_method(callee.to_method(), arg_count),
      ValueKind::Native => self.call_native(callee.to_native(), arg_count),
      ValueKind::Class => self.call_class(callee.to_class(), arg_count),
      ValueKind::Fun => self.internal_error(&format!(
        "Function {} was not wrapped in a closure.",
        callee.to_fun().name
      )),
      _ => {
        let class_name = self.get_class(callee).to_class().name;
        self.runtime_error(
          self.dep_manager.error_classes().runtime,
          &format!("{} is not callable.", class_name),
        )
      }
    }
  }

  fn call_class(&mut self, class: Gc<Class>, arg_count: u8) -> Signal {
    let instance = val!(self.gc.borrow_mut().manage(Instance::new(class), self));
    self.set_val(-(arg_count as isize) - 1, instance);

    match class.init {
      Some(init) => self.resolve_call(init, arg_count),
      None => {
        if arg_count != 0 {
          self.runtime_error(
            self.dep_manager.error_classes().runtime,
            &format!("Expected 0 arguments but got {}", arg_count),
          )
        } else {
          Signal::Ok
        }
      }
    }
  }

  /// call a native function immediately returning the result
  fn call_native(&mut self, native: Gc<Box<dyn Native>>, arg_count: u8) -> Signal {
    let meta = native.meta();

    let args = unsafe {
      std::slice::from_raw_parts(
        self.stack_top.offset(-(arg_count as isize)),
        arg_count as usize,
      )
    };

    // check that the current function is called with the right number of args and types
    if let Some(signal) = self.check_native_arity(meta, args) {
      return signal;
    }

    let this = if meta.is_method {
      Some(self.get_val(-(arg_count as isize) - 1))
    } else {
      None
    };

    match meta.environment {
      Environment::StackLess => match native.call(&mut Hooks::new(self), this, args) {
        Call::Ok(value) => {
          self.stack_top = unsafe { self.stack_top.offset(-(arg_count as isize) - 1) };
          self.push(value);

          Signal::OkReturn
        }
        Call::Err(error) => self.set_error(error),
        Call::Exit(code) => self.set_exit(code),
      },
      Environment::Normal => {
        let native_closure = self
          .gc
          .borrow_mut()
          .manage(Closure::new(self.native_fun_stub), self);
        self.push_frame(native_closure, arg_count);

        match native.call(&mut Hooks::new(self), this, args) {
          Call::Ok(value) => {
            self.pop_frame();
            self.push(value);

            Signal::OkReturn
          }
          Call::Err(error) => self.set_error(error),
          Call::Exit(code) => self.set_exit(code),
        }
      }
    }
  }

  /// call a laythe function setting it as the new call frame
  fn call(&mut self, closure: Gc<Closure>, arg_count: u8) -> Signal {
    // check that the current function is called with the right number of args
    if let Some(error) = self.check_arity(closure.fun, arg_count) {
      return error;
    }

    // set the current current instruction pointer. check for overflow
    if self.frame_count == FRAME_MAX {
      return self.runtime_error(self.dep_manager.error_classes().runtime, "Stack overflow.");
    }

    self.push_frame(closure, arg_count);
    Signal::Ok
  }

  /// Push a call frame onto the the call frame stack
  #[inline]
  fn push_frame(&mut self, closure: Gc<Closure>, arg_count: u8) {
    unsafe {
      self.store_ip();
      let frame = &mut *self.current_frame.offset(1);
      frame.closure = closure;
      frame.ip = closure.fun.chunk().instructions.get_unchecked(0) as *const u8;
      frame.slots = self.stack_top.offset(-(arg_count as isize) - 1);
      self.current_frame = frame as *mut CallFrame;
      self.load_ip();
    }

    self.current_fun = closure.fun;
    self.frame_count += 1;
  }

  /// Pop a frame off the call stack
  #[inline]
  fn pop_frame(&mut self) -> Option<Signal> {
    self.close_upvalues(NonNull::from(unsafe { &*self.slots() }));
    self.frame_count -= 1;

    // if the frame was the whole script signal an ok interrupt
    if self.frame_count == 1 {
      self.drop();
      return Some(Signal::Exit);
    }

    // pull the current frame out of the stack and set the cached frame
    unsafe {
      self.stack_top = self.slots();
      self.current_frame = self.current_frame.offset(-1);
      self.current_fun = self.closure().fun;
      self.load_ip();
    }

    None
  }

  /// check that the number of args is valid for the function arity
  fn check_arity(&mut self, fun: Gc<Fun>, arg_count: u8) -> Option<Signal> {
    match fun.arity.check(arg_count) {
      Ok(_) => None,
      Err(error) => match error {
        ArityError::Fixed(arity) => Some(self.runtime_error(
          self.dep_manager.error_classes().runtime,
          &format!(
            "{} expected {} argument(s) but got {}.",
            fun.name, arity, arg_count,
          ),
        )),
        ArityError::Variadic(arity) => Some(self.runtime_error(
          self.dep_manager.error_classes().runtime,
          &format!(
            "{} expected at least {} argument(s) but got {}.",
            fun.name, arity, arg_count,
          ),
        )),
        ArityError::DefaultLow(arity) => Some(self.runtime_error(
          self.dep_manager.error_classes().runtime,
          &format!(
            "{} expected at least {} argument(s) but got {}.",
            fun.name, arity, arg_count,
          ),
        )),
        ArityError::DefaultHigh(arity) => Some(self.runtime_error(
          self.dep_manager.error_classes().runtime,
          &format!(
            "{} expected at most {} argument(s) but got {}.",
            fun.name, arity, arg_count,
          ),
        )),
      },
    }
  }

  fn check_native_arity(&mut self, native_meta: &NativeMeta, args: &[Value]) -> Option<Signal> {
    match native_meta.signature.check(args) {
      Ok(()) => None,
      Err(err) => {
        let callable_type = if native_meta.is_method {
          "method"
        } else {
          "function"
        };

        match err {
          SignatureError::LengthFixed(expected) => Some(self.runtime_error(
            self.dep_manager.error_classes().runtime,
            &format!(
              "{} {} expected {} argument(s) but received {}.",
              callable_type,
              native_meta.name,
              expected,
              args.len(),
            ),
          )),
          SignatureError::LengthVariadic(expected) => Some(self.runtime_error(
            self.dep_manager.error_classes().runtime,
            &format!(
              "{} {} expected at least {} argument(s) but received {}.",
              callable_type,
              native_meta.name,
              expected,
              args.len(),
            ),
          )),
          SignatureError::LengthDefaultLow(expected) => Some(self.runtime_error(
            self.dep_manager.error_classes().runtime,
            &format!(
              "{} {} expected at least {} argument(s) but received {}.",
              callable_type,
              native_meta.name,
              expected,
              args.len(),
            ),
          )),
          SignatureError::LengthDefaultHigh(expected) => Some(self.runtime_error(
            self.dep_manager.error_classes().runtime,
            &format!(
              "{} {} expected at most {} argument(s) but received {}.",
              callable_type,
              native_meta.name,
              expected,
              args.len(),
            ),
          )),
          SignatureError::TypeWrong(parameter) => Some(self.runtime_error(
            self.dep_manager.error_classes().runtime,
            &format!(
              "{} {}'s parameter {} required a {} but received a {}.",
              callable_type,
              native_meta.name,
              native_meta.signature.parameters[parameter as usize].name,
              native_meta.signature.parameters[parameter as usize].kind,
              ParameterKind::from(args[parameter as usize])
            ),
          )),
        }
      }
    }
  }

  /// Call a bound method
  fn call_method(&mut self, bound: Gc<Method>, arg_count: u8) -> Signal {
    self.set_val(-(arg_count as isize) - 1, bound.receiver);
    self.resolve_call(bound.method, arg_count)
  }

  /// bind a method to an instance
  fn bind_method(&mut self, class: Gc<Class>, name: Gc<SmolStr>) -> Signal {
    match class.get_method(&name) {
      Some(method) => {
        let bound = self
          .gc
          .borrow_mut()
          .manage(Method::new(self.peek(0), method), self);
        self.set_val(-1, val!(bound));
        Signal::Ok
      }
      None => self.runtime_error(
        self.dep_manager.error_classes().runtime,
        &format!("Undefined property {}", name.as_str()),
      ),
    }
  }

  /// invoke a method from the provided class
  fn invoke_from_class(
    &mut self,
    class: Gc<Class>,
    method_name: Gc<SmolStr>,
    arg_count: u8,
  ) -> Signal {
    match class.get_method(&method_name) {
      Some(method) => self.resolve_call(method, arg_count),
      None => self.runtime_error(
        self.dep_manager.error_classes().property,
        &format!(
          "Undefined property {} on class {}.",
          method_name, class.name
        ),
      ),
    }
  }

  /// capture an upvalue return an existing upvalue if already captured
  fn capture_upvalue(&mut self, local_index: NonNull<Value>) -> Value {
    let closest_upvalue = self
      .open_upvalues
      .iter()
      .rev()
      .find(|upvalue| match ***upvalue {
        Upvalue::Open(index) => index <= local_index,
        Upvalue::Closed(_) => self.internal_error("Encountered closed upvalue."),
      });

    if let Some(upvalue) = closest_upvalue {
      if let Upvalue::Open(index) = **upvalue {
        if index == local_index {
          return val!(*upvalue);
        }
      }
    }

    let created_upvalue = self
      .gc
      .borrow_mut()
      .manage(Upvalue::Open(local_index), self);
    self.open_upvalues.push(created_upvalue);

    val!(created_upvalue)
  }

  /// hoist all open upvalue above the last index
  fn close_upvalues(&mut self, last_value: NonNull<Value>) {
    if !self.open_upvalues.is_empty() {
      let mut retain = self.open_upvalues.len();

      for upvalue in self.open_upvalues.iter_mut().rev() {
        let value = match **upvalue {
          Upvalue::Open(value) => value,
          Upvalue::Closed(_) => self.internal_error("Unexpected closed upvalue."),
        };

        if value < last_value {
          break;
        }

        retain -= 1;
        upvalue.hoist();
      }

      self.open_upvalues.truncate(retain)
    }
  }

  /// Print debugging information for a caught exceptions
  #[cfg(feature = "debug")]
  fn print_exception_debug(&self, frame: &CallFrame, idx: usize) -> io::Result<()> {
    let mut stdio = self.io.stdio();
    let mut stdout = stdio.stdout();

    exception_catch(&mut stdout, frame, idx)
  }

  /// Print debugging information for the current instruction
  #[cfg(feature = "debug")]
  fn print_state(&self, ip: *const u8) -> io::Result<usize> {
    let mut stdio = self.io.stdio();

    self.print_stack_debug(&mut stdio)?;

    #[cfg(feature = "debug_upvalue")]
    self.print_upvalue_debug(&mut stdio)?;

    let start = &self.current_fun.chunk().instructions[0] as *const u8;
    let offset = ptr_len(start, ip);
    disassemble_instruction(&mut stdio, &self.current_fun.chunk(), offset, true)
  }

  /// Print the current stack
  #[cfg(feature = "debug")]
  fn print_stack_debug(&self, stdio: &mut Stdio) -> io::Result<()> {
    let stdout = stdio.stdout();

    if self.frame_count > 2 {
      write!(stdout, "Frame Stack:  ")?;
      for frame in self.frames[1..(self.frame_count)].iter() {
        let fun = frame.closure.fun;
        write!(stdout, "[ {}:{} ]", *fun.module.name(), *fun.name)?;
      }
      writeln!(stdout)?;
    }

    write!(stdout, "Local Stack:  ")?;
    unsafe {
      let start = self.slots();
      let len = ptr_len(start, self.stack_top);
      let slice = std::slice::from_raw_parts(start, len);

      for value in slice {
        let s = value.to_string();
        write!(
          stdout,
          "[ {} ]",
          s.chars().into_iter().take(60).collect::<String>()
        )?;
      }
    }

    writeln!(stdout)
  }

  /// Print the current upvalues
  #[cfg(feature = "debug_upvalue")]
  fn print_upvalue_debug(&self, stdio: &mut Stdio) -> io::Result<()> {
    let stdout = stdio.stdout();

    write!(stdout, "Open UpVal:   ")?;
    for upvalue in &self.open_upvalues {
      match &**upvalue {
        Upvalue::Open(stack_ptr) => {
          write!(stdout, "[ stack {} ]", unsafe { *stack_ptr.as_ref() })?;
        }
        Upvalue::Closed(closed) => {
          write!(stdout, "[ heap {} ]", closed)?;
        }
      }
    }

    writeln!(stdout)
  }

  /// Convert an execute result to a call result
  fn to_call_result(&self, execute_result: ExecuteResult) -> Call {
    match execute_result {
      ExecuteResult::FunResult(value) => Call::Ok(value),
      ExecuteResult::Ok => self.internal_error("Accidental early exit in hook call."),
      ExecuteResult::CompileError => {
        self.internal_error("Compiler error should occur before code is executed.")
      }
      ExecuteResult::RuntimeError => match self.current_error {
        Some(error) => Call::Err(error),
        None => self.internal_error("Error not set on vm executor."),
      },
      ExecuteResult::InternalError => self.internal_error("Internal error encountered"),
    }
  }

  /// Report an internal issue to the user
  fn internal_error(&self, message: &str) -> ! {
    panic!(format!("Internal Error: {}", message))
  }

  /// Report a known laythe runtime error to the user
  fn runtime_error(&mut self, error: Gc<Class>, message: &str) -> Signal {
    let error_message = val!(self.gc.borrow_mut().manage_str(message, self));
    self.push(error_message);

    let mode = ExecuteMode::CallFunction(self.frame_count);
    let result = match self.resolve_call(val!(error), 1) {
      Signal::Ok => self.execute(mode),
      Signal::OkReturn => ExecuteResult::FunResult(self.pop()),
      Signal::RuntimeError => ExecuteResult::RuntimeError,
      _ => self.internal_error("Unexpected signal in run_fun."),
    };

    match result {
      ExecuteResult::FunResult(error) => {
        if error.is_instance() {
          self.set_error(error.to_instance())
        } else {
          self.internal_error("Failed to construct error")
        }
      }
      _ => self.internal_error("Failed to construct error"),
    }
  }

  /// Set the current error place the vm signal a runtime error
  fn set_error(&mut self, error: Gc<Instance>) -> Signal {
    self.current_error = Some(error);
    Signal::RuntimeError
  }

  /// Set the current error place the vm signal a runtime error
  fn set_exit(&mut self, code: u16) -> Signal {
    self.exit_code = code;
    Signal::Exit
  }

  /// Search for a catch block up the stack, printing the error if no catch is found
  fn stack_unwind(&mut self, error: Gc<Instance>) -> Option<ExecuteResult> {
    let mut popped_frames = None;
    let mut stack_top = None;
    self.store_ip();

    // travel down stack frames looking for an applicable handle
    for (idx, frame) in self.frames[1..self.frame_count]
      .iter_mut()
      .rev()
      .enumerate()
    {
      let fun = frame.closure.fun;
      let instructions = &*fun.chunk().instructions;

      let offset = ptr_len(&instructions[0], frame.ip);
      if let Some(offset) = fun.has_catch_jump(offset as u16) {
        // if found set all the stack information to the correct state
        frame.ip = &instructions[offset as usize] as *const u8;

        self.frame_count -= idx;
        self.current_frame = frame as *mut CallFrame;
        self.current_fun = frame.closure.fun;
        self.ip = frame.ip;

        if let Some(top) = stack_top {
          self.stack_top = top;
        }

        // track frames popped for debugging info
        popped_frames = Some(idx);
        break;
      }

      stack_top = Some(frame.slots);
    }

    match popped_frames {
      Some(_idx) => {
        #[cfg(feature = "debug")]
        {
          let frame = unsafe { &*self.current_frame };
          if let Err(_) = self.print_exception_debug(frame, _idx) {
            return Some(ExecuteResult::InternalError);
          }
        }

        None
      }
      None => {
        self.print_error(error);
        Some(ExecuteResult::RuntimeError)
      }
    }
  }

  /// Print an error message and the current call stack to the user
  fn print_error(&mut self, error: Gc<Instance>) {
    let message = error[0].to_str();

    let mut stdio = self.io.stdio();
    let stderr = stdio.stderr();
    writeln!(stderr, "{}: {}", error.class.name, message).expect("Unable to write to stderr");

    for frame in self.frames[1..self.frame_count].iter().rev() {
      let closure = &frame.closure;
      let location: String = match &**closure.fun.name {
        SCRIPT => SCRIPT.to_owned(),
        _ => format!("{}()", closure.fun.name),
      };

      let offset = ptr_len(&closure.fun.chunk().instructions[0], frame.ip);
      writeln!(
        stderr,
        "  [line {}] in {}",
        closure.fun.chunk().get_line(offset),
        location
      )
      .expect("Unable to write to stderr");
    }

    self.reset_stack();
  }
}

impl From<VmDependencies> for Vm {
  /// Construct a vm from a set of dependencies. This is meant to be used when targeting
  /// different environments
  fn from(dependencies: VmDependencies) -> Self {
    let no_gc_context = NoContext::new(dependencies.gc);
    let hooks = GcHooks::new(&no_gc_context);

    let std_lib = dependencies.std_lib;
    let global = std_lib
      .import(
        &hooks,
        Import::new(vec![hooks.manage_str(STD), hooks.manage_str(GLOBAL)]),
      )
      .expect("Could not retrieve global module");

    let fun = Fun::new(hooks.manage_str(PLACEHOLDER_NAME), global);

    let managed_fun = hooks.manage(fun);
    let closure = hooks.manage(Closure::new(managed_fun));

    let mut frames = vec![CallFrame::new(closure); FRAME_MAX];
    let mut stack = vec![VALUE_NIL; DEFAULT_STACK_MAX];

    let builtin = builtin_from_module(&hooks, &global)
      .expect("Failed to generate builtin class from global module");

    let cwd = dependencies
      .io
      .env()
      .current_dir()
      .expect("Could not obtain the current working directory.");

    let mut dep_manager = hooks.manage(DepManager::new(
      dependencies.io.clone(),
      builtin,
      hooks.manage(cwd),
    ));
    dep_manager.add_package(&hooks, std_lib);

    let current_frame = &mut frames[0];
    let current_fun = current_frame.closure.fun;
    let stack_top = &mut stack[1] as *mut Value;
    let ip = ptr::null();

    let current_frame = current_frame as *mut CallFrame;
    let mut native_fun_stub = hooks.manage(Fun::new(hooks.manage_str("native"), global));
    native_fun_stub.write_instruction(&hooks, AlignedByteCode::Nil, 0);

    let gc = RefCell::new(no_gc_context.done());

    Vm {
      io: dependencies.io,
      stack,
      frames,
      gc,
      dep_manager,
      global,
      frame_count: 1,
      script: None,
      current_fun,
      current_frame,
      current_error: None,
      exit_code: 0,
      stack_top,
      ip,
      native_fun_stub,
      open_upvalues: Vec::with_capacity(100),
    }
  }
}

impl RootTrace for Vm {
  fn trace(&self) -> bool {
    self.script.map(|script| script.trace());

    unsafe {
      let start = &self.stack[0] as *const Value;
      let len = ptr_len(start, self.stack_top);
      let slice = std::slice::from_raw_parts(start, len);

      slice.iter().for_each(|value| {
        value.trace();
      });
    }

    self.frames[0..self.frame_count].iter().for_each(|frame| {
      frame.closure.trace();
    });

    self.open_upvalues.iter().for_each(|upvalue| {
      upvalue.trace();
    });

    self.dep_manager.trace();
    self.native_fun_stub.trace();
    self.global.trace();
    self.current_error.map(|error| error.trace());

    true
  }

  fn trace_debug(&self, stdout: &mut dyn Write) -> bool {
    self.script.map(|script| script.trace_debug(stdout));

    unsafe {
      let start = &self.stack[0] as *const Value;
      let len = ptr_len(start, self.stack_top) + 1;
      let slice = std::slice::from_raw_parts(start, len);

      slice.iter().for_each(|value| {
        value.trace_debug(stdout);
      });
    }

    self.frames[0..self.frame_count].iter().for_each(|frame| {
      frame.closure.trace_debug(stdout);
    });

    self.open_upvalues.iter().for_each(|upvalue| {
      upvalue.trace_debug(stdout);
    });

    self.dep_manager.trace_debug(stdout);
    self.native_fun_stub.trace_debug(stdout);
    self.global.trace_debug(stdout);
    self.current_error.map(|error| error.trace_debug(stdout));

    true
  }
}

impl HookContext for Vm {
  fn gc_context(&self) -> &dyn GcContext {
    self
  }

  fn value_context(&mut self) -> &mut dyn ValueContext {
    self
  }

  fn io(&mut self) -> Io {
    self.io.clone()
  }
}

impl GcContext for Vm {
  fn gc(&self) -> std::cell::RefMut<'_, Allocator> {
    self.gc.borrow_mut()
  }
}

impl ValueContext for Vm {
  fn call(&mut self, callable: Value, args: &[Value]) -> Call {
    let result = self.run_fun(callable, args);
    self.to_call_result(result)
  }

  fn call_method(&mut self, this: Value, method: Value, args: &[Value]) -> Call {
    let result = self.run_method(this, method, args);
    self.to_call_result(result)
  }

  fn get_method(&mut self, this: Value, method_name: Gc<SmolStr>) -> Call {
    self.get_method(this, method_name)
  }

  fn get_class(&mut self, this: Value) -> Value {
    self.get_class(this)
  }
}
