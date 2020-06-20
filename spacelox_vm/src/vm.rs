use crate::{
  call_frame::CallFrame,
  compiler::{Compiler, CompilerResult, Parser},
  constants::{DEFAULT_STACK_MAX, FRAME_MAX, REPL_MODULE},
  dep_manager::DepManager,
};
use spacelox_core::{
  chunk::{ByteCode, UpvalueIndex},
  constants::{PLACEHOLDER_NAME, SCRIPT},
  hooks::{CallContext, GcContext, GcHooks, HookContext, Hooks, NoContext},
  module::Module,
  native::{NativeFun, NativeMeta, NativeMethod},
  object::SlHashMap,
  object::{Class, Closure, Fun, Instance, Method, SlVec, Upvalue},
  package::{Import, Package},
  signature::{ArityError, ParameterKind},
  utils::{is_falsey, ptr_len, use_sentinel_nan},
  value::{Value, ValueVariant, VALUE_NIL},
  CallResult, SlError,
};
use spacelox_env::{
  env::EnvIo,
  fs::FsIo,
  io::{Io, NativeIo},
  managed::{Managed, Trace},
  memory::{Gc, NO_GC},
  stdio::StdIo,
};
use spacelox_lib::{create_std_lib, global::builtin_from_global_module, GLOBAL, STD};
use std::convert::TryInto;
use std::mem;
use std::ptr;
use std::{path::PathBuf, ptr::NonNull};

#[cfg(feature = "debug")]
use crate::debug::disassemble_instruction;

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
  RuntimeError,
  CompileError,
}

pub enum RunMode {
  Normal,
  CallFunction(usize),
}

pub fn default_native_vm() -> Vm<NativeIo> {
  let io = NativeIo();
  Vm::new(io)
}

/// A set of dependencies needed by the virtual machine
pub struct VmDependencies<I: Io + 'static> {
  /// access to the environments io
  io: I,

  /// The garbage collector
  gc: Gc,

  /// The value stack
  stack: Vec<Value>,

  /// The callframe stack
  frames: Vec<CallFrame>,

  /// The native functions
  std_lib: Managed<Package>,
}

/// The virtual machine for the spacelox programming language
pub struct Vm<I: Io + 'static> {
  /// A stack holding all local variable currently in use
  stack: Vec<Value>,

  /// A stack holding call frames currently in use
  frames: Vec<CallFrame>,

  /// The vm's garbage collector
  gc: Gc,

  /// The environments io access
  io: I,

  /// an object to manage the current dependencies
  dep_manager: Managed<DepManager<I>>,

  /// The global module
  global: Managed<Module>,
}

impl<I: Io> Vm<I> {
  pub fn new(io: I) -> Vm<I> {
    let gc = Gc::new(Box::new(io.stdio()));
    let mut no_gc_context = NoContext::new(&gc);
    let hooks = GcHooks::new(&mut no_gc_context);

    let cwd = io
      .envio()
      .current_dir()
      .expect("Could not obtain the current working directory.");

    let module = hooks.manage(Module::new(
      hooks.manage_str(PLACEHOLDER_NAME.to_string()),
      hooks.manage(PathBuf::from(PLACEHOLDER_NAME)),
    ));
    let fun = Fun::new(hooks.manage_str(String::from(PLACEHOLDER_NAME)), module);

    let managed_fun = hooks.manage(fun);
    let closure = hooks.manage(Closure::new(managed_fun));

    let frames = vec![CallFrame::new(closure); FRAME_MAX];
    let stack = vec![VALUE_NIL; DEFAULT_STACK_MAX];

    let std_lib = create_std_lib(&hooks).expect("Standard library creation failed");
    let global = std_lib
      .import(
        &hooks,
        Import::new(vec![
          hooks.manage_str(STD.to_string()),
          hooks.manage_str(GLOBAL.to_string()),
        ]),
      )
      .expect("Could not retrieve global module");

    let builtin = builtin_from_global_module(&hooks, &global)
      .expect("Failed to generate builtin class from global module");

    let mut dep_manager = hooks.manage(DepManager::new(io, builtin, hooks.manage(cwd)));
    dep_manager.add_package(std_lib);

    Vm {
      io,
      stack,
      frames,
      gc,
      dep_manager,
      global,
    }
  }

  /// Start the interactive repl
  pub fn repl(&mut self) {
    let stdio = self.io.stdio();

    let main_module = self
      .main_module(self.dep_manager.src_dir.join(PathBuf::from(REPL_MODULE)))
      .expect("TODO");

    loop {
      let mut buffer = String::new();

      stdio.print("> ");
      stdio.flush().expect("Could not write to stdout");

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
    match self.io.fsio().canonicalize(&module_path) {
      Ok(module_path) => {
        let mut directory = module_path.clone();
        directory.pop();

        self.dep_manager.src_dir = self.gc.manage(directory, &NO_GC);
        let main_module = match self.main_module(module_path) {
          Ok(module) => module,
          Err(err) => {
            return err;
          }
        };

        self.interpret(main_module, source)
      }
      Err(err) => {
        self.io.stdio().println(&err.message);
        ExecuteResult::RuntimeError
      }
    }
  }

  /// Interpret the provided spacelox script returning the execution result
  fn interpret(&mut self, main_module: Managed<Module>, source: &str) -> ExecuteResult {
    match self.compile(main_module, source) {
      Ok(fun) => {
        let script_closure = self.gc.manage(Closure::new(fun), &NO_GC);
        let script = Value::from(script_closure);

        let mut executor = VmExecutor::new(self, script);
        executor.run(RunMode::Normal)
      }
      Err(()) => ExecuteResult::CompileError,
    }
  }

  /// Compile the provided spacelox source into the virtual machine's bytecode
  fn compile(&mut self, module: Managed<Module>, source: &str) -> CompilerResult {
    let mut parser = Parser::new(self.io.stdio(), source);

    let mut compiler_context = NoContext::new(&self.gc);
    let hooks = GcHooks::new(&mut compiler_context);

    let compiler = Compiler::new(module, self.io, &mut parser, &hooks);
    compiler.compile()
  }

  fn main_module(&self, module_path: PathBuf) -> Result<Managed<Module>, ExecuteResult> {
    let no_gc_context = NoContext::new(&self.gc);
    let hooks = GcHooks::new(&no_gc_context);

    let module = match Module::from_path(&hooks, hooks.manage(module_path)) {
      Some(module) => module,
      None => {
        self
          .io
          .stdio()
          .println("Unable to extract filename from script path.");
        return Err(ExecuteResult::RuntimeError);
      }
    };
    let mut module = hooks.manage(module);

    match self.global.transfer_exported(&hooks, &mut module) {
      Ok(_) => {}
      Err(_) => {
        self.io.stdio().println("Transfer global module failed.");
        return Err(ExecuteResult::RuntimeError);
      }
    }

    Ok(module)
  }
}

impl<I: Io> From<VmDependencies<I>> for Vm<I> {
  /// Construct a vm from a set of dependencies. This is meant to be used when targeting
  /// different environments
  fn from(dependencies: VmDependencies<I>) -> Self {
    let gc = dependencies.gc;
    let mut no_gc_context = NoContext::new(&gc);
    let hooks = GcHooks::new(&mut no_gc_context);

    let std_lib = dependencies.std_lib;
    let global = std_lib
      .import(
        &hooks,
        Import::new(vec![
          hooks.manage_str(STD.to_string()),
          hooks.manage_str(GLOBAL.to_string()),
        ]),
      )
      .expect("Could not retrieve global module");

    let builtin = builtin_from_global_module(&hooks, &global)
      .expect("Failed to generate builtin class from global module");

    let cwd = dependencies
      .io
      .envio()
      .current_dir()
      .expect("Could not obtain the current working directory.");

    let mut dep_manager =
      hooks.manage(DepManager::new(dependencies.io, builtin, hooks.manage(cwd)));
    dep_manager.add_package(std_lib);

    Vm {
      io: dependencies.io,
      stack: dependencies.stack,
      frames: dependencies.frames,
      gc,
      dep_manager,
      global,
    }
  }
}

struct VmExecutor<'a, I: Io + 'static> {
  /// A stack of call frames for the current execution
  frames: &'a mut Vec<CallFrame>,

  /// A stack holding all local variable currently in use
  stack: &'a mut [Value],

  /// global variable present in the vm
  global: Managed<Module>,

  /// the standard lib package
  dep_manager: Managed<DepManager<I>>,

  /// A reference to a object currently in the vm
  gc: &'a mut Gc,

  /// The environments io access
  io: &'a I,

  /// A collection of currently available upvalues
  open_upvalues: Vec<Managed<Upvalue>>,

  /// the main script level function
  script: Value,

  /// The current frame's function
  current_fun: Managed<Fun>,

  /// The current frame's closure
  current_frame: *mut CallFrame,

  /// The current error if one is active
  current_error: Option<SlError>,

  /// index to the top of the value stack
  stack_top: *mut Value,

  /// The current frame depth of the program
  frame_count: usize,
}

impl<'a, I: Io> VmExecutor<'a, I> {
  /// Create an instance of the vm executor that can execute the provided script.
  pub fn new(vm: &'a mut Vm<I>, script: Value) -> VmExecutor<'a, I> {
    let current_frame = { &mut vm.frames[0] };
    let current_fun = current_frame.closure.fun;
    let stack_top = &mut vm.stack[1] as *mut Value;

    let current_frame = current_frame as *mut CallFrame;
    let mut executor = VmExecutor {
      frames: &mut vm.frames,
      frame_count: 1,
      stack: &mut vm.stack,
      script,
      current_fun,
      current_frame,
      current_error: None,
      gc: &mut vm.gc,
      io: &mut vm.io,
      stack_top,
      global: vm.global,
      dep_manager: vm.dep_manager,
      open_upvalues: Vec::with_capacity(100),
    };

    executor.set_ip(&executor.script.to_closure().fun.chunk().instructions[0]);
    let result = executor.call(executor.script.to_closure(), 0);

    let mut current_module = executor.current_fun.module;
    vm.global
      .transfer_exported(&GcHooks::new(&mut executor), &mut current_module)
      .expect("Transfer global module failed");

    match result {
      Signal::Ok => executor,
      _ => executor.internal_error("Main script call failed."),
    }
  }

  /// Run a spacelox function on top of the current stack.
  /// This acts as a hook for native functions to execute spacelox function
  fn run_fun(&mut self, callable: Value, args: &[Value]) -> ExecuteResult {
    for arg in args {
      self.push(*arg);
    }

    let mode = RunMode::CallFunction(self.frame_count);
    match self.resolve_call(callable, args.len() as u8) {
      Signal::Ok => self.run(mode),
      Signal::OkReturn => ExecuteResult::FunResult(self.pop()),
      Signal::RuntimeError => ExecuteResult::RuntimeError,
      _ => self.internal_error("Unexpected signal in run_fun."),
    }
  }

  /// Run a spacelox method on top of the current stack.
  /// This acts as a hook for native functions to execute spacelox function
  fn run_method(&mut self, this: Value, method: Value, args: &[Value]) -> ExecuteResult {
    self.push(this);
    for arg in args {
      self.push(*arg);
    }

    let mode = RunMode::CallFunction(self.frame_count);
    match self.resolve_call(method, args.len() as u8) {
      Signal::Ok => self.run(mode),
      Signal::OkReturn => ExecuteResult::FunResult(self.pop()),
      Signal::RuntimeError => ExecuteResult::RuntimeError,
      _ => self.internal_error("Unexpected signal in run_method."),
    }
  }

  /// Run a method on a spacelox value on top of the current stack.
  /// This acts as a hook for native function to execute spacelox methods
  pub fn run_method_by_name(
    &mut self,
    this: Value,
    method_name: Managed<String>,
    args: &[Value],
  ) -> ExecuteResult {
    let class = this.value_class(self.dep_manager.primitive_classes());

    self.push(this);
    for arg in args {
      self.push(*arg);
    }

    let mode = RunMode::CallFunction(self.frame_count);
    match self.invoke_from_class(class, method_name, args.len() as u8) {
      Signal::Ok => self.run(mode),
      Signal::OkReturn => ExecuteResult::FunResult(self.pop()),
      Signal::RuntimeError => ExecuteResult::RuntimeError,
      _ => self.internal_error("Unexpected signal in run_method_by_name."),
    }
  }

  /// Main virtual machine execution loop. This will run the until the program interrupts
  /// from a normal exit or from a runtime error.
  pub fn run(&mut self, mode: RunMode) -> ExecuteResult {
    #[cfg(feature = "debug")]
    let mut last_ip: *const u8 = &self.current_fun.chunk().instructions[0] as *const u8;

    loop {
      // get the current instruction
      let op_code: ByteCode = ByteCode::from(self.read_byte());

      #[cfg(feature = "debug")]
      {
        let ip = unsafe { self.ip().offset(-1) };
        self.print_debug(ip, last_ip);
        last_ip = ip;
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
        ByteCode::Greater => self.op_greater(),
        ByteCode::Less => self.op_less(),
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
        ByteCode::Nil => self.op_literal(VALUE_NIL),
        ByteCode::True => self.op_literal(Value::from(true)),
        ByteCode::False => self.op_literal(Value::from(false)),
        ByteCode::List => self.op_literal(Value::from(self.gc.manage(SlVec::default(), self))),
        ByteCode::ListInit => self.op_list(),
        ByteCode::Map => self.op_literal(Value::from(self.gc.manage(SlHashMap::default(), self))),
        ByteCode::MapInit => self.op_map(),
        ByteCode::IterNext => self.op_iter_next(),
        ByteCode::IterCurrent => self.op_iter_current(),
        ByteCode::Constant => self.op_constant(),
        ByteCode::ConstantLong => self.op_constant_long(),
        ByteCode::Print => self.op_print(),
        ByteCode::Call => self.op_call(),
        ByteCode::Invoke => self.op_invoke(),
        ByteCode::SuperInvoke => self.op_super_invoke(),
        ByteCode::Closure => self.op_closure(),
        ByteCode::Method => self.op_method(),
        ByteCode::Class => self.op_class(),
        ByteCode::Inherit => self.op_inherit(),
        ByteCode::GetSuper => self.op_get_super(),
        ByteCode::CloseUpvalue => self.op_close_upvalue(),
        ByteCode::Return => self.op_return(),
      };

      match result {
        Signal::OkReturn => {
          if let RunMode::CallFunction(depth) = mode {
            if depth == self.frame_count {
              return ExecuteResult::FunResult(self.get_val(-1));
            }
          }
        }
        Signal::Ok => (),
        Signal::RuntimeError => {
          let current_error = self.current_error.clone();
          match &current_error {
            Some(error) => {
              self.print_error(error);
              return ExecuteResult::RuntimeError;
            }
            None => self.internal_error("Runtime error was not set."),
          }
        }
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
    unsafe {
      self.set_ip(self.ip().offset(offset));
    }
  }

  /// Get the current instruction
  #[inline]
  fn ip(&self) -> *const u8 {
    unsafe { (*self.current_frame).ip }
  }

  /// Get the current frame slots
  #[inline]
  fn slots(&self) -> *mut Value {
    unsafe { (*self.current_frame).slots }
  }

  /// Get the current closure
  #[inline]
  fn closure(&self) -> Managed<Closure> {
    unsafe { (*self.current_frame).closure }
  }

  /// Set the current
  #[inline]
  fn set_ip(&mut self, new_ip: *const u8) {
    unsafe { (*self.current_frame).ip = new_ip }
  }

  /// read a u8 out of the bytecode
  #[inline]
  fn read_byte(&mut self) -> u8 {
    let byte = unsafe { *self.ip() };
    self.update_ip(1);
    byte
  }

  /// read a u16 out of the bytecode
  #[inline]
  fn read_short(&mut self) -> u16 {
    let slice = unsafe { std::slice::from_raw_parts(self.ip(), 2) };
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
    self.stack_top = unsafe { self.stack_top.offset(-1) };
    self.get_val(0)
  }

  /// drop a value off the stack
  #[inline]
  fn drop(&mut self) {
    self.stack_top = unsafe { self.stack_top.offset(-1) };
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
  fn read_string(&self, index: u16) -> Managed<String> {
    self.read_constant(index).to_str()
  }

  /// reset the stack in case of interrupt
  fn reset_stack(&mut self) {
    self.stack_top = &mut self.stack[1] as *mut Value;
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

  /// create a list from a list literal
  fn op_list(&mut self) -> Signal {
    let arg_count = self.read_short();
    let args = unsafe {
      std::slice::from_raw_parts(
        self.stack_top.offset(-(arg_count as isize)),
        arg_count as usize,
      )
    };
    let mut list = self.peek(arg_count as isize).to_list();
    self
      .gc
      .grow(&mut list, self, |list| list.extend_from_slice(args));
    self.stack_top = unsafe { self.stack_top.offset(-(arg_count as isize)) };

    Signal::Ok
  }

  /// create a list from a list literal
  fn op_map(&mut self) -> Signal {
    let arg_count = self.read_short();
    let mut map = self.peek(arg_count as isize * 2).to_map();
    self
      .gc
      .grow(&mut map, self, |map| map.reserve(arg_count as usize));

    for i in 0..arg_count {
      let key = self.get_val(-(i as isize * 2) - 2);
      let value = self.get_val(-(i as isize * 2) - 1);

      map.insert(key, value);
    }
    self.stack_top = unsafe { self.stack_top.offset(-(arg_count as isize * 2)) };
    Signal::Ok
  }

  /// move an iterator to the next element
  fn op_iter_next(&mut self) -> Signal {
    let receiver = self.peek(0);

    if receiver.is_iter() {
      self.update_ip(2);
      match receiver.to_iter().next(&mut Hooks::new(self)) {
        Ok(value) => {
          self.set_val(-1, value);
          Signal::Ok
        }
        Err(error) => self.set_error(error),
      }
    } else {
      let constant = self.read_short();
      let method_name = self.read_string(constant);
      self.invoke(receiver, method_name, 0)
    }
  }

  /// get the current value from an iterator
  fn op_iter_current(&mut self) -> Signal {
    let value = self.peek(0);

    if value.is_iter() {
      self.update_ip(2);
      let result = value.to_iter().current();
      self.set_val(-1, result);
      Signal::Ok
    } else {
      let slot = self.read_short();
      let name = self.read_string(slot);
      self.get_property(value, name)
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
  fn invoke(&mut self, receiver: Value, method_name: Managed<String>, arg_count: u8) -> Signal {
    if receiver.is_instance() {
      let instance = receiver.to_instance();
      if let Some(field) = instance.get_field(&method_name) {
        self.set_val(-(arg_count as isize) - 1, *field);
        return self.resolve_call(*field, arg_count);
      }
    }

    let class = receiver.value_class(&self.dep_manager.primitive_classes());
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
    let class = Value::from(self.gc.manage(Class::new(name), self));
    self.push(class);
    Signal::Ok
  }

  /// Get this classes super class
  fn op_get_super(&mut self) -> Signal {
    let slot = self.read_short();
    let name = self.read_string(slot);
    let super_class = self.pop().to_class();

    self.bind_method(super_class, name)
  }

  /// Inherit a class by directly copying methods onto the subclass.
  fn op_inherit(&mut self) -> Signal {
    let mut class = self.peek(0).to_class();

    let peek = self.peek(1);
    if peek.is_class() {
      class.inherit(&GcHooks::new(self), peek.to_class());

      self.drop();
      Signal::Ok
    } else {
      self.runtime_error("Superclass must be a class.")
    }
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
    let target = self.peek(2);
    let index = self.peek(1);

    if target.is_list() && index.is_num() {
      let mut list = target.to_list();
      let num = index.to_num();

      let rounded = num as usize;
      if rounded >= list.len() {
        return self.runtime_error(&format!(
          "Index out of bounds. list was length {} but attempted to index with {}.",
          list.len(),
          rounded
        ));
      }

      list[rounded] = self.pop();
      self.drop();
      return Signal::Ok;
    } else if target.is_map() {
      let mut map = target.to_map();

      if index.is_num() {
        let num = index.to_num();
        let value = self.pop();
        self.gc.grow(&mut map, self, |map| {
          map.insert(Value::from(use_sentinel_nan(num)), value)
        });
        self.drop();
        return Signal::Ok;
      } else {
        let value = self.pop();
        self.gc.grow(&mut map, self, |map| map.insert(index, value));
        self.drop();
        return Signal::Ok;
      }
    }

    self.runtime_error(&format!(
      "{} cannot be indexed by {}.",
      target.value_type(),
      index.value_type()
    ))
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
      return self.runtime_error(&format!("Undefined variable {}", string.as_str()));
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
      instance.set_field(&GcHooks::new(self), name, value);

      let popped = self.pop();
      self.drop();
      self.push(popped);

      return Signal::Ok;
    }

    self.runtime_error("Only instances have settable fields.")
  }

  fn op_set_upvalue(&mut self) -> Signal {
    let slot = self.read_byte();
    let value = self.peek(0);
    let upvalue = &mut self.closure().upvalues[slot as usize];

    let open_index = match &mut *upvalue.to_upvalue() {
      Upvalue::Open(stack_ptr) => Some(*stack_ptr),
      Upvalue::Closed(store) => {
        **store = value;
        None
      }
    };

    if let Some(stack_ptr) = open_index {
      unsafe { ptr::write(stack_ptr.as_ptr(), value) }
    }

    Signal::Ok
  }

  fn op_get_index(&mut self) -> Signal {
    let index = self.pop();
    let target = self.pop();

    if target.is_list() && index.is_num() {
      let list = target.to_list();
      let num = index.to_num();

      let rounded = num as usize;
      if rounded >= list.len() {
        return self.runtime_error(&format!(
          "Index out of bounds. list was length 0 but attempted to index with {}.",
          rounded
        ));
      }

      self.push(list[rounded]);
      return Signal::Ok;
    } else if target.is_map() {
      let map = target.to_map();

      if index.is_num() {
        let num = index.to_num();
        return match map.get(&Value::from(use_sentinel_nan(num))) {
          Some(value) => {
            self.push(*value);
            Signal::Ok
          }
          None => self.runtime_error(&format!("Key {} does not exist in map", index)),
        };
      } else {
        return match map.get(&index) {
          Some(value) => {
            self.push(*value);
            Signal::Ok
          }
          None => self.runtime_error(&format!("Key {} does not exist in map", index)),
        };
      }
    }

    self.runtime_error(&format!(
      "{} cannot be indexed with {}.",
      target.value_type(),
      index.value_type()
    ))
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
      None => self.runtime_error(&format!("Undefined variable {}", string.as_str())),
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
      Upvalue::Closed(store) => **store,
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

  fn get_property(&mut self, value: Value, name: Managed<String>) -> Signal {
    let kind = value.kind();
    match kind {
      ValueVariant::Instance => {
        let instance = value.to_instance();

        if let Some(value) = instance.get_field(&name) {
          self.set_val(-1, *value);
          return Signal::Ok;
        }
      }
      ValueVariant::Iter => {
        let iter = value.to_iter();

        if let Some(value) = iter.get_field(&name) {
          self.set_val(-1, *value);
          return Signal::Ok;
        }
      }
      _ => (),
    }

    let class = self
      .dep_manager
      .primitive_classes()
      .for_variant(value, kind);
    self.bind_method(class, name)
  }

  fn op_import(&mut self) -> Signal {
    let index_path = self.read_short();
    let path = self.read_string(index_path);

    let mut dep_manager = self.dep_manager;
    let current_module = self.current_fun.module;

    match dep_manager.import(&GcHooks::new(self), current_module, path) {
      Ok(module) => {
        self.push(Value::from(
          self.gc.manage(module.import(&GcHooks::new(self)), self),
        ));
        Signal::Ok
      }
      Err(error) => self.set_error(error),
    }
  }

  fn op_export(&mut self) -> Signal {
    let index = self.read_short();
    let name = self.read_string(index);
    let mut copy = self.current_fun.module;

    match copy.export_symbol(&GcHooks::new(self), name) {
      Ok(_) => Signal::Ok,
      Err(err) => self.set_error(err),
    }
  }

  /// return from a spacelox function placing the result on top of the stack
  fn op_return(&mut self) -> Signal {
    // get the function result close upvalues and pop frame
    let result = self.pop();
    self.close_upvalues(NonNull::from(unsafe { &*self.slots() }));
    self.frame_count -= 1;

    // if the frame was the whole script signal an ok interrupt
    if self.frame_count == 1 {
      self.drop();
      return Signal::Exit;
    }

    // pull the current frame out of the stack and set the cached frame
    unsafe {
      self.stack_top = self.slots();
      self.current_frame = self.current_frame.offset(-1);
      self.current_fun = self.closure().fun;
    }

    // push the result onto the stack
    self.push(result);
    Signal::OkReturn
  }

  fn op_negate(&mut self) -> Signal {
    let pop = self.pop();

    if pop.is_num() {
      self.push(Value::from(-pop.to_num()));
      Signal::Ok
    } else {
      self.runtime_error("Operand must be a number.")
    }
  }

  fn op_not(&mut self) -> Signal {
    let value = self.pop();
    self.push(Value::from(is_falsey(value)));
    Signal::Ok
  }

  fn op_add(&mut self) -> Signal {
    let (right, left) = (self.pop(), self.pop());

    if right.is_num() && left.is_num() {
      self.push(Value::from(left.to_num() + right.to_num()));
      Signal::Ok
    } else if right.is_str() && left.is_str() {
      let result = format!("{}{}", left.to_str(), right.to_str());
      let string = self.gc.manage_str(result, self);
      self.push(Value::from(string));
      Signal::Ok
    } else {
      self.runtime_error("Operands must be two numbers or two strings.")
    }
  }

  fn op_sub(&mut self) -> Signal {
    let (right, left) = (self.pop(), self.pop());

    if right.is_num() && left.is_num() {
      self.push(Value::from(left.to_num() - right.to_num()));
      return Signal::Ok;
    }

    self.runtime_error("Operands must be numbers.")
  }

  fn op_mul(&mut self) -> Signal {
    let (right, left) = (self.pop(), self.pop());

    if right.is_num() && left.is_num() {
      self.push(Value::from(left.to_num() * right.to_num()));
      return Signal::Ok;
    }

    self.runtime_error("Operands must be numbers.")
  }

  fn op_div(&mut self) -> Signal {
    let (right, left) = (self.pop(), self.pop());

    if right.is_num() && left.is_num() {
      self.push(Value::from(left.to_num() / right.to_num()));
      return Signal::Ok;
    }

    self.runtime_error("Operands must be numbers.")
  }

  fn op_less(&mut self) -> Signal {
    let (right, left) = (self.pop(), self.pop());

    if right.is_num() && left.is_num() {
      self.push(Value::from(left.to_num() < right.to_num()));
      return Signal::Ok;
    }

    self.runtime_error("Operands must be numbers.")
  }

  fn op_greater(&mut self) -> Signal {
    let (right, left) = (self.pop(), self.pop());

    if right.is_num() && left.is_num() {
      self.push(Value::from(left.to_num() > right.to_num()));
      return Signal::Ok;
    }

    self.runtime_error("Operands must be numbers.")
  }

  fn op_equal(&mut self) -> Signal {
    let right = self.pop();
    let left = self.pop();

    self.push(Value::from(left == right));
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

  fn op_closure(&mut self) -> Signal {
    let slot = self.read_short();
    let fun = self.read_constant(slot).to_fun();
    let mut closure = Closure::new(fun);

    for _ in 0..fun.upvalue_count {
      let upvalue_index: UpvalueIndex = unsafe { mem::transmute(self.read_short()) };

      match upvalue_index {
        UpvalueIndex::Local(index) => {
          let value = unsafe { &*self.slots().offset(index as isize) };
          closure
            .upvalues
            .push(self.capture_upvalue(NonNull::from(value)));
        }
        UpvalueIndex::Upvalue(upvalue) => {
          let upvalue = self.closure().upvalues[upvalue as usize];
          closure.upvalues.push(upvalue);
        }
      }
    }

    let closure = Value::from(self.gc.manage(closure, self));
    self.push(closure);
    Signal::Ok
  }

  fn op_close_upvalue(&mut self) -> Signal {
    self.close_upvalues(NonNull::from(unsafe { &*self.stack_top.offset(-1) }));
    self.drop();
    Signal::Ok
  }

  fn op_print(&mut self) -> Signal {
    self.io.stdio().println(&format!("{}", self.pop()));
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
      ValueVariant::Closure => self.call(callee.to_closure(), arg_count),
      ValueVariant::Method => self.call_method(callee.to_method(), arg_count),
      ValueVariant::NativeFun => self.call_native_fun(callee.to_native_fun(), arg_count),
      ValueVariant::NativeMethod => self.call_native_method(callee.to_native_method(), arg_count),
      ValueVariant::Class => self.call_class(callee.to_class(), arg_count),
      ValueVariant::Fun => self.internal_error(&format!(
        "Function {} was not wrapped in a closure.",
        callee.to_fun().name
      )),
      _ => self.runtime_error("Can only call functions and classes."),
    }
  }

  fn call_class(&mut self, class: Managed<Class>, arg_count: u8) -> Signal {
    let value = Value::from(self.gc.manage(Instance::new(class), self));
    self.set_val(-(arg_count as isize) - 1, value);

    match class.init {
      Some(init) => self.resolve_call(init, arg_count),
      None => {
        if arg_count != 0 {
          self.runtime_error(&format!("Expected 0 arguments but got {}", arg_count))
        } else {
          Signal::Ok
        }
      }
    }
  }

  /// call a native function immediately returning the result
  fn call_native_fun(&mut self, native: Managed<Box<dyn NativeFun>>, arg_count: u8) -> Signal {
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

    match native.call(&mut Hooks::new(self), args) {
      Ok(value) => {
        self.stack_top = unsafe { self.stack_top.offset(-(arg_count as isize) - 1) };
        self.push(value);

        Signal::OkReturn
      }
      Err(error) => self.set_error(error),
    }
  }

  /// call a native method immediately returning the result
  fn call_native_method(
    &mut self,
    native: Managed<Box<dyn NativeMethod>>,
    arg_count: u8,
  ) -> Signal {
    let meta = native.meta();

    let args = unsafe {
      std::slice::from_raw_parts(
        self.stack_top.offset(-(arg_count as isize)),
        arg_count as usize,
      )
    };

    // check that the current function is called with the right number of args and types
    if let Some(error) = self.check_native_arity(meta, args) {
      return error;
    }

    let this = self.get_val(-(arg_count as isize) - 1);

    match native.call(&mut Hooks::new(self), this, &args) {
      Ok(value) => {
        self.stack_top = unsafe { self.stack_top.offset(-(arg_count as isize) - 1) };
        self.push(value);

        Signal::OkReturn
      }
      Err(error) => self.set_error(error),
    }
  }

  /// call a spacelox function setting it as the new call frame
  fn call(&mut self, closure: Managed<Closure>, arg_count: u8) -> Signal {
    // check that the current function is called with the right number of args
    if let Some(error) = self.check_arity(closure.fun, arg_count) {
      return error;
    }

    // set the current current instruction pointer. check for overflow
    if self.frame_count == FRAME_MAX {
      return self.runtime_error("Stack overflow.");
    }

    unsafe {
      let frame = &mut *self.current_frame.offset(1);
      frame.closure = closure;
      frame.ip = closure.fun.chunk().instructions.get_unchecked(0) as *const u8;
      frame.slots = self.stack_top.offset(-(arg_count as isize) - 1);
      self.current_frame = frame as *mut CallFrame;
    }

    self.current_fun = closure.fun;
    self.frame_count += 1;

    Signal::Ok
  }

  /// check that the number of args is valid for the function arity
  fn check_arity(&mut self, fun: Managed<Fun>, arg_count: u8) -> Option<Signal> {
    match fun.arity.check(arg_count) {
      Ok(_) => None,
      Err(error) => match error {
        ArityError::Fixed(arity) => Some(self.runtime_error(&format!(
          "{} expected {} argument(s) but got {}.",
          fun.name, arity, arg_count,
        ))),
        ArityError::Variadic(arity) => Some(self.runtime_error(&format!(
          "{} expected at least {} argument(s) but got {}.",
          fun.name, arity, arg_count,
        ))),
        ArityError::DefaultLow(arity) => Some(self.runtime_error(&format!(
          "{} expected at least {} argument(s) but got {}.",
          fun.name, arity, arg_count,
        ))),
        ArityError::DefaultHigh(arity) => Some(self.runtime_error(&format!(
          "{} expected at most {} argument(s) but got {}.",
          fun.name, arity, arg_count,
        ))),
      },
    }
  }

  fn check_native_arity(&mut self, native_meta: &NativeMeta, args: &[Value]) -> Option<Signal> {
    match native_meta.signature.check(args) {
      Ok(()) => None,
      Err(err) => match err {
        spacelox_core::signature::SignatureError::LengthFixed(expected) => {
          Some(self.runtime_error(&format!(
            "{} expected {} argument(s) but received {}.",
            native_meta.name,
            expected,
            args.len(),
          )))
        }
        spacelox_core::signature::SignatureError::LengthVariadic(expected) => {
          Some(self.runtime_error(&format!(
            "{} expected at least {} argument(s) but received {}.",
            native_meta.name,
            expected,
            args.len(),
          )))
        }
        spacelox_core::signature::SignatureError::LengthDefaultLow(expected) => {
          Some(self.runtime_error(&format!(
            "{} expected at least {} argument(s) but received {}.",
            native_meta.name,
            expected,
            args.len(),
          )))
        }
        spacelox_core::signature::SignatureError::LengthDefaultHigh(expected) => {
          Some(self.runtime_error(&format!(
            "{} expected at most {} argument(s) but received {}.",
            native_meta.name,
            expected,
            args.len(),
          )))
        }
        spacelox_core::signature::SignatureError::TypeWrong(parameter) => {
          Some(self.runtime_error(&format!(
            "{} parameter {} must be a {} not a {}.",
            native_meta.name,
            native_meta.signature.parameters[parameter as usize].name,
            native_meta.signature.parameters[parameter as usize].kind,
            ParameterKind::from(args[parameter as usize])
          )))
        }
      },
    }
  }

  /// Call a bound method
  fn call_method(&mut self, bound: Managed<Method>, arg_count: u8) -> Signal {
    self.set_val(-(arg_count as isize) - 1, bound.receiver);
    self.resolve_call(bound.method, arg_count)
  }

  /// bind a method to an instance
  fn bind_method(&mut self, class: Managed<Class>, name: Managed<String>) -> Signal {
    match class.get_method(&name) {
      Some(method) => {
        let bound = self.gc.manage(Method::new(self.peek(0), *method), self);
        self.set_val(-1, Value::from(bound));
        Signal::Ok
      }
      None => self.runtime_error(&format!("Undefined property {}", name.as_str())),
    }
  }

  /// invoke a method from the provided class
  fn invoke_from_class(
    &mut self,
    class: Managed<Class>,
    method_name: Managed<String>,
    arg_count: u8,
  ) -> Signal {
    match class.get_method(&method_name) {
      Some(method) => self.resolve_call(*method, arg_count),
      None => self.runtime_error(&format!("Undefined property {}.", method_name.as_str())),
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
          return Value::from(*upvalue);
        }
      }
    }

    let created_upvalue = self.gc.manage(Upvalue::Open(local_index), self);
    self.open_upvalues.push(created_upvalue);

    Value::from(created_upvalue)
  }

  /// hoist all open upvalue above the last index
  fn close_upvalues(&mut self, last_index: NonNull<Value>) {
    if self.open_upvalues.len() > 0 {
      for upvalue in self.open_upvalues.iter_mut().rev() {
        let index = match **upvalue {
          Upvalue::Open(index) => index,
          Upvalue::Closed(_) => self.internal_error("Unexpected closed upvalue."),
        };

        if index < last_index {
          break;
        }

        upvalue.hoist();
      }

      self.open_upvalues.retain(|upvalue| upvalue.is_open())
    }
  }

  /// Print debugging information for the current instruction
  #[cfg(feature = "debug")]
  fn print_debug(&self, ip: *const u8, last_ip: *const u8) {
    let stdio = self.io.stdio();

    self.print_stack_debug(&stdio);

    #[cfg(feature = "debug_upvalue")]
    self.print_upvalue_debug(&stdio);

    let start = &self.current_fun.chunk().instructions[0] as *const u8;
    let offset = ptr_len(start, ip);
    let last_offset = ptr_len(start, last_ip);
    disassemble_instruction(&stdio, &self.current_fun.chunk(), offset, last_offset);
  }

  /// Print the current stack
  #[cfg(feature = "debug")]
  fn print_stack_debug(&self, stdio: &I::StdIo) {
    stdio.print("Stack:        ");

    unsafe {
      let start = &self.stack[1] as *const Value;
      let len = ptr_len(start, self.stack_top);
      let slice = std::slice::from_raw_parts(start, len);

      for value in slice {
        stdio.print(&format!("[ {} ]", *value));
      }
    }

    stdio.println("");
  }

  /// Print the current upvalues
  #[cfg(feature = "debug_upvalue")]
  fn print_upvalue_debug(&self, stdio: &I::StdIo) {
    stdio.print("Open UpVal:   ");
    self
      .open_upvalues
      .iter()
      .for_each(|upvalue| match &**upvalue {
        Upvalue::Open(stack_ptr) => {
          stdio.print(&format!("[ stack {} ]", unsafe { *stack_ptr.as_ref() }));
        }
        Upvalue::Closed(closed) => {
          stdio.print(&format!("[ heap {} ]", closed));
        }
      });
    stdio.println("");
  }

  /// Convert an execute result to a call result
  fn to_call_result(&self, execute_result: ExecuteResult) -> CallResult {
    match execute_result {
      ExecuteResult::FunResult(value) => Ok(value),
      ExecuteResult::Ok => self.internal_error("Accidental early exit in hook call."),
      ExecuteResult::CompileError => self.internal_error("Compiler error should occur before code is executed."),
      ExecuteResult::RuntimeError => match self.current_error.clone() {
        Some(error) => Err(error),
        None => self.internal_error("Error not set on vm executor."),
      },
    }
  }

  /// Report an internal issue to the user
  fn internal_error(&self, message: &str) -> ! {
    panic!(format!("Internal Error: {}", message))
  }

  /// Report a known spacelox runtime error to the user
  fn runtime_error(&mut self, message: &str) -> Signal {
    self.set_error(SlError::new(
      self.gc.manage_str(String::from(message), self),
    ));
    Signal::RuntimeError
  }

  /// Set the current error place the vm signal a runtime error
  fn set_error(&mut self, error: SlError) -> Signal {
    self.current_error = Some(error);
    Signal::RuntimeError
  }

  /// Print an error message and the current call stack to the user
  fn print_error(&mut self, error: &SlError) {
    let message = error.message;

    let stdio = self.io.stdio();
    stdio.eprintln(&*message);
    stdio.eprintln("");

    for frame in self.frames[1..self.frame_count].iter().rev() {
      let closure = &frame.closure;
      let location: String = match &**closure.fun.name {
        SCRIPT => SCRIPT.to_owned(),
        _ => format!("{}()", closure.fun.name),
      };

      stdio.eprintln(&format!(
        "[line {}] in {}",
        closure.fun.chunk().get_line(frame.ip as usize),
        location
      ));
    }

    self.reset_stack();
  }
}

impl<'a, I: Io> Trace for VmExecutor<'a, I> {
  fn trace(&self) -> bool {
    self.script.trace();

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
    self.global.trace();

    true
  }

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    self.script.trace_debug(stdio);

    unsafe {
      let start = &self.stack[0] as *const Value;
      let len = ptr_len(start, self.stack_top) + 1;
      let slice = std::slice::from_raw_parts(start, len);

      slice.iter().for_each(|value| {
        value.trace_debug(stdio);
      });
    }

    self.frames[0..self.frame_count].iter().for_each(|frame| {
      frame.closure.trace_debug(stdio);
    });

    self.open_upvalues.iter().for_each(|upvalue| {
      upvalue.trace_debug(stdio);
    });

    self.dep_manager.trace_debug(stdio);
    self.global.trace_debug(stdio);

    true
  }
}

impl<'a, I: Io> HookContext for VmExecutor<'a, I> {
  fn gc_context(&self) -> &dyn GcContext {
    self
  }

  fn call_context(&mut self) -> &mut dyn CallContext {
    self
  }
}

impl<'a, I: Io> GcContext for VmExecutor<'a, I> {
  fn gc(&self) -> &Gc {
    self.gc
  }
}

impl<'a, I: Io> CallContext for VmExecutor<'a, I> {
  fn call(&mut self, callable: Value, args: &[Value]) -> CallResult {
    let result = self.run_fun(callable, args);
    self.to_call_result(result)
  }

  fn call_method(&mut self, this: Value, method: Value, args: &[Value]) -> CallResult {
    let result = self.run_method(this, method, args);
    self.to_call_result(result)
  }

  fn call_method_by_name(
    &mut self,
    this: Value,
    method_name: Managed<String>,
    args: &[Value],
  ) -> CallResult {
    let result = self.run_method_by_name(this, method_name, args);
    self.to_call_result(result)
  }
}
