use crate::{
  byte_code::{AlignedByteCode, ByteCode, CaptureIndex},
  cache::InlineCache,
  compiler::{Compiler, Parser, Resolver},
  constants::{MAX_FRAME_SIZE, REPL_MODULE},
  source::{Source, VmFileId, VmFiles},
  FeResult,
};
use codespan_reporting::term::{self, Config};
use laythe_core::{
  captures::Captures,
  constants::{PLACEHOLDER_NAME, SELF},
  hooks::{GcContext, GcHooks, HookContext, Hooks, NoContext, ValueContext},
  if_let_obj,
  managed::{
    Allocate, Array, DebugHeapRef, Gc, GcObj, GcObject, GcStr, Instance, Object, Trace, TraceRoot,
    Tuple,
  },
  match_obj,
  memory::Allocator,
  module::{Import, Module, Package},
  object::{
    Channel, Class, Closure, Fiber, Fun, List, LyBox, Map, Method, Native, NativeMeta, ObjectKind,
    ReceiveResult, SendResult, UnwindResult,
  },
  signature::{ArityError, Environment, ParameterKind, SignatureError},
  to_obj_kind,
  utils::{is_falsey, IdEmitter},
  val,
  value::{Value, VALUE_NIL, VALUE_TRUE},
  Call, LyError,
};
use laythe_env::io::Io;
use laythe_lib::{builtin_from_module, create_std_lib, BuiltIn};
use laythe_native::io::io_native;
use std::mem;
use std::path::PathBuf;
use std::ptr;
use std::{cell::RefCell, cmp::Ordering};
use std::{collections::VecDeque, io::Write};
use std::{convert::TryInto, usize};

#[cfg(feature = "debug")]
use crate::debug::disassemble_instruction;

#[cfg(feature = "debug")]
use std::io;

#[cfg(feature = "debug")]
use laythe_env::stdio::Stdio;

const VERSION: &str = "0.1.0";

#[derive(Debug, Clone, PartialEq)]
enum Signal {
  Ok,
  OkReturn,
  ContextSwitch,
  Exit,
  RuntimeError,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExecuteResult {
  Ok(Value),
  Exit(u16),
  RuntimeError,
  CompileError,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VmExit {
  Ok,
  RuntimeError,
  CompileError,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ExecuteMode {
  Normal,
  CallFunction(usize),
}

pub fn default_native_vm() -> Vm {
  Vm::new(io_native())
}

/// The virtual machine for the laythe programming language
pub struct Vm {
  /// The current running fiber
  fiber: GcObj<Fiber>,

  /// The main fiber
  main_fiber: GcObj<Fiber>,

  /// The vm's garbage collector
  gc: RefCell<Allocator>,

  /// The environments io access
  io: Io,

  /// The currently loaded files
  files: VmFiles,

  /// The queue of runnable fibers
  fiber_queue: VecDeque<GcObj<Fiber>>,

  /// The root directory
  root_dir: PathBuf,

  /// The builtin class the vm need reference to
  builtin: BuiltIn,

  /// A collection of packages that have already been loaded
  packages: Map<GcStr, Gc<Package>>,

  /// A utility to emit ids for modules
  emitter: IdEmitter,

  /// A cache for full filepath to individual modules
  module_cache: Map<GcStr, Gc<Module>>,

  /// Inline caches for each module
  inline_cache: Vec<InlineCache>,

  /// The global module
  global: Gc<Module>,

  /// The current frame's function
  current_fun: GcObj<Fun>,

  /// What exit code is currently set
  exit_code: u16,

  /// pointer to the current instruction
  ip: *const u8,

  /// A vec of stub functions so native functions can have a call frame
  native_fun_stubs: Vec<GcObj<Fun>>,

  /// A placeholder capture for functions without any captures
  capture_stub: Captures,
}

impl Vm {
  pub fn new(io: Io) -> Vm {
    let gc = Allocator::new(io.stdio());
    let context = NoContext::new(gc);
    let hooks = GcHooks::new(&context);

    let root_dir = io
      .env()
      .current_dir()
      .expect("Could not obtain the current working directory.");

    let mut emitter = IdEmitter::default();
    let std_lib = create_std_lib(&hooks, &mut emitter).expect("Standard library creation failed");
    let global = std_lib.root_module();

    let current_fun = Fun::stub(
      &hooks,
      hooks.manage_str(PLACEHOLDER_NAME),
      global,
      AlignedByteCode::Nil,
    );

    let current_fun = hooks.manage_obj(current_fun);
    let capture_stub = Captures::new(&hooks, &[]);
    let fiber = hooks.manage_obj(
      Fiber::new(current_fun, capture_stub).expect("Unable to generate placeholder fiber"),
    );

    let builtin = builtin_from_module(&hooks, &global)
      .expect("Failed to generate builtin class from global module");

    let gc = RefCell::new(context.done());
    let inline_cache: Vec<InlineCache> = (0..emitter.id_count())
      .map(|_| InlineCache::new(0, 0))
      .collect();

    let mut vm = Vm {
      io,
      fiber,
      main_fiber: fiber,
      gc,
      files: VmFiles::default(),
      fiber_queue: VecDeque::new(),
      builtin,
      root_dir,
      packages: Map::default(),
      emitter,
      module_cache: Map::default(),
      inline_cache,
      global,
      current_fun,
      exit_code: 0,
      ip: ptr::null(),
      native_fun_stubs: vec![],
      capture_stub,
    };
    vm.add_package(std_lib);

    vm
  }

  /// The current version of the virtual machine
  pub fn version() -> &'static str {
    VERSION
  }

  /// Start the interactive repl
  pub fn repl(&mut self) -> (i32, VmExit) {
    let mut stdio = self.io.stdio();

    let repl_path = self.root_dir.join(PathBuf::from(REPL_MODULE));

    let main_module = self.module(repl_path.clone(), SELF);

    loop {
      let mut buffer = String::new();

      write!(stdio.stdout(), "laythe:> ").expect("Could not write to stdout");
      stdio.stdout().flush().expect("Could not write to stdout");

      match stdio.read_line(&mut buffer) {
        Ok(_) => {
          let source_content = self.manage_str(buffer);
          self.push_root(source_content);
          let source = Source::new(source_content);

          let managed_path = self.manage_str(repl_path.to_string_lossy());
          self.push_root(managed_path);

          let file_id = self.files.upsert(managed_path, source_content);
          self.pop_roots(2);

          self.interpret(true, main_module, &source, file_id);
        },
        Err(error) => panic!("{}", error),
      }
    }
  }

  /// Run the provided source file
  pub fn run(&mut self, module_path: PathBuf, source_content: &str) -> (i32, VmExit) {
    match self.io.fs().canonicalize(&module_path) {
      Ok(module_path) => {
        let mut directory = module_path.clone();
        directory.pop();

        self.root_dir = directory;
        let source_content = self.manage_str(source_content);
        self.push_root(source_content);
        let source = Source::new(source_content);

        let managed_path = self.manage_str(module_path.to_string_lossy());
        self.push_root(managed_path);

        let file_id = self.files.upsert(managed_path, source_content);
        self.pop_roots(2);

        let main_module = self.module(module_path, SELF);

        match self.interpret(false, main_module, &source, file_id) {
          ExecuteResult::Ok(_) => self.internal_error("Shouldn't exit vm with ok result"),
          ExecuteResult::Exit(code) => match code {
            0 => (0, VmExit::Ok),
            _ => (code as i32, VmExit::RuntimeError),
          },
          ExecuteResult::RuntimeError => (1, VmExit::RuntimeError),
          ExecuteResult::CompileError => (1, VmExit::CompileError),
        }
      },
      Err(err) => {
        writeln!(self.io.stdio().stderr(), "{}", &err.to_string())
          .expect("Unable to write to stderr");
        (1, VmExit::RuntimeError)
      },
    }
  }

  /// Add a package to the vm
  fn add_package(&mut self, package: Gc<Package>) {
    self.packages.insert(package.name(), package);
  }

  /// Interpret the provided laythe script returning the execution result
  fn interpret(
    &mut self,
    repl: bool,
    main_module: Gc<Module>,
    source: &Source,
    file_id: VmFileId,
  ) -> ExecuteResult {
    match self.compile(repl, main_module, source, file_id) {
      Ok(fun) => {
        self.prepare(fun);
        self.execute(ExecuteMode::Normal)
      },
      Err(errors) => {
        let mut stdio = self.io.stdio();
        let stderr_color = stdio.stderr_color();
        for error in errors.iter() {
          term::emit(stderr_color, &Config::default(), &self.files, error)
            .expect("Unable to write to stderr");
        }
        ExecuteResult::CompileError
      },
    }
  }

  /// Compile the provided laythe source into the virtual machine's bytecode
  fn compile(
    &mut self,
    repl: bool,
    module: Gc<Module>,
    source: &Source,
    file_id: VmFileId,
  ) -> FeResult<GcObj<Fun>, VmFileId> {
    let (ast, line_offsets) = Parser::new(source, file_id).parse();
    self
      .files
      .update_line_offsets(file_id, line_offsets.clone())
      .expect("File id not set for line offsets");

    let mut ast = ast?;

    Resolver::new(self.global, &self.gc.borrow(), source, file_id, repl).resolve(&mut ast)?;

    let gc = self.gc.replace(Allocator::default());
    let compiler = Compiler::new(module, &line_offsets, file_id, repl, self, gc);

    #[cfg(feature = "debug")]
    let compiler = compiler.with_io(self.io.clone());

    let (result, gc, cache_id_emitter) = compiler.compile(&ast);
    self.gc.replace(gc);

    result.map(|fun| {
      let cache = InlineCache::new(
        cache_id_emitter.property_count(),
        cache_id_emitter.invoke_count(),
      );

      if module.id() < self.inline_cache.len() {
        self.inline_cache[module.id()] = cache;
      } else {
        self.inline_cache.push(cache);
      }
      self.manage_obj(fun)
    })
  }

  /// Reset the vm to execute another script
  fn prepare(&mut self, script: GcObj<Fun>) {
    let fiber = match Fiber::new(script, self.capture_stub) {
      Ok(fiber) => fiber,
      Err(_) => self.internal_error("Unable to generate initial fiber"),
    };

    self.fiber = self.manage_obj(fiber);
    self.main_fiber = self.fiber;
    self.fiber.activate();
    self.load_ip();

    self.current_fun = script;
    let mut current_module = self.current_fun.module();

    self
      .global
      .transfer_exported(&GcHooks::new(self), &mut current_module);
  }

  /// Prepare the main module for use
  fn module(&mut self, module_path: PathBuf, name: &str) -> Gc<Module> {
    let main_id = self.emitter.emit();
    let hooks = GcHooks::new(self);

    let name = hooks.manage_str(name);
    let module_class = Class::with_inheritance(&hooks, name, self.builtin.dependencies.module);

    let mut module = hooks.manage(Module::new(module_class, module_path, main_id));
    hooks.push_root(module);

    // transfer the symbols from the global module into the main module
    self.global.transfer_exported(&hooks, &mut module);

    hooks.pop_roots(1);
    let package = hooks.manage(Package::new(name, module));

    self.packages.insert(name, package);
    module
  }

  /// Run a laythe function on top of the current stack.
  /// This acts as a hook for native functions to execute laythe function
  unsafe fn run_fun(&mut self, callable: Value, args: &[Value]) -> ExecuteResult {
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
    match self.resolve_call(callable, args.len() as u8) {
      Signal::Ok => self.execute(mode),
      Signal::OkReturn => ExecuteResult::Ok(self.fiber.pop()),
      Signal::RuntimeError => ExecuteResult::RuntimeError,
      _ => self.internal_error("Unexpected signal in run_fun."),
    }
  }

  /// Run a laythe method on top of the current stack.
  /// This acts as a hook for native functions to execute laythe function
  unsafe fn run_method(&mut self, this: Value, method: Value, args: &[Value]) -> ExecuteResult {
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
    match self.resolve_call(method, args.len() as u8) {
      Signal::Ok => self.execute(mode),
      Signal::OkReturn => ExecuteResult::Ok(self.fiber.pop()),
      Signal::RuntimeError => ExecuteResult::RuntimeError,
      _ => self.internal_error("Unexpected signal in run_method."),
    }
  }

  /// Get a method for this this value with a given method name
  unsafe fn get_method(&mut self, this: Value, method_name: GcStr) -> Call {
    let class = self.value_class(this);

    match class.get_method(&method_name) {
      Some(method) => Call::Ok(method),
      None => {
        let error_message = val!(self.manage_str(format!(
          "Class {} does not have method {}",
          class.name(),
          method_name
        )));

        let result = self.run_fun(val!(self.builtin.errors.import), &[error_message]);

        self.to_call_result(result)
      },
    }
  }

  /// Main virtual machine execution loop. This will run the until the program interrupts
  /// from a normal exit or from a runtime error.
  fn execute(&mut self, mode: ExecuteMode) -> ExecuteResult {
    unsafe {
      loop {
        // get the current instruction
        let op_code: ByteCode = ByteCode::from(self.read_byte());

        #[cfg(feature = "debug")]
        {
          let ip = self.ip.sub(1);
          self.print_state(ip).expect("Unable to print state");
        }

        // execute the decoded instruction
        let result = match op_code {
          ByteCode::Negate => self.op_negate(),
          ByteCode::Add => self.op_add(),
          ByteCode::Subtract => self.op_sub(),
          ByteCode::Multiply => self.op_mul(),
          ByteCode::Divide => self.op_div(),
          ByteCode::Not => self.op_not(),
          ByteCode::And => self.op_and(),
          ByteCode::Or => self.op_or(),
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
          ByteCode::Box => self.op_box(),
          ByteCode::EmptyBox => self.op_empty_box(),
          ByteCode::FillBox => self.op_fill_box(),
          ByteCode::GetGlobal => self.op_get_global(),
          ByteCode::SetGlobal => self.op_set_global(),
          ByteCode::GetLocal => self.op_get_local(),
          ByteCode::SetLocal => self.op_set_local(),
          ByteCode::GetBox => self.op_get_box(),
          ByteCode::SetBox => self.op_set_box(),
          ByteCode::GetCapture => self.op_get_capture(),
          ByteCode::SetCapture => self.op_set_capture(),
          ByteCode::GetProperty => self.op_get_property(),
          ByteCode::SetProperty => self.op_set_property(),
          ByteCode::Import => self.op_import(),
          ByteCode::ImportSymbol => self.op_import_symbol(),
          ByteCode::Export => self.op_export(),
          ByteCode::Drop => self.op_drop(),
          ByteCode::DropN => self.op_drop_n(),
          ByteCode::Dup => self.op_dup(),
          ByteCode::Nil => self.op_literal(VALUE_NIL),
          ByteCode::True => self.op_literal(val!(true)),
          ByteCode::False => self.op_literal(val!(false)),
          ByteCode::List => self.op_list(),
          ByteCode::Tuple => self.op_tuple(),
          ByteCode::Map => self.op_map(),
          ByteCode::Launch => self.op_launch(),
          ByteCode::Channel => self.op_channel(),
          ByteCode::BufferedChannel => self.op_buffered_channel(),
          ByteCode::Receive => self.op_receive(),
          ByteCode::Send => self.op_send(),
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
          ByteCode::Return => self.op_return(),
        };

        match result {
          Signal::Ok => (),
          Signal::OkReturn => {
            if let ExecuteMode::CallFunction(depth) = mode {
              if depth == self.fiber.frames().len() {
                return ExecuteResult::Ok(self.fiber.pop());
              }
            }
          },
          Signal::ContextSwitch => match self.fiber_queue.pop_front() {
            Some(fiber) => self.context_switch(fiber),
            None => {
              let mut stdio = self.io().stdio();
              let stderr = stdio.stderr();
              writeln!(stderr, "Fatal error deadlock.").expect("Unable to write to stderr");
              return ExecuteResult::RuntimeError;
            },
          },
          Signal::RuntimeError => match self.fiber.error() {
            Some(error) => {
              if let Some(execute_result) = self.stack_unwind(error, mode) {
                return execute_result;
              }
            },
            None => self.internal_error("Runtime error was not set."),
          },
          Signal::Exit => {
            return ExecuteResult::Exit(self.exit_code);
          },
        }
      }
    }
  }

  fn value_class(&self, value: Value) -> GcObj<Class> {
    self.builtin.primitives.for_value(value)
  }

  fn manage<R: 'static + Trace + Copy + DebugHeapRef, T: Allocate<R>>(&self, data: T) -> R {
    self.gc.borrow_mut().manage(data, self)
  }

  fn manage_obj<T: 'static + Object>(&self, data: T) -> GcObj<T> {
    self.gc.borrow_mut().manage_obj(data, self)
  }

  fn manage_tuple(&self, slice: &[Value]) -> Tuple {
    self.gc.borrow_mut().manage_tuple(slice, self)
  }

  fn manage_instance(&self, class: GcObj<Class>) -> Instance {
    self.gc.borrow_mut().manage_instance(class, self)
  }

  fn manage_str<S: AsRef<str>>(&self, string: S) -> GcStr {
    self.gc.borrow_mut().manage_str(string, self)
  }

  fn push_root<T: 'static + Trace>(&self, data: T) {
    self.gc.borrow_mut().push_root(data)
  }

  fn pop_roots(&self, count: usize) {
    self.gc.borrow_mut().pop_roots(count)
  }

  /// Update the current instruction pointer
  unsafe fn update_ip(&mut self, offset: isize) {
    self.ip = self.ip.offset(offset)
  }

  /// Store the ip in the current frame
  fn load_ip(&mut self) {
    self.ip = self.fiber.load_ip()
  }

  /// Store the ip in the current frame
  fn store_ip(&mut self) {
    self.fiber.store_ip(self.ip)
  }

  /// Get the current frame slots
  fn stack_start(&self) -> *mut Value {
    self.fiber.stack_start()
  }

  /// Get the current closure
  unsafe fn inline_cache(&mut self) -> &InlineCache {
    self
      .inline_cache
      .get_unchecked(self.current_fun.module_id())
  }

  /// Get the current closure
  unsafe fn inline_cache_mut(&mut self) -> &mut InlineCache {
    self
      .inline_cache
      .get_unchecked_mut(self.current_fun.module_id())
  }

  /// read a u8 out of the bytecode
  unsafe fn read_byte(&mut self) -> u8 {
    let byte = ptr::read(self.ip);
    self.update_ip(1);
    byte
  }

  /// read a u16 out of the bytecode
  unsafe fn read_short(&mut self) -> u16 {
    let slice = std::slice::from_raw_parts(self.ip, 2);
    let buffer = slice.try_into().expect("slice of incorrect length.");
    let short = u16::from_ne_bytes(buffer);
    self.update_ip(2);

    short
  }

  /// read a u32 out of the bytecode
  unsafe fn read_slot(&mut self) -> u32 {
    let slice = std::slice::from_raw_parts(self.ip, 4);
    let buffer = slice.try_into().expect("slice of incorrect length.");
    let short = u32::from_ne_bytes(buffer);
    self.update_ip(4);

    short
  }

  /// read a constant from the current chunk
  unsafe fn read_constant(&self, index: u16) -> Value {
    self
      .current_fun
      .chunk()
      .get_constant_unchecked(index as usize)
  }

  /// read a constant as a string from the current chunk
  unsafe fn read_string(&self, index: u16) -> GcStr {
    self.read_constant(index).to_obj().to_str()
  }

  /// push a literal value onto the stack
  unsafe fn op_literal(&mut self, value: Value) -> Signal {
    self.fiber.push(value);
    Signal::Ok
  }

  /// drop a value off the stack
  unsafe fn op_drop(&mut self) -> Signal {
    self.fiber.drop();
    Signal::Ok
  }

  /// drop a value off the stack
  unsafe fn op_drop_n(&mut self) -> Signal {
    let count = self.read_byte() as usize;
    self.fiber.drop_n(count);
    Signal::Ok
  }

  /// duplicate the top value on the stack
  unsafe fn op_dup(&mut self) -> Signal {
    let top = self.fiber.peek(0);
    self.fiber.push(top);
    Signal::Ok
  }

  /// create a list from a list literal
  unsafe fn op_list(&mut self) -> Signal {
    let arg_count = self.read_short() as usize;

    let args = self.fiber.stack_slice(arg_count);
    let list = val!(self.manage_obj(List::from(args)));
    self.fiber.drop_n(arg_count);
    self.fiber.push(list);

    Signal::Ok
  }

  /// create a list from a list literal
  unsafe fn op_tuple(&mut self) -> Signal {
    let arg_count = self.read_short() as usize;

    let args = self.fiber.stack_slice(arg_count);
    let list = val!(self.manage_tuple(args));
    self.fiber.drop_n(arg_count);
    self.fiber.push(list);

    Signal::Ok
  }

  /// create a map from a map literal
  unsafe fn op_map(&mut self) -> Signal {
    let arg_count = self.read_short() as usize;
    let mut map = self.manage_obj(Map::with_capacity(arg_count as usize));

    for i in 0..arg_count {
      let key = self.fiber.peek(i * 2 + 1);
      let value = self.fiber.peek(i * 2);

      map.insert(key, value);
    }

    self.fiber.drop_n(arg_count * 2);
    self.fiber.push(val!(map));

    Signal::Ok
  }

  /// launch a function on a separate fiber
  unsafe fn op_launch(&mut self) -> Signal {
    let arg_count = self.read_byte();
    let callee = self.fiber.peek(arg_count as usize);
    let frame_count = self.fiber.frames().len();
    let current_fun = self.current_fun;

    // call function as normal to get setup
    let signal = self.resolve_call(callee, arg_count);
    match signal {
      Signal::Ok => (),
      Signal::OkReturn => (),
      _ => return signal,
    };

    let hooks = GcHooks::new(self);
    let mut fiber = self.fiber;

    // call init fiber which will peel off the last frame if it's above the previous
    // water mark
    if let Some(new_fiber) = fiber.split_fiber(&hooks, frame_count, arg_count as usize) {
      // put the fiber in the queue
      self.fiber_queue.push_back(new_fiber);
      self.current_fun = current_fun;
      self.load_ip();
    }

    Signal::Ok
  }

  /// create synchronous channel
  unsafe fn op_channel(&mut self) -> Signal {
    let hooks = GcHooks::new(self);
    let channel = self.manage_obj(Channel::sync(&hooks));
    self.fiber.push(val!(channel));

    Signal::Ok
  }

  /// create a buffered channel
  unsafe fn op_buffered_channel(&mut self) -> Signal {
    let capacity = self.fiber.pop();

    if !capacity.is_num() {
      return self.runtime_error(
        self.builtin.errors.type_,
        &format!(
          "function \"chan\"'s parameter \"capacity\" required a number but received a {}.",
          ParameterKind::from(capacity)
        ),
      );
    }

    let capacity = capacity.to_num();
    if capacity.fract() != 0.0 || capacity < 1.0 {
      return self.runtime_error(
        self.builtin.errors.type_,
        "buffer must be an positive integer.",
      );
    }

    let hooks = GcHooks::new(self);
    let channel = self.manage_obj(Channel::with_capacity(&hooks, capacity as usize));
    self.fiber.push(val!(channel));

    Signal::Ok
  }

  /// receive from a channel
  unsafe fn op_receive(&mut self) -> Signal {
    let channel = self.fiber.pop();

    if_let_obj!(ObjectKind::Channel(mut channel) = (channel) {
      self.fiber.add_used_channel(channel);

      match channel.receive(self.fiber) {
        ReceiveResult::Ok(value) => {
          self.fiber.push(value);
          Signal::Ok
        }
        ReceiveResult::NoReceiveAccess => {
          self.runtime_error(self.builtin.errors.value, "todo no read access")
        }
        ReceiveResult::EmptyBlock(fiber) => {
          if let Some(mut fiber) = fiber.or_else(|| self.fiber.get_runnable())  {
            fiber.unblock();
            self.fiber_queue.push_back(fiber);
          }

          self.fiber.push(val!(channel));
          self.update_ip(-1);
          self.fiber.block();
          Signal::ContextSwitch
        },
        ReceiveResult::Empty(fiber) => {
          if let Some(mut fiber) = fiber.or_else(|| self.fiber.get_runnable()) {
            fiber.unblock();
            self.fiber_queue.push_back(fiber);
          }

          self.fiber.push(val!(channel));
          self.update_ip(-1);
          self.fiber.sleep();
          Signal::ContextSwitch
        },
        ReceiveResult::Closed => {
          self.fiber.push(VALUE_NIL);
          Signal::Ok
        }
      }
    } else {
      self.runtime_error(
        self.builtin.errors.type_,
        "Can only drain from a channel.",
      )
    })
  }

  /// send to a channel
  unsafe fn op_send(&mut self) -> Signal {
    let channel = self.fiber.pop();
    let value = self.fiber.peek(0);

    if_let_obj!(ObjectKind::Channel(mut channel) = (channel) {
      self.fiber.add_used_channel(channel);

      match channel.send(self.fiber, value) {
        SendResult::Ok => Signal::Ok,
        SendResult::NoSendAccess => self.runtime_error(
          self.builtin.errors.runtime,
          "Attempted to send into a receive only channel.",
        ),
        SendResult::FullBlock(fiber) => {
          // if channel has a waiter put into
          // the fiber queue
          if let Some(mut fiber) = fiber.or_else(|| self.fiber.get_runnable()) {
            fiber.unblock();
            self.fiber_queue.push_back(fiber);
          }

          // the channel enqueued the result but is now full so we need to switch
          self.fiber.block();
          Signal::ContextSwitch
        }
        SendResult::Full(fiber) => {
          // if channel has a waiter put into
          // the fiber queue
          if let Some(mut fiber) = fiber.or_else(|| self.fiber.get_runnable()) {
            fiber.unblock();
            self.fiber_queue.push_back(fiber);
          }

          // replace the channel on the stack and rewind the
          // ip to the beginning of this instruction
          self.fiber.push(val!(channel));
          self.update_ip(-1);
          self.fiber.sleep();
          Signal::ContextSwitch
        }
        SendResult::Closed => self.runtime_error(
          self.builtin.errors.runtime,
          "Attempted to send into a closed channel.",
        ),
      }
    } else {
      self.runtime_error(self.builtin.errors.runtime, "Attempted to send into a non channel.")
    })
  }

  /// create a map from a map literal
  unsafe fn op_interpolate(&mut self) -> Signal {
    let arg_count = self.read_short() as usize;
    let args = self.fiber.stack_slice(arg_count as usize);

    let mut length: usize = 0;
    for arg in args {
      length += arg.to_obj().to_str().len();
    }

    let mut buffers = String::with_capacity(length);
    for arg in args {
      buffers.push_str(&arg.to_obj().to_str())
    }

    self.fiber.drop_n(arg_count);
    let interpolated = val!(self.manage_str(buffers));
    self.fiber.push(interpolated);

    Signal::Ok
  }

  /// move an iterator to the next element
  unsafe fn op_iter_next(&mut self) -> Signal {
    let receiver = self.fiber.peek(0);

    if_let_obj!(ObjectKind::Enumerator(mut enumerator) = (receiver) {
      self.update_ip(2);
      match enumerator.next(&mut Hooks::new(self)) {
        Call::Ok(value) => {
          self.fiber.peek_set(0, value);
          Signal::Ok
        },
        Call::Err(LyError::Err(error)) => self.set_error(error),
        Call::Err(LyError::Exit(code)) => self.set_exit(code),
      }
    } else {
      let constant = self.read_short();
      let method_name = self.read_string(constant);
      self.invoke(receiver, method_name, 0)
    })
  }

  /// get the current value from an iterator
  unsafe fn op_iter_current(&mut self) -> Signal {
    let receiver = self.fiber.peek(0);

    if_let_obj!(ObjectKind::Enumerator(enumerator) = (receiver) {
      self.update_ip(2);
      let result = enumerator.current();
      self.fiber.peek_set(0, result);
      Signal::Ok
    } else {
      let constant = self.read_short();
      let method_name = self.read_string(constant);
      self.invoke(receiver, method_name, 0)
    })
  }

  /// call a function or method
  unsafe fn op_call(&mut self) -> Signal {
    let arg_count = self.read_byte();
    let callee = self.fiber.peek(arg_count as usize);

    self.resolve_call(callee, arg_count)
  }

  /// invoke a method on an instance's class
  unsafe fn op_invoke(&mut self) -> Signal {
    let constant = self.read_short();
    let arg_count = self.read_byte();
    let inline_slot = self.read_slot() as usize;

    let method_name = self.read_string(constant);
    let receiver = self.fiber.peek(arg_count as usize);

    let class = self.value_class(receiver);
    match self.inline_cache().get_invoke_cache(inline_slot, class) {
      Some(method) => self.resolve_call(method, arg_count),
      None => {
        if_let_obj!(ObjectKind::Instance(instance) = (receiver) {
          if let Some(field) = instance.get_field(method_name) {
            self.fiber.peek_set(arg_count as usize, *field);
            self.inline_cache_mut().clear_invoke_cache(inline_slot);
            return self.resolve_call(*field, arg_count);
          }
        });

        match class.get_method(&method_name) {
          Some(method) => {
            self
              .inline_cache_mut()
              .set_invoke_cache(inline_slot, class, method);
            self.resolve_call(method, arg_count)
          },
          None => self.runtime_error(
            self.builtin.errors.property,
            &format!(
              "Undefined property {} on class {}.",
              method_name,
              class.name()
            ),
          ),
        }
      },
    }
  }

  /// invoke a method
  unsafe fn invoke(&mut self, receiver: Value, method_name: GcStr, arg_count: u8) -> Signal {
    if_let_obj!(ObjectKind::Instance(instance) = (receiver) {
      if let Some(field) = instance.get_field(method_name) {
        self.fiber.peek_set(arg_count as usize, *field);
        return self.resolve_call(*field, arg_count);
      }
    });

    let class = self.value_class(receiver);
    self.invoke_from_class(class, method_name, arg_count)
  }

  /// Invoke a method on a instance's super class
  unsafe fn op_super_invoke(&mut self) -> Signal {
    let constant = self.read_short();
    let arg_count = self.read_byte();
    let inline_slot = self.read_slot() as usize;

    let method_name = self.read_string(constant);
    let super_class = self.fiber.pop().to_obj().to_class();

    match self
      .inline_cache()
      .get_invoke_cache(inline_slot, super_class)
    {
      Some(method) => self.resolve_call(method, arg_count),
      None => match super_class.get_method(&method_name) {
        Some(method) => {
          self
            .inline_cache_mut()
            .set_invoke_cache(inline_slot, super_class, method);
          self.resolve_call(method, arg_count)
        },
        None => self.runtime_error(
          self.builtin.errors.property,
          &format!(
            "Undefined property {} on class {}.",
            method_name,
            super_class.name()
          ),
        ),
      },
    }
  }

  /// Generate a new class
  unsafe fn op_class(&mut self) -> Signal {
    let slot = self.read_short();
    let name = self.read_string(slot);

    let class = val!(self.manage_obj(Class::bare(name)));
    self.fiber.push(class);
    Signal::Ok
  }

  unsafe fn op_inherit(&mut self) -> Signal {
    let super_class = self.fiber.peek(1);

    if !super_class.is_obj() {
      return self.runtime_error(self.builtin.errors.runtime, "Superclass must be a class.");
    }

    let super_class = super_class.to_obj();
    let super_class = match_obj!((&super_class) {
      ObjectKind::Class(class) => {
        class
      },
      ObjectKind::LyBox(ly_box) => {
        if ly_box.value.is_obj_kind(ObjectKind::Class) {
          ly_box.value.to_obj().to_class()
        } else {
          return self.runtime_error(self.builtin.errors.runtime, "Superclass must be a class.");
        }
      },
      _ => {
        return self.runtime_error(self.builtin.errors.runtime, "Superclass must be a class.");
      },
    });

    let hooks = GcHooks::new(self);
    let mut sub_class = self.fiber.peek(0).to_obj().to_class();

    sub_class.inherit(&hooks, super_class);
    sub_class.meta_from_super(&hooks);

    Signal::Ok
  }

  /// Get this classes super class
  unsafe fn op_get_super(&mut self) -> Signal {
    let slot = self.read_short();
    let name = self.read_string(slot);
    let super_class = self.fiber.pop().to_obj().to_class();

    self.bind_method(super_class, name)
  }

  /// Loop by performing an unconditional jump to a new instruction
  unsafe fn op_loop(&mut self) -> Signal {
    let jump = self.read_short() as isize;
    self.update_ip(-jump);
    Signal::Ok
  }

  /// Jump if the condition evaluates to a falsey value
  unsafe fn op_jump_if_false(&mut self) -> Signal {
    let jump = self.read_short();
    if is_falsey(self.fiber.peek(0)) {
      self.update_ip(jump as isize);
    }
    self.fiber.drop();

    Signal::Ok
  }

  /// Unconditionally jump to some other instruction
  unsafe fn op_jump(&mut self) -> Signal {
    let jump = self.read_short();
    self.update_ip(jump as isize);
    Signal::Ok
  }

  /// Define a global variable
  unsafe fn op_define_global(&mut self) -> Signal {
    let slot = self.read_short();
    let name = self.read_string(slot);
    let global = self.fiber.pop();
    let mut current_module = self.current_fun.module();
    match current_module.insert_symbol(&GcHooks::new(self), name, global) {
      Ok(_) => Signal::Ok,
      Err(_) => Signal::Ok,
    }
  }

  /// Box a local
  unsafe fn op_box(&mut self) -> Signal {
    let slot = self.read_byte() as isize;
    let slot = self.stack_start().offset(slot);
    let local = *slot;
    *slot = val!(self.manage_obj(LyBox::new(local)));
    Signal::Ok
  }

  /// Create a new empty box
  unsafe fn op_empty_box(&mut self) -> Signal {
    let value = val!(self.manage_obj(LyBox::default()));
    self.fiber.push(value);

    Signal::Ok
  }

  /// Move the top of the stack into a box
  unsafe fn op_fill_box(&mut self) -> Signal {
    let value = self.fiber.pop();
    self.fiber.peek(0).to_obj().to_box().value = value;

    Signal::Ok
  }

  unsafe fn op_set_global(&mut self) -> Signal {
    let slot = self.read_short();
    let name = self.read_string(slot);
    let value = self.fiber.peek(0);

    let mut current_module = self.current_fun.module();
    if current_module.set_symbol(name, value).is_err() {
      return self.runtime_error(
        self.builtin.errors.property,
        &format!("Undefined variable {}", name),
      );
    }

    Signal::Ok
  }

  unsafe fn op_set_local(&mut self) -> Signal {
    let slot = self.read_byte() as isize;
    let copy = self.fiber.peek(0);

    *self.stack_start().offset(slot) = copy;

    Signal::Ok
  }

  unsafe fn op_set_box(&mut self) -> Signal {
    let slot = self.read_byte() as isize;
    let copy = self.fiber.peek(0);

    (*self.stack_start().offset(slot)).to_obj().to_box().value = copy;

    Signal::Ok
  }

  unsafe fn op_set_capture(&mut self) -> Signal {
    let slot = self.read_byte();
    let value = self.fiber.peek(0);
    self
      .fiber
      .captures()
      .set_capture_value(slot as usize, value);

    Signal::Ok
  }

  unsafe fn op_set_property(&mut self) -> Signal {
    let slot = self.read_short();
    let instance = self.fiber.peek(1);
    let name = self.read_string(slot);
    let inline_slot = self.read_slot() as usize;

    if_let_obj!(ObjectKind::Instance(mut instance) = (instance) {
      let class = instance.class();

      match self.inline_cache().get_property_cache(inline_slot, class) {
        Some(property_slot) => {
          let value = self.fiber.pop();

          self.fiber.drop();
          self.fiber.push(value);

          instance[property_slot] = value;
          return Signal::Ok;
        },
        None => {
          let property_slot = class.get_field_index(&name);
          let value = self.fiber.pop();

          self.fiber.drop();
          self.fiber.push(value);

          return match property_slot {
            Some(property_slot) => {
              let cache = self.inline_cache_mut();
              cache.set_property_cache(inline_slot, class, property_slot as usize);
              instance[property_slot as usize] = value;
              Signal::Ok
            },
            None => self.runtime_error(
              self.builtin.errors.property,
              &format!("Undefined property {} on class {}.", name, class.name()),
            ),
          };
        },
      }
    });

    self.runtime_error(
      self.builtin.errors.runtime,
      "Only instances have settable fields.",
    )
  }

  unsafe fn op_get_global(&mut self) -> Signal {
    let store_index = self.read_short();
    let string = self.read_string(store_index);

    match self.current_fun.module().get_symbol(string) {
      Some(gbl) => {
        self.fiber.push(gbl);
        Signal::Ok
      },
      None => self.runtime_error(
        self.builtin.errors.runtime,
        &format!("Undefined variable {}", string),
      ),
    }
  }

  unsafe fn op_get_local(&mut self) -> Signal {
    let slot = self.read_byte() as isize;
    let local = *self.stack_start().offset(slot);
    self.fiber.push(local);
    Signal::Ok
  }

  unsafe fn op_get_box(&mut self) -> Signal {
    let slot = self.read_byte() as isize;
    let local = *self.stack_start().offset(slot);
    let local = local.to_obj().to_box().value;
    self.fiber.push(local);
    Signal::Ok
  }

  unsafe fn op_get_capture(&mut self) -> Signal {
    let slot = self.read_byte();

    let upvalue = self.fiber.captures().get_capture_value(slot as usize);
    self.fiber.push(upvalue);

    Signal::Ok
  }

  unsafe fn op_get_property(&mut self) -> Signal {
    let slot = self.read_short();
    let value = self.fiber.peek(0);
    let name = self.read_string(slot);
    let inline_slot = self.read_slot() as usize;

    if_let_obj!(ObjectKind::Instance(instance) = (value) {
      let class = instance.class();
      match self.inline_cache().get_property_cache(inline_slot, class) {
        Some(property_slot) => {
          self.fiber.peek_set(0, instance[property_slot]);
          return Signal::Ok;
        },
        None => {
          if let Some(property_slot) = class.get_field_index(&name) {
            self
              .inline_cache_mut()
              .set_property_cache(inline_slot, class, property_slot as usize);

            self.fiber.peek_set(0, instance[property_slot as usize]);
            return Signal::Ok;
          }
        },
      }
    });

    let class = self.value_class(value);
    let cache = self.inline_cache_mut();
    cache.clear_property_cache(inline_slot);
    self.bind_method(class, name)
  }

  unsafe fn op_import(&mut self) -> Signal {
    let index_path = self.read_short();
    let path = self.read_constant(index_path).to_obj().to_list();

    let path_segments = self.extract_import_path(path);

    // check if fully resolved module has already been loaded
    let resolved = self.full_import_path(&path_segments);
    self.push_root(resolved);

    if let Some(module) = self.module_cache.get(&resolved) {
      let imported = module.module_instance(&GcHooks::new(self));
      self.fiber.push(val!(imported));
      self.pop_roots(2);
      return Signal::Ok;
    }

    let import = self.build_import(&path_segments);
    self.push_root(import);

    let result = match self.packages.get(&import.package()) {
      Some(package) => match package.import(&GcHooks::new(self), import) {
        Ok(module) => {
          self.fiber.push(val!(module));
          Signal::Ok
        },
        Err(err) => self.runtime_error(self.builtin.errors.runtime, &err.to_string()),
      },
      None => self.runtime_error(
        self.builtin.errors.import,
        &format!("Package {} does not exist", &import.package()),
      ),
    };

    self.pop_roots(2);
    result
  }

  unsafe fn op_import_symbol(&mut self) -> Signal {
    let index_path = self.read_short();
    let index_name = self.read_short();
    let path = self.read_constant(index_path).to_obj().to_list();
    let name = self.read_string(index_name);

    let path_segments = self.extract_import_path(path);

    // check if fully resolved module has already been loaded
    let resolved = self.full_import_path(&path_segments);
    self.push_root(resolved);

    if let Some(module) = self.module_cache.get(&resolved) {
      let imported = module.module_instance(&GcHooks::new(self));
      self.fiber.push(val!(imported));
      self.pop_roots(2);
      return Signal::Ok;
    }

    let import = self.build_import(&path_segments);
    self.push_root(import);

    let result = match self.packages.get(&import.package()) {
      Some(package) => match package.import_symbol(&GcHooks::new(self), import, name) {
        Ok(module) => {
          self.fiber.push(val!(module));
          Signal::Ok
        },
        Err(err) => self.runtime_error(self.builtin.errors.runtime, &err.to_string()),
      },
      None => self.runtime_error(
        self.builtin.errors.import,
        &format!("Package {} does not exist", &import.package()),
      ),
    };

    self.pop_roots(2);
    result
  }

  fn extract_import_path(&mut self, path: GcObj<List<Value>>) -> List<GcStr> {
    let mut path_segments: List<GcStr> = List::with_capacity(path.len());
    path_segments.extend(path.iter().map(|segment| segment.to_obj().to_str()));
    path_segments
  }

  fn full_import_path(&mut self, path_segments: &[GcStr]) -> GcStr {
    let mut buffer = String::new();
    for segment in &path_segments[..path_segments.len() - 1] {
      buffer.push_str(segment);
      buffer.push('/');
    }

    buffer.push_str(&path_segments[path_segments.len() - 1]);

    self.manage_str(buffer)
  }

  fn build_import(&mut self, path_segments: &[GcStr]) -> Gc<Import> {
    match path_segments.split_first() {
      Some((package, path)) => {
        // generate a new import object
        let path = self.manage(path);
        self.manage(Import::new(*package, path))
      },
      None => {
        // generate a new import object
        let path: Array<GcStr> = self.manage(&[] as &[GcStr]);
        self.manage(Import::new(path_segments[0], path))
      },
    }
  }

  unsafe fn op_export(&mut self) -> Signal {
    let index = self.read_short();
    let name = self.read_string(index);
    let mut current_module = self.current_fun.module();

    match current_module.export_symbol(name) {
      Ok(_) => Signal::Ok,
      Err(error) => self.runtime_error(self.builtin.errors.export, &error.to_string()),
    }
  }

  /// return from a laythe function placing the result on top of the stack
  unsafe fn op_return(&mut self) -> Signal {
    // get the function result and pop frame
    let result = self.fiber.pop();

    // pop a frame from the call stack return signal if provided
    if let Some(signal) = self.pop_frame() {
      return signal;
    }

    // push result onto stack
    self.fiber.push(result);
    Signal::OkReturn
  }

  unsafe fn op_negate(&mut self) -> Signal {
    let pop = self.fiber.pop();

    if pop.is_num() {
      self.fiber.push(val!(-pop.to_num()));
      Signal::Ok
    } else {
      self.runtime_error(self.builtin.errors.runtime, "Operand must be a number.")
    }
  }

  unsafe fn op_not(&mut self) -> Signal {
    let value = self.fiber.pop();
    self.fiber.push(val!(is_falsey(value)));
    Signal::Ok
  }

  unsafe fn op_add(&mut self) -> Signal {
    let (right, left) = (self.fiber.pop(), self.fiber.pop());

    if right.is_num() && left.is_num() {
      self.fiber.push(val!(left.to_num() + right.to_num()));
      Signal::Ok
    } else if right.is_obj_kind(ObjectKind::String) && left.is_obj_kind(ObjectKind::String) {
      let left = left.to_obj().to_str();
      let right = right.to_obj().to_str();

      let mut buffer = String::with_capacity(left.len() + right.len());
      buffer.push_str(&*left);
      buffer.push_str(&*right);

      let string = self.manage_str(buffer);
      self.fiber.push(val!(string));
      Signal::Ok
    } else {
      self.runtime_error(
        self.builtin.errors.runtime,
        "Operands must be two numbers or two strings.",
      )
    }
  }

  unsafe fn op_sub(&mut self) -> Signal {
    let (right, left) = (self.fiber.pop(), self.fiber.pop());

    if right.is_num() && left.is_num() {
      self.fiber.push(val!(left.to_num() - right.to_num()));
      return Signal::Ok;
    }

    self.runtime_error(self.builtin.errors.runtime, "Operands must be numbers.")
  }

  unsafe fn op_mul(&mut self) -> Signal {
    let (right, left) = (self.fiber.pop(), self.fiber.pop());

    if right.is_num() && left.is_num() {
      self.fiber.push(val!(left.to_num() * right.to_num()));
      return Signal::Ok;
    }

    self.runtime_error(self.builtin.errors.runtime, "Operands must be numbers.")
  }

  unsafe fn op_div(&mut self) -> Signal {
    let (right, left) = (self.fiber.pop(), self.fiber.pop());

    if right.is_num() && left.is_num() {
      self.fiber.push(val!(left.to_num() / right.to_num()));
      return Signal::Ok;
    }

    self.runtime_error(self.builtin.errors.runtime, "Operands must be numbers.")
  }

  unsafe fn op_and(&mut self) -> Signal {
    let jump = self.read_short();
    let left = self.fiber.peek(0);

    if is_falsey(left) {
      self.update_ip(jump as isize);
    } else {
      self.fiber.drop();
    }

    Signal::Ok
  }

  unsafe fn op_or(&mut self) -> Signal {
    let jump = self.read_short();
    let left = self.fiber.peek(0);

    if is_falsey(left) {
      self.fiber.drop();
    } else {
      self.update_ip(jump as isize);
    }

    Signal::Ok
  }

  unsafe fn op_less(&mut self) -> Signal {
    let (right, left) = (self.fiber.pop(), self.fiber.pop());

    if right.is_num() && left.is_num() {
      self.fiber.push(val!(left.to_num() < right.to_num()));
      return Signal::Ok;
    }

    if right.is_obj_kind(ObjectKind::String) && left.is_obj_kind(ObjectKind::String) {
      self.fiber.push(val!(
        (*left.to_obj().to_str()).cmp(&right.to_obj().to_str()) == Ordering::Less
      ));
      return Signal::Ok;
    }

    self.runtime_error(self.builtin.errors.runtime, "Operands must be numbers.")
  }

  unsafe fn op_less_equal(&mut self) -> Signal {
    let (right, left) = (self.fiber.pop(), self.fiber.pop());

    if right.is_num() && left.is_num() {
      self.fiber.push(val!(left.to_num() <= right.to_num()));
      return Signal::Ok;
    }

    if right.is_obj_kind(ObjectKind::String) && left.is_obj_kind(ObjectKind::String) {
      if left == right {
        self.fiber.push(VALUE_TRUE);
      } else {
        self.fiber.push(val!(
          (*left.to_obj().to_str()).cmp(&right.to_obj().to_str()) == Ordering::Less
        ));
      }

      return Signal::Ok;
    }

    self.runtime_error(
      self.builtin.errors.runtime,
      "Operands must be numbers or strings.",
    )
  }

  unsafe fn op_greater(&mut self) -> Signal {
    let (right, left) = (self.fiber.pop(), self.fiber.pop());

    if right.is_num() && left.is_num() {
      self.fiber.push(val!(left.to_num() > right.to_num()));
      return Signal::Ok;
    }

    if right.is_obj_kind(ObjectKind::String) && left.is_obj_kind(ObjectKind::String) {
      self.fiber.push(val!(
        (*left.to_obj().to_str()).cmp(&right.to_obj().to_str()) == Ordering::Greater
      ));
      return Signal::Ok;
    }

    self.runtime_error(
      self.builtin.errors.runtime,
      "Operands must be numbers or strings.",
    )
  }

  unsafe fn op_greater_equal(&mut self) -> Signal {
    let (right, left) = (self.fiber.pop(), self.fiber.pop());

    if right.is_num() && left.is_num() {
      self.fiber.push(val!(left.to_num() >= right.to_num()));
      return Signal::Ok;
    }

    if right.is_obj_kind(ObjectKind::String) && left.is_obj_kind(ObjectKind::String) {
      if left == right {
        self.fiber.push(VALUE_TRUE);
      } else {
        self.fiber.push(val!(
          (*left.to_obj().to_str()).cmp(&right.to_obj().to_str()) == Ordering::Greater
        ));
      }

      return Signal::Ok;
    }

    self.runtime_error(
      self.builtin.errors.runtime,
      "Operands must be numbers or strings.",
    )
  }

  unsafe fn op_equal(&mut self) -> Signal {
    let right = self.fiber.pop();
    let left = self.fiber.pop();

    self.fiber.push(val!(left == right));
    Signal::Ok
  }

  unsafe fn op_not_equal(&mut self) -> Signal {
    let right = self.fiber.pop();
    let left = self.fiber.pop();

    self.fiber.push(val!(left != right));
    Signal::Ok
  }

  unsafe fn op_method(&mut self) -> Signal {
    let slot = self.read_short();
    let name = self.read_string(slot);

    let class = self.fiber.peek(1);
    let method = self.fiber.peek(0);

    if class.is_obj_kind(ObjectKind::Class) && method.is_obj_kind(ObjectKind::Closure) {
      class.to_obj().to_class().add_method(name, method);
    } else {
      self.internal_error("Invalid Stack for op_method.");
    }

    self.fiber.drop();
    Signal::Ok
  }

  unsafe fn op_field(&mut self) -> Signal {
    let slot = self.read_short();
    let name = self.read_string(slot);

    let class = self.fiber.peek(0);

    if_let_obj!(ObjectKind::Class(mut class) = (class) {
      class.add_field(name);
    } else {
      self.internal_error("Invalid Stack for op_method.");
    });

    Signal::Ok
  }

  unsafe fn op_static_method(&mut self) -> Signal {
    let slot = self.read_short();
    let name = self.read_string(slot);

    let class = self.fiber.peek(1);
    let method = self.fiber.peek(0);

    if class.is_obj_kind(ObjectKind::Class) && method.is_obj_kind(ObjectKind::Closure) {
      let class = class.to_obj().to_class();

      match class.meta_class() {
        Some(mut meta) => {
          meta.add_method(name, method);
        },
        None => self.internal_error(&format!("{} meta class not set.", class.name())),
      }
    } else {
      self.internal_error("Invalid Stack for op_static_method.");
    }

    self.fiber.drop();
    Signal::Ok
  }

  unsafe fn op_closure(&mut self) -> Signal {
    let slot = self.read_short();
    let fun = self.read_constant(slot).to_obj().to_fun();

    let captures = (0..fun.capture_count())
      .map(|_| {
        let capture_index: CaptureIndex = mem::transmute(self.read_short());
        match capture_index {
          CaptureIndex::Local(index) => (*self.fiber.stack_start().offset(index as isize))
            .to_obj()
            .to_box(),
          CaptureIndex::Enclosing(index) => self.fiber.captures().get_capture(index as usize),
        }
      })
      .collect::<Vec<GcObj<LyBox>>>();

    let captures = Captures::new(&GcHooks::new(self), &captures);
    let closure = self.manage_obj(Closure::new(fun, captures));

    self.fiber.push(val!(closure));

    Signal::Ok
  }

  unsafe fn op_constant(&mut self) -> Signal {
    let slot = self.read_byte();
    let constant = self.read_constant(slot as u16);
    self.fiber.push(constant);

    Signal::Ok
  }

  unsafe fn op_constant_long(&mut self) -> Signal {
    let slot = self.read_short();
    let constant = self.read_constant(slot);
    self.fiber.push(constant);

    Signal::Ok
  }

  /// resolve the current value and call in the appropriate manner
  unsafe fn resolve_call(&mut self, callee: Value, arg_count: u8) -> Signal {
    if !callee.is_obj() {
      let class_name = self.value_class(callee).name();
      return self.runtime_error(
        self.builtin.errors.runtime,
        &format!("{} is not callable.", class_name),
      );
    }

    match_obj!((&callee.to_obj()) {
      ObjectKind::Closure(closure) => {
        self.call_closure(closure, arg_count)
      },
      ObjectKind::Method(method) => {
        self.call_method(method, arg_count)
      },
      ObjectKind::Native(native) => {
        self.call_native(native, arg_count)
      },
      ObjectKind::Class(class) => {
        self.call_class(class, arg_count)
      },
      ObjectKind::Fun(fun) => {
        self.call(fun, arg_count)
      },
      _ => {
        let class_name = self.value_class(callee).name();
        self.runtime_error(
          self.builtin.errors.runtime,
          &format!("{} is not callable.", class_name),
        )
      },
    })
  }

  /// call a class creating a new instance of that class
  unsafe fn call_class(&mut self, class: GcObj<Class>, arg_count: u8) -> Signal {
    let instance = val!(self.manage_instance(class));
    self.fiber.peek_set(arg_count as usize, instance);

    match class.init() {
      Some(init) => self.resolve_call(init, arg_count),
      None => {
        if arg_count != 0 {
          self.runtime_error(
            self.builtin.errors.runtime,
            &format!("Expected 0 arguments but got {}", arg_count),
          )
        } else {
          Signal::Ok
        }
      },
    }
  }

  /// call a native function immediately returning the result
  unsafe fn call_native(&mut self, native: GcObj<Native>, arg_count: u8) -> Signal {
    let meta = native.meta();
    let mut fiber = self.fiber;
    let args = fiber.stack_slice(arg_count as usize);

    // check that the current function is called with the right number of args and types
    if let Some(signal) = self.check_native_arity(meta, args) {
      return signal;
    }

    let this = if meta.is_method {
      Some(fiber.peek(arg_count as usize))
    } else {
      None
    };

    #[cfg(debug_assertions)]
    let roots_before = self.gc().temp_roots();

    match meta.environment {
      Environment::StackLess => match native.call(&mut Hooks::new(self), this, args) {
        Call::Ok(value) => {
          fiber.drop_n(arg_count as usize + 1);
          fiber.push(value);

          #[cfg(debug_assertions)]
          {
            let roots_current = self.gc().temp_roots();
            assert_roots(native, roots_before, roots_current);
          }
          Signal::OkReturn
        },
        Call::Err(LyError::Err(error)) => self.set_error(error),
        Call::Err(LyError::Exit(code)) => self.set_exit(code),
      },
      Environment::Normal => {
        let mut stub = self.native_fun_stubs.pop().unwrap_or_else(|| {
          self.manage_obj(Fun::stub(
            &GcHooks::new(self),
            meta.name,
            self.global,
            AlignedByteCode::Nil,
          ))
        });
        stub.set_name(meta.name);
        self.push_root(stub);

        self.pop_roots(1);

        self.push_frame(stub, self.capture_stub, arg_count);

        let result = native.call(&mut Hooks::new(self), this, args);
        self.native_fun_stubs.push(stub);

        match result {
          Call::Ok(value) => {
            self.pop_frame();
            fiber.push(value);

            #[cfg(debug_assertions)]
            {
              let roots_current = self.gc().temp_roots();
              assert_roots(native, roots_before, roots_current);
            }
            Signal::OkReturn
          },
          Call::Err(LyError::Err(error)) => self.set_error(error),
          Call::Err(LyError::Exit(code)) => self.set_exit(code),
        }
      },
    }
  }

  /// call a laythe function setting it as the new call frame
  unsafe fn call_closure(&mut self, closure: GcObj<Closure>, arg_count: u8) -> Signal {
    // check that the current function is called with the right number of args
    if let Some(error) = self.check_arity(closure.fun(), arg_count) {
      return error;
    }

    // set the current current instruction pointer. check for overflow
    if self.fiber.frames().len() == MAX_FRAME_SIZE {
      return self.runtime_error(self.builtin.errors.runtime, "Stack overflow.");
    }

    self.push_frame(closure.fun(), closure.captures(), arg_count);
    Signal::Ok
  }

  /// call a laythe function setting it as the new call frame
  unsafe fn call(&mut self, fun: GcObj<Fun>, arg_count: u8) -> Signal {
    // check that the current function is called with the right number of args
    if let Some(error) = self.check_arity(fun, arg_count) {
      return error;
    }

    // set the current current instruction pointer. check for overflow
    if self.fiber.frames().len() == MAX_FRAME_SIZE {
      return self.runtime_error(self.builtin.errors.runtime, "Stack overflow.");
    }

    self.push_frame(fun, self.capture_stub, arg_count);
    Signal::Ok
  }

  /// Swap between the current fiber and the provided fiber
  unsafe fn context_switch(&mut self, fiber: GcObj<Fiber>) {
    if !self.fiber.is_complete() {
      self.store_ip();
    }

    self.fiber = fiber;
    self.fiber.activate();

    self.load_ip();
    self.current_fun = fiber.fun();
  }

  /// Push a call frame onto the the call frame stack
  unsafe fn push_frame(&mut self, closure: GcObj<Fun>, captures: Captures, arg_count: u8) {
    self.store_ip();

    self.fiber.push_frame(closure, captures, arg_count as usize);
    self.load_ip();

    self.current_fun = closure;
  }

  /// Pop a frame off the call stack. If no frame remain
  /// return the exit signal otherwise set the instruction
  /// pointer and current function
  unsafe fn pop_frame(&mut self) -> Option<Signal> {
    match self.fiber.pop_frame() {
      Some(current_fun) => match current_fun {
        Some(current_fun) => {
          self.current_fun = current_fun;
          self.load_ip();
          None
        },
        None => {
          if self.fiber == self.main_fiber {
            Some(Signal::Exit)
          } else {
            if let Some(mut fiber) = Fiber::complete(self.fiber) {
              fiber.unblock();
              self.fiber_queue.push_back(fiber);
            }
            Some(Signal::ContextSwitch)
          }
        },
      },
      None => self.internal_error("Compilation failure attempted to pop last frame"),
    }
  }

  /// check that the number of args is valid for the function arity
  unsafe fn check_arity(&mut self, fun: GcObj<Fun>, arg_count: u8) -> Option<Signal> {
    match fun.arity().check(arg_count) {
      Ok(_) => None,
      Err(error) => match error {
        ArityError::Fixed(arity) => Some(self.runtime_error(
          self.builtin.errors.runtime,
          &format!(
            "{} expected {} argument(s) but got {}.",
            fun.name(),
            arity,
            arg_count,
          ),
        )),
        ArityError::Variadic(arity) => Some(self.runtime_error(
          self.builtin.errors.runtime,
          &format!(
            "{} expected at least {} argument(s) but got {}.",
            fun.name(),
            arity,
            arg_count,
          ),
        )),
        ArityError::DefaultLow(arity) => Some(self.runtime_error(
          self.builtin.errors.runtime,
          &format!(
            "{} expected at least {} argument(s) but got {}.",
            fun.name(),
            arity,
            arg_count,
          ),
        )),
        ArityError::DefaultHigh(arity) => Some(self.runtime_error(
          self.builtin.errors.runtime,
          &format!(
            "{} expected at most {} argument(s) but got {}.",
            fun.name(),
            arity,
            arg_count,
          ),
        )),
      },
    }
  }

  /// Check the arity of a native functio
  unsafe fn check_native_arity(
    &mut self,
    native_meta: &NativeMeta,
    args: &[Value],
  ) -> Option<Signal> {
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
            self.builtin.errors.runtime,
            &format!(
              "{} \"{}\" expected {} argument(s) but received {}.",
              callable_type,
              &*native_meta.name,
              expected,
              args.len(),
            ),
          )),
          SignatureError::LengthVariadic(expected) => Some(self.runtime_error(
            self.builtin.errors.runtime,
            &format!(
              "{} \"{}\" expected at least {} argument(s) but received {}.",
              callable_type,
              &*native_meta.name,
              expected,
              args.len(),
            ),
          )),
          SignatureError::LengthDefaultLow(expected) => Some(self.runtime_error(
            self.builtin.errors.runtime,
            &format!(
              "{} \"{}\" expected at least {} argument(s) but received {}.",
              callable_type,
              &*native_meta.name,
              expected,
              args.len(),
            ),
          )),
          SignatureError::LengthDefaultHigh(expected) => Some(self.runtime_error(
            self.builtin.errors.runtime,
            &format!(
              "{} \"{}\" expected at most {} argument(s) but received {}.",
              callable_type,
              &*native_meta.name,
              expected,
              args.len(),
            ),
          )),
          SignatureError::TypeWrong(parameter) => Some(self.runtime_error(
            self.builtin.errors.type_,
            &format!(
              "{} \"{}\"'s parameter \"{}\" required a {} but received a {}.",
              callable_type,
              &*native_meta.name,
              &*native_meta.signature.parameters[parameter as usize].name,
              native_meta.signature.parameters[parameter as usize].kind,
              ParameterKind::from(args[parameter as usize])
            ),
          )),
        }
      },
    }
  }

  /// Call a bound method
  unsafe fn call_method(&mut self, bound: GcObj<Method>, arg_count: u8) -> Signal {
    self.fiber.peek_set(arg_count as usize, bound.receiver());
    self.resolve_call(bound.method(), arg_count)
  }

  /// bind a method to an instance
  unsafe fn bind_method(&mut self, class: GcObj<Class>, name: GcStr) -> Signal {
    match class.get_method(&name) {
      Some(method) => {
        let bound = self.manage_obj(Method::new(self.fiber.peek(0), method));
        self.fiber.peek_set(0, val!(bound));
        Signal::Ok
      },
      None => self.runtime_error(
        self.builtin.errors.runtime,
        &format!("Undefined property {} on class {}.", name, class.name()),
      ),
    }
  }

  /// invoke a method from the provided class
  unsafe fn invoke_from_class(
    &mut self,
    class: GcObj<Class>,
    method_name: GcStr,
    arg_count: u8,
  ) -> Signal {
    match class.get_method(&method_name) {
      Some(method) => self.resolve_call(method, arg_count),
      None => self.runtime_error(
        self.builtin.errors.property,
        &format!(
          "Undefined property {} on class {}.",
          method_name,
          class.name()
        ),
      ),
    }
  }

  /// Print debugging information for the current instruction
  #[cfg(feature = "debug")]
  unsafe fn print_state(&self, ip: *const u8) -> io::Result<usize> {
    let mut stdio = self.io.stdio();

    self.print_stack_debug(&mut stdio)?;

    let start = self.current_fun.chunk().instructions().as_ptr();
    let offset = ip.offset_from(start) as usize;
    disassemble_instruction(&mut stdio, &self.current_fun.chunk(), offset, false)
  }

  /// Print debugging information for the current hook
  #[cfg(feature = "debug")]
  unsafe fn print_hook_state(&self, hook_name: &str, with: &str) -> io::Result<()> {
    let mut stdio = self.io.stdio();

    self.print_stack_debug(&mut stdio)?;

    let stdout = stdio.stdout();
    writeln!(stdout, "  Vm Hook {}: {}", hook_name, with)
  }

  /// Print the current stack
  #[cfg(feature = "debug")]
  unsafe fn print_stack_debug(&self, stdio: &mut Stdio) -> io::Result<()> {
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
        s.chars().into_iter().take(60).collect::<String>()
      )?;
    }

    writeln!(stdout)
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

  /// Report an internal issue to the user
  fn internal_error(&self, message: &str) -> ! {
    panic!("Internal Error: {}", message)
  }

  /// Report a known laythe runtime error to the user
  unsafe fn runtime_error(&mut self, error: GcObj<Class>, message: &str) -> Signal {
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

  /// Set the current error place the vm signal a runtime error
  fn set_error(&mut self, error: Instance) -> Signal {
    self.fiber.set_error(error);
    Signal::RuntimeError
  }

  /// Set the current error place the vm signal a runtime error
  fn set_exit(&mut self, code: u16) -> Signal {
    self.exit_code = code;
    Signal::Exit
  }

  /// Search for a catch block up the stack, printing the error if no catch is found
  unsafe fn stack_unwind(&mut self, error: Instance, mode: ExecuteMode) -> Option<ExecuteResult> {
    self.store_ip();

    let bottom_frame = match mode {
      ExecuteMode::Normal => 0,
      ExecuteMode::CallFunction(depth) => depth,
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

  /// Print an error message and the current call stack to the user
  fn print_error(&mut self, error: Instance) {
    let mut stdio = self.io.stdio();
    let stderr = stdio.stderr();

    self.fiber.print_error(stderr, error);
  }
}

#[cfg(debug_assertions)]
fn assert_roots(native: GcObj<Native>, roots_before: usize, roots_now: usize) {
  assert!(
    roots_before == roots_now,
    "Native function {} increased roots by {}.",
    native.meta().name,
    roots_now - roots_before,
  );
}

impl TraceRoot for Vm {
  fn trace(&self) {
    self.fiber.trace();
    self.main_fiber.trace();
    for fiber in &self.fiber_queue {
      fiber.trace();
    }
    self.files.trace();
    self.packages.trace();
    self.module_cache.trace();
    self.capture_stub.trace();

    for stub in &self.native_fun_stubs {
      stub.trace();
    }
  }

  fn trace_debug(&self, log: &mut dyn Write) {
    self.fiber.trace_debug(log);
    self.main_fiber.trace_debug(log);
    for fiber in &self.fiber_queue {
      fiber.trace_debug(log);
    }
    self.files.trace_debug(log);
    self.packages.trace_debug(log);
    self.module_cache.trace_debug(log);
    self.capture_stub.trace_debug(log);

    for stub in &self.native_fun_stubs {
      stub.trace_debug(log);
    }
  }

  fn can_collect(&self) -> bool {
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
    let result = unsafe { self.run_fun(callable, args) };
    self.to_call_result(result)
  }

  fn call_method(&mut self, this: Value, method: Value, args: &[Value]) -> Call {
    let result = unsafe { self.run_method(this, method, args) };
    self.to_call_result(result)
  }

  fn get_method(&mut self, this: Value, method_name: GcStr) -> Call {
    unsafe { self.get_method(this, method_name) }
  }

  fn get_class(&mut self, this: Value) -> Value {
    val!(self.value_class(this))
  }
}
