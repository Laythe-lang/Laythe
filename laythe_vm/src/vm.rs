use crate::{
  cache::InlineCache,
  call_frame::CallFrame,
  compiler::{Compiler, Parser},
  constants::{DEFAULT_STACK_MAX, FRAME_MAX, REPL_MODULE},
  files::{VmFileId, VmFiles},
  FeResult,
};
use codespan_reporting::term::{self, Config};
use laythe_core::{
  chunk::{AlignedByteCode, ByteCode, UpvalueIndex},
  constants::{PLACEHOLDER_NAME, SCRIPT, SELF},
  hooks::{GcContext, GcHooks, HookContext, Hooks, NoContext, ValueContext},
  managed::{Gc, GcStr, Manage, Trace, TraceRoot},
  memory::Allocator,
  module::{Import, Module, Package},
  object::{
    Class, Closure, Fun, FunBuilder, Instance, List, Map, Method, Native, NativeMeta, Upvalue,
  },
  signature::{ArityError, Environment, ParameterKind, SignatureError},
  utils::{is_falsey, ptr_len, IdEmitter},
  val,
  value::{Value, ValueKind, VALUE_NIL, VALUE_TRUE},
  Call,
};
use laythe_env::io::Io;
use laythe_lib::{builtin_from_module, create_std_lib, BuiltIn};
use laythe_native::io::io_native;
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

  /// The currently loaded files
  files: VmFiles,

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

  /// A collection of currently available upvalues
  open_upvalues: Vec<Gc<Upvalue>>,

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

    let root_dir = io
      .env()
      .current_dir()
      .expect("Could not obtain the current working directory.");

    let mut emitter = IdEmitter::default();
    let std_lib = create_std_lib(&hooks, &mut emitter).expect("Standard library creation failed");
    let global = std_lib.root_module();

    let builder = FunBuilder::new(hooks.manage_str(PLACEHOLDER_NAME), global);

    let managed_fun = hooks.manage(builder.build());
    let closure = hooks.manage(Closure::without_upvalues(managed_fun));

    let mut frames = vec![CallFrame::new(closure); FRAME_MAX];
    let mut stack = vec![VALUE_NIL; DEFAULT_STACK_MAX];

    let builtin = builtin_from_module(&hooks, &global)
      .expect("Failed to generate builtin class from global module");

    let current_frame = &mut frames[0];
    let current_fun = current_frame.closure.fun();
    let stack_top = &mut stack[1] as *mut Value;
    let ip = ptr::null();

    let current_frame = current_frame as *mut CallFrame;
    let mut native_builder = FunBuilder::new(hooks.manage_str("native"), global);
    native_builder.write_instruction(AlignedByteCode::Nil, 0);

    let native_fun_stub = hooks.manage(native_builder.build());

    let gc = RefCell::new(no_gc_context.done());
    let inline_cache: Vec<InlineCache> = (0..emitter.id_count())
      .map(|_| InlineCache::new(0, 0))
      .collect();

    let mut vm = Vm {
      io,
      stack,
      frames,
      gc,
      files: VmFiles::default(),
      builtin,
      root_dir,
      packages: Map::default(),
      emitter,
      module_cache: Map::default(),
      inline_cache,
      global,
      frame_count: 1,
      current_fun,
      current_frame,
      current_error: None,
      exit_code: 0,
      stack_top,
      ip,
      native_fun_stub,
      open_upvalues: Vec::with_capacity(100),
    };
    vm.add_package(std_lib);

    vm
  }

  /// The current version of the virtual machine
  pub fn version() -> &'static str {
    VERSION
  }

  /// Start the interactive repl
  pub fn repl(&mut self) -> ExecuteResult {
    let mut stdio = self.io.stdio();

    let repl_path = self.root_dir.join(PathBuf::from(REPL_MODULE));
    let main_id = self.emitter.emit();

    let main_module = self.main_module(repl_path.clone(), main_id);

    loop {
      let mut buffer = String::new();

      if write!(stdio.stdout(), "laythe:> ").is_err() {
        return ExecuteResult::InternalError;
      }
      stdio.stdout().flush().expect("Could not write to stdout");

      match stdio.read_line(&mut buffer) {
        Ok(_) => {
          let managed_source = self.manage_str(buffer);
          self.push_root(managed_source);

          let managed_path = self.manage_str(repl_path.to_string_lossy());
          self.push_root(managed_path);

          let file_id = self.files.upsert(managed_path, managed_source);
          self.pop_roots(2);

          self.interpret(main_module, &managed_source, file_id);
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

        self.root_dir = directory;
        let managed_source = self.manage_str(source);
        self.push_root(managed_source);

        let managed_path = self.manage_str(module_path.to_string_lossy());
        self.push_root(managed_path);

        let file_id = self.files.upsert(managed_path, managed_source);
        self.pop_roots(2);

        let main_id = self.emitter.emit();
        let main_module = self.main_module(module_path, main_id);

        self.interpret(main_module, &managed_source, file_id)
      }
      Err(err) => {
        writeln!(self.io.stdio().stderr(), "{}", &err.to_string())
          .expect("Unable to write to stderr");
        ExecuteResult::RuntimeError
      }
    }
  }

  /// Add a package to the vm
  pub fn add_package(&mut self, package: Gc<Package>) {
    self.packages.insert(package.name(), package);
  }

  /// Interpret the provided laythe script returning the execution result
  fn interpret(
    &mut self,
    main_module: Gc<Module>,
    source: &str,
    file_id: VmFileId,
  ) -> ExecuteResult {
    match self.compile(main_module, source, file_id) {
      Ok(fun) => {
        self.prepare(fun);
        self.execute(ExecuteMode::Normal)
      }
      Err(errors) => {
        let mut stdio = self.io.stdio();
        let stderr_color = stdio.stderr_color();
        for error in errors.iter() {
          term::emit(stderr_color, &Config::default(), &self.files, error)
            .expect("Unable to write to stderr");
        }
        ExecuteResult::CompileError
      }
    }
  }

  /// Compile the provided laythe source into the virtual machine's bytecode
  fn compile(
    &mut self,
    module: Gc<Module>,
    source: &str,
    file_id: VmFileId,
  ) -> FeResult<Gc<Fun>, VmFileId> {
    let (ast, line_offsets) = Parser::new(&source, file_id).parse();
    self
      .files
      .update_line_offsets(file_id, line_offsets.clone())
      .expect("File id not set for line offsets");

    let ast = ast?;
    let gc = self.gc.replace(Allocator::default());
    let compiler = Compiler::new(module, &ast, &line_offsets, file_id, self, gc);

    #[cfg(feature = "debug")]
    let compiler = compiler.with_io(self.io.clone());

    let (result, gc, cache_id_emitter) = compiler.compile();
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
      self.manage(fun)
    })
  }

  /// Reset the vm to execute another script
  fn prepare(&mut self, script: Gc<Fun>) {
    let script = self.manage(Closure::without_upvalues(script));

    self.reset_stack();
    let result = self.call(script, 0);

    if result != Signal::Ok {
      self.internal_error("Main script call failed.");
    }

    let mut current_module = self.current_fun.module();
    self
      .global
      .transfer_exported(&GcHooks::new(self), &mut current_module);
  }

  /// Prepare the main module for use
  fn main_module(&mut self, module_path: PathBuf, main_id: usize) -> Gc<Module> {
    let hooks = GcHooks::new(self);

    let name = hooks.manage_str(SELF);
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
  fn get_method(&mut self, this: Value, method_name: GcStr) -> Call {
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
      }
    }
  }

  /// Get the class for this value
  fn get_class(&mut self, this: Value) -> Value {
    val!(self.builtin.primitives.for_value(this, this.kind()))
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
        ByteCode::ImportSymbol => self.op_import_symbol(),
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
            if let Some(execute_result) = self.stack_unwind(error) {
              return execute_result;
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

  #[inline]
  fn value_class(&self, value: Value) -> Gc<Class> {
    self.builtin.primitives.for_value(value, value.kind())
  }

  #[inline]
  fn manage<T: 'static + Manage>(&self, data: T) -> Gc<T> {
    self.gc.borrow_mut().manage(data, self)
  }

  #[inline]
  fn manage_str<S: AsRef<str>>(&self, string: S) -> GcStr {
    self.gc.borrow_mut().manage_str(string, self)
  }

  #[inline]
  fn push_root<T: 'static + Trace>(&self, data: T) {
    self.gc.borrow_mut().push_root(data)
  }

  #[inline]
  fn pop_roots(&self, count: usize) {
    self.gc.borrow_mut().pop_roots(count)
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

  /// Get the current closure
  #[inline]
  fn inline_cache(&mut self) -> &InlineCache {
    &self.inline_cache[self.current_fun.module_id()]
  }

  /// Get the current closure
  #[inline]
  fn inline_cache_mut(&mut self) -> &mut InlineCache {
    &mut self.inline_cache[self.current_fun.module_id()]
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

  /// read a u32 out of the bytecode
  #[inline]
  fn read_slot(&mut self) -> u32 {
    let slice = unsafe { std::slice::from_raw_parts(self.ip, 4) };
    let buffer = slice.try_into().expect("slice of incorrect length.");
    let short = u32::from_ne_bytes(buffer);
    self.update_ip(4);

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
      self
        .current_fun
        .chunk()
        .get_constant_unchecked(index as usize)
    }
  }

  /// read a constant as a string from the current chunk
  #[inline]
  fn read_string(&self, index: u16) -> GcStr {
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
    let list = val!(self.manage(List::from(args)));
    self.stack_top = unsafe { self.stack_top.offset(-(arg_count as isize)) };
    self.push(list);

    Signal::Ok
  }

  /// create a map from a map literal
  fn op_map(&mut self) -> Signal {
    let arg_count = self.read_short();
    let mut map = self.manage(Map::with_capacity(arg_count as usize));

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
      buffers.push_str(&arg.to_str())
    }

    self.stack_top = unsafe { self.stack_top.offset(-(arg_count as isize)) };
    let interpolated = val!(self.manage_str(buffers));
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
    let inline_slot = self.read_slot() as usize;

    let method_name = self.read_string(constant);
    let receiver = self.peek(arg_count as isize);

    let class = self.value_class(receiver);
    match self.inline_cache().get_invoke_cache(inline_slot, class) {
      Some(method) => self.resolve_call(method, arg_count),
      None => {
        if receiver.is_instance() {
          let instance = receiver.to_instance();
          if let Some(field) = instance.get_field(&method_name) {
            self.set_val(-(arg_count as isize) - 1, *field);
            self.inline_cache_mut().clear_invoke_cache(inline_slot);
            return self.resolve_call(*field, arg_count);
          }
        }

        match class.get_method(&method_name) {
          Some(method) => {
            self
              .inline_cache_mut()
              .set_invoke_cache(inline_slot, class, method);
            self.resolve_call(method, arg_count)
          }
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
    }
  }

  /// invoke a method
  fn invoke(&mut self, receiver: Value, method_name: GcStr, arg_count: u8) -> Signal {
    if receiver.is_instance() {
      let instance = receiver.to_instance();
      if let Some(field) = instance.get_field(&method_name) {
        self.set_val(-(arg_count as isize) - 1, *field);
        return self.resolve_call(*field, arg_count);
      }
    }

    let class = self.value_class(receiver);
    self.invoke_from_class(class, method_name, arg_count)
  }

  /// Invoke a method on a instance's super class
  fn op_super_invoke(&mut self) -> Signal {
    let constant = self.read_short();
    let arg_count = self.read_byte();
    let inline_slot = self.read_slot() as usize;

    let method_name = self.read_string(constant);
    let super_class = self.pop().to_class();

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
        }
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
  fn op_class(&mut self) -> Signal {
    let slot = self.read_short();
    let name = self.read_string(slot);

    let class = val!(self.manage(Class::bare(name)));
    self.push(class);
    Signal::Ok
  }

  fn op_inherit(&mut self) -> Signal {
    let super_class = self.peek(1);

    if !super_class.is_class() {
      return self.runtime_error(self.builtin.errors.runtime, "Superclass must be a class.");
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
    let mut current_module = self.current_fun.module();
    match current_module.insert_symbol(&GcHooks::new(self), name, global) {
      Ok(_) => Signal::Ok,
      Err(_) => Signal::Ok,
    }
  }

  fn op_set_index(&mut self) -> Signal {
    let receiver = self.peek(2_isize);

    let class = self.value_class(receiver);

    match class.index_set() {
      Some(index_set) => {
        let value = self.peek(1);
        let signal = self.resolve_call(index_set, 2);
        self.drop();
        self.push(value);
        signal
      }
      None => self.runtime_error(
        self.builtin.errors.method_not_found,
        &format!("No method []= on class {}.", class.name()),
      ),
    }
  }

  fn op_set_global(&mut self) -> Signal {
    let slot = self.read_short();
    let string = self.read_string(slot);
    let peek = self.peek(0);

    let mut current_module = self.current_fun.module();
    if current_module.set_symbol(string, peek).is_err() {
      return self.runtime_error(
        self.builtin.errors.property,
        &format!("Undefined variable {}", string),
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
    let instance = self.peek(1);
    let name = self.read_string(slot);
    let inline_slot = self.read_slot() as usize;

    if instance.is_instance() {
      let mut instance = instance.to_instance();
      let class = instance.class();

      match self.inline_cache().get_property_cache(inline_slot, class) {
        Some(property_slot) => {
          let value = self.pop();

          self.drop();
          self.push(value);

          instance[property_slot] = value;
          return Signal::Ok;
        }
        None => {
          let property_slot = class.get_field_index(&name);
          let value = self.pop();

          self.drop();
          self.push(value);

          return match property_slot {
            Some(property_slot) => {
              let cache = self.inline_cache_mut();
              cache.set_property_cache(inline_slot, class, property_slot as usize);
              instance[property_slot as usize] = value;
              Signal::Ok
            }
            None => self.runtime_error(
              self.builtin.errors.property,
              &format!("Undefined property {} on class {}.", name, class.name()),
            ),
          };
        }
      }
    }

    self.runtime_error(
      self.builtin.errors.runtime,
      "Only instances have settable fields.",
    )
  }

  fn op_set_upvalue(&mut self) -> Signal {
    let slot = self.read_byte();
    let value = self.peek(0);
    self.closure().set_value(slot as usize, value);

    Signal::Ok
  }

  fn op_get_index(&mut self) -> Signal {
    let receiver = self.peek(1_isize);

    let class = self.value_class(receiver);

    match class.index_get() {
      Some(index_get) => self.resolve_call(index_get, 1),
      None => self.runtime_error(
        self.builtin.errors.method_not_found,
        &format!("No method [] on class {}.", class.name()),
      ),
    }
  }

  fn op_get_global(&mut self) -> Signal {
    let store_index = self.read_short();
    let string = self.read_string(store_index);

    match self.current_fun.module().get_symbol(string) {
      Some(gbl) => {
        self.push(gbl);
        Signal::Ok
      }
      None => self.runtime_error(
        self.builtin.errors.runtime,
        &format!("Undefined variable {}", string),
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
    self.push(self.closure().get_value(slot as usize));

    Signal::Ok
  }

  fn op_get_property(&mut self) -> Signal {
    let slot = self.read_short();
    let value = self.peek(0);
    let name = self.read_string(slot);
    let inline_slot = self.read_slot() as usize;

    if value.is_instance() {
      let instance = value.to_instance();
      let class = instance.class();
      match self.inline_cache().get_property_cache(inline_slot, class) {
        Some(property_slot) => {
          self.set_val(-1, instance[property_slot]);
          return Signal::Ok;
        }
        None => {
          if let Some(property_slot) = class.get_field_index(&name) {
            self
              .inline_cache_mut()
              .set_property_cache(inline_slot, class, property_slot as usize);

            let instance = value.to_instance();
            self.set_val(-1, instance[property_slot as usize]);
            return Signal::Ok;
          }
        }
      }
    }

    let class = self.value_class(value);
    let cache = self.inline_cache_mut();
    cache.clear_property_cache(inline_slot);
    self.bind_method(class, name)
  }

  fn op_import(&mut self) -> Signal {
    let index_path = self.read_short();
    let path = self.read_constant(index_path).to_list();

    let mut path_segments: Gc<List<GcStr>> = self
      .gc
      .borrow_mut()
      .manage(List::with_capacity(path.len()), self);

    path_segments.extend(path.iter().map(|segment| segment.to_str()));

    let mut buffer = String::new();
    for segment in &path_segments[..path_segments.len() - 1] {
      buffer.push_str(segment);
      buffer.push('/')
    }

    buffer.push_str(&path_segments[path_segments.len() - 1]);

    // check if fully resolved module has already been loaded
    let resolved = self.manage_str(buffer);
    if let Some(module) = self.module_cache.get(&resolved) {
      let imported = module.module_instance(&GcHooks::new(self));
      self.push(val!(imported));
      return Signal::Ok;
    }

    let import = match path_segments.split_first() {
      Some((package, path)) => {
        // generate a new import object
        let path = self.gc().manage(List::from(path), self);
        self.gc().manage(Import::new(*package, path), self)
      }
      None => {
        // generate a new import object
        let path = self.gc().manage(List::new(), self);
        self.gc().manage(Import::new(path_segments[0], path), self)
      }
    };

    self.gc().push_root(import);

    let result = match self.packages.get(&import.package()) {
      Some(package) => match package.import(&GcHooks::new(self), import) {
        Ok(module) => {
          self.push(val!(module));
          Signal::Ok
        }
        Err(err) => self.runtime_error(self.builtin.errors.runtime, &err.to_string()),
      },
      None => self.runtime_error(
        self.builtin.errors.import,
        &format!("Package {} does not exist", &import.package()),
      ),
    };

    self.gc().pop_roots(1);
    result
  }

  fn op_import_symbol(&mut self) -> Signal {
    let index_path = self.read_short();
    let index_name = self.read_short();
    let path = self.read_constant(index_path).to_list();
    let name = self.read_string(index_name);

    let mut path_segments: Gc<List<GcStr>> = self
      .gc
      .borrow_mut()
      .manage(List::with_capacity(path.len()), self);

    path_segments.extend(path.iter().map(|segment| segment.to_str()));

    let mut buffer = String::new();
    for segment in &path_segments[..path_segments.len() - 1] {
      buffer.push_str(segment);
      buffer.push('/')
    }

    buffer.push_str(&path_segments[path_segments.len() - 1]);

    // check if fully resolved module has already been loaded
    let resolved = self.manage_str(buffer);
    if let Some(module) = self.module_cache.get(&resolved) {
      let imported = module.module_instance(&GcHooks::new(self));
      self.push(val!(imported));
      return Signal::Ok;
    }

    let import = match path_segments.split_first() {
      Some((package, path)) => {
        // generate a new import object
        let path = self.gc().manage(List::from(path), self);
        self.gc().manage(Import::new(*package, path), self)
      }
      None => {
        // generate a new import object
        let path = self.gc().manage(List::new(), self);
        self.gc().manage(Import::new(path_segments[0], path), self)
      }
    };

    self.gc().push_root(import);

    let result = match self.packages.get(&import.package()) {
      Some(package) => match package.import_symbol(&GcHooks::new(self), import, name) {
        Ok(module) => {
          self.push(val!(module));
          Signal::Ok
        }
        Err(err) => self.runtime_error(self.builtin.errors.runtime, &err.to_string()),
      },
      None => self.runtime_error(
        self.builtin.errors.import,
        &format!("Package {} does not exist", &import.package()),
      ),
    };

    self.gc().pop_roots(1);
    result
  }

  fn op_export(&mut self) -> Signal {
    let index = self.read_short();
    let name = self.read_string(index);
    let mut current_module = self.current_fun.module();

    match current_module.export_symbol(&GcHooks::new(self), name) {
      Ok(_) => Signal::Ok,
      Err(error) => self.runtime_error(self.builtin.errors.export, &error.to_string()),
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
      self.runtime_error(self.builtin.errors.runtime, "Operand must be a number.")
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
      buffer.push_str(&*left);
      buffer.push_str(&*right);

      let string = self.manage_str(buffer);
      self.push(val!(string));
      Signal::Ok
    } else {
      self.runtime_error(
        self.builtin.errors.runtime,
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

    self.runtime_error(self.builtin.errors.runtime, "Operands must be numbers.")
  }

  fn op_mul(&mut self) -> Signal {
    let (right, left) = (self.pop(), self.pop());

    if right.is_num() && left.is_num() {
      self.push(val!(left.to_num() * right.to_num()));
      return Signal::Ok;
    }

    self.runtime_error(self.builtin.errors.runtime, "Operands must be numbers.")
  }

  fn op_div(&mut self) -> Signal {
    let (right, left) = (self.pop(), self.pop());

    if right.is_num() && left.is_num() {
      self.push(val!(left.to_num() / right.to_num()));
      return Signal::Ok;
    }

    self.runtime_error(self.builtin.errors.runtime, "Operands must be numbers.")
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

    self.runtime_error(self.builtin.errors.runtime, "Operands must be numbers.")
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
      self.builtin.errors.runtime,
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
      self.builtin.errors.runtime,
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
      self.builtin.errors.runtime,
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
      match class.to_class().meta_class() {
        Some(mut meta) => {
          meta.add_method(&GcHooks::new(self), name, method);
        }
        None => self.internal_error(&format!("{} meta class not set.", class.to_class().name())),
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

    let upvalues = (0..fun.upvalue_count())
      .map(|_| {
        let upvalue_index: UpvalueIndex = unsafe { mem::transmute(self.read_short()) };

        match upvalue_index {
          UpvalueIndex::Local(index) => {
            let value = unsafe { &*self.slots().offset(index as isize) };
            self.capture_upvalue(NonNull::from(value))
          }
          UpvalueIndex::Upvalue(upvalue) => self.closure().get_upvalue(upvalue as usize),
        }
      })
      .collect::<Vec<Gc<Upvalue>>>()
      .into_boxed_slice();

    let closure = self.manage(Closure::new(fun, upvalues));

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

  /// resolve the current value and call in the appropriate manner
  fn resolve_call(&mut self, callee: Value, arg_count: u8) -> Signal {
    match callee.kind() {
      ValueKind::Closure => self.call(callee.to_closure(), arg_count),
      ValueKind::Method => self.call_method(callee.to_method(), arg_count),
      ValueKind::Native => self.call_native(callee.to_native(), arg_count),
      ValueKind::Class => self.call_class(callee.to_class(), arg_count),
      ValueKind::Fun => self.internal_error(&format!(
        "Function {} was not wrapped in a closure.",
        callee.to_fun().name()
      )),
      _ => {
        let class_name = self.get_class(callee).to_class().name();
        self.runtime_error(
          self.builtin.errors.runtime,
          &format!("{} is not callable.", class_name),
        )
      }
    }
  }

  /// call a class creating a new instance of that class
  fn call_class(&mut self, class: Gc<Class>, arg_count: u8) -> Signal {
    let instance = val!(self.manage(Instance::new(class)));
    self.set_val(-(arg_count as isize) - 1, instance);

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
      }
    }
  }

  /// call a native function immediately returning the result
  fn call_native(&mut self, native: Gc<Native>, arg_count: u8) -> Signal {
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

    #[cfg(debug_assertions)]
    let roots_before = self.gc().temp_roots();

    match meta.environment {
      Environment::StackLess => match native.call(&mut Hooks::new(self), this, args) {
        Call::Ok(value) => {
          self.stack_top = unsafe { self.stack_top.offset(-(arg_count as isize) - 1) };
          self.push(value);

          #[cfg(debug_assertions)]
          {
            let roots_current = self.gc().temp_roots();
            assert_roots(native, roots_before, roots_current);
          }
          Signal::OkReturn
        }
        Call::Err(error) => self.set_error(error),
        Call::Exit(code) => self.set_exit(code),
      },
      Environment::Normal => {
        let native_closure = self.manage(Closure::without_upvalues(self.native_fun_stub));
        self.push_frame(native_closure, arg_count);

        match native.call(&mut Hooks::new(self), this, args) {
          Call::Ok(value) => {
            self.pop_frame();
            self.push(value);

            #[cfg(debug_assertions)]
            {
              let roots_current = self.gc().temp_roots();
              assert_roots(native, roots_before, roots_current);
            }
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
    if let Some(error) = self.check_arity(closure.fun(), arg_count) {
      return error;
    }

    // set the current current instruction pointer. check for overflow
    if self.frame_count == FRAME_MAX {
      return self.runtime_error(self.builtin.errors.runtime, "Stack overflow.");
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
      frame.ip = closure.fun().chunk().instructions().get_unchecked(0) as *const u8;
      frame.slots = self.stack_top.offset(-(arg_count as isize) - 1);
      self.current_frame = frame as *mut CallFrame;
      self.load_ip();
    }

    self.current_fun = closure.fun();
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
      self.current_fun = self.closure().fun();
      self.load_ip();
    }

    None
  }

  /// check that the number of args is valid for the function arity
  fn check_arity(&mut self, fun: Gc<Fun>, arg_count: u8) -> Option<Signal> {
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
            self.builtin.errors.runtime,
            &format!(
              "{} {} expected {} argument(s) but received {}.",
              callable_type,
              native_meta.name,
              expected,
              args.len(),
            ),
          )),
          SignatureError::LengthVariadic(expected) => Some(self.runtime_error(
            self.builtin.errors.runtime,
            &format!(
              "{} {} expected at least {} argument(s) but received {}.",
              callable_type,
              native_meta.name,
              expected,
              args.len(),
            ),
          )),
          SignatureError::LengthDefaultLow(expected) => Some(self.runtime_error(
            self.builtin.errors.runtime,
            &format!(
              "{} {} expected at least {} argument(s) but received {}.",
              callable_type,
              native_meta.name,
              expected,
              args.len(),
            ),
          )),
          SignatureError::LengthDefaultHigh(expected) => Some(self.runtime_error(
            self.builtin.errors.runtime,
            &format!(
              "{} {} expected at most {} argument(s) but received {}.",
              callable_type,
              native_meta.name,
              expected,
              args.len(),
            ),
          )),
          SignatureError::TypeWrong(parameter) => Some(self.runtime_error(
            self.builtin.errors.runtime,
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
    self.set_val(-(arg_count as isize) - 1, bound.receiver());
    self.resolve_call(bound.method(), arg_count)
  }

  /// bind a method to an instance
  fn bind_method(&mut self, class: Gc<Class>, name: GcStr) -> Signal {
    match class.get_method(&name) {
      Some(method) => {
        let bound = self.manage(Method::new(self.peek(0), method));
        self.set_val(-1, val!(bound));
        Signal::Ok
      }
      None => self.runtime_error(
        self.builtin.errors.runtime,
        &format!("Undefined property {}", name),
      ),
    }
  }

  /// invoke a method from the provided class
  fn invoke_from_class(&mut self, class: Gc<Class>, method_name: GcStr, arg_count: u8) -> Signal {
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

  /// capture an upvalue return an existing upvalue if already captured
  fn capture_upvalue(&mut self, local_index: NonNull<Value>) -> Gc<Upvalue> {
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
          return *upvalue;
        }
      }
    }

    let created_upvalue = self.manage(Upvalue::Open(local_index));
    self.open_upvalues.push(created_upvalue);

    created_upvalue
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

    let start = &self.current_fun.chunk().instructions()[0] as *const u8;
    let offset = ptr_len(start, ip);
    disassemble_instruction(&mut stdio, &self.current_fun.chunk(), offset, false)
  }

  /// Print the current stack
  #[cfg(feature = "debug")]
  fn print_stack_debug(&self, stdio: &mut Stdio) -> io::Result<()> {
    let stdout = stdio.stdout();

    if self.frame_count > 2 {
      write!(stdout, "Frame Stack:  ")?;
      for frame in self.frames[1..(self.frame_count)].iter() {
        let fun = frame.closure.fun();
        write!(stdout, "[ {}:{} ]", fun.module().name(), fun.name())?;
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
    let error_message = val!(self.manage_str(message));
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
      let fun = frame.closure.fun();
      let instructions = &*fun.chunk().instructions();

      let offset = ptr_len(&instructions[0], frame.ip);
      if let Some(offset) = fun.has_catch_jump(offset as u16) {
        // if found set all the stack information to the correct state
        frame.ip = &instructions[offset as usize] as *const u8;

        self.frame_count -= idx;
        self.current_frame = frame as *mut CallFrame;
        self.current_fun = frame.closure.fun();
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
    writeln!(stderr, "{}: {}", error.class().name(), message).expect("Unable to write to stderr");

    for frame in self.frames[1..self.frame_count].iter().rev() {
      let fun = frame.closure.fun();
      let location: String = match &*fun.name() {
        SCRIPT => SCRIPT.to_owned(),
        _ => format!("{}()", fun.name()),
      };

      let offset = ptr_len(&fun.chunk().instructions()[0], frame.ip);
      writeln!(
        stderr,
        "  [line {}] in {}",
        fun.chunk().get_line(offset),
        location
      )
      .expect("Unable to write to stderr");
    }

    self.reset_stack();
  }
}

#[cfg(debug_assertions)]
fn assert_roots(native: Gc<Native>, roots_before: usize, roots_now: usize) {
  assert!(
    roots_before == roots_now,
    "Native function {} increased roots by {}.",
    native.meta().name,
    roots_now - roots_before,
  );
}

impl From<VmDependencies> for Vm {
  /// Construct a vm from a set of dependencies. This is meant to be used when targeting
  /// different environments
  fn from(dependencies: VmDependencies) -> Self {
    let no_gc_context = NoContext::new(dependencies.gc);
    let hooks = GcHooks::new(&no_gc_context);

    let std_lib = dependencies.std_lib;
    let global = std_lib.root_module();

    let fun = FunBuilder::new(hooks.manage_str(PLACEHOLDER_NAME), global);

    let managed_fun = hooks.manage(fun.build());
    let closure = hooks.manage(Closure::without_upvalues(managed_fun));

    let mut frames = vec![CallFrame::new(closure); FRAME_MAX];
    let mut stack = vec![VALUE_NIL; DEFAULT_STACK_MAX];

    let builtin = builtin_from_module(&hooks, &global)
      .expect("Failed to generate builtin class from global module");

    let root_dir = dependencies
      .io
      .env()
      .current_dir()
      .expect("Could not obtain the current working directory.");

    let current_frame = &mut frames[0];
    let current_fun = current_frame.closure.fun();
    let stack_top = &mut stack[1] as *mut Value;
    let ip = ptr::null();

    let current_frame = current_frame as *mut CallFrame;
    let mut native_builder = FunBuilder::new(hooks.manage_str("native"), global);
    native_builder.write_instruction(AlignedByteCode::Nil, 0);

    let native_fun_stub = hooks.manage(native_builder.build());

    let gc = RefCell::new(no_gc_context.done());

    let mut vm = Vm {
      io: dependencies.io,
      stack,
      frames,
      gc,
      files: VmFiles::default(),
      emitter: IdEmitter::default(),
      inline_cache: vec![],
      root_dir,
      builtin,
      packages: Map::default(),
      module_cache: Map::default(),
      global,
      frame_count: 1,
      current_fun,
      current_frame,
      current_error: None,
      exit_code: 0,
      stack_top,
      ip,
      native_fun_stub,
      open_upvalues: Vec::with_capacity(100),
    };
    vm.add_package(std_lib);

    vm
  }
}

impl TraceRoot for Vm {
  fn trace(&self) {
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

    self.files.trace();
    self.packages.trace();
    self.module_cache.trace();
    self.native_fun_stub.trace();
    if let Some(current_error) = self.current_error {
      current_error.trace()
    }
  }

  fn trace_debug(&self, log: &mut dyn Write) {
    unsafe {
      let start = &self.stack[0] as *const Value;
      let len = ptr_len(start, self.stack_top) + 1;
      let slice = std::slice::from_raw_parts(start, len);

      slice.iter().for_each(|value| {
        value.trace_debug(log);
      });
    }

    self.frames[0..self.frame_count].iter().for_each(|frame| {
      frame.closure.trace_debug(log);
    });

    self.open_upvalues.iter().for_each(|upvalue| {
      upvalue.trace_debug(log);
    });

    self.files.trace_debug(log);
    self.packages.trace_debug(log);
    self.module_cache.trace_debug(log);
    self.native_fun_stub.trace_debug(log);
    if let Some(current_error) = self.current_error {
      current_error.trace_debug(log)
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
    let result = self.run_fun(callable, args);
    self.to_call_result(result)
  }

  fn call_method(&mut self, this: Value, method: Value, args: &[Value]) -> Call {
    let result = self.run_method(this, method, args);
    self.to_call_result(result)
  }

  fn get_method(&mut self, this: Value, method_name: GcStr) -> Call {
    self.get_method(this, method_name)
  }

  fn get_class(&mut self, this: Value) -> Value {
    self.get_class(this)
  }
}
