#[cfg(feature = "debug")]
mod debug;

mod basic;
mod error;
mod hooks;
mod impls;
mod ops;

use crate::{
  byte_code::{AlignedByteCode, ByteCode},
  cache::InlineCache,
  compiler::{Compiler, Parser, Resolver},
  constants::REPL_MODULE,
  source::{Source, VmFileId, VmFiles},
  FeResult,
};
use codespan_reporting::term::{self, Config};
use laythe_core::{
  captures::Captures,
  constants::{PLACEHOLDER_NAME, SELF},
  hooks::{GcHooks, HookContext, NoContext},
  managed::{Gc, GcObj, GcStr},
  memory::Allocator,
  module::{Module, Package},
  object::{Class, Fiber, Fun, Map, Native},
  utils::IdEmitter,
  val,
  value::{Value, VALUE_NIL},
};
use laythe_env::io::Io;
use laythe_lib::{builtin_from_module, create_std_lib, BuiltIn};
use laythe_native::io::io_native;
use std::cell::RefCell;
use std::collections::VecDeque;
use std::path::PathBuf;
use std::ptr;
use std::usize;

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

    let main_module = self.module(SELF);

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

        let main_module = self.module(SELF);

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
  fn module(&mut self, name: &str) -> Gc<Module> {
    let main_id = self.emitter.emit();
    let hooks = GcHooks::new(self);

    let name = hooks.manage_str(name);
    let module_class = Class::with_inheritance(&hooks, name, self.builtin.dependencies.module);

    let mut module = hooks.manage(Module::new(module_class, main_id));
    hooks.push_root(module);

    // transfer the symbols from the global module into the main module
    self.global.transfer_exported(&hooks, &mut module);

    hooks.pop_roots(1);
    let package = hooks.manage(Package::new(name, module));

    self.packages.insert(name, package);
    module
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
