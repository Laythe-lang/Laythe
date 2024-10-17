// mod compiler;
mod ir;
mod parser;
mod peephole;
mod resolver;
mod scanner;

use bumpalo::{collections, Bump};
use laythe_lib::global::ERROR_CLASS_NAME;
pub use parser::Parser;
pub use resolver::Resolver;

use crate::{
  byte_code::{CaptureIndex, Label, SymbolicByteCode},
  cache::CacheIdEmitter,
  chunk_builder::ChunkBuilder,
  source::{LineOffsets, VmFileId},
  FeResult,
};
use codespan_reporting::diagnostic::{self, Diagnostic};
use ir::{
  ast::{self, Decl, Expr, Primary, Spanned, Stmt, Trailer},
  token::{Lexeme, Token, TokenKind},
};
use laythe_core::{
  constants::{
    INDEX_GET, INDEX_SET, ITER, ITER_VAR, OBJECT, SCRIPT, SELF, SUPER, UNINITIALIZED_VAR,
  },
  hooks::{GcContext, GcHooks, NoContext},
  managed::{AllocResult, Allocate, DebugHeap, Gc, GcStr, ListBuilder, Trace, TraceRoot},
  memory::Allocator,
  module,
  object::{self, FunBuilder, FunKind, Map},
  signature::Arity,
  val,
  value::Value,
};
use laythe_env::io::Io;
use object::Fun;
use std::{
  cell::{RefCell, RefMut},
  io::Write,
  mem,
  ptr::NonNull,
  rc::Rc,
};

use self::{
  ir::symbol_table::{Symbol, SymbolState, SymbolTable},
  peephole::peephole_compile,
};

#[cfg(feature = "debug")]
use crate::debug::print_symbolic_code;

#[derive(Debug)]
pub struct Local<'a> {
  /// name of the local
  symbol: &'a Symbol,

  /// depth of the local
  depth: usize,
}

#[derive(Debug, Clone)]
pub struct ClassAttributes {
  /// kind of current function in this class
  fun_kind: Option<FunKind>,

  /// the fields present on this class
  fields: Vec<GcStr>,

  /// The name of this class
  name: GcStr,
}

impl ClassAttributes {
  fn new(name: GcStr) -> Self {
    ClassAttributes {
      fun_kind: None,
      fields: vec![],
      name,
    }
  }

  fn add_field(&mut self, field: GcStr) {
    self.fields.push(field);
  }
}

impl DebugHeap for ClassAttributes {
  fn fmt_heap(&self, f: &mut std::fmt::Formatter, _: usize) -> std::fmt::Result {
    f.debug_struct("ClassInfo")
      .field("fun_kind", &self.fun_kind)
      .field("fields", &self.fields)
      .field("name", &self.name)
      .finish()
  }
}

impl Allocate<Gc<Self>> for ClassAttributes {
  fn alloc(self) -> AllocResult<Gc<Self>> {
    Gc::alloc_result(self)
  }
}

impl Trace for ClassAttributes {
  fn trace(&self) {
    self.name.trace();
    self.fields.iter().for_each(|field| field.trace());
  }

  fn trace_debug(&self, log: &mut dyn Write) {
    self.name.trace_debug(log);
    self.fields.iter().for_each(|field| field.trace_debug(log));
  }
}

#[derive(Debug, Clone, Copy)]
pub struct TryAttributes {
  scope_depth: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct LoopAttributes {
  scope_depth: usize,
  start: Label,
  end: Label,
}

#[derive(Default)]
pub struct LabelEmitter(u32);

impl LabelEmitter {
  pub fn emit(&mut self) -> Label {
    let result = self.0;
    self.0 += 1;
    Label::new(result)
  }
}

pub struct Compiler<'a, 'src> {
  #[allow(dead_code)]
  io: Option<Io>,

  /// The roots from the surround context
  root_trace: &'a dyn TraceRoot,

  /// The current function
  fun: FunBuilder,

  /// The current chunk
  chunk: ChunkBuilder,

  /// The type the current function scope
  fun_kind: FunKind,

  /// The current module
  module: Gc<module::Module>,

  /// line offsets for the current file
  line_offsets: &'a LineOffsets,

  /// line offsets for the current file
  alloc: &'a Bump,

  /// All the errors found during compilation
  errors: Vec<Diagnostic<VmFileId>>,

  /// The parent compiler if it exists note uses
  /// unsafe pointer
  enclosing: Option<NonNull<Compiler<'a, 'src>>>,

  /// The info on the current class
  class_attributes: Option<Gc<ClassAttributes>>,

  /// The info on the current loop
  try_attributes: Option<TryAttributes>,

  /// The info on the current loop
  loop_attributes: Option<LoopAttributes>,

  /// The id emitter for inline cache
  cache_id_emitter: Rc<RefCell<CacheIdEmitter>>,

  /// a label emitter
  label_emitter: LabelEmitter,

  /// hooks into the surround context. Used to allocate laythe objects
  gc: RefCell<Allocator>,

  /// The file id for the current file
  file_id: VmFileId,

  /// Current scope depth
  scope_depth: usize,

  /// Are we currently compile for a repl
  repl: bool,

  /// The local variables currently in scope
  locals: collections::Vec<'a, Local<'a>>,

  /// local tables to this function
  local_tables: collections::Vec<'a, &'a SymbolTable<'src>>,

  /// is this function the script with the global table
  global_table: Option<&'a SymbolTable<'src>>,

  /// captures in this function
  captures: Vec<CaptureIndex>,

  /// A set of constants used in the current function
  constants: Map<Value, usize>,
}

impl<'a, 'src: 'a> Compiler<'a, 'src> {
  /// Create a new compiler at the module scope level. This struct will take a an ast
  /// produced by the parser and emit Laythe bytecode.
  ///
  /// # Examples
  /// ```
  /// use laythe_vm::{
  ///   compiler::{Compiler, Parser},
  ///   source::{Source, VM_FILE_TEST_ID},
  /// };
  /// use laythe_core::{
  ///   module::Module,
  ///   object::Class,
  ///   memory::{NO_GC, Allocator},
  /// };
  /// use std::path::PathBuf;
  /// use bumpalo::Bump;
  ///
  /// let mut gc = Allocator::default();
  ///
  /// let name = gc.manage_str("module", &NO_GC);
  /// let class = gc.manage_obj(Class::bare(name), &NO_GC);
  /// let module_path = gc.manage_str("module/path", &NO_GC);
  /// let module = gc.manage(Module::new(class, module_path, 0), &NO_GC);
  ///
  /// let file_id = VM_FILE_TEST_ID;
  /// let source = Source::new(gc.manage_str("print('10');", &NO_GC));
  /// let (_, line_offsets) = Parser::new(&source, file_id).parse();
  ///
  /// let alloc = Bump::new();
  /// let compiler = Compiler::new(module, &alloc, &line_offsets, file_id, false, &NO_GC, gc);
  /// ```
  pub fn new(
    module: Gc<module::Module>,
    alloc: &'a Bump,
    line_offsets: &'a LineOffsets,
    file_id: VmFileId,
    repl: bool,
    root_trace: &'a dyn TraceRoot,
    mut gc: Allocator,
  ) -> Self {
    gc.push_root(module);
    let fun_name = gc.manage_str(SCRIPT, root_trace);
    let fun = FunBuilder::new(fun_name, module, Arity::default());
    let chunk = ChunkBuilder::default();
    gc.pop_roots(1);

    Self {
      io: None,
      fun,
      chunk,
      file_id,
      root_trace,
      module,
      line_offsets,
      alloc,
      cache_id_emitter: Rc::new(RefCell::new(CacheIdEmitter::default())),
      label_emitter: LabelEmitter::default(),
      errors: vec![],
      fun_kind: FunKind::Script,
      scope_depth: 0,
      repl,
      class_attributes: None,
      loop_attributes: None,
      try_attributes: None,
      gc: RefCell::new(gc),
      enclosing: None,
      local_tables: collections::Vec::new_in(alloc),
      global_table: None,
      locals: collections::Vec::new_in(alloc),
      captures: vec![],
      constants: object::Map::default(),
    }
  }

  /// Compile the provided ast into managed function objects that
  /// contain the vm bytecode
  pub fn compile(
    mut self,
    ast: &'a ast::Module<'src>,
  ) -> (FeResult<Fun>, Allocator, CacheIdEmitter) {
    self.begin_global_scope(&ast.symbols);
    self.declare_script();

    for decl in &ast.decls {
      self.decl(decl);
    }

    let end = ast.end();
    let cache_id_emitter = Rc::clone(&self.cache_id_emitter);
    let (fun, errors, _, gc) = self.end_compiler(end);
    if errors.is_empty() {
      (Ok(fun), gc, cache_id_emitter.take())
    } else {
      (Err(errors), gc, cache_id_emitter.take())
    }
  }

  // create a child compiler to compile a function inside the enclosing module
  fn child<'b>(
    name: GcStr,
    arity: Arity,
    fun_kind: FunKind,
    enclosing: &mut Compiler<'b, 'src>,
  ) -> Compiler<'b, 'src> {
    let fun = FunBuilder::new(name, enclosing.module, arity);
    let chunk = ChunkBuilder::default();

    let gc = RefCell::new(Allocator::default());
    gc.swap(&enclosing.gc);

    #[cfg(feature = "debug")]
    let io: Option<Io> = enclosing.io.clone();

    #[cfg(not(feature = "debug"))]
    let io: Option<Io> = None;

    Compiler {
      io,
      fun,
      chunk,
      file_id: enclosing.file_id,
      module: enclosing.module,
      line_offsets: enclosing.line_offsets,
      cache_id_emitter: Rc::clone(&enclosing.cache_id_emitter),
      label_emitter: LabelEmitter::default(),
      alloc: enclosing.alloc,
      errors: vec![],
      root_trace: enclosing.root_trace,
      fun_kind,
      scope_depth: enclosing.scope_depth,
      repl: enclosing.repl,
      class_attributes: enclosing.class_attributes,
      loop_attributes: None,
      try_attributes: None,
      gc,
      locals: collections::Vec::new_in(enclosing.alloc),
      global_table: None,
      local_tables: collections::Vec::new_in(enclosing.alloc),
      enclosing: Some(NonNull::from(enclosing)),
      captures: vec![],
      constants: object::Map::default(),
    }
  }

  #[cfg(feature = "debug")]
  pub fn with_io(mut self, io: Io) -> Self {
    self.io = Some(io);
    self
  }

  /// End this compilers compilation emitting a final return
  /// and shrinking the function to the correct size
  fn end_compiler(
    mut self,
    line: u32,
  ) -> (Fun, Vec<Diagnostic<VmFileId>>, Vec<CaptureIndex>, Allocator) {
    self.emit_return(line);

    let context = NoContext::new(self.gc.into_inner());
    let hooks = GcHooks::new(&context);

    #[cfg(feature = "debug")]
    {
      Compiler::print_chunk(
        &self.fun,
        &self.chunk,
        &self.class_attributes,
        &self.io,
        self.fun_kind,
      );
    }

    let result = peephole_compile(
      &hooks,
      self.fun,
      self.chunk,
      self.alloc,
      Rc::clone(&self.cache_id_emitter),
    );

    match result {
      Ok(fun) => (fun, self.errors, self.captures, context.done()),
      Err(errors) => {
        let fun = Fun::stub(&hooks, hooks.manage_str("error"), self.module);
        self.errors.extend_from_slice(&errors);
        (fun, self.errors, self.captures, context.done())
      },
    }
  }

  /// Print the chunk if debug and an error occurred
  #[cfg(feature = "debug")]
  fn print_chunk(
    fun: &FunBuilder,
    chunk: &ChunkBuilder,
    class_info: &Option<Gc<ClassAttributes>>,
    io: &Option<Io>,
    fun_kind: FunKind,
  ) {
    let name = match fun_kind {
      FunKind::Script => "script.lay".to_string(),
      FunKind::Fun => fun.name().to_string(),
      FunKind::Method | FunKind::StaticMethod | FunKind::Initializer => {
        let name = class_info.expect("Class info not set").name;
        format!("{}:{}", name, fun.name())
      },
    };

    let mut stdio = io.as_ref().unwrap().stdio();
    print_symbolic_code(&mut stdio, chunk, &name).expect("could not write to stdio");
  }

  /// Emit byte code for a return
  fn emit_return(&mut self, line: u32) {
    match self.fun_kind {
      FunKind::Initializer => self.emit_byte(SymbolicByteCode::GetLocal(0), line),
      _ => self.emit_byte(SymbolicByteCode::Nil, line),
    }

    if self.try_attributes.is_some() {
      self.emit_byte(SymbolicByteCode::PopHandler, line);
    }

    self.emit_byte(SymbolicByteCode::Return, line);
  }

  fn loop_scope(
    &mut self,
    end_line: u32,
    start: Label,
    end: Label,
    table: &'a SymbolTable<'src>,
    cb: impl FnOnce(&mut Self),
  ) {
    // set this loop as current
    let loop_attributes = LoopAttributes {
      scope_depth: self.scope_depth,
      start,
      end,
    };
    let enclosing_loop = mem::replace(&mut self.loop_attributes, Some(loop_attributes));

    self.scope(end_line, table, cb);
    self.emit_byte(SymbolicByteCode::Loop(start), end_line);
    self.emit_byte(SymbolicByteCode::Label(end), end_line);

    self.loop_attributes = enclosing_loop;
  }

  /// scope the provided closure
  fn scope(&mut self, end_line: u32, table: &'a SymbolTable<'src>, cb: impl FnOnce(&mut Self)) {
    self.begin_scope(table);

    cb(self);
    self.end_scope(end_line);
  }

  /// Increase the scope depth by 1
  fn begin_scope(&mut self, table: &'a SymbolTable<'src>) {
    self.scope_depth += 1;
    self.local_tables.push(table);
  }

  /// Set the global scope
  fn begin_global_scope(&mut self, table: &'a SymbolTable<'src>) {
    assert!(self.global_table.is_none());
    assert!(self.local_tables.is_empty());
    assert!(self.scope_depth == 0);

    self.scope_depth += 1;
    self.global_table = Some(table);
  }

  /// Decrease the scope depth by 1
  fn end_scope(&mut self, end_line: u32) {
    self.scope_depth -= 1;
    let new_local_count = self.drop_local_count(self.scope_depth);

    self.drop_locals(end_line, new_local_count);

    self.locals.truncate(new_local_count);
    self.local_tables.pop();
  }

  /// Drop all locals to a specified scope depth
  fn drop_locals(&mut self, line: u32, new_local_count: usize) {
    let local_count = self.locals.len();

    for _ in 0..(local_count - new_local_count) {
      self.emit_byte(SymbolicByteCode::Drop, line);
    }
  }

  /// Determine what the local count would be if dropped
  fn drop_local_count(&mut self, scope_depth: usize) -> usize {
    let mut drop_idx = self.locals.len();

    while drop_idx > 0 && self.locals[drop_idx - 1].depth > scope_depth {
      drop_idx -= 1;
    }

    drop_idx
  }

  /// retrieve a named variable from either local or global scope
  fn variable_get(&mut self, name: &Token<'src>) {
    match self.resolve_local(name.str()) {
      Some((local, state)) => match state {
        SymbolState::Initialized => self.emit_byte(SymbolicByteCode::GetLocal(local), name.end()),
        SymbolState::Captured => self.emit_byte(SymbolicByteCode::GetBox(local), name.end()),
        SymbolState::GlobalInitialized => {
          let global_index = self.identifier_constant(name.str());
          self.emit_byte(SymbolicByteCode::GetGlobal(global_index), name.end())
        },
        SymbolState::Uninitialized => panic!(
          "Unexpected uninitialized symbol {} in {}.",
          name.str(),
          self.fun.name()
        ),
      },
      None => match self.resolve_capture(name.str()) {
        Some((local, state)) => match state {
          SymbolState::Captured => self.emit_byte(SymbolicByteCode::GetCapture(local), name.end()),
          SymbolState::GlobalInitialized => {
            let global_index = self.identifier_constant(name.str());
            self.emit_byte(SymbolicByteCode::GetGlobal(global_index), name.end())
          },
          SymbolState::Initialized => panic!(
            "Unexpected local symbol {} in {}.",
            name.str(),
            self.fun.name()
          ),
          SymbolState::Uninitialized => panic!(
            "Unexpected uninitialized symbol {} in {}.",
            name.str(),
            self.fun.name()
          ),
        },
        None => {
          if self.repl {
            let global_index = self.identifier_constant(name.str());
            self.emit_byte(SymbolicByteCode::GetGlobal(global_index), name.end())
          } else {
            panic!("Symbol {} not found in {}.", name.str(), self.fun.name());
          }
        },
      },
    }
  }

  fn variable_set(&mut self, name: &Token<'src>) {
    match self.resolve_local(name.str()) {
      Some((local, state)) => match state {
        SymbolState::Initialized => self.emit_byte(SymbolicByteCode::SetLocal(local), name.end()),
        SymbolState::Captured => self.emit_byte(SymbolicByteCode::SetBox(local), name.end()),
        SymbolState::GlobalInitialized => {
          let global_index = self.identifier_constant(name.str());
          self.emit_byte(SymbolicByteCode::SetGlobal(global_index), name.end())
        },
        SymbolState::Uninitialized => panic!(
          "Unexpected uninitialized symbol {} in {}.",
          name.str(),
          self.fun.name()
        ),
      },
      None => match self.resolve_capture(name.str()) {
        Some((local, state)) => match state {
          SymbolState::Captured => self.emit_byte(SymbolicByteCode::SetCapture(local), name.end()),
          SymbolState::GlobalInitialized => {
            let global_index = self.identifier_constant(name.str());
            self.emit_byte(SymbolicByteCode::SetGlobal(global_index), name.end())
          },
          SymbolState::Initialized => panic!(
            "Unexpected local symbol {} in {}.",
            name.str(),
            self.fun.name()
          ),
          SymbolState::Uninitialized => panic!(
            "Unexpected uninitialized symbol {} in {}.",
            name.str(),
            self.fun.name()
          ),
        },
        None => {
          if self.repl {
            let global_index = self.identifier_constant(name.str());
            self.emit_byte(SymbolicByteCode::SetGlobal(global_index), name.end())
          } else {
            panic!("Symbol {} not found in {}.", name.str(), self.fun.name())
          }
        },
      },
    }
  }

  fn emit_local_get(&mut self, state: SymbolState, index: u8, end: u32) {
    let byte = match state {
      SymbolState::Initialized => SymbolicByteCode::GetLocal(index),
      SymbolState::Captured => SymbolicByteCode::GetBox(index),
      _ => panic!("Expected local symbol"),
    };
    self.emit_byte(byte, end);
  }

  fn emit_local_set(&mut self, state: SymbolState, index: u8, end: u32) {
    let byte = match state {
      SymbolState::Initialized => SymbolicByteCode::SetLocal(index),
      SymbolState::Captured => SymbolicByteCode::SetBox(index),
      _ => panic!("Expected local symbol"),
    };
    self.emit_byte(byte, end);
  }

  /// resolve a token to a local if it exists
  fn resolve_local(&mut self, name: &str) -> Option<(u8, SymbolState)> {
    let local_count = self.locals.len();

    for (i, local) in self.locals.iter().rev().enumerate() {
      if name == local.symbol.name() {
        return Some(((local_count - i - 1) as u8, local.symbol.state()));
      }
    }

    if let Some(global_table) = self.global_table {
      if let Some(symbol) = global_table.get(name) {
        return Some((0, symbol.state()));
      }
    }

    // not found
    Option::None
  }

  /// resolve a token to an capture in an enclosing scope if it exists
  fn resolve_capture(&mut self, name: &str) -> Option<(u8, SymbolState)> {
    match self.enclosing {
      Some(mut parent_ptr) => {
        let parent = unsafe { parent_ptr.as_mut() };
        match parent.resolve_local(name) {
          Some((local, state)) => match state {
            SymbolState::GlobalInitialized => Some((0, state)),
            _ => Some((self.add_capture(CaptureIndex::Local(local)), state)),
          },
          None => parent
            .resolve_capture(name)
            .map(|(capture, state)| match state {
              SymbolState::GlobalInitialized => (0, state),
              _ => (self.add_capture(CaptureIndex::Enclosing(capture)), state),
            }),
        }
      },
      None => None,
    }
  }

  /// add an capture
  fn add_capture(&mut self, capture: CaptureIndex) -> u8 {
    // check for existing captures
    if let Some(i) =
      self
        .captures
        .iter()
        .position(|existing_capture| match (&existing_capture, &capture) {
          (CaptureIndex::Local(existing), CaptureIndex::Local(new)) => *existing == *new,
          _ => false,
        })
    {
      return i as u8;
    }

    let capture_count = self.fun.capture_count();

    // prevent overflow
    if capture_count == u8::MAX {
      self.error("Too many closure variables in function.", None);
      return 0;
    }

    self.captures.push(capture);
    self.fun.inc_capture();

    capture_count
  }

  /// Define a variable
  fn declare_variable(&mut self, name: &Token<'src>, offset: u32) -> SymbolState {
    // if global exit
    if self.scope_depth == 1 {
      return self
        .global_table
        .and_then(|table| table.get(name.str()).map(|symbol| symbol.state()))
        .expect("Expected global symbol");
    }

    if self.locals.len() == u8::MAX as usize {
      self.error(
        &format!("Too many local variables in function {}.", self.fun.name()),
        Some(name),
      );
    }

    let table = self
      .local_tables
      .last()
      .expect("Expected local symbol table.");

    let symbol = table.get(name.str()).expect("Expected symbol.");

    self.locals.push(Local {
      symbol,
      depth: self.scope_depth,
    });

    if let SymbolState::Captured = symbol.state() {
      self.emit_byte(SymbolicByteCode::EmptyBox, offset);
    }

    symbol.state()
  }

  /// declare variable only variable
  fn declare_local_variable(&mut self, name: &str) -> SymbolState {
    assert!(self.scope_depth > 1);

    if self.locals.len() == u8::MAX as usize {
      self.error(
        &format!("Too many local variables in function {}.", self.fun.name()),
        None,
      );
    }

    let table = self
      .local_tables
      .last()
      .expect("Expected local symbol table.");

    let symbol = table.get(name).expect("Expected symbol.");

    self.locals.push(Local {
      symbol,
      depth: self.scope_depth,
    });

    symbol.state()
  }

  // declare the script
  fn declare_script(&mut self) {
    let table = self.global_table.expect("Expected global symbol table.");

    self.locals.push(Local {
      symbol: table.get(UNINITIALIZED_VAR).expect("Expected symbol."),
      depth: self.scope_depth,
    });
  }

  /// Define a variable
  fn define_variable(&mut self, variable: u16, state: SymbolState, offset: u32) {
    if let SymbolState::Captured = state {
      self.emit_byte(SymbolicByteCode::FillBox, offset);
    }

    if self.scope_depth > 1 {
      return;
    }

    self.emit_byte(SymbolicByteCode::DefineGlobal(variable), offset);
  }

  /// Define a local only variable
  fn define_local_variable(&mut self, name: &str, state: SymbolState, offset: u32) {
    assert!(self.scope_depth > 1);

    if let SymbolState::Captured = state {
      let (slot, _) = self.resolve_local(name).expect("Expected local symbol.");
      self.emit_byte(SymbolicByteCode::Box(slot), offset);
    }
  }

  /// Emit an invoke for a known method call
  fn emit_known_invoke(&mut self, name: &str, args: u8, offset: u32) {
    let name = self.identifier_constant(name);
    self.emit_byte(SymbolicByteCode::Invoke((name, args)), offset);
    self.emit_byte(SymbolicByteCode::InvokeSlot, offset)
  }

  /// Emit a provided instruction
  fn emit_byte(&mut self, op_code: SymbolicByteCode, offset: u32) {
    let line = self
      .line_offsets
      .offset_line(offset as usize)
      .expect("Line offset out of bounds");

    self.write_instruction(op_code, line as u16 + 1);
  }

  /// write instruction to the current function
  fn write_instruction(&mut self, op_code: SymbolicByteCode, line: u16) {
    self.chunk.write_instruction(op_code, line)
  }

  /// Parse a variable from the provided token return it's new constant
  /// identifer if an identifer was identified
  fn make_identifier(&mut self, name: &Token<'src>, offset: u32) -> (u16, SymbolState) {
    let state = self.declare_variable(name, offset);
    if self.scope_depth > 1 {
      return (0, state);
    }
    (self.identifier_constant(name.str()), state)
  }

  /// Generate a constant from the provided identifier token
  fn identifier_constant(&mut self, name: &str) -> u16 {
    self.string_constant(name)
  }

  /// Generate a constant from the provided identifier token
  fn string_constant(&mut self, str: &str) -> u16 {
    let identifer = self.gc.borrow_mut().manage_str(str, self);
    self.make_constant(val!(identifer))
  }

  /// Add a constant to the current chunk
  fn make_constant(&mut self, value: Value) -> u16 {
    match self.constants.get(&value) {
      Some(index) => *index as u16,
      None => {
        let index = self.chunk.add_constant(value);
        if index > u16::MAX as usize {
          self.error("Too many constants in one chunk.", None);
          return 0;
        }

        self.constants.insert(value, index);
        index as u16
      },
    }
  }

  /// Emit byte code for a constant
  fn emit_constant(&mut self, value: Value, line: u32) {
    let index = self.make_constant(value);

    if index <= u8::MAX as u16 {
      self.emit_byte(SymbolicByteCode::Constant(index as u8), line);
    } else {
      self.emit_byte(SymbolicByteCode::ConstantLong(index), line);
    }
  }

  /// Indicate an error occurred at he current index
  fn error_at_current(&mut self, message: &str, token: Option<&Token>) {
    self.error_at(message, token);
  }

  /// Indicate an error occurred at the previous index
  fn error(&mut self, message: &str, token: Option<&Token>) {
    self.error_at(message, token);
  }

  /// Print an error to the console for a user to address
  fn error_at(&mut self, message: &str, token: Option<&Token>) {
    let error = Diagnostic::error().with_message(message);

    let error = match token {
      Some(token) => {
        error.with_labels(vec![diagnostic::Label::primary(self.file_id, token.span())])
      },
      None => error,
    };

    self.errors.push(error);
  }

  /// Compile a declaration
  fn decl(&mut self, decl: &'a Decl<'src>) {
    match decl {
      Decl::Symbol(symbol) => self.symbol(symbol),
      Decl::Export(export) => self.export(export),
      Decl::Stmt(stmt) => self.stmt(stmt),
      Decl::Error(error) => self.visit_error(error),
    }
  }

  /// Compile a statement
  fn stmt(&mut self, stmt: &'a Stmt<'src>) {
    match stmt {
      Stmt::Expr(expr) => {
        self.expr(expr);
        self.emit_byte(SymbolicByteCode::Drop, expr.end());
      },
      Stmt::ImplicitReturn(expr) => {
        self.expr(expr);
        self.emit_byte(SymbolicByteCode::Return, expr.end());
      },
      Stmt::Import(import) => self.import(import),
      Stmt::For(for_) => self.for_(for_),
      Stmt::If(if_) => self.if_(if_),
      Stmt::Return(return_) => self.return_(return_),
      Stmt::Launch(launch) => self.launch(launch),
      Stmt::Break(break_) => self.break_(break_),
      Stmt::Continue(continue_) => self.continue_(continue_),
      Stmt::While(while_) => self.while_(while_),
      Stmt::Try(try_) => self.try_(try_),
      Stmt::Raise(raise) => self.raise(raise),
    }
  }

  /// Compile an expression
  fn expr(&mut self, expr: &'a Expr<'src>) {
    match expr {
      Expr::Assign(assign) => self.assign(assign),
      Expr::Send(drain) => self.send(drain),
      Expr::AssignBinary(assign_binary) => self.assign_binary(assign_binary),
      Expr::Ternary(ternary) => self.ternary(ternary),
      Expr::Binary(binary) => self.binary(binary),
      Expr::Unary(unary) => self.unary(unary),
      Expr::Atom(atom) => self.atom(atom),
    }
  }

  /// Compile a the base of an expression
  fn primary(&mut self, primary: &'a Primary<'src>) {
    match primary {
      Primary::Channel(token) => self.channel(token),
      Primary::True(token) => self.true_(token),
      Primary::False(token) => self.false_(token),
      Primary::Nil(token) => self.nil(token),
      Primary::Number(token) => self.number(token),
      Primary::Grouping(expr) => self.expr(expr),
      Primary::String(token) => self.string(token),
      Primary::Interpolation(interpolation) => self.interpolation(interpolation),
      Primary::Ident(token) => self.identifier(token),
      Primary::InstanceAccess(instance_access) => self.instance_access(instance_access),
      Primary::Self_(token) => self.self_(token),
      Primary::Super(token) => self.super_(token),
      Primary::Lambda(fun) => self.lambda(fun),
      Primary::List(list) => self.list(list),
      Primary::Tuple(tuple) => self.tuple(tuple),
      Primary::Map(map) => self.map(map),
    }
  }

  /// Compile a symbol declaration
  fn symbol(&mut self, symbol: &'a ast::Symbol<'src>) {
    match symbol {
      ast::Symbol::Class(class) => self.class(class),
      ast::Symbol::Fun(fun) => self.fun(fun),
      ast::Symbol::Let(let_) => self.let_(let_),
      _ => 0,
    };
  }

  /// Compile an export declaration
  fn export(&mut self, export: &'a ast::Symbol<'src>) {
    let symbol = match &export {
      ast::Symbol::Class(class) => Some(self.class(class)),
      ast::Symbol::Fun(fun) => Some(self.fun(fun)),
      ast::Symbol::Let(let_) => Some(self.let_(let_)),
      _ => None,
    };

    // emit error if not at module level
    if self.scope_depth == 1 {
      if let Some(symbol) = symbol {
        self.emit_byte(SymbolicByteCode::Export(symbol), export.end());
      }
    } else {
      self.error_at_current("Can only export from the module scope.", None)
    }
  }

  /// Compile a class declaration
  fn class(&mut self, class: &'a ast::Class<'src>) -> u16 {
    // declare the class by name
    let class_name = &class.name;
    let class_state = self.declare_variable(class_name, class.start());

    let class_constant = self.identifier_constant(class_name.str());

    self.emit_byte(SymbolicByteCode::Class(class_constant), class_name.end());
    self.define_variable(class_constant, class_state, class_name.end());

    // set this class as the current class compiler
    let managed_class_name = self.gc.borrow_mut().manage_str(class_name.str(), self);
    let class_attributes = self
      .gc
      .borrow_mut()
      .manage(ClassAttributes::new(managed_class_name), self);
    let enclosing_class = mem::replace(&mut self.class_attributes, Some(class_attributes));
    if let Some(enclosing_class) = enclosing_class {
      self.gc.borrow_mut().push_root(enclosing_class);
    }

    // start a new scope with the super keyword present
    self.begin_scope(&class.symbols);

    // Load an explicit or implicit super class
    let span = if let Some(super_class) = &class.super_class {
      super_class.type_ref.name.span()
    } else {
      class.name.span()
    };

    let super_state = self.declare_local_variable(SUPER);

    // Load an explicit or implicit super class
    if let Some(super_class) = &class.super_class {
      self.variable_get(&super_class.type_ref.name);
    } else {
      self.variable_get(&Token::new(
        TokenKind::Identifier,
        Lexeme::Slice(OBJECT),
        class.name.start(),
        class.name.end(),
      ));
    };

    self.define_local_variable(SUPER, super_state, span.end);

    self.variable_get(class_name);
    self.emit_byte(SymbolicByteCode::Inherit, span.end);

    // process the initializer
    let field_line = if let Some(init) = &class.init {
      self.method(init, FunKind::Initializer);
      init.start()
    } else {
      class.start()
    };

    self.emit_fields(field_line);

    // process methods
    for method in &class.methods {
      self.method(method, FunKind::Method);
    }

    // process static methods
    for static_method in &class.static_methods {
      self.static_method(static_method);
    }

    self.emit_byte(SymbolicByteCode::Drop, class.end());

    // if we have a super drop the extra scope with super
    self.end_scope(class.end());

    // restore the enclosing class compiler
    if enclosing_class.is_some() {
      self.gc.borrow_mut().pop_roots(1);
    }
    self.class_attributes = enclosing_class;
    class_constant
  }

  /// Emit field instructions
  fn emit_fields(&mut self, line: u32) {
    let class_info = self.class_attributes.expect("Current class unset");

    class_info.fields.iter().for_each(|f| {
      let constant = self.make_constant(val!(*f));
      self.emit_byte(SymbolicByteCode::Field(constant), line)
    })
  }

  /// Compile a method
  fn method(&mut self, method: &'a ast::Fun<'src>, fun_kind: FunKind) {
    let constant = method
      .name
      .as_ref()
      .map(|name| self.identifier_constant(name.str()))
      .expect("Expect method name");

    self
      .class_attributes
      .expect("Class compiler not set")
      .fun_kind = Some(fun_kind);

    self.function(method, fun_kind);
    self.emit_byte(SymbolicByteCode::Method(constant), method.end());
  }

  /// Compile a static method
  fn static_method(&mut self, static_method: &'a ast::Fun<'src>) {
    let constant = static_method
      .name
      .as_ref()
      .map(|name| self.identifier_constant(name.str()))
      .expect("Expected method name.");

    self
      .class_attributes
      .expect("Class compiler not set")
      .fun_kind = Some(FunKind::StaticMethod);

    self.function(static_method, FunKind::StaticMethod);
    self.emit_byte(
      SymbolicByteCode::StaticMethod(constant),
      static_method.end(),
    );
  }

  /// Compile a plain function
  fn fun(&mut self, fun: &'a ast::Fun<'src>) -> u16 {
    let (constant, fun_state) = fun
      .name
      .as_ref()
      .map(|name| self.make_identifier(name, fun.end()))
      .expect("Expected function name");

    self.function(fun, FunKind::Fun);
    self.define_variable(constant, fun_state, fun.end());

    constant
  }

  /// Compile a let binding
  fn let_(&mut self, let_: &'a ast::Let<'src>) -> u16 {
    let var_state = self.declare_variable(&let_.name, let_.end());
    let variable = self.identifier_constant(let_.name.str());

    match &let_.value {
      Some(v) => self.expr(v),
      None => self.emit_byte(SymbolicByteCode::Nil, let_.name.end()),
    }

    self.define_variable(variable, var_state, let_.end());
    variable
  }

  /// Compile a function objects that presents, functions, methods
  /// and lambdas
  fn function(&mut self, fun: &'a ast::Fun<'src>, fun_kind: FunKind) {
    // manage name assume name "lambda" if none is provided
    let name = fun
      .name
      .as_ref()
      .map(|name| self.gc.borrow_mut().manage_str(name.str(), self))
      .unwrap_or_else(|| self.gc.borrow_mut().manage_str("lambda", self));

    let arity = Arity::Fixed(fun.call_sig.params.len() as u8);

    // create a new child compiler for this function
    let mut compiler = Compiler::child(name, arity, fun_kind, self);

    compiler.begin_scope(&fun.symbols);

    match fun_kind {
      FunKind::Method | FunKind::Initializer => {
        let self_state = compiler.declare_local_variable(SELF);
        compiler.define_local_variable(SELF, self_state, fun.start());
      },
      FunKind::Fun | FunKind::StaticMethod => {
        compiler.declare_local_variable(UNINITIALIZED_VAR);
      },
      _ => panic!("Did not expect script."),
    };

    compiler.call_sig(&fun.call_sig);

    match &fun.body {
      ast::FunBody::Block(block) => compiler.block(block),
      ast::FunBody::Expr(expr) => {
        compiler.expr(expr);
        compiler.emit_byte(SymbolicByteCode::Return, expr.end());
      },
    };

    let end_line = fun.end();

    // end compilation of function chunk
    let (fun, errors, captures, gc) = compiler.end_compiler(end_line);

    self.gc.replace(gc);

    let fun = self.gc.borrow_mut().manage_obj(fun, self);

    if !errors.is_empty() {
      self.errors.extend_from_slice(&errors);
    }

    if fun.capture_count() == 0 && matches!(fun_kind, FunKind::Fun | FunKind::Script) {
      self.emit_constant(val!(fun), end_line);
    } else {
      let index = self.make_constant(val!(fun));
      self.emit_byte(SymbolicByteCode::Closure(index), end_line);

      // emit capture index instructions
      captures
        .iter()
        .for_each(|capture| self.emit_byte(SymbolicByteCode::CaptureIndex(*capture), end_line));
    }
  }

  /// Compile an import statement
  fn import(&mut self, import: &'a ast::Import<'src>) {
    let value = {
      let hooks = GcHooks::new(self);

      let mut list = hooks.manage_obj(ListBuilder::new(&[], import.path.len()));
      hooks.push_root(list);


      for segment in &import.path {
        list.push(val!(hooks.manage_str(segment.str())), &hooks)
      }

      hooks.pop_roots(1);
      val!(list)
    };

    let path = self.make_constant(value);

    match &import.stem {
      ast::ImportStem::None => {
        self.emit_byte(SymbolicByteCode::Import(path), import.start());
        let (name, _) =
          self.make_identifier(&import.path()[import.path().len() - 1], import.start());
        self.emit_byte(SymbolicByteCode::DefineGlobal(name), import.end());
      },
      ast::ImportStem::Rename(rename) => {
        self.emit_byte(SymbolicByteCode::Import(path), import.start());
        let (name, _) = self.make_identifier(rename, import.start());
        self.emit_byte(SymbolicByteCode::DefineGlobal(name), import.end());
      },
      ast::ImportStem::Symbols(symbols) => {
        for symbol in symbols {
          let symbol_slot = self.identifier_constant(symbol.symbol.str());
          self.emit_byte(
            SymbolicByteCode::ImportSymbol((path, symbol_slot)),
            symbol.start(),
          );

          let name = match &symbol.rename {
            Some(rename) => self.make_identifier(rename, import.start()).0,
            None => self.make_identifier(&symbol.symbol, import.start()).0,
          };

          self.emit_byte(SymbolicByteCode::DefineGlobal(name), import.end());
        }
      },
    }
  }

  /// Compile a for loop
  fn for_(&mut self, for_: &'a ast::For<'src>) {
    const NEXT: &str = "next";
    const CURRENT: &str = "current";

    // new scope for full loop including loop variables
    self.scope(for_.end(), &for_.symbols, |self_| {
      // push iterable onto stack
      self_.expr(&for_.iter);
      let expr_line = for_.iter.end();

      // get constant for 'iter' method
      let iter_method_const = self_.string_constant(ITER);

      // declare the hidden local $iter variable
      self_.declare_local_variable(ITER_VAR);
      self_.emit_byte(SymbolicByteCode::Invoke((iter_method_const, 0)), expr_line);
      self_.emit_byte(SymbolicByteCode::InvokeSlot, expr_line);
      self_.define_local_variable(ITER_VAR, SymbolState::Initialized, expr_line);

      let item_line = for_.item.end();
      let item_state = self_.declare_local_variable(for_.item.str());

      // initial fill iteration item with nil and define
      self_.emit_byte(SymbolicByteCode::Nil, item_line);
      self_.define_local_variable(for_.item.str(), item_state, item_line);

      // mark start of loop
      let start_label = self_.label_emitter.emit();
      self_.emit_byte(SymbolicByteCode::Label(start_label), item_line);

      // define iterator method constants
      let next_const = self_.string_constant(NEXT);
      let current_const = self_.string_constant(CURRENT);

      // call next on iterator
      let (iterator_variable, iterator_state) = self_
        .resolve_local(ITER_VAR)
        .expect("Iterator variable was not defined.");

      self_.emit_local_get(iterator_state, iterator_variable, expr_line);
      self_.emit_byte(SymbolicByteCode::IterNext(next_const), expr_line);

      // check at end of iterator
      let end_label = self_.label_emitter.emit();
      self_.emit_byte(SymbolicByteCode::JumpIfFalse(end_label), expr_line);

      // assign $iter.current to loop variable
      let (loop_variable, loop_state) = self_
        .resolve_local(for_.item.str())
        .expect("Loop variable was not defined.");

      self_.emit_local_get(iterator_state, iterator_variable, expr_line);
      self_.emit_byte(SymbolicByteCode::IterCurrent(current_const), expr_line);
      self_.emit_local_set(loop_state, loop_variable, expr_line);
      self_.emit_byte(SymbolicByteCode::Drop, expr_line);

      // loop body
      self_.loop_scope(
        for_.body.end(),
        start_label,
        end_label,
        &for_.body.symbols,
        |self_| {
          self_.block(&for_.body);
        },
      );
    });
  }

  /// Compile a while loop
  fn while_(&mut self, while_: &'a ast::While<'src>) {
    let start_label = self.label_emitter.emit();
    self.emit_byte(SymbolicByteCode::Label(start_label), while_.cond.start());

    self.expr(&while_.cond);
    let end_label = self.label_emitter.emit();
    self.emit_byte(SymbolicByteCode::JumpIfFalse(end_label), while_.cond.end());

    self.loop_scope(
      while_.end(),
      start_label,
      end_label,
      &while_.body.symbols,
      |self_| {
        self_.block(&while_.body);
      },
    );
  }

  /// Compile a if statement
  fn if_(&mut self, if_: &'a ast::If<'src>) {
    self.expr(&if_.cond);

    // parse then branch
    let then_label = self.label_emitter.emit();
    self.emit_byte(SymbolicByteCode::JumpIfFalse(then_label), if_.cond.end());

    self.scope(if_.body.end(), &if_.body.symbols, |self_| {
      self_.block(&if_.body);
    });

    match &if_.else_ {
      Some(else_) => {
        // emit else jump
        let else_label = self.label_emitter.emit();
        self.emit_byte(SymbolicByteCode::Jump(else_label), if_.body.end());
        self.emit_byte(SymbolicByteCode::Label(then_label), if_.body.end());

        match else_ {
          ast::Else::If(if_) => self.if_(if_),
          ast::Else::Block(block) => self.scope(block.end(), &block.symbols, |self_| {
            self_.block(block);
          }),
        }

        self.emit_byte(SymbolicByteCode::Label(else_label), if_.body.end());
      },
      None => self.emit_byte(SymbolicByteCode::Label(then_label), if_.body.end()),
    }
  }

  /// Compile a launch statement
  fn launch(&mut self, launch: &'a ast::Launch<'src>) {
    match &launch.closure {
      // we assume an atom from the parser
      Expr::Atom(atom) => match atom.trailers.last() {
        Some(Trailer::Call(call)) => {
          // emit instruction for everything but the actual call
          self.primary(&atom.primary);
          self.apply_trailers(&atom.trailers[..atom.trailers.len() - 1]);

          // emit for each argument for the launch call
          for expr in &call.args {
            self.expr(expr);
          }

          // emit for the actual launch
          self.emit_byte(SymbolicByteCode::Launch(call.args.len() as u8), call.end());
        },
        _ => unreachable!("Unexpected expression after launch."),
      },
      _ => unreachable!("Unexpected expression after launch."),
    }
  }

  /// Compile a return statement
  fn return_(&mut self, return_: &'a ast::Return<'src>) {
    match &return_.value {
      Some(v) => {
        self.expr(v);

        if self.try_attributes.is_some() {
          self.emit_byte(SymbolicByteCode::PopHandler, v.end());
        }

        self.emit_byte(SymbolicByteCode::Return, v.end());
      },
      None => self.emit_return(return_.start()),
    }
  }

  /// Compile a return statement
  fn continue_(&mut self, continue_: &Token) {
    let loop_attributes = self
      .loop_attributes
      .expect("Parser should have caught the loop constraint");

    let new_local_count = self.drop_local_count(loop_attributes.scope_depth);
    self.drop_locals(continue_.end(), new_local_count);

    // if our try catch is inside this loop
    // a break will jump outside of it so we need to pop the handler
    if let Some(try_attributes) = self.try_attributes {
      if try_attributes.scope_depth > loop_attributes.scope_depth {
        self.emit_byte(SymbolicByteCode::PopHandler, continue_.start());
      }
    }

    self.emit_byte(
      SymbolicByteCode::Loop(loop_attributes.start),
      continue_.start(),
    );
  }

  /// Compile a return statement
  fn break_(&mut self, break_: &Token) {
    let loop_attributes = self
      .loop_attributes
      .expect("Parser should have caught the loop constraint");

    let new_local_count = self.drop_local_count(loop_attributes.scope_depth);
    self.drop_locals(break_.end(), new_local_count);

    // if our try catch is inside this loop
    // a break will jump outside of it so we need to pop the handler
    if let Some(try_attributes) = self.try_attributes {
      if try_attributes.scope_depth > loop_attributes.scope_depth {
        self.emit_byte(SymbolicByteCode::PopHandler, break_.start());
      }
    }

    self.emit_byte(SymbolicByteCode::Jump(loop_attributes.end), break_.start());
  }

  /// Compile a try catch block
  fn try_(&mut self, try_: &'a ast::Try<'src>) {
    // set this try block as the current
    let try_attributes = TryAttributes {
      scope_depth: self.scope_depth,
    };
    let enclosing_try = mem::replace(&mut self.try_attributes, Some(try_attributes));

    let catch_label = self.label_emitter.emit();

    // We currently use zero as a placeholder as the peephole compiler
    // determines the actual value
    self.emit_byte(
      SymbolicByteCode::PushHandler((0, catch_label)),
      try_.block.start(),
    );

    self.scope(try_.block.end(), &try_.block.symbols, |self_| {
      self_.block(&try_.block);
    });

    self.emit_byte(SymbolicByteCode::PopHandler, try_.block.end());

    let try_end_label = self.label_emitter.emit();
    self.emit_byte(SymbolicByteCode::Jump(try_end_label), try_.block.end());

    // Temp shim in first catch block only
    // For now we basically ignore the rest of the catch blocks and don't bind anything to the
    // provided variable. This was be implemented later
    let catch = try_.catches.first().expect("Expected catch block");
    self.emit_byte(SymbolicByteCode::Label(catch_label), catch.start());
    self.try_attributes = enclosing_try;

    for catch in &try_.catches {
      self.catch(catch, try_end_label);
    }

    self.emit_byte(SymbolicByteCode::ContinueUnwind, catch.end());
    self.emit_byte(SymbolicByteCode::Label(try_end_label), catch.end());
  }

  /// Compile a catch block
  fn catch(&mut self, catch: &'a ast::Catch<'src>, try_end_label: Label) {
    let default_error = &Token::new(
      TokenKind::Identifier,
      Lexeme::Slice(ERROR_CLASS_NAME),
      catch.name.end(),
      catch.name.end(),
    );

    let catch_end_label: Label = self.label_emitter.emit();
    self.scope(catch.end(), &catch.symbols, |self_| {
      // Load error class onto stack
      self_.variable_get(catch.class.as_ref().unwrap_or(default_error));
      self_.emit_byte(
        SymbolicByteCode::CheckHandler(catch_end_label),
        catch.start(),
      );

      // If we end the catch signal that we are done unwinding
      self_.emit_byte(SymbolicByteCode::FinishUnwind, catch.start());
      self_.emit_byte(SymbolicByteCode::PopHandler, catch.start());

      let var_state = self_.declare_variable(&catch.name, catch.name.end());
      let variable: u16 = self_.identifier_constant(catch.name.str());

      self_.emit_byte(SymbolicByteCode::GetError, catch.name.start());

      self_.define_variable(variable, var_state, catch.end());

      self_.scope(catch.end(), &catch.block.symbols, |self__| {
        self__.block(&catch.block);
      });
    });

    self.emit_byte(SymbolicByteCode::Jump(try_end_label), catch.end());
    self.emit_byte(SymbolicByteCode::Label(catch_end_label), catch.end());
  }

  /// Compile a raise statement
  fn raise(&mut self, raise: &'a ast::Raise<'src>) {
    self.expr(&raise.error);

    self.emit_byte(SymbolicByteCode::Raise, raise.end());
  }

  /// Compile a block
  fn block(&mut self, block: &'a ast::Block<'src>) {
    for decl in &block.decls {
      self.decl(decl);
    }
  }

  /// Compile an assignment expression
  fn assign(&mut self, assign: &'a ast::Assign<'src>) {
    match &assign.lhs {
      Expr::Atom(atom) => match atom.trailers.last() {
        // if we have trailers compile to last trailer and emit specialized
        // set instruction
        Some(last) => {
          self.primary(&atom.primary);
          self.apply_trailers(&atom.trailers[..atom.trailers.len() - 1]);

          match last {
            Trailer::Index(index) => {
              self.expr(&assign.rhs);
              self.expr(&index.index);

              self.emit_known_invoke(INDEX_SET, 2, assign.rhs.end());
            },
            Trailer::Access(access) => {
              if self.fun_kind == FunKind::Initializer && atom.trailers.len() == 1 {
                if let Primary::Self_(_) = atom.primary {
                  let mut class_info = self.class_attributes.unwrap();

                  if !class_info.fields.iter().any(|f| *f == access.prop.str()) {
                    let field = self.gc.borrow_mut().manage_str(access.prop.str(), self);
                    class_info.add_field(field);
                  }
                }
              }

              let name = self.identifier_constant(access.prop.str());

              self.expr(&assign.rhs);
              self.emit_byte(SymbolicByteCode::SetProperty(name), access.end());
              self.emit_byte(SymbolicByteCode::PropertySlot, access.end());
            },
            Trailer::Call(_) => {
              unreachable!("Unexpected expression on left hand side of assignment.")
            },
          }
        },
        None => match &atom.primary {
          Primary::Ident(name) => {
            self.expr(&assign.rhs);
            self.variable_set(name);
          },
          Primary::InstanceAccess(instance_access) => {
            self.instance_access_self(instance_access);
            let property = instance_access.property();

            if self.fun_kind == FunKind::Initializer {
              let mut class_info = self.class_attributes.unwrap();

              if !class_info.fields.iter().any(|f| *f == property) {
                let field = self.gc.borrow_mut().manage_str(property, self);
                class_info.add_field(field);
              }
            }

            let name = self.identifier_constant(property);

            self.expr(&assign.rhs);
            self.emit_byte(SymbolicByteCode::SetProperty(name), instance_access.end());
            self.emit_byte(SymbolicByteCode::PropertySlot, instance_access.end());
          },
          _ => unreachable!("Unexpected expression on left hand side of assignment."),
        },
      },
      _ => unreachable!("Unexpected expression on left hand side of assignment."),
    }
  }

  fn send(&mut self, send: &'a ast::Send<'src>) {
    self.expr(&send.rhs);

    match &send.lhs {
      Expr::Atom(atom) => match atom.trailers.last() {
        // if we have trailers compile to last trailer and emit specialized
        // set instruction
        Some(last) => {
          self.primary(&atom.primary);
          self.apply_trailers(&atom.trailers[..atom.trailers.len() - 1]);

          match last {
            Trailer::Index(index) => {
              // self.expr(&send.rhs);
              self.expr(&index.index);

              self.emit_known_invoke(INDEX_GET, 1, index.end());
              self.emit_byte(SymbolicByteCode::Send, send.lhs.end())
            },
            Trailer::Access(access) => {
              if self.fun_kind == FunKind::Initializer && atom.trailers.len() == 1 {
                if let Primary::Self_(_) = atom.primary {
                  let mut class_info = self.class_attributes.unwrap();

                  if !class_info.fields.iter().any(|f| *f == access.prop.str()) {
                    let field = self.gc.borrow_mut().manage_str(access.prop.str(), self);
                    class_info.add_field(field);
                  }
                }
              }

              let name = self.identifier_constant(access.prop.str());

              self.emit_byte(SymbolicByteCode::GetProperty(name), access.end());
              self.emit_byte(SymbolicByteCode::PropertySlot, access.end());

              self.emit_byte(SymbolicByteCode::Send, send.lhs.end())
            },
            Trailer::Call(_) => {
              unreachable!("Unexpected expression on left hand side of send.")
            },
          }
        },
        None => match &atom.primary {
          Primary::Ident(name) => {
            self.variable_get(name);

            self.emit_byte(SymbolicByteCode::Send, send.lhs.end())
          },
          Primary::InstanceAccess(instance_access) => {
            self.instance_access_self(instance_access);
            let property = instance_access.property();

            if self.fun_kind == FunKind::Initializer {
              let mut class_info = self.class_attributes.unwrap();

              if !class_info.fields.iter().any(|f| *f == property) {
                let field = self.gc.borrow_mut().manage_str(property, self);
                class_info.add_field(field);
              }
            }

            let name = self.identifier_constant(instance_access.property());

            self.emit_byte(SymbolicByteCode::GetProperty(name), instance_access.end());
            self.emit_byte(SymbolicByteCode::PropertySlot, instance_access.end());

            self.emit_byte(SymbolicByteCode::Send, send.lhs.end())
          },
          _ => unreachable!("Unexpected expression on left hand side of assignment."),
        },
      },
      _ => unreachable!("Unexpected expression on left hand side of assignment."),
    }
  }

  /// Compile a binary assignment expression
  fn assign_binary(&mut self, assign_binary: &'a ast::AssignBinary<'src>) {
    let binary_op = |comp: &mut Compiler<'a, 'src>| match &assign_binary.op {
      ast::AssignBinaryOp::Add => comp.emit_byte(SymbolicByteCode::Add, assign_binary.rhs.end()),
      ast::AssignBinaryOp::Sub => {
        comp.emit_byte(SymbolicByteCode::Subtract, assign_binary.rhs.end())
      },
      ast::AssignBinaryOp::Mul => {
        comp.emit_byte(SymbolicByteCode::Multiply, assign_binary.rhs.end())
      },
      ast::AssignBinaryOp::Div => comp.emit_byte(SymbolicByteCode::Divide, assign_binary.rhs.end()),
    };

    match &assign_binary.lhs {
      Expr::Atom(atom) => match atom.trailers.last() {
        // if we have trailers compile to last trailer and emit specialized
        // set instruction
        Some(last) => {
          self.primary(&atom.primary);
          self.apply_trailers(&atom.trailers[..atom.trailers.len() - 1]);

          match last {
            Trailer::Index(index) => {
              self.emit_byte(SymbolicByteCode::Dup, assign_binary.lhs.end());

              self.expr(&index.index);

              self.emit_known_invoke(INDEX_GET, 1, assign_binary.rhs.end());

              self.expr(&assign_binary.rhs);
              binary_op(self);

              self.expr(&index.index);

              self.emit_known_invoke(INDEX_SET, 2, assign_binary.rhs.end());
            },
            Trailer::Access(access) => {
              if self.fun_kind == FunKind::Initializer && atom.trailers.len() == 1 {
                if let Primary::Self_(_) = atom.primary {
                  let mut class_info = self.class_attributes.unwrap();

                  if !class_info.fields.iter().any(|f| *f == access.prop.str()) {
                    let field = self.gc.borrow_mut().manage_str(access.prop.str(), self);
                    class_info.add_field(field);
                  }
                }
              }

              let name = self.identifier_constant(access.prop.str());

              self.emit_byte(SymbolicByteCode::Dup, access.end());
              self.emit_byte(SymbolicByteCode::GetProperty(name), access.end());
              self.emit_byte(SymbolicByteCode::PropertySlot, access.end());

              self.expr(&assign_binary.rhs);
              binary_op(self);

              self.emit_byte(SymbolicByteCode::SetProperty(name), access.end());
              self.emit_byte(SymbolicByteCode::PropertySlot, access.end());
            },
            Trailer::Call(_) => {
              unreachable!("Unexpected expression on left hand side of assignment.")
            },
          }
        },
        None => match &atom.primary {
          Primary::Ident(name) => {
            self.variable_get(name);
            self.expr(&assign_binary.rhs);
            binary_op(self);
            self.variable_set(name);
          },
          Primary::InstanceAccess(instance_access) => {
            self.instance_access_self(instance_access);
            let property = instance_access.property();

            if self.fun_kind == FunKind::Initializer {
              let mut class_info = self.class_attributes.unwrap();

              if !class_info.fields.iter().any(|f| *f == property) {
                let field = self.gc.borrow_mut().manage_str(property, self);
                class_info.add_field(field);
              }
            }

            let name = self.identifier_constant(property);

            self.emit_byte(SymbolicByteCode::Dup, instance_access.end());
            self.emit_byte(SymbolicByteCode::GetProperty(name), instance_access.end());
            self.emit_byte(SymbolicByteCode::PropertySlot, instance_access.end());

            self.expr(&assign_binary.rhs);
            binary_op(self);

            self.emit_byte(SymbolicByteCode::SetProperty(name), instance_access.end());
            self.emit_byte(SymbolicByteCode::PropertySlot, instance_access.end());
          },
          _ => unreachable!("Unexpected expression on left hand side of assignment."),
        },
      },
      _ => unreachable!("Unexpected expression on left hand side of assignment."),
    }
  }

  /// Compile a ternary expression
  fn ternary(&mut self, ternary: &'a ast::Ternary<'src>) {
    self.expr(&ternary.cond);

    // parse then branch
    let then_label = self.label_emitter.emit();
    self.emit_byte(
      SymbolicByteCode::JumpIfFalse(then_label),
      ternary.cond.end(),
    );
    self.expr(&ternary.then);

    // emit else jump
    let else_label = self.label_emitter.emit();
    self.emit_byte(SymbolicByteCode::Jump(else_label), ternary.then.end());
    self.emit_byte(SymbolicByteCode::Label(then_label), ternary.then.end());

    self.expr(&ternary.else_);
    self.emit_byte(SymbolicByteCode::Label(else_label), ternary.then.end());
  }

  /// Compile a binary expression
  fn binary(&mut self, binary: &'a ast::Binary<'src>) {
    self.expr(&binary.lhs);

    // emit for rhs if we're not an "&&" or "||"
    match &binary.op {
      ast::BinaryOp::And | ast::BinaryOp::Or => (),
      _ => self.expr(&binary.rhs),
    }

    // emit for binary operation
    match &binary.op {
      ast::BinaryOp::Add => self.emit_byte(SymbolicByteCode::Add, binary.rhs.end()),
      ast::BinaryOp::Sub => self.emit_byte(SymbolicByteCode::Subtract, binary.rhs.end()),
      ast::BinaryOp::Mul => self.emit_byte(SymbolicByteCode::Multiply, binary.rhs.end()),
      ast::BinaryOp::Div => self.emit_byte(SymbolicByteCode::Divide, binary.rhs.end()),
      ast::BinaryOp::Lt => self.emit_byte(SymbolicByteCode::Less, binary.rhs.end()),
      ast::BinaryOp::LtEq => self.emit_byte(SymbolicByteCode::LessEqual, binary.rhs.end()),
      ast::BinaryOp::Gt => self.emit_byte(SymbolicByteCode::Greater, binary.rhs.end()),
      ast::BinaryOp::GtEq => self.emit_byte(SymbolicByteCode::GreaterEqual, binary.rhs.end()),
      ast::BinaryOp::Eq => self.emit_byte(SymbolicByteCode::Equal, binary.rhs.end()),
      ast::BinaryOp::Ne => self.emit_byte(SymbolicByteCode::NotEqual, binary.rhs.end()),
      ast::BinaryOp::And => {
        let and_label = self.label_emitter.emit();
        self.emit_byte(SymbolicByteCode::And(and_label), binary.lhs.end());
        self.expr(&binary.rhs);
        self.emit_byte(SymbolicByteCode::Label(and_label), binary.rhs.end());
      },
      ast::BinaryOp::Or => {
        let or_label = self.label_emitter.emit();
        self.emit_byte(SymbolicByteCode::Or(or_label), binary.lhs.end());
        self.expr(&binary.rhs);
        self.emit_byte(SymbolicByteCode::Label(or_label), binary.rhs.end());
      },
    }
  }

  /// Compile a unary expression
  fn unary(&mut self, unary: &'a ast::Unary<'src>) {
    self.expr(&unary.expr);

    match &unary.op {
      ast::UnaryOp::Not => self.emit_byte(SymbolicByteCode::Not, unary.expr.end()),
      ast::UnaryOp::Negate => self.emit_byte(SymbolicByteCode::Negate, unary.expr.end()),
      ast::UnaryOp::Receive => self.emit_byte(SymbolicByteCode::Receive, unary.expr.end()),
    }
  }

  /// Compile a call expression
  fn call(&mut self, call: &'a ast::Call<'src>) {
    for expr in &call.args {
      self.expr(expr);
      self.emit_byte(SymbolicByteCode::ArgumentDelimiter, expr.end());
    }

    self.emit_byte(SymbolicByteCode::Call(call.args.len() as u8), call.end());
  }

  /// Compile an indexing expression
  fn index(&mut self, index: &'a ast::Index<'src>) {
    self.expr(&index.index);

    self.emit_known_invoke(INDEX_GET, 1, index.end());
  }

  /// Compile an access expression
  fn access(&mut self, access: &ast::Access) {
    let name = self.identifier_constant(access.prop.str());
    self.emit_byte(SymbolicByteCode::GetProperty(name), access.prop.end());
    self.emit_byte(SymbolicByteCode::PropertySlot, access.end());
  }

  /// Compile an atom expression
  fn atom(&mut self, atom: &'a ast::Atom<'src>) {
    self.primary(&atom.primary);
    self.apply_trailers(&atom.trailers);
  }

  /// Compile trailers onto a base primary
  fn apply_trailers(&mut self, trailers: &'a [Trailer<'src>]) {
    for trailer in trailers.iter() {
      match trailer {
        Trailer::Call(call) => self.call(call),
        Trailer::Index(index) => self.index(index),
        Trailer::Access(access) => self.access(access),
      }
    }
  }

  /// Compile a channel declaration
  fn channel(&mut self, channel: &'a ast::Channel<'src>) {
    match &channel.expr {
      Some(expr) => {
        self.expr(expr);
        self.emit_byte(SymbolicByteCode::BufferedChannel, channel.end());
      },
      None => self.emit_byte(SymbolicByteCode::Channel, channel.end()),
    }
  }

  /// Compile a true token
  fn true_(&mut self, true_: &Token) {
    self.emit_byte(SymbolicByteCode::True, true_.end());
  }

  /// Compile a false token
  fn false_(&mut self, false_: &Token) {
    self.emit_byte(SymbolicByteCode::False, false_.end());
  }

  /// Compile a nil token
  fn nil(&mut self, nil: &Token) {
    self.emit_byte(SymbolicByteCode::Nil, nil.end());
  }

  /// Compile a number token
  fn number(&mut self, token: &Token) {
    let value = val!(token.str().parse::<f64>().expect("Unable to parse float"));
    self.emit_constant(value, token.end());
  }

  /// Compile a string token
  fn string(&mut self, token: &Token) {
    let value = val!(self.gc.borrow_mut().manage_str(token.str(), self));
    self.emit_constant(value, token.end());
  }

  /// Compile a string token
  fn interpolation(&mut self, interpolation: &'a ast::Interpolation<'src>) {
    const STR: &str = "str";
    let str_constant = self.string_constant(STR);

    let value = val!(self
      .gc
      .borrow_mut()
      .manage_str(interpolation.start.str(), self));
    self.emit_constant(value, interpolation.start.end());

    for segment in interpolation.segments.iter() {
      match segment {
        ast::StringSegments::Token(token) => {
          let value = val!(self.gc.borrow_mut().manage_str(token.str(), self));
          self.emit_constant(value, token.end());
        },
        ast::StringSegments::Expr(expr) => {
          self.expr(expr);
          self.emit_byte(SymbolicByteCode::Invoke((str_constant, 0)), expr.end());
          self.emit_byte(SymbolicByteCode::InvokeSlot, expr.end());
        },
      }
    }

    let value = val!(self
      .gc
      .borrow_mut()
      .manage_str(interpolation.end.str(), self));
    self.emit_constant(value, interpolation.end.end());
    self.emit_byte(
      SymbolicByteCode::Interpolate((interpolation.segments.len() + 2) as u16),
      interpolation.end(),
    );
  }

  /// Compile a identifer token
  fn identifier(&mut self, token: &Token<'src>) {
    self.variable_get(token);
  }

  /// Compile instance access
  fn instance_access(&mut self, instance_access: &ast::InstanceAccess<'src>) {
    if self.instance_access_self(instance_access) {
      let name = self.identifier_constant(instance_access.property());

      self.emit_byte(SymbolicByteCode::GetProperty(name), instance_access.end());
      self.emit_byte(SymbolicByteCode::PropertySlot, instance_access.end());
    }
  }

  /// Compile instance access self load
  fn instance_access_self(&mut self, instance_access: &ast::InstanceAccess<'src>) -> bool {
    self
      .class_attributes
      .map(|class_compiler| class_compiler.fun_kind)
      .and_then(|fun_kind| {
        fun_kind.and_then(|fun_kind| match fun_kind {
          FunKind::Method | FunKind::Initializer => {
            self.variable_get(&Token::new(
              TokenKind::Self_,
              Lexeme::Slice(SELF),
              instance_access.start(),
              instance_access.start() + 1,
            ));
            Some(())
          },
          _ => None,
        })
      })
      .or_else(|| {
        self.error(
          "Cannot access property off 'self' with '@' outside of class instance methods.",
          Some(&instance_access.access),
        );
        None
      })
      .is_some()
  }

  /// Compile the self token
  fn self_(&mut self, self_: &Token<'src>) {
    self
      .class_attributes
      .map(|class_compiler| class_compiler.fun_kind)
      .and_then(|fun_kind| {
        fun_kind.and_then(|fun_kind| match fun_kind {
          FunKind::Method | FunKind::Initializer => {
            self.variable_get(self_);
            Some(())
          },
          _ => None,
        })
      })
      .or_else(|| {
        self.error(
          "Cannot use 'self' outside of class instance methods.",
          Some(self_),
        );
        None
      });
  }

  /// Compile the super token
  fn super_(&mut self, super_: &ast::Super<'src>) {
    if self.class_attributes.is_none() {
      self.error(
        "Cannot use 'super' outside of a class.",
        Some(&super_.super_),
      );
    }

    let name = self.identifier_constant(super_.access.str());

    // load self on top of stack
    self.variable_get(&Token::new(
      TokenKind::Self_,
      Lexeme::Slice(SELF),
      super_.end(),
      super_.end(),
    ));

    self.variable_get(&super_.super_);
    self.emit_byte(SymbolicByteCode::GetSuper(name), super_.end());
  }

  /// Compile a lambda expression
  fn lambda(&mut self, fun: &'a ast::Fun<'src>) {
    self.function(fun, FunKind::Fun);
  }

  /// Compile a list literal
  fn list(&mut self, list: &'a ast::Collection<'src>) {
    for item in list.items.iter() {
      self.expr(item);
    }

    self.emit_byte(SymbolicByteCode::List(list.items.len() as u16), list.end());
  }

  /// Compile a list literal
  fn tuple(&mut self, list: &'a ast::Collection<'src>) {
    for item in list.items.iter() {
      self.expr(item);
    }

    self.emit_byte(SymbolicByteCode::Tuple(list.items.len() as u16), list.end());
  }

  /// Compile a map literal
  fn map(&mut self, map: &'a ast::Map<'src>) {
    for (key, value) in map.entries.iter() {
      self.expr(key);
      self.expr(value);
    }

    self.emit_byte(SymbolicByteCode::Map(map.entries.len() as u16), map.end());
  }

  /// Set the functions arity from the call signature
  fn call_sig(&mut self, call_sig: &'a ast::CallSignature<'src>) {
    for param in &call_sig.params {
      let param_state = self.declare_local_variable(param.name.str());
      self.define_local_variable(param.name.str(), param_state, param.name.end());
    }
  }

  /// Do nothing if we encounter an error token
  fn visit_error(&mut self, _: &[Token]) {}
}

impl<'a, 'src: 'a> GcContext for Compiler<'a, 'src> {
  fn gc(&self) -> RefMut<'_, Allocator> {
    self.gc.borrow_mut()
  }
}

impl<'a, 'src: 'a> TraceRoot for Compiler<'a, 'src> {
  fn trace(&self) {
    match self.enclosing {
      Some(enclosing) => unsafe { enclosing.as_ref().trace() },
      None => self.root_trace.trace(),
    };

    self.fun.trace();
    self.module.trace();

    if let Some(class_info) = self.class_attributes {
      class_info.trace();
    }

    self.constants.keys().for_each(|key| {
      key.trace();
    });
  }

  fn trace_debug(&self, log: &mut dyn Write) {
    match self.enclosing {
      Some(enclosing) => unsafe { enclosing.as_ref().trace_debug(log) },
      None => self.root_trace.trace_debug(log),
    };

    self.fun.trace_debug(log);
    self.module.trace_debug(log);

    if let Some(class_info) = self.class_attributes {
      class_info.trace_debug(log)
    }

    self.constants.keys().for_each(|key| {
      key.trace_debug(log);
    });
  }

  fn can_collect(&self) -> bool {
    true
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::{
    byte_code::{decode_u16, decode_u32, AlignedByteCode},
    compiler::Parser,
    debug::disassemble_chunk,
    source::{Source, VM_FILE_TEST_ID},
  };
  use laythe_core::{
    hooks::{GcHooks, NoContext},
    managed::GcObj,
    memory::{NoGc, NO_GC},
    object::{Class, ObjectKind},
  };
  use laythe_env::stdio::{support::StdioTestContainer, Stdio};
  use module::Module;

  #[cfg(feature = "debug")]
  use laythe_native::io::io_native;

  enum ByteCodeTest {
    Code(AlignedByteCode),
    Fun((u16, usize, Vec<ByteCodeTest>)),
  }

  pub fn test_class(hooks: &GcHooks, name: &str) -> GcObj<Class> {
    let mut object_class = hooks.manage_obj(Class::bare(hooks.manage_str("Object")));
    let mut class_class = hooks.manage_obj(Class::bare(hooks.manage_str("Class")));
    class_class.inherit(hooks, object_class);

    let class_copy = class_class;
    class_class.set_meta(class_copy);

    // create object's meta class
    let mut object_meta_class = hooks.manage_obj(Class::bare(
      hooks.manage_str(format!("{} metaClass", &*object_class.name())),
    ));

    object_meta_class.inherit(hooks, class_class);
    object_meta_class.set_meta(class_class);

    object_class.set_meta(object_meta_class);

    Class::with_inheritance(hooks, hooks.manage_str(name), object_class)
  }

  pub fn test_fun(hooks: &GcHooks, module: Gc<Module>) -> GcObj<Fun> {
    let fun = Fun::stub(hooks, hooks.manage_str("print"), module);
    hooks.manage_obj(fun)
  }

  fn dummy_module(hooks: &GcHooks) -> Gc<Module> {
    let module_class = test_class(&hooks, "Module");
    let object_class = module_class.super_class().expect("Expected Object");
    hooks.push_root(module_class);
    let module_path = hooks.manage_str("module/path");
    hooks.push_root(module_path);
    let mut module = hooks.manage(Module::new(module_class, module_path, 0));
    hooks.pop_roots(2);
    hooks.push_root(module);

    let print = test_fun(hooks, module);
    assert!(module.insert_symbol(print.name(), val!(print)).is_ok());

    let error_class = test_class(&hooks, "Error");
    assert!(module
      .insert_symbol(error_class.name(), val!(error_class))
      .is_ok());

    assert!(module
      .insert_symbol(object_class.name(), val!(object_class))
      .is_ok());
    assert!(module.export_symbol(print.name()).is_ok());
    assert!(module.export_symbol(error_class.name()).is_ok());
    assert!(module.export_symbol(object_class.name()).is_ok());

    hooks.pop_roots(1);

    module
  }

  fn test_compile(src: &str, context: &NoContext) -> Fun {
    let hooks = &GcHooks::new(context);
    let repl = false;

    let src = hooks.manage_str(src);
    hooks.push_root(src);
    let source = Source::new(hooks.manage_str(src));
    let (ast, line_offsets) = Parser::new(&source, VM_FILE_TEST_ID).parse();
    assert!(ast.is_ok(), "{}", src);
    let mut ast = ast.unwrap();

    let module = dummy_module(hooks);

    let gc = context.replace_gc(Allocator::default());

    assert!(Resolver::new(module, &gc, &source, VM_FILE_TEST_ID, repl)
      .resolve(&mut ast)
      .is_ok());

    let fake_vm_root: &NoGc = &NO_GC;
    let alloc = Bump::new();
    let compiler = Compiler::new(
      module,
      &alloc,
      &line_offsets,
      VM_FILE_TEST_ID,
      repl,
      fake_vm_root,
      gc,
    );
    #[cfg(feature = "debug")]
    let compiler = compiler.with_io(io_native());

    let (result, gc, _) = compiler.compile(&ast);
    context.replace_gc(gc);

    assert!(result.is_ok());
    result.unwrap()
  }

  fn decode_byte_code(fun: &Fun) -> Vec<AlignedByteCode> {
    let bytes = &fun.chunk().instructions();
    let mut decoded = Vec::new();
    let mut offset = 0;

    while offset < bytes.len() {
      let (byte_code, new_offset) = AlignedByteCode::decode(&bytes, offset);

      match byte_code {
        AlignedByteCode::Closure(closure) => {
          decoded.push(byte_code);
          offset = decode_byte_code_closure(fun, &mut decoded, new_offset, closure)
        },
        AlignedByteCode::GetProperty(_)
        | AlignedByteCode::SetProperty(_)
        | AlignedByteCode::Invoke(_)
        | AlignedByteCode::SuperInvoke(_) => {
          decoded.push(byte_code);
          offset = decode_byte_code_slot(fun, &mut decoded, new_offset)
        },
        _ => {
          decoded.push(byte_code);
          offset = new_offset;
        },
      }
    }

    decoded
  }

  fn decode_byte_code_closure(
    fun: &Fun,
    decoded: &mut Vec<AlignedByteCode>,
    offset: usize,
    slot: u16,
  ) -> usize {
    let inner_fun = fun.chunk().get_constant(slot as usize).to_obj().to_fun();
    let mut current_offset = offset;

    let byte_slice = &fun.chunk().instructions();
    for _ in 0..inner_fun.capture_count() {
      let scalar = decode_u16(&byte_slice[offset..offset + 2]);

      let capture_index: CaptureIndex = unsafe { mem::transmute(scalar) };
      decoded.push(AlignedByteCode::CaptureIndex(capture_index));
      current_offset = current_offset + 2;
    }

    current_offset
  }

  fn decode_byte_code_slot(fun: &Fun, decoded: &mut Vec<AlignedByteCode>, offset: usize) -> usize {
    let byte_slice = fun.chunk().instructions();
    decoded.push(AlignedByteCode::Slot(decode_u32(
      &byte_slice[offset..offset + 4],
    )));

    offset + 4
  }

  fn assert_simple_bytecode(fun: &Fun, max_slots: usize, code: &[AlignedByteCode]) {
    let stdio_container = StdioTestContainer::default();
    let mut stdio = Stdio::new(Box::new(stdio_container.make_stdio()));

    if let Err(_) = disassemble_chunk(&mut stdio, &fun.chunk(), "test") {
      stdio_container.log_stdio();
      assert!(false)
    }
    stdio_container.log_stdio();
    let decoded_byte_code = decode_byte_code(fun);

    decoded_byte_code
      .iter()
      .zip(code.iter())
      .enumerate()
      .for_each(|(index, (actual, expected))| {
        assert_eq!(
          actual, expected,
          "compiled {:?} but expected {:?} at aligned instruction {}",
          actual, expected, index
        )
      });

    assert_eq!(decoded_byte_code.len(), code.len());
    assert_eq!(fun.max_slots(), max_slots);
  }

  fn assert_fun_bytecode(fun: &Fun, max_slots: usize, code: &[ByteCodeTest]) {
    let stdio_container = StdioTestContainer::default();
    let mut stdio = Stdio::new(Box::new(stdio_container.make_stdio()));

    assert!(disassemble_chunk(&mut stdio, &fun.chunk(), &*fun.name()).is_ok());
    stdio_container.log_stdio();

    let decoded_byte_code = decode_byte_code(fun);

    for i in 0..code.len() {
      match decoded_byte_code[i] {
        AlignedByteCode::Closure(index) => {
          let fun = fun.chunk().get_constant(index as usize).to_obj().to_fun();

          match &code[i] {
            ByteCodeTest::Fun((expected_index, max_slots, inner)) => {
              assert_eq!(*expected_index, index, "Function constant index not in expected spot. Found at index {} expected at index {}", expected_index, index);
              assert_fun_bytecode(&*fun, *max_slots, &inner);
            },
            _ => assert!(false),
          }
        },
        AlignedByteCode::Constant(index) => {
          let constant = fun.chunk().get_constant(index as usize);
          if constant.is_obj_kind(ObjectKind::Fun) {
            let fun = constant.to_obj().to_fun();

            match &code[i] {
              ByteCodeTest::Fun((expected, max_slots, inner)) => {
                assert_eq!(*expected, index as u16);
                assert_fun_bytecode(&*fun, *max_slots, &inner);
              },
              _ => assert!(false),
            }
          } else {
            match &code[i] {
              ByteCodeTest::Code(byte_code) => {
                assert_eq!(&decoded_byte_code[i], byte_code);
              },
              _ => assert!(false),
            }
          }
        },
        AlignedByteCode::ConstantLong(index) => {
          let constant = fun.chunk().get_constant(index as usize);

          if constant.is_obj_kind(ObjectKind::Fun) {
            let fun = constant.to_obj().to_fun();

            match &code[i] {
              ByteCodeTest::Fun((expected, max_slots, inner)) => {
                assert_eq!(*expected, index);
                assert_fun_bytecode(&*fun, *max_slots, &inner);
              },
              _ => assert!(false),
            }
          } else {
            match &code[i] {
              ByteCodeTest::Code(byte_code) => {
                assert_eq!(&decoded_byte_code[i], byte_code);
              },
              _ => assert!(false),
            }
          }
        },
        _ => match &code[i] {
          ByteCodeTest::Code(byte_code) => {
            assert_eq!(&decoded_byte_code[i], byte_code);
          },
          _ => assert!(false),
        },
      }
    }

    assert_eq!(
      decoded_byte_code.len(),
      code.len(),
      "for fn {} expected instruction length to be {} but recieved {}",
      fun.name(),
      code.len(),
      decoded_byte_code.len()
    );
    assert_eq!(
      fun.max_slots(),
      max_slots,
      "for fn {} expected max slots to be {} but recieved {}.",
      fun.name(),
      max_slots,
      fun.max_slots()
    );
  }

  #[test]
  fn import() {
    let example = r#"
      import std.time;
    "#;

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::Import(0),
        AlignedByteCode::DefineGlobal(1),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn export_variable() {
    let example = "
      export let x = 10;
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::Constant(1),
        AlignedByteCode::DefineGlobal(0),
        AlignedByteCode::Export(0),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn export_fun() {
    let example = "
      export fn example() {}
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_fun_bytecode(
      &fun,
      2,
      &vec![
        ByteCodeTest::Fun((
          1,
          2,
          vec![
            ByteCodeTest::Code(AlignedByteCode::Nil),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::DefineGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::Export(0)),
        ByteCodeTest::Code(AlignedByteCode::Nil),
        ByteCodeTest::Code(AlignedByteCode::Return),
      ],
    );
  }

  #[test]
  fn export_class() {
    let example = "
      export class example {}
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      3,
      &vec![
        AlignedByteCode::Class(0),
        AlignedByteCode::DefineGlobal(0),
        AlignedByteCode::GetGlobal(1),
        AlignedByteCode::GetGlobal(0),
        AlignedByteCode::Inherit,
        AlignedByteCode::DropN(2),
        AlignedByteCode::Export(0),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn empty_try_catch() {
    let example = "
      try {

      } catch e {

      }
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::PushHandler((1, 4)),
        AlignedByteCode::PopHandler,
        AlignedByteCode::Jump(14),
        AlignedByteCode::GetGlobal(0),
        AlignedByteCode::CheckHandler(7),
        AlignedByteCode::FinishUnwind,
        AlignedByteCode::PopHandler,
        AlignedByteCode::GetError,
        AlignedByteCode::Drop,
        AlignedByteCode::Jump(1),
        AlignedByteCode::ContinueUnwind,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn filled_try_catch() {
    let example = r#"
      try {
        let empty = {};
        empty["missing"];
      } catch e: Error {
        print("no!");
      }
    "#;

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      4,
      &vec![
        AlignedByteCode::PushHandler((1, 21)),
        AlignedByteCode::Map(0),
        AlignedByteCode::GetLocal(1),
        AlignedByteCode::Constant(1),
        AlignedByteCode::Invoke((2, 1)),
        AlignedByteCode::Slot(0),
        AlignedByteCode::DropN(2),
        AlignedByteCode::PopHandler,
        AlignedByteCode::Jump(22),
        AlignedByteCode::GetGlobal(3),
        AlignedByteCode::CheckHandler(15),
        AlignedByteCode::FinishUnwind,
        AlignedByteCode::PopHandler,
        AlignedByteCode::GetError,
        AlignedByteCode::GetGlobal(5),
        AlignedByteCode::Constant(6),
        AlignedByteCode::Call(1),
        AlignedByteCode::DropN(2),
        AlignedByteCode::Jump(1),
        AlignedByteCode::ContinueUnwind,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn nested_try_catch() {
    let example = r#"
      try {
        [][3];
        try {
          [][1];
        } catch e: Error {
          print("woops!");
        }
      } catch e {
        print("no!");
      }
    "#;

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      4,
      &vec![
        AlignedByteCode::PushHandler((1, 63)),
        AlignedByteCode::List(0),
        AlignedByteCode::Constant(0),
        AlignedByteCode::Invoke((1, 1)),
        AlignedByteCode::Slot(0),
        AlignedByteCode::Drop,
        AlignedByteCode::PushHandler((1, 18)),
        AlignedByteCode::List(0),
        AlignedByteCode::Constant(2),
        AlignedByteCode::Invoke((1, 1)),
        AlignedByteCode::Slot(1),
        AlignedByteCode::Drop,
        AlignedByteCode::PopHandler,
        AlignedByteCode::Jump(22),
        AlignedByteCode::GetGlobal(3),
        AlignedByteCode::CheckHandler(15),
        AlignedByteCode::FinishUnwind,
        AlignedByteCode::PopHandler,
        AlignedByteCode::GetError,
        AlignedByteCode::GetGlobal(5),
        AlignedByteCode::Constant(6),
        AlignedByteCode::Call(1),
        AlignedByteCode::DropN(2),
        AlignedByteCode::Jump(1),
        AlignedByteCode::ContinueUnwind,
        AlignedByteCode::PopHandler,
        AlignedByteCode::Jump(22),
        AlignedByteCode::GetGlobal(3),
        AlignedByteCode::CheckHandler(15),
        AlignedByteCode::FinishUnwind,
        AlignedByteCode::PopHandler,
        AlignedByteCode::GetError,
        AlignedByteCode::GetGlobal(5),
        AlignedByteCode::Constant(7),
        AlignedByteCode::Call(1),
        AlignedByteCode::DropN(2),
        AlignedByteCode::Jump(1),
        AlignedByteCode::ContinueUnwind,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn try_with_break() {
    let example = r#"
      while true {
        try {
          break;
        } catch e {
          break;
        }
      }
    "#;

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::True,
        AlignedByteCode::JumpIfFalse(26),
        AlignedByteCode::PushHandler((1, 4)),
        AlignedByteCode::PopHandler,
        AlignedByteCode::Jump(17),
        AlignedByteCode::GetGlobal(0),
        AlignedByteCode::CheckHandler(7),
        AlignedByteCode::FinishUnwind,
        AlignedByteCode::PopHandler,
        AlignedByteCode::GetError,
        AlignedByteCode::Drop,
        AlignedByteCode::Jump(4),
        AlignedByteCode::ContinueUnwind,
        AlignedByteCode::Loop(30),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn try_with_continue() {
    let example = r#"
      while true {
        try {
          continue;
        } catch e: Error {
          continue;
        }
      }
    "#;

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::True,
        AlignedByteCode::JumpIfFalse(26),
        AlignedByteCode::PushHandler((1, 4)),
        AlignedByteCode::PopHandler,
        AlignedByteCode::Loop(13),
        AlignedByteCode::GetGlobal(0),
        AlignedByteCode::CheckHandler(7),
        AlignedByteCode::FinishUnwind,
        AlignedByteCode::PopHandler,
        AlignedByteCode::GetError,
        AlignedByteCode::Drop,
        AlignedByteCode::Loop(26),
        AlignedByteCode::ContinueUnwind,
        AlignedByteCode::Loop(30),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn try_with_return() {
    let example = r#"
      fn example() {
        try {
          return;
        } catch _ {
          return;
        }
      }
    "#;

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_fun_bytecode(
      &fun,
      2,
      &vec![
        ByteCodeTest::Fun((
          // example
          1,
          3,
          vec![
            ByteCodeTest::Code(AlignedByteCode::PushHandler((1, 3))),
            ByteCodeTest::Code(AlignedByteCode::Nil),
            ByteCodeTest::Code(AlignedByteCode::PopHandler),
            ByteCodeTest::Code(AlignedByteCode::Return),
            ByteCodeTest::Code(AlignedByteCode::GetGlobal(0)),
            ByteCodeTest::Code(AlignedByteCode::CheckHandler(5)),
            ByteCodeTest::Code(AlignedByteCode::FinishUnwind),
            ByteCodeTest::Code(AlignedByteCode::PopHandler),
            ByteCodeTest::Code(AlignedByteCode::GetError),
            ByteCodeTest::Code(AlignedByteCode::Nil),
            ByteCodeTest::Code(AlignedByteCode::Return),
            ByteCodeTest::Code(AlignedByteCode::ContinueUnwind),
            ByteCodeTest::Code(AlignedByteCode::Nil),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::DefineGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::Nil),
        ByteCodeTest::Code(AlignedByteCode::Return),
      ],
    );
  }

  #[test]
  fn class_with_inherit() {
    let example = "
      class A {}

      class B : A {}
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      3,
      &vec![
        AlignedByteCode::Class(0),
        AlignedByteCode::DefineGlobal(0),
        AlignedByteCode::GetGlobal(1),
        AlignedByteCode::GetGlobal(0),
        AlignedByteCode::Inherit,
        AlignedByteCode::DropN(2),
        AlignedByteCode::Class(2),
        AlignedByteCode::DefineGlobal(2),
        AlignedByteCode::GetGlobal(0),
        AlignedByteCode::GetGlobal(2),
        AlignedByteCode::Inherit,
        AlignedByteCode::DropN(2),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn class_empty() {
    let example = "
      class A {}
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      3,
      &vec![
        AlignedByteCode::Class(0),
        AlignedByteCode::DefineGlobal(0),
        AlignedByteCode::GetGlobal(1),
        AlignedByteCode::GetGlobal(0),
        AlignedByteCode::Inherit,
        AlignedByteCode::DropN(2),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn class_with_methods() {
    let example = "
      class A {
        init() {
          self.field = true;
        }

        getField() {
          return self.field;
        }

        getGetField() {
          return self.getField();
        }
      }
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_fun_bytecode(
      &fun,
      4,
      &vec![
        ByteCodeTest::Code(AlignedByteCode::Class(0)),
        ByteCodeTest::Code(AlignedByteCode::DefineGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::GetGlobal(1)),
        ByteCodeTest::Code(AlignedByteCode::GetGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::Inherit),
        ByteCodeTest::Fun((
          3,
          3,
          vec![
            ByteCodeTest::Code(AlignedByteCode::GetLocal(0)),
            ByteCodeTest::Code(AlignedByteCode::True),
            ByteCodeTest::Code(AlignedByteCode::SetProperty(0)),
            ByteCodeTest::Code(AlignedByteCode::Slot(0)),
            ByteCodeTest::Code(AlignedByteCode::Drop),
            ByteCodeTest::Code(AlignedByteCode::GetLocal(0)),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::Method(2)),
        ByteCodeTest::Code(AlignedByteCode::Field(4)),
        ByteCodeTest::Fun((
          6,
          2,
          vec![
            ByteCodeTest::Code(AlignedByteCode::GetLocal(0)),
            ByteCodeTest::Code(AlignedByteCode::GetProperty(0)),
            ByteCodeTest::Code(AlignedByteCode::Slot(1)),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::Method(5)),
        ByteCodeTest::Fun((
          8,
          2,
          vec![
            ByteCodeTest::Code(AlignedByteCode::GetLocal(0)),
            ByteCodeTest::Code(AlignedByteCode::Invoke((0, 0))),
            ByteCodeTest::Code(AlignedByteCode::Slot(0)),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::Method(7)),
        ByteCodeTest::Code(AlignedByteCode::DropN(2)),
        ByteCodeTest::Code(AlignedByteCode::Nil),
        ByteCodeTest::Code(AlignedByteCode::Return),
      ],
    );
  }

  #[test]
  fn class_with_methods_implicit() {
    let example = "
      class A {
        init() {
          self.field = true;
        }

        getField() {
          self.field
        }

        getGetField() {
          self.getField()
        }
      }
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_fun_bytecode(
      &fun,
      4,
      &vec![
        ByteCodeTest::Code(AlignedByteCode::Class(0)),
        ByteCodeTest::Code(AlignedByteCode::DefineGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::GetGlobal(1)),
        ByteCodeTest::Code(AlignedByteCode::GetGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::Inherit),
        ByteCodeTest::Fun((
          3,
          3,
          vec![
            ByteCodeTest::Code(AlignedByteCode::GetLocal(0)),
            ByteCodeTest::Code(AlignedByteCode::True),
            ByteCodeTest::Code(AlignedByteCode::SetProperty(0)),
            ByteCodeTest::Code(AlignedByteCode::Slot(0)),
            ByteCodeTest::Code(AlignedByteCode::Drop),
            ByteCodeTest::Code(AlignedByteCode::GetLocal(0)),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::Method(2)),
        ByteCodeTest::Code(AlignedByteCode::Field(4)),
        ByteCodeTest::Fun((
          6,
          2,
          vec![
            ByteCodeTest::Code(AlignedByteCode::GetLocal(0)),
            ByteCodeTest::Code(AlignedByteCode::GetProperty(0)),
            ByteCodeTest::Code(AlignedByteCode::Slot(1)),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::Method(5)),
        ByteCodeTest::Fun((
          8,
          2,
          vec![
            ByteCodeTest::Code(AlignedByteCode::GetLocal(0)),
            ByteCodeTest::Code(AlignedByteCode::Invoke((0, 0))),
            ByteCodeTest::Code(AlignedByteCode::Slot(0)),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::Method(7)),
        ByteCodeTest::Code(AlignedByteCode::DropN(2)),
        ByteCodeTest::Code(AlignedByteCode::Nil),
        ByteCodeTest::Code(AlignedByteCode::Return),
      ],
    );
  }

  #[test]
  fn class_with_instance_access() {
    let example = "
      class A {
        init() {
          @field = true;
        }

        getField() {
          @field
        }

        getGetField() {
          self.getField()
        }
      }
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_fun_bytecode(
      &fun,
      4,
      &vec![
        ByteCodeTest::Code(AlignedByteCode::Class(0)),
        ByteCodeTest::Code(AlignedByteCode::DefineGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::GetGlobal(1)),
        ByteCodeTest::Code(AlignedByteCode::GetGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::Inherit),
        ByteCodeTest::Fun((
          3,
          3,
          vec![
            ByteCodeTest::Code(AlignedByteCode::GetLocal(0)),
            ByteCodeTest::Code(AlignedByteCode::True),
            ByteCodeTest::Code(AlignedByteCode::SetProperty(0)),
            ByteCodeTest::Code(AlignedByteCode::Slot(0)),
            ByteCodeTest::Code(AlignedByteCode::Drop),
            ByteCodeTest::Code(AlignedByteCode::GetLocal(0)),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::Method(2)),
        ByteCodeTest::Code(AlignedByteCode::Field(4)),
        ByteCodeTest::Fun((
          6,
          2,
          vec![
            ByteCodeTest::Code(AlignedByteCode::GetLocal(0)),
            ByteCodeTest::Code(AlignedByteCode::GetProperty(0)),
            ByteCodeTest::Code(AlignedByteCode::Slot(1)),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::Method(5)),
        ByteCodeTest::Fun((
          8,
          2,
          vec![
            ByteCodeTest::Code(AlignedByteCode::GetLocal(0)),
            ByteCodeTest::Code(AlignedByteCode::Invoke((0, 0))),
            ByteCodeTest::Code(AlignedByteCode::Slot(0)),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::Method(7)),
        ByteCodeTest::Code(AlignedByteCode::DropN(2)),
        ByteCodeTest::Code(AlignedByteCode::Nil),
        ByteCodeTest::Code(AlignedByteCode::Return),
      ],
    );
  }

  #[test]
  fn class_property_assign_set() {
    let example = "
    class A {
      init() {
        self.a = 10;
        self.a /= 5;
      }
    }
  ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_fun_bytecode(
      &fun,
      4,
      &vec![
        ByteCodeTest::Code(AlignedByteCode::Class(0)),
        ByteCodeTest::Code(AlignedByteCode::DefineGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::GetGlobal(1)),
        ByteCodeTest::Code(AlignedByteCode::GetGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::Inherit),
        ByteCodeTest::Fun((
          3,
          4,
          vec![
            ByteCodeTest::Code(AlignedByteCode::GetLocal(0)),
            ByteCodeTest::Code(AlignedByteCode::Constant(1)),
            ByteCodeTest::Code(AlignedByteCode::SetProperty(0)),
            ByteCodeTest::Code(AlignedByteCode::Slot(0)),
            ByteCodeTest::Code(AlignedByteCode::Drop),
            ByteCodeTest::Code(AlignedByteCode::GetLocal(0)),
            ByteCodeTest::Code(AlignedByteCode::Dup),
            ByteCodeTest::Code(AlignedByteCode::GetProperty(0)),
            ByteCodeTest::Code(AlignedByteCode::Slot(1)),
            ByteCodeTest::Code(AlignedByteCode::Constant(2)),
            ByteCodeTest::Code(AlignedByteCode::Divide),
            ByteCodeTest::Code(AlignedByteCode::SetProperty(0)),
            ByteCodeTest::Code(AlignedByteCode::Slot(2)),
            ByteCodeTest::Code(AlignedByteCode::Drop),
            ByteCodeTest::Code(AlignedByteCode::GetLocal(0)),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::Method(2)),
        ByteCodeTest::Code(AlignedByteCode::Field(4)),
        ByteCodeTest::Code(AlignedByteCode::DropN(2)),
        ByteCodeTest::Code(AlignedByteCode::Nil),
        ByteCodeTest::Code(AlignedByteCode::Return),
      ],
    );
  }

  #[test]
  fn class_instance_access_assign_set() {
    let example = "
    class A {
      init() {
        @a = 10;
        @a /= 5;
      }
    }
  ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_fun_bytecode(
      &fun,
      4,
      &vec![
        ByteCodeTest::Code(AlignedByteCode::Class(0)),
        ByteCodeTest::Code(AlignedByteCode::DefineGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::GetGlobal(1)),
        ByteCodeTest::Code(AlignedByteCode::GetGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::Inherit),
        ByteCodeTest::Fun((
          3,
          4,
          vec![
            ByteCodeTest::Code(AlignedByteCode::GetLocal(0)),
            ByteCodeTest::Code(AlignedByteCode::Constant(1)),
            ByteCodeTest::Code(AlignedByteCode::SetProperty(0)),
            ByteCodeTest::Code(AlignedByteCode::Slot(0)),
            ByteCodeTest::Code(AlignedByteCode::Drop),
            ByteCodeTest::Code(AlignedByteCode::GetLocal(0)),
            ByteCodeTest::Code(AlignedByteCode::Dup),
            ByteCodeTest::Code(AlignedByteCode::GetProperty(0)),
            ByteCodeTest::Code(AlignedByteCode::Slot(1)),
            ByteCodeTest::Code(AlignedByteCode::Constant(2)),
            ByteCodeTest::Code(AlignedByteCode::Divide),
            ByteCodeTest::Code(AlignedByteCode::SetProperty(0)),
            ByteCodeTest::Code(AlignedByteCode::Slot(2)),
            ByteCodeTest::Code(AlignedByteCode::Drop),
            ByteCodeTest::Code(AlignedByteCode::GetLocal(0)),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::Method(2)),
        ByteCodeTest::Code(AlignedByteCode::Field(4)),
        ByteCodeTest::Code(AlignedByteCode::DropN(2)),
        ByteCodeTest::Code(AlignedByteCode::Nil),
        ByteCodeTest::Code(AlignedByteCode::Return),
      ],
    );
  }

  #[test]
  fn class_with_static_methods() {
    let example = "
      class A {
        static sayHi() {
          return 'hi';
        }

        static sayBye() {
          return 'bye';
        }
      }
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_fun_bytecode(
      &fun,
      4,
      &vec![
        ByteCodeTest::Code(AlignedByteCode::Class(0)),
        ByteCodeTest::Code(AlignedByteCode::DefineGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::GetGlobal(1)),
        ByteCodeTest::Code(AlignedByteCode::GetGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::Inherit),
        ByteCodeTest::Fun((
          3,
          2,
          vec![
            ByteCodeTest::Code(AlignedByteCode::Constant(0)),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::StaticMethod(2)),
        ByteCodeTest::Fun((
          5,
          2,
          vec![
            ByteCodeTest::Code(AlignedByteCode::Constant(0)),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::StaticMethod(4)),
        ByteCodeTest::Code(AlignedByteCode::DropN(2)),
        ByteCodeTest::Code(AlignedByteCode::Nil),
        ByteCodeTest::Code(AlignedByteCode::Return),
      ],
    );
  }

  #[test]
  fn class_with_captures() {
    let example = "
      class A {
        foo() {
          fn bar(a) {
            return [self, a];
          }

          return bar;
        }
      }
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_fun_bytecode(
      &fun,
      4,
      &vec![
        ByteCodeTest::Code(AlignedByteCode::Class(0)),
        ByteCodeTest::Code(AlignedByteCode::DefineGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::GetGlobal(1)),
        ByteCodeTest::Code(AlignedByteCode::GetGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::Inherit),
        ByteCodeTest::Fun((
          3,
          3,
          vec![
            ByteCodeTest::Code(AlignedByteCode::Box(0)),
            ByteCodeTest::Fun((
              0,
              3,
              vec![
                ByteCodeTest::Code(AlignedByteCode::GetCapture(0)),
                ByteCodeTest::Code(AlignedByteCode::GetLocal(1)),
                ByteCodeTest::Code(AlignedByteCode::List(2)),
                ByteCodeTest::Code(AlignedByteCode::Return),
              ],
            )),
            ByteCodeTest::Code(AlignedByteCode::CaptureIndex(CaptureIndex::Local(0))),
            ByteCodeTest::Code(AlignedByteCode::GetLocal(1)),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::Method(2)),
        ByteCodeTest::Code(AlignedByteCode::DropN(2)),
        ByteCodeTest::Code(AlignedByteCode::Nil),
        ByteCodeTest::Code(AlignedByteCode::Return),
      ],
    );
  }

  #[test]
  fn class_instance_access_with_captures() {
    let example = "
      class A {
        init() {
          @a = true;
        }

        foo() {
          fn bar() { @a }

          return bar;
        }
      }
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_fun_bytecode(
      &fun,
      4,
      &vec![
        ByteCodeTest::Code(AlignedByteCode::Class(0)),
        ByteCodeTest::Code(AlignedByteCode::DefineGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::GetGlobal(1)),
        ByteCodeTest::Code(AlignedByteCode::GetGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::Inherit),
        ByteCodeTest::Fun((
          3,
          3,
          vec![
            ByteCodeTest::Code(AlignedByteCode::GetLocal(0)),
            ByteCodeTest::Code(AlignedByteCode::True),
            ByteCodeTest::Code(AlignedByteCode::SetProperty(0)),
            ByteCodeTest::Code(AlignedByteCode::Slot(0)),
            ByteCodeTest::Code(AlignedByteCode::Drop),
            ByteCodeTest::Code(AlignedByteCode::GetLocal(0)),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::Method(2)),
        ByteCodeTest::Code(AlignedByteCode::Field(4)),
        ByteCodeTest::Fun((
          6,
          3,
          vec![
            ByteCodeTest::Code(AlignedByteCode::Box(0)),
            ByteCodeTest::Fun((
              0,
              2,
              vec![
                ByteCodeTest::Code(AlignedByteCode::GetCapture(0)),
                ByteCodeTest::Code(AlignedByteCode::GetProperty(0)),
                ByteCodeTest::Code(AlignedByteCode::Slot(1)),
                ByteCodeTest::Code(AlignedByteCode::Return),
              ],
            )),
            ByteCodeTest::Code(AlignedByteCode::CaptureIndex(CaptureIndex::Local(0))),
            ByteCodeTest::Code(AlignedByteCode::GetLocal(1)),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::Method(5)),
        ByteCodeTest::Code(AlignedByteCode::DropN(2)),
        ByteCodeTest::Code(AlignedByteCode::Nil),
        ByteCodeTest::Code(AlignedByteCode::Return),
      ],
    );
  }

  #[test]
  fn launch() {
    let example = "
      launch print();
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::GetGlobal(0),
        AlignedByteCode::Launch(0),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn raise() {
    let example = "
      while true { raise Error('bummer'); print(10); }
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      3,
      &vec![
        AlignedByteCode::True,
        AlignedByteCode::JumpIfFalse(8),
        AlignedByteCode::GetGlobal(0),
        AlignedByteCode::Constant(1),
        AlignedByteCode::Call(1),
        AlignedByteCode::Raise,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn channel_send_index() {
    let example = "
      let a = [chan()];
      a[0] <- 5;
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      4,
      &vec![
        AlignedByteCode::Channel,
        AlignedByteCode::List(1),
        AlignedByteCode::DefineGlobal(0),
        AlignedByteCode::Constant(1),
        AlignedByteCode::GetGlobal(0),
        AlignedByteCode::Constant(2),
        AlignedByteCode::Invoke((3, 1)),
        AlignedByteCode::Slot(0),
        AlignedByteCode::Send,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn channel_send_property() {
    let example = "
      let b = 10;
      b.b <- 5;
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      3,
      &vec![
        AlignedByteCode::Constant(1),
        AlignedByteCode::DefineGlobal(0),
        AlignedByteCode::Constant(2),
        AlignedByteCode::GetGlobal(0),
        AlignedByteCode::GetProperty(0),
        AlignedByteCode::Slot(0),
        AlignedByteCode::Send,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn channel_send_variable() {
    let example = "
      let b = chan();
      b <- 5;
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      3,
      &vec![
        AlignedByteCode::Channel,
        AlignedByteCode::DefineGlobal(0),
        AlignedByteCode::Constant(1),
        AlignedByteCode::GetGlobal(0),
        AlignedByteCode::Send,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn list_index_set() {
    let example = "
      let a = [print, print, print];
      a[1] = 5;
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      4,
      &vec![
        AlignedByteCode::GetGlobal(1),
        AlignedByteCode::GetGlobal(1),
        AlignedByteCode::GetGlobal(1),
        AlignedByteCode::List(3),
        AlignedByteCode::DefineGlobal(0),
        AlignedByteCode::GetGlobal(0),
        AlignedByteCode::Constant(2),
        AlignedByteCode::Constant(3),
        AlignedByteCode::Invoke((4, 2)),
        AlignedByteCode::Slot(0),
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn list_index_get() {
    let example = "
      let a = [\"john\", \"joe\", \"jim\"];
      print(a[1]);
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      4,
      &vec![
        AlignedByteCode::Constant(1),
        AlignedByteCode::Constant(2),
        AlignedByteCode::Constant(3),
        AlignedByteCode::List(3),
        AlignedByteCode::DefineGlobal(0),
        AlignedByteCode::GetGlobal(4),
        AlignedByteCode::GetGlobal(0),
        AlignedByteCode::Constant(5),
        AlignedByteCode::Invoke((6, 1)),
        AlignedByteCode::Slot(0),
        AlignedByteCode::Call(1),
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn list_index_assign_set() {
    let example = "
    let a = [1, 2, 3];
    a[1] += 5;
  ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      4,
      &vec![
        AlignedByteCode::Constant(1),
        AlignedByteCode::Constant(2),
        AlignedByteCode::Constant(3),
        AlignedByteCode::List(3),
        AlignedByteCode::DefineGlobal(0),
        AlignedByteCode::GetGlobal(0),
        AlignedByteCode::Dup,
        AlignedByteCode::Constant(1),
        AlignedByteCode::Invoke((4, 1)),
        AlignedByteCode::Slot(0),
        AlignedByteCode::Constant(5),
        AlignedByteCode::Add,
        AlignedByteCode::Constant(1),
        AlignedByteCode::Invoke((6, 2)),
        AlignedByteCode::Slot(1),
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn list_initializer() {
    let example = "
      let a = [1, 2, nil, false, \"cat\"];
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      6,
      &vec![
        AlignedByteCode::Constant(1),
        AlignedByteCode::Constant(2),
        AlignedByteCode::Nil,
        AlignedByteCode::False,
        AlignedByteCode::Constant(3),
        AlignedByteCode::List(5),
        AlignedByteCode::DefineGlobal(0),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn list_empty() {
    let example = "
      let a = [];
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::List(0),
        AlignedByteCode::DefineGlobal(0),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn tuple_initializer() {
    let example = "
      let a = (1, 2, nil, false, \"cat\");
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      6,
      &vec![
        AlignedByteCode::Constant(1),
        AlignedByteCode::Constant(2),
        AlignedByteCode::Nil,
        AlignedByteCode::False,
        AlignedByteCode::Constant(3),
        AlignedByteCode::Tuple(5),
        AlignedByteCode::DefineGlobal(0),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn tuple_empty() {
    let example = "
      let a = ();
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::Tuple(0),
        AlignedByteCode::DefineGlobal(0),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn map_initializer() {
    let example = "
      let a = {
        \"key1\": 10,
        \"key2\": nil,
      };
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      5,
      &vec![
        AlignedByteCode::Constant(1),
        AlignedByteCode::Constant(2),
        AlignedByteCode::Constant(3),
        AlignedByteCode::Nil,
        AlignedByteCode::Map(2),
        AlignedByteCode::DefineGlobal(0),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn map_empty() {
    let example = "
      let a = {};
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::Map(0),
        AlignedByteCode::DefineGlobal(0),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn lambda_expression_body() {
    let example = "
    let example = || 10;
    example();
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_fun_bytecode(
      &fun,
      2,
      &vec![
        ByteCodeTest::Fun((
          // example
          1,
          2,
          vec![
            ByteCodeTest::Code(AlignedByteCode::Constant(0)),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::DefineGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::GetGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::Call(0)),
        ByteCodeTest::Code(AlignedByteCode::Drop),
        ByteCodeTest::Code(AlignedByteCode::Nil),
        ByteCodeTest::Code(AlignedByteCode::Return),
      ],
    );
  }

  #[test]
  fn lambda_block_body() {
    let example = "
    let example = || { return 10; };
    example();
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_fun_bytecode(
      &fun,
      2,
      &vec![
        ByteCodeTest::Fun((
          // example
          1,
          2,
          vec![
            ByteCodeTest::Code(AlignedByteCode::Constant(0)),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::DefineGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::GetGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::Call(0)),
        ByteCodeTest::Code(AlignedByteCode::Drop),
        ByteCodeTest::Code(AlignedByteCode::Nil),
        ByteCodeTest::Code(AlignedByteCode::Return),
      ],
    );
  }

  #[test]
  fn fn_with_variables() {
    let example = "
    fn example(a, b, c) {
      return a + b + c;
    }
    example(1, 2, 3);
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_fun_bytecode(
      &fun,
      5,
      &vec![
        ByteCodeTest::Fun((
          // example
          1,
          3,
          vec![
            ByteCodeTest::Code(AlignedByteCode::GetLocal(1)),
            ByteCodeTest::Code(AlignedByteCode::GetLocal(2)),
            ByteCodeTest::Code(AlignedByteCode::Add),
            ByteCodeTest::Code(AlignedByteCode::GetLocal(3)),
            ByteCodeTest::Code(AlignedByteCode::Add),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::DefineGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::GetGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::Constant(2)),
        ByteCodeTest::Code(AlignedByteCode::Constant(3)),
        ByteCodeTest::Code(AlignedByteCode::Constant(4)),
        ByteCodeTest::Code(AlignedByteCode::Call(3)),
        ByteCodeTest::Code(AlignedByteCode::Drop),
        ByteCodeTest::Code(AlignedByteCode::Nil),
        ByteCodeTest::Code(AlignedByteCode::Return),
      ],
    );
  }

  #[test]
  fn open_capture() {
    let example = "
    fn example() {
      let x = 0;
      fn middle() {
        fn inner() {
          return x;
        }

        return inner();
      }

      return middle();
    }
    example();
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_fun_bytecode(
      &fun,
      2,
      &vec![
        ByteCodeTest::Fun((
          // example
          1,
          4,
          vec![
            ByteCodeTest::Code(AlignedByteCode::EmptyBox),
            ByteCodeTest::Code(AlignedByteCode::Constant(1)),
            ByteCodeTest::Code(AlignedByteCode::FillBox),
            ByteCodeTest::Fun((
              // middle
              2,
              3,
              vec![
                ByteCodeTest::Fun((
                  // inner
                  0,
                  2,
                  vec![
                    ByteCodeTest::Code(AlignedByteCode::GetCapture(0)),
                    ByteCodeTest::Code(AlignedByteCode::Return),
                  ],
                )),
                ByteCodeTest::Code(AlignedByteCode::CaptureIndex(CaptureIndex::Enclosing(0))),
                ByteCodeTest::Code(AlignedByteCode::GetLocal(1)),
                ByteCodeTest::Code(AlignedByteCode::Call(0)),
                ByteCodeTest::Code(AlignedByteCode::Return),
              ],
            )),
            ByteCodeTest::Code(AlignedByteCode::CaptureIndex(CaptureIndex::Local(1))), //
            ByteCodeTest::Code(AlignedByteCode::GetLocal(2)),
            ByteCodeTest::Code(AlignedByteCode::Call(0)),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::DefineGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::GetGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::Call(0)),
        ByteCodeTest::Code(AlignedByteCode::Drop),
        ByteCodeTest::Code(AlignedByteCode::Nil),
        ByteCodeTest::Code(AlignedByteCode::Return),
      ],
    );
  }

  #[test]
  fn close_capture() {
    let example = "
    fn example() {
      let a = 10;
      fn inner() {
        return a;
      }
      return inner;
    }
    let inner = example();
    inner();
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_fun_bytecode(
      &fun,
      2,
      &vec![
        ByteCodeTest::Fun((
          1,
          4,
          vec![
            ByteCodeTest::Code(AlignedByteCode::EmptyBox),
            ByteCodeTest::Code(AlignedByteCode::Constant(1)),
            ByteCodeTest::Code(AlignedByteCode::FillBox),
            ByteCodeTest::Fun((
              2,
              2,
              vec![
                ByteCodeTest::Code(AlignedByteCode::GetCapture(0)),
                ByteCodeTest::Code(AlignedByteCode::Return),
              ],
            )),
            ByteCodeTest::Code(AlignedByteCode::CaptureIndex(CaptureIndex::Local(1))),
            ByteCodeTest::Code(AlignedByteCode::GetLocal(2)),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::DefineGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::GetGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::Call(0)),
        ByteCodeTest::Code(AlignedByteCode::DefineGlobal(2)),
        ByteCodeTest::Code(AlignedByteCode::GetGlobal(2)),
        ByteCodeTest::Code(AlignedByteCode::Call(0)),
        ByteCodeTest::Code(AlignedByteCode::Drop),
        ByteCodeTest::Code(AlignedByteCode::Nil),
        ByteCodeTest::Code(AlignedByteCode::Return),
      ],
    );
  }

  #[test]
  fn empty_fun() {
    let example = "fn example() {} example();";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_fun_bytecode(
      &fun,
      2,
      &vec![
        ByteCodeTest::Fun((
          1,
          2,
          vec![
            ByteCodeTest::Code(AlignedByteCode::Nil),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::DefineGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::GetGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::Call(0)),
        ByteCodeTest::Code(AlignedByteCode::Drop),
        ByteCodeTest::Code(AlignedByteCode::Nil),
        ByteCodeTest::Code(AlignedByteCode::Return),
      ],
    );
  }

  #[test]
  fn param_fun() {
    let example = "
    fn example(a) {
      return a;
    }
    let a = 1;
    example(a);
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_fun_bytecode(
      &fun,
      3,
      &vec![
        ByteCodeTest::Fun((
          1,
          2,
          vec![
            ByteCodeTest::Code(AlignedByteCode::GetLocal(1)),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::DefineGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::Constant(3)),
        ByteCodeTest::Code(AlignedByteCode::DefineGlobal(2)),
        ByteCodeTest::Code(AlignedByteCode::GetGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::GetGlobal(2)),
        ByteCodeTest::Code(AlignedByteCode::Call(1)),
        ByteCodeTest::Code(AlignedByteCode::Drop),
        ByteCodeTest::Code(AlignedByteCode::Nil),
        ByteCodeTest::Code(AlignedByteCode::Return),
      ],
    );
  }

  #[test]
  fn empty_fun_basic() {
    let example = "fn example() { let a = 10; return a; } example();";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_fun_bytecode(
      &fun,
      2,
      &vec![
        ByteCodeTest::Fun((
          1,
          3,
          vec![
            ByteCodeTest::Code(AlignedByteCode::Constant(1)),
            ByteCodeTest::Code(AlignedByteCode::GetLocal(1)),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::DefineGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::GetGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::Call(0)),
        ByteCodeTest::Code(AlignedByteCode::Drop),
        ByteCodeTest::Code(AlignedByteCode::Nil),
        ByteCodeTest::Code(AlignedByteCode::Return),
      ],
    );
  }

  #[test]
  fn implicit_return() {
    let example = "fn example() { 10 } example();";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_fun_bytecode(
      &fun,
      2,
      &vec![
        ByteCodeTest::Fun((
          1,
          2,
          vec![
            ByteCodeTest::Code(AlignedByteCode::Constant(0)),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::DefineGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::GetGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::Call(0)),
        ByteCodeTest::Code(AlignedByteCode::Drop),
        ByteCodeTest::Code(AlignedByteCode::Nil),
        ByteCodeTest::Code(AlignedByteCode::Return),
      ],
    );
  }

  #[test]
  fn map() {
    let example = "let a = { \"cat\": \"bat\", 10: nil };";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      5,
      &vec![
        AlignedByteCode::Constant(1),
        AlignedByteCode::Constant(2),
        AlignedByteCode::Constant(3),
        AlignedByteCode::Nil,
        AlignedByteCode::Map(2),
        AlignedByteCode::DefineGlobal(0),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn list() {
    let example = "let a = [1, 2, 3, \"cat\"];";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      5,
      &vec![
        AlignedByteCode::Constant(1),
        AlignedByteCode::Constant(2),
        AlignedByteCode::Constant(3),
        AlignedByteCode::Constant(4),
        AlignedByteCode::List(4),
        AlignedByteCode::DefineGlobal(0),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn channel() {
    let example = "
    let a = chan(5);
    let b = chan();
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::Constant(1),
        AlignedByteCode::BufferedChannel,
        AlignedByteCode::DefineGlobal(0),
        AlignedByteCode::Channel,
        AlignedByteCode::DefineGlobal(2),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn for_loop() {
    let example = "for x in [1, 2, 3] { print(x); }";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      5,
      &vec![
        AlignedByteCode::Constant(0),
        AlignedByteCode::Constant(1),
        AlignedByteCode::Constant(2),
        AlignedByteCode::List(3),
        AlignedByteCode::Invoke((3, 0)),
        AlignedByteCode::Slot(0),
        AlignedByteCode::Nil,
        AlignedByteCode::GetLocal(1),
        AlignedByteCode::IterNext(4),
        AlignedByteCode::JumpIfFalse(19),
        AlignedByteCode::GetLocal(1),
        AlignedByteCode::IterCurrent(5),
        AlignedByteCode::SetLocal(2),
        AlignedByteCode::Drop,
        AlignedByteCode::GetGlobal(6),
        AlignedByteCode::GetLocal(2),
        AlignedByteCode::Call(1),
        AlignedByteCode::Drop,
        AlignedByteCode::Loop(27),
        AlignedByteCode::DropN(2),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn for_loop_with_locals() {
    let example = "for x in [1, 2, 3] { let x = 10; let y = 10; }";
    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      5,
      &vec![
        AlignedByteCode::Constant(0),
        AlignedByteCode::Constant(1),
        AlignedByteCode::Constant(2),
        AlignedByteCode::List(3),
        AlignedByteCode::Invoke((3, 0)),
        AlignedByteCode::Slot(0),
        AlignedByteCode::Nil,
        AlignedByteCode::GetLocal(1),
        AlignedByteCode::IterNext(4),
        AlignedByteCode::JumpIfFalse(17),
        AlignedByteCode::GetLocal(1),
        AlignedByteCode::IterCurrent(5),
        AlignedByteCode::SetLocal(2),
        AlignedByteCode::Drop,
        AlignedByteCode::Constant(7),
        AlignedByteCode::Constant(7),
        AlignedByteCode::DropN(2),
        AlignedByteCode::Loop(25),
        AlignedByteCode::DropN(2),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn while_loop() {
    let example = "while true { print(10); }";
    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      3,
      &vec![
        AlignedByteCode::True,
        AlignedByteCode::JumpIfFalse(11),
        AlignedByteCode::GetGlobal(0),
        AlignedByteCode::Constant(1),
        AlignedByteCode::Call(1),
        AlignedByteCode::Drop,
        AlignedByteCode::Loop(15),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn while_loop_with_locals() {
    let example = "while true { let x = 10; let y = 10; }";
    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      3,
      &vec![
        AlignedByteCode::True,
        AlignedByteCode::JumpIfFalse(9),
        AlignedByteCode::Constant(1),
        AlignedByteCode::Constant(1),
        AlignedByteCode::DropN(2),
        AlignedByteCode::Loop(13),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn break_() {
    let example = "while true { break; print(10); }";
    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::True,
        AlignedByteCode::JumpIfFalse(3),
        AlignedByteCode::Jump(0),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn break_drop_locals() {
    let example = "while true { let x = 10; break; }";
    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::True,
        AlignedByteCode::JumpIfFalse(6),
        AlignedByteCode::Constant(1),
        AlignedByteCode::Drop,
        AlignedByteCode::Jump(0),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn break_fun_nested() {
    let example = "
      fn f() {
        while true { break; let x; }
      }
    ";
    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_fun_bytecode(
      &fun,
      2,
      &vec![
        ByteCodeTest::Fun((
          1,
          2,
          vec![
            ByteCodeTest::Code(AlignedByteCode::True),
            ByteCodeTest::Code(AlignedByteCode::JumpIfFalse(3)),
            ByteCodeTest::Code(AlignedByteCode::Jump(0)),
            ByteCodeTest::Code(AlignedByteCode::Nil),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::DefineGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::Nil),
        ByteCodeTest::Code(AlignedByteCode::Return),
      ],
    );
  }

  #[test]
  fn continue_() {
    let example = "while true { continue; print(10); }";
    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::True,
        AlignedByteCode::JumpIfFalse(3),
        AlignedByteCode::Loop(7),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn continue_drop_locals() {
    let example = "while true { let x = 10; continue; }";
    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::True,
        AlignedByteCode::JumpIfFalse(6),
        AlignedByteCode::Constant(1),
        AlignedByteCode::Drop,
        AlignedByteCode::Loop(10),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn continue_fun_nested() {
    let example = "
      fn f() {
        while true { continue; let x; }
      }
    ";
    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_fun_bytecode(
      &fun,
      2,
      &vec![
        ByteCodeTest::Fun((
          1,
          2,
          vec![
            ByteCodeTest::Code(AlignedByteCode::True),
            ByteCodeTest::Code(AlignedByteCode::JumpIfFalse(3)),
            ByteCodeTest::Code(AlignedByteCode::Loop(7)),
            ByteCodeTest::Code(AlignedByteCode::Nil),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::DefineGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::Nil),
        ByteCodeTest::Code(AlignedByteCode::Return),
      ],
    );
  }

  #[test]
  fn and_operator() {
    let example = "true && false;";
    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::True,
        AlignedByteCode::And(1),
        AlignedByteCode::False,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn or_operator() {
    let example = "false || true;";
    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::False,
        AlignedByteCode::Or(1),
        AlignedByteCode::True,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn if_condition() {
    let example = "if (3 < 10) { print(\"hi\"); }";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      3,
      &vec![
        AlignedByteCode::Constant(0),
        AlignedByteCode::Constant(1),
        AlignedByteCode::Less,
        AlignedByteCode::JumpIfFalse(8),
        AlignedByteCode::GetGlobal(2),
        AlignedByteCode::Constant(3),
        AlignedByteCode::Call(1),
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn if_else_condition() {
    let example = "if (3 < 10) { print(\"hi\"); } else { print(\"bye\"); }";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      3,
      &vec![
        AlignedByteCode::Constant(0),
        AlignedByteCode::Constant(1),
        AlignedByteCode::Less,
        AlignedByteCode::JumpIfFalse(11),
        AlignedByteCode::GetGlobal(2),
        AlignedByteCode::Constant(3),
        AlignedByteCode::Call(1),
        AlignedByteCode::Drop,
        AlignedByteCode::Jump(8),
        AlignedByteCode::GetGlobal(2),
        AlignedByteCode::Constant(4),
        AlignedByteCode::Call(1),
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn declare_local() {
    let example = "if true { let x = 10; }";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::True,
        AlignedByteCode::JumpIfFalse(3),
        AlignedByteCode::Constant(1),
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_get_local() {
    let example = "if true { let x = 10; print(x); }";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      &fun,
      4,
      &vec![
        AlignedByteCode::True,
        AlignedByteCode::JumpIfFalse(11),
        AlignedByteCode::Constant(1),
        AlignedByteCode::GetGlobal(2),
        AlignedByteCode::GetLocal(1),
        AlignedByteCode::Call(1),
        AlignedByteCode::DropN(2),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_set_local() {
    let example = "if true { let x = 10; x = 5; }";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      3,
      &vec![
        AlignedByteCode::True,
        AlignedByteCode::JumpIfFalse(8),
        AlignedByteCode::Constant(1),
        AlignedByteCode::Constant(2),
        AlignedByteCode::SetLocal(1),
        AlignedByteCode::DropN(2),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_define_global_nil() {
    let example = "let x;";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::Nil,
        AlignedByteCode::DefineGlobal(0),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_define_global_val() {
    let example = "let x = 10;";

    let context = NoContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::Constant(1),
        AlignedByteCode::DefineGlobal(0),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_get_global() {
    let example = "
      let x = 10;
      print(x);
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      &fun,
      3,
      &vec![
        AlignedByteCode::Constant(1),
        AlignedByteCode::DefineGlobal(0),
        AlignedByteCode::GetGlobal(2),
        AlignedByteCode::GetGlobal(0),
        AlignedByteCode::Call(1),
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_set_global() {
    let example = "
      let x;
      x = \"cat\";
    ";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::Nil,
        AlignedByteCode::DefineGlobal(0),
        AlignedByteCode::Constant(1),
        AlignedByteCode::SetGlobal(0),
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_pop() {
    let example = "false;";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::False,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_return() {
    let example = "";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      &fun,
      2,
      &vec![AlignedByteCode::Nil, AlignedByteCode::Return],
    );
  }

  #[test]
  fn op_number() {
    let example = "5.18;";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::Constant(0),
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_string() {
    let example = "\"example\";";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::Constant(0),
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_interpolate() {
    let example = "\"${10} ${\"stuff\"}\";";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      &fun,
      6,
      &vec![
        AlignedByteCode::Constant(1),
        AlignedByteCode::Constant(2),
        AlignedByteCode::Invoke((0, 0)),
        AlignedByteCode::Slot(0),
        AlignedByteCode::Constant(3),
        AlignedByteCode::Constant(4),
        AlignedByteCode::Invoke((0, 0)),
        AlignedByteCode::Slot(1),
        AlignedByteCode::Constant(1),
        AlignedByteCode::Interpolate(5),
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_false() {
    let example = "false;";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::False,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_true() {
    let example = "true;";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::True,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_channel() {
    let example = "chan();";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::Channel,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_buffered_channel() {
    let example = "chan(5);";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::Constant(0),
        AlignedByteCode::BufferedChannel,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_nil() {
    let example = "nil;";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::Nil,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_not() {
    let example = "!false;";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::False,
        AlignedByteCode::Not,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_negate() {
    let example = "-(15);";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::Constant(0),
        AlignedByteCode::Negate,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn ternary() {
    let example = "10 > 5 ? \"example\" : nil;";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      &fun,
      3,
      &vec![
        AlignedByteCode::Constant(0),
        AlignedByteCode::Constant(1),
        AlignedByteCode::Greater,
        AlignedByteCode::JumpIfFalse(5),
        AlignedByteCode::Constant(2),
        AlignedByteCode::Jump(1),
        AlignedByteCode::Nil,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_receive() {
    let example = "<- true;";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      &fun,
      2,
      &vec![
        AlignedByteCode::True,
        AlignedByteCode::Receive,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_add() {
    let example = "10 + 4;";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      &fun,
      3,
      &vec![
        AlignedByteCode::Constant(0),
        AlignedByteCode::Constant(1),
        AlignedByteCode::Add,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_subtract() {
    let example = "10 - 4;";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      &fun,
      3,
      &vec![
        AlignedByteCode::Constant(0),
        AlignedByteCode::Constant(1),
        AlignedByteCode::Subtract,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_divide() {
    let example = "10 / 4;";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      &fun,
      3,
      &vec![
        AlignedByteCode::Constant(0),
        AlignedByteCode::Constant(1),
        AlignedByteCode::Divide,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_multi() {
    let example = "10 * 4;";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      &fun,
      3,
      &vec![
        AlignedByteCode::Constant(0),
        AlignedByteCode::Constant(1),
        AlignedByteCode::Multiply,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_equal() {
    let example = "true == nil;";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      &fun,
      3,
      &vec![
        AlignedByteCode::True,
        AlignedByteCode::Nil,
        AlignedByteCode::Equal,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_not_equal() {
    let example = "true != nil;";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      &fun,
      3,
      &vec![
        AlignedByteCode::True,
        AlignedByteCode::Nil,
        AlignedByteCode::NotEqual,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_less() {
    let example = "3 < 5;";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      &fun,
      3,
      &vec![
        AlignedByteCode::Constant(0),
        AlignedByteCode::Constant(1),
        AlignedByteCode::Less,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_less_equal() {
    let example = "3 <= 5;";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      &fun,
      3,
      &vec![
        AlignedByteCode::Constant(0),
        AlignedByteCode::Constant(1),
        AlignedByteCode::LessEqual,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_greater() {
    let example = "3 > 5;";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      &fun,
      3,
      &vec![
        AlignedByteCode::Constant(0),
        AlignedByteCode::Constant(1),
        AlignedByteCode::Greater,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_greater_equal() {
    let example = "3 >= 5;";

    let context = NoContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      &fun,
      3,
      &vec![
        AlignedByteCode::Constant(0),
        AlignedByteCode::Constant(1),
        AlignedByteCode::GreaterEqual,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }
}
