// mod compiler;
mod ir;
mod parser;
mod resolver;
mod scanner;

pub use parser::Parser;
pub use resolver::Resolver;

use crate::{
  byte_code::{CaptureIndex, Label, SymbolicByteCode},
  cache::CacheIdEmitter,
  source::LineOffsets,
  FeResult,
};
use codespan_reporting::diagnostic::{self, Diagnostic};
use ir::{
  ast::{self, Decl, Expr, Primary, Spanned, Stmt, Trailer},
  token::{Lexeme, Token, TokenKind},
};
use laythe_core::{
  chunk::ChunkBuilder,
  constants::{INDEX_GET, INDEX_SET, OBJECT, SUPER, UNINITIALIZED_VAR},
  constants::{ITER, ITER_VAR, SCRIPT, SELF},
  hooks::{GcContext, GcHooks, NoContext},
  managed::{AllocResult, Allocate, DebugHeap, Gc, GcObj, GcStr, Trace, TraceRoot},
  memory::Allocator,
  module, object,
  object::{FunBuilder, FunKind, List, Map},
  signature::Arity,
  val,
  value::Value,
};
use laythe_env::io::Io;
use object::{Fun, TryBlock};
use std::{
  cell::{RefCell, RefMut},
  convert::TryInto,
  io::Write,
  mem,
  ptr::NonNull,
  rc::Rc,
};

use self::ir::symbol_table::{Symbol, SymbolState, SymbolTable};

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

#[derive(Debug, Clone)]
pub struct LoopAttributes {
  scope_depth: usize,
  start: Label,
  end: Label,
}

impl DebugHeap for LoopAttributes {
  fn fmt_heap(&self, f: &mut std::fmt::Formatter, _: usize) -> std::fmt::Result {
    f.debug_struct("LoopInfo")
      .field("scope_depth", &self.scope_depth)
      .field("start", &self.start)
      .field("end", &self.start)
      .finish()
  }
}

impl Allocate<Gc<Self>> for LoopAttributes {
  fn alloc(self) -> AllocResult<Gc<Self>> {
    Gc::alloc_result(self)
  }
}

impl Trace for LoopAttributes {
  fn trace(&self) {}

  fn trace_debug(&self, _log: &mut dyn Write) {}
}

#[derive(Debug)]
enum ScopeExit {
  Normal,
  Early,
  Implicit,
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

pub struct Compiler<'a, 'src, FileId> {
  #[allow(dead_code)]
  io: Option<Io>,

  /// The roots from the surround context
  root_trace: &'a dyn TraceRoot,

  /// The current function
  fun: FunBuilder<SymbolicByteCode>,

  /// The type the current function scope
  fun_kind: FunKind,

  /// The current module
  module: Gc<module::Module>,

  /// line offsets for the current file
  line_offsets: &'a LineOffsets,

  /// All the errors found during compilation
  errors: Vec<Diagnostic<FileId>>,

  /// The parent compiler if it exists note uses
  /// unsafe pointer
  enclosing: Option<NonNull<Compiler<'a, 'src, FileId>>>,

  /// The info on the current class
  class_attributes: Option<Gc<ClassAttributes>>,

  /// The info on the current loop
  loop_attributes: Option<Gc<LoopAttributes>>,

  /// Should we early exit scope (break / continue)
  exit_scope: ScopeExit,

  /// The id emmiter for inline cache
  cache_id_emitter: Rc<RefCell<CacheIdEmitter>>,

  /// a label emitter
  label_emitter: LabelEmitter,

  /// hooks into the surround context. Used to allocate laythe objects
  gc: RefCell<Allocator>,

  /// The file id for the current file
  file_id: FileId,

  /// Current scope depth
  scope_depth: usize,

  /// Are we currently compile for a repl
  repl: bool,

  /// The current number of slots in use,
  slots: i32,

  /// The local variables currently in scope
  locals: Vec<Local<'a>>,

  /// local tables to this function
  local_tables: Vec<&'a SymbolTable<'src>>,

  /// is this function the script with the global table
  global_table: Option<&'a SymbolTable<'src>>,

  /// captures in this function
  captures: Vec<CaptureIndex>,

  /// A set of constants used in the current function
  constants: Map<Value, usize>,
}

impl<'a, 'src: 'a, FileId: Copy> Compiler<'a, 'src, FileId> {
  /// Create a new compiler at the module scope level. This struct will take a an ast
  /// produced by the parser and emit Laythe bytecode.
  ///
  /// # Examples
  /// ```
  /// use laythe_vm::{
  ///   compiler::{Compiler, Parser},
  ///   source::Source,
  /// };
  /// use laythe_core::{
  ///   module::Module,
  ///   object::Class,
  ///   memory::{NO_GC, Allocator},
  /// };
  /// use std::path::PathBuf;
  ///
  /// let mut gc = Allocator::default();
  ///
  /// let name = gc.manage_str("module", &NO_GC);
  /// let class = gc.manage_obj(Class::bare(name), &NO_GC);
  /// let module = gc.manage(Module::new(class, 0), &NO_GC);
  ///
  /// let file_id = 0;
  /// let source = Source::new(gc.manage_str("print('10');", &NO_GC));
  /// let (_, line_offsets) = Parser::new(&source, file_id).parse();
  ///
  /// let compiler = Compiler::new(module, &line_offsets, file_id, false, &NO_GC, gc);
  /// ```
  pub fn new(
    module: Gc<module::Module>,
    line_offsets: &'a LineOffsets,
    file_id: FileId,
    repl: bool,
    root_trace: &'a dyn TraceRoot,
    mut gc: Allocator,
  ) -> Self {
    gc.push_root(module);
    let fun_name = gc.manage_str(SCRIPT, root_trace);
    let fun = FunBuilder::new(fun_name, module, Arity::default());
    gc.pop_roots(1);

    Self {
      io: None,
      fun,
      file_id,
      root_trace,
      module,
      line_offsets,
      cache_id_emitter: Rc::new(RefCell::new(CacheIdEmitter::default())),
      label_emitter: LabelEmitter::default(),
      errors: vec![],
      fun_kind: FunKind::Script,
      scope_depth: 0,
      slots: 1,
      repl,
      class_attributes: None,
      loop_attributes: None,
      exit_scope: ScopeExit::Normal,
      gc: RefCell::new(gc),
      enclosing: None,
      local_tables: vec![],
      global_table: None,
      locals: vec![],
      captures: vec![],
      constants: object::Map::default(),
    }
  }

  /// Compile the provided ast into managed function objects that
  /// contain the vm bytecode
  pub fn compile(
    mut self,
    ast: &'a ast::Module<'src>,
  ) -> (FeResult<Fun, FileId>, Allocator, CacheIdEmitter) {
    self.begin_global_scope(&ast.symbols);
    self.declare_script();

    for decl in &ast.decls {
      self.decl(decl);
    }

    let end = ast.end();
    let cache_id_emitter = self.cache_id_emitter.replace(CacheIdEmitter::default());
    let (fun, errors, _, gc) = self.end_compiler(end, ScopeExit::Normal);
    if errors.is_empty() {
      (Ok(fun), gc, cache_id_emitter)
    } else {
      (Err(errors), gc, cache_id_emitter)
    }
  }

  // create a child compiler to compile a function inside the enclosing module
  fn child<'b>(
    name: GcStr,
    arity: Arity,
    fun_kind: FunKind,
    enclosing: &mut Compiler<'b, 'src, FileId>,
  ) -> Compiler<'b, 'src, FileId> {
    let fun = FunBuilder::new(name, enclosing.module, arity);

    let gc = RefCell::new(Allocator::default());
    gc.swap(&enclosing.gc);

    #[cfg(feature = "debug")]
    let io: Option<Io> = enclosing.io.clone();

    #[cfg(not(feature = "debug"))]
    let io: Option<Io> = None;

    Compiler {
      io,
      fun,
      file_id: enclosing.file_id,
      module: enclosing.module,
      line_offsets: enclosing.line_offsets,
      cache_id_emitter: Rc::clone(&enclosing.cache_id_emitter),
      label_emitter: LabelEmitter::default(),
      errors: vec![],
      root_trace: enclosing.root_trace,
      fun_kind,
      scope_depth: enclosing.scope_depth,
      slots: 1,
      repl: enclosing.repl,
      class_attributes: enclosing.class_attributes,
      loop_attributes: enclosing.loop_attributes,
      exit_scope: ScopeExit::Normal,
      gc,
      locals: vec![],
      global_table: None,
      enclosing: Some(NonNull::from(enclosing)),
      local_tables: vec![],
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
    exit: ScopeExit,
  ) -> (Fun, Vec<Diagnostic<FileId>>, Vec<CaptureIndex>, Allocator) {
    match exit {
      ScopeExit::Normal => self.emit_return(line),
      ScopeExit::Implicit => self.emit_byte(SymbolicByteCode::Return, line),
      ScopeExit::Early => (),
    }

    let context = NoContext::new(self.gc.into_inner());
    let hooks = GcHooks::new(&context);

    #[cfg(feature = "debug")]
    {
      Compiler::<FileId>::print_chunk(&self.fun, &self.class_attributes, &self.io, self.fun_kind);
    }

    let fun = self.fun.build(&hooks);
    (fun, self.errors, self.captures, context.done())
  }

  /// Print the chunk if debug and an error occurred
  #[cfg(feature = "debug")]
  fn print_chunk(
    fun: &FunBuilder<SymbolicByteCode>,
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
    print_symbolic_code(&mut stdio, fun.chunk(), &name).expect("could not write to stdio");
  }

  /// Emit byte code for a return
  fn emit_return(&mut self, line: u32) {
    match self.fun_kind {
      FunKind::Initializer => self.emit_byte(SymbolicByteCode::GetLocal(0), line),
      _ => self.emit_byte(SymbolicByteCode::Nil, line),
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
    // set this class as the current class compiler
    let loop_attributes = self.gc.borrow_mut().manage(
      LoopAttributes {
        scope_depth: self.scope_depth,
        start,
        end,
      },
      self,
    );
    let enclosing_loop = mem::replace(&mut self.loop_attributes, Some(loop_attributes));
    if let Some(enclosing_loop) = enclosing_loop {
      self.gc.borrow_mut().push_root(enclosing_loop);
    }

    self.scope(end_line, table, cb);
    self.emit_byte(SymbolicByteCode::Label(end), end_line);

    if enclosing_loop.is_some() {
      self.gc.borrow_mut().pop_roots(1);
    }
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
    let local_count = self.drop_locals(end_line, self.scope_depth);
    self.locals.truncate(local_count);
    self.local_tables.pop();
  }

  /// Drop all locals to a specified scope depth
  fn drop_locals(&mut self, line: u32, scope_depth: usize) -> usize {
    let mut drop_idx = self.locals.len();

    while drop_idx > 0 && self.locals[drop_idx - 1].depth > scope_depth {
      drop_idx -= 1;
    }

    let dropped = self.locals.len() - drop_idx;

    match dropped {
      0 => (),
      1 => self.emit_byte(SymbolicByteCode::Drop, line),
      _ => self.emit_byte(SymbolicByteCode::DropN(dropped as u8), line),
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
    if capture_count == std::u8::MAX {
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

    if self.locals.len() == std::u8::MAX as usize {
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

    if self.locals.len() == std::u8::MAX as usize {
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

  /// Get inline cache property id
  fn emit_property_id(&self) -> u32 {
    self.cache_id_emitter.borrow_mut().emit_property()
  }

  /// Get inline cache property id
  fn emit_invoke_id(&self) -> u32 {
    self.cache_id_emitter.borrow_mut().emit_invoke()
  }

  /// Emit a provided instruction
  fn emit_byte(&mut self, op_code: SymbolicByteCode, offset: u32) {
    let line = self
      .line_offsets
      .offset_line(offset as usize)
      .expect("Line offset out of bounds");

    self.slots += op_code.stack_effect();
    debug_assert!(self.slots >= 0);
    self.fun.update_max_slots(self.slots);

    self.write_instruction(op_code, line as u32 + 1);
  }

  /// The current chunk
  fn current_chunk(&self) -> &ChunkBuilder<SymbolicByteCode> {
    self.fun.chunk()
  }

  /// write instruction to the current function
  fn write_instruction(&mut self, op_code: SymbolicByteCode, line: u32) {
    self.fun.write_instruction(op_code, line)
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
        let index = self.fun.add_constant(value);
        if index > std::u16::MAX as usize {
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

    if index <= std::u8::MAX as u16 {
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
        self.exit_scope = ScopeExit::Implicit
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
  fn primary(&mut self, primary: &'a Primary<'src>, trailers: &'a [Trailer<'src>]) -> bool {
    match primary {
      Primary::Channel(token) => self.channel(token),
      Primary::True(token) => self.true_(token),
      Primary::False(token) => self.false_(token),
      Primary::Nil(token) => self.nil(token),
      Primary::Number(token) => self.number(token),
      Primary::Grouping(expr) => {
        self.expr(expr);
        false
      },
      Primary::String(token) => self.string(token),
      Primary::Interpolation(interpolation) => self.interpolation(interpolation),
      Primary::Ident(token) => self.identifier(token),
      Primary::InstanceAccess(instance_access) => self.instance_access(instance_access),
      Primary::Self_(token) => self.self_(token),
      Primary::Super(token) => self.super_(token, trailers),
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

    let exit = match &fun.body {
      ast::FunBody::Block(block) => compiler.block(block),
      ast::FunBody::Expr(expr) => {
        compiler.expr(expr);
        compiler.emit_byte(SymbolicByteCode::Return, expr.end());
        ScopeExit::Early
      },
    };

    let end_line = fun.end();

    // end compilation of function chunk
    let (fun, errors, captures, gc) = compiler.end_compiler(end_line, exit);

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
      let mut gc = self.gc.borrow_mut();

      let mut list: GcObj<List<Value>> =
        gc.manage_obj(List::with_capacity(import.path.len()), self);
      gc.push_root(list);

      for segment in &import.path {
        list.push(val!(gc.manage_str(segment.str(), self)))
      }

      gc.pop_roots(1);
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
      self_.emit_byte(SymbolicByteCode::Slot(self_.emit_invoke_id()), expr_line);
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
      self_.emit_byte(SymbolicByteCode::Loop(start_label), for_.end());
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

    self.emit_byte(SymbolicByteCode::Loop(start_label), while_.end());
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
          let skip_first = self.primary(&atom.primary, &atom.trailers);
          self.apply_trailers(skip_first, &atom.trailers[..atom.trailers.len() - 1]);

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
        self.emit_byte(SymbolicByteCode::Return, v.end());
      },
      None => self.emit_return(return_.start()),
    }
    self.exit_scope = ScopeExit::Early;
  }

  /// Compile a return statement
  fn continue_(&mut self, continue_: &Token) {
    let loop_info = self
      .loop_attributes
      .expect("Parser should have caught the loop constraint");

    self.drop_locals(continue_.end(), loop_info.scope_depth);
    self.emit_byte(SymbolicByteCode::Loop(loop_info.start), continue_.start());
    self.exit_scope = ScopeExit::Early;
  }

  /// Compile a return statement
  fn break_(&mut self, break_: &Token) {
    let loop_info = self
      .loop_attributes
      .expect("Parser should have caught the loop constraint");

    self.drop_locals(break_.end(), loop_info.scope_depth);
    self.emit_byte(SymbolicByteCode::Jump(loop_info.end), break_.start());
    self.exit_scope = ScopeExit::Early;
  }

  /// Compile a try catch block
  fn try_(&mut self, try_: &'a ast::Try<'src>) {
    let start = self.current_chunk().instructions().len();
    let slots = self.slots as usize;

    self.scope(try_.block.end(), &try_.block.symbols, |self_| {
      self_.block(&try_.block);
    });

    let catch_label = self.label_emitter.emit();
    self.emit_byte(SymbolicByteCode::Jump(catch_label), try_.block.end());
    let end = self.current_chunk().instructions().len();

    self.scope(try_.catch.end(), &try_.catch.symbols, |self_| {
      self_.block(&try_.catch);
    });

    self.emit_byte(SymbolicByteCode::Label(catch_label), try_.catch.end());
    self.fun.add_try(TryBlock::new(start, end, slots));
  }

  /// Compile a block
  fn block(&mut self, block: &'a ast::Block<'src>) -> ScopeExit {
    for decl in &block.decls {
      if let ScopeExit::Early = self.exit_scope {
        break;
      }
      self.decl(decl);
    }

    mem::replace(&mut self.exit_scope, ScopeExit::Normal)
  }

  /// Compile an assignment expression
  fn assign(&mut self, assign: &'a ast::Assign<'src>) {
    match &assign.lhs {
      Expr::Atom(atom) => match atom.trailers.last() {
        // if we have trailers compile to last trailer and emit specialized
        // set instruction
        Some(last) => {
          let skip_first = self.primary(&atom.primary, &atom.trailers);
          self.apply_trailers(skip_first, &atom.trailers[..atom.trailers.len() - 1]);

          match last {
            Trailer::Index(index) => {
              self.expr(&assign.rhs);
              self.expr(&index.index);

              let name = self.identifier_constant(INDEX_SET);
              self.emit_byte(SymbolicByteCode::Invoke((name, 2)), assign.rhs.end());
              self.emit_byte(
                SymbolicByteCode::Slot(self.emit_invoke_id()),
                assign.rhs.end(),
              )
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
              self.emit_byte(
                SymbolicByteCode::Slot(self.emit_property_id()),
                access.end(),
              );
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
            self.emit_byte(
              SymbolicByteCode::Slot(self.emit_property_id()),
              instance_access.end(),
            );
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
          let skip_first = self.primary(&atom.primary, &atom.trailers);
          self.apply_trailers(skip_first, &atom.trailers[..atom.trailers.len() - 1]);

          match last {
            Trailer::Index(index) => {
              // self.expr(&send.rhs);
              self.expr(&index.index);

              let name = self.identifier_constant(INDEX_GET);
              self.emit_byte(SymbolicByteCode::Invoke((name, 1)), index.end());
              self.emit_byte(SymbolicByteCode::Slot(self.emit_invoke_id()), index.end());

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
              self.emit_byte(
                SymbolicByteCode::Slot(self.emit_property_id()),
                access.end(),
              );

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
            self.emit_byte(
              SymbolicByteCode::Slot(self.emit_property_id()),
              instance_access.end(),
            );

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
    let binary_op = |comp: &mut Compiler<'a, 'src, FileId>| match &assign_binary.op {
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
          let skip_first = self.primary(&atom.primary, &atom.trailers);
          self.apply_trailers(skip_first, &atom.trailers[..atom.trailers.len() - 1]);

          match last {
            Trailer::Index(index) => {
              self.emit_byte(SymbolicByteCode::Dup, assign_binary.lhs.end());

              self.expr(&index.index);

              let name = self.identifier_constant(INDEX_GET);
              self.emit_byte(SymbolicByteCode::Invoke((name, 1)), assign_binary.rhs.end());
              self.emit_byte(
                SymbolicByteCode::Slot(self.emit_invoke_id()),
                assign_binary.rhs.end(),
              );

              self.expr(&assign_binary.rhs);
              binary_op(self);

              self.expr(&index.index);

              let name = self.identifier_constant(INDEX_SET);
              self.emit_byte(SymbolicByteCode::Invoke((name, 2)), assign_binary.rhs.end());
              self.emit_byte(
                SymbolicByteCode::Slot(self.emit_invoke_id()),
                assign_binary.rhs.end(),
              );
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
              self.emit_byte(
                SymbolicByteCode::Slot(self.emit_property_id()),
                access.end(),
              );

              self.expr(&assign_binary.rhs);
              binary_op(self);

              self.emit_byte(SymbolicByteCode::SetProperty(name), access.end());
              self.emit_byte(
                SymbolicByteCode::Slot(self.emit_property_id()),
                access.end(),
              );
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
            self.emit_byte(
              SymbolicByteCode::Slot(self.emit_property_id()),
              instance_access.end(),
            );

            self.expr(&assign_binary.rhs);
            binary_op(self);

            self.emit_byte(SymbolicByteCode::SetProperty(name), instance_access.end());
            self.emit_byte(
              SymbolicByteCode::Slot(self.emit_property_id()),
              instance_access.end(),
            );
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

    // emit for rhs if we're not an "and" or "or"
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
  fn call(&mut self, call: &'a ast::Call<'src>) -> bool {
    for expr in &call.args {
      self.expr(expr);
    }

    self.emit_byte(SymbolicByteCode::Call(call.args.len() as u8), call.end());
    false
  }

  /// Compile an indexing expression
  fn index(&mut self, index: &'a ast::Index<'src>) -> bool {
    self.expr(&index.index);

    let name = self.identifier_constant(INDEX_GET);
    self.emit_byte(SymbolicByteCode::Invoke((name, 1)), index.end());
    self.emit_byte(SymbolicByteCode::Slot(self.emit_invoke_id()), index.end());

    false
  }

  /// Compile an access expression
  fn access(&mut self, access: &ast::Access, trailers: &'a [Trailer<'src>]) -> bool {
    let name = self.identifier_constant(access.prop.str());

    match trailers.first() {
      Some(trailer) => {
        if let Trailer::Call(call) = trailer {
          for expr in &call.args {
            self.expr(expr);
          }
          self.emit_byte(
            SymbolicByteCode::Invoke((name, call.args.len() as u8)),
            trailer.end(),
          );
          self.emit_byte(SymbolicByteCode::Slot(self.emit_invoke_id()), trailer.end());
          true
        } else {
          self.emit_byte(SymbolicByteCode::GetProperty(name), access.prop.end());
          self.emit_byte(
            SymbolicByteCode::Slot(self.emit_property_id()),
            access.end(),
          );
          false
        }
      },
      None => {
        self.emit_byte(SymbolicByteCode::GetProperty(name), access.prop.end());
        self.emit_byte(
          SymbolicByteCode::Slot(self.emit_property_id()),
          access.end(),
        );
        false
      },
    }
  }

  /// Compile an atom expression
  fn atom(&mut self, atom: &'a ast::Atom<'src>) {
    let skip_first = self.primary(&atom.primary, &atom.trailers);
    self.apply_trailers(skip_first, &atom.trailers);
  }

  /// Compile trailers onto a base primary
  fn apply_trailers(&mut self, skip_first: bool, trailers: &'a [Trailer<'src>]) {
    let mut skip = skip_first;
    for (idx, trailer) in trailers.iter().enumerate() {
      if skip {
        skip = false;
        continue;
      }

      skip = match trailer {
        Trailer::Call(call) => self.call(call),
        Trailer::Index(index) => self.index(index),
        Trailer::Access(access) => self.access(access, &trailers[(idx + 1)..]),
      }
    }
  }

  /// Compile a channel declaration
  fn channel(&mut self, channel: &'a ast::Channel<'src>) -> bool {
    match &channel.expr {
      Some(expr) => {
        self.expr(expr);
        self.emit_byte(SymbolicByteCode::BufferedChannel, channel.end());
      },
      None => self.emit_byte(SymbolicByteCode::Channel, channel.end()),
    }

    false
  }

  /// Compile a true token
  fn true_(&mut self, true_: &Token) -> bool {
    self.emit_byte(SymbolicByteCode::True, true_.end());
    false
  }

  /// Compile a false token
  fn false_(&mut self, false_: &Token) -> bool {
    self.emit_byte(SymbolicByteCode::False, false_.end());
    false
  }

  /// Compile a nil token
  fn nil(&mut self, nil: &Token) -> bool {
    self.emit_byte(SymbolicByteCode::Nil, nil.end());
    false
  }

  /// Compile a number token
  fn number(&mut self, token: &Token) -> bool {
    let value = val!(token.str().parse::<f64>().expect("Unable to parse float"));
    self.emit_constant(value, token.end());
    false
  }

  /// Compile a string token
  fn string(&mut self, token: &Token) -> bool {
    let value = val!(self.gc.borrow_mut().manage_str(token.str(), self));
    self.emit_constant(value, token.end());
    false
  }

  /// Compile a string token
  fn interpolation(&mut self, interpolation: &'a ast::Interpolation<'src>) -> bool {
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
          self.emit_byte(SymbolicByteCode::Slot(self.emit_invoke_id()), expr.end());
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

    false
  }

  /// Compile a identifer token
  fn identifier(&mut self, token: &Token<'src>) -> bool {
    self.variable_get(token);
    false
  }

  /// Compile instance access
  fn instance_access(&mut self, instance_access: &ast::InstanceAccess<'src>) -> bool {
    if self.instance_access_self(instance_access) {
      let name = self.identifier_constant(instance_access.property());

      self.emit_byte(SymbolicByteCode::GetProperty(name), instance_access.end());
      self.emit_byte(
        SymbolicByteCode::Slot(self.emit_property_id()),
        instance_access.end(),
      );
    }

    false
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
  fn self_(&mut self, self_: &Token<'src>) -> bool {
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

    false
  }

  /// Compile the super token
  fn super_(&mut self, super_: &ast::Super<'src>, trailers: &'a [Trailer<'src>]) -> bool {
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

    match trailers.first() {
      Some(trailer) => match trailer {
        Trailer::Call(call) => {
          for arg in &call.args {
            self.expr(arg);
          }

          self.variable_get(&super_.super_);
          self.emit_byte(
            SymbolicByteCode::SuperInvoke((name, call.args.len() as u8)),
            super_.access.end(),
          );
          self.emit_byte(
            SymbolicByteCode::Slot(self.emit_invoke_id()),
            super_.access.end(),
          );
          true
        },
        _ => {
          self.variable_get(&super_.super_);
          self.emit_byte(SymbolicByteCode::GetSuper(name), super_.end());
          false
        },
      },
      None => {
        self.variable_get(&super_.super_);

        self.emit_byte(SymbolicByteCode::GetSuper(name), super_.access.end());
        false
      },
    }
  }

  /// Compile a lambda expression
  fn lambda(&mut self, fun: &'a ast::Fun<'src>) -> bool {
    self.function(fun, FunKind::Fun);
    false
  }

  /// Compile a list literal
  fn list(&mut self, list: &'a ast::Collection<'src>) -> bool {
    for item in list.items.iter() {
      self.expr(item);
    }

    self.emit_byte(SymbolicByteCode::List(list.items.len() as u16), list.end());

    false
  }

  /// Compile a list literal
  fn tuple(&mut self, list: &'a ast::Collection<'src>) -> bool {
    for item in list.items.iter() {
      self.expr(item);
    }

    self.emit_byte(SymbolicByteCode::Tuple(list.items.len() as u16), list.end());

    false
  }

  /// Compile a map literal
  fn map(&mut self, map: &'a ast::Map<'src>) -> bool {
    for (key, value) in map.entries.iter() {
      self.expr(key);
      self.expr(value);
    }

    self.emit_byte(SymbolicByteCode::Map(map.entries.len() as u16), map.end());

    false
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

impl<'a, 'src: 'a, FileId> GcContext for Compiler<'a, 'src, FileId> {
  fn gc(&self) -> RefMut<'_, Allocator> {
    self.gc.borrow_mut()
  }
}

impl<'a, 'src: 'a, FileId> TraceRoot for Compiler<'a, 'src, FileId> {
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
    if let Some(loop_info) = self.loop_attributes {
      loop_info.trace();
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
    if let Some(loop_info) = self.loop_attributes {
      loop_info.trace_debug(log)
    }

    self.constants.keys().for_each(|key| {
      key.trace_debug(log);
    });
  }

  fn can_collect(&self) -> bool {
    true
  }
}

// #[cfg(test)]
// mod test {
//   use super::*;
//   use crate::{
//     byte_code::{decode_u16, decode_u32},
//     compiler::Parser,
//     debug::disassemble_chunk,
//     source::Source,
//   };
//   use laythe_core::{
//     hooks::{GcHooks, NoContext},
//     managed::GcObj,
//     memory::{NoGc, NO_GC},
//     object::{Class, ObjectKind},
//   };
//   use laythe_env::stdio::{support::StdioTestContainer, Stdio};
//   use module::Module;

//   #[cfg(feature = "debug")]
//   use laythe_native::io::io_native;

//   enum ByteCodeTest {
//     Code(SymbolicByteCode),
//     Fun((u16, usize, Vec<ByteCodeTest>)),
//   }

//   pub fn test_class(hooks: &GcHooks, name: &str) -> GcObj<Class> {
//     let mut object_class = hooks.manage_obj(Class::bare(hooks.manage_str("Object")));
//     let mut class_class = hooks.manage_obj(Class::bare(hooks.manage_str("Class")));
//     class_class.inherit(hooks, object_class);

//     let class_copy = class_class;
//     class_class.set_meta(class_copy);

//     // create object's meta class
//     let mut object_meta_class = hooks.manage_obj(Class::bare(
//       hooks.manage_str(format!("{} metaClass", &*object_class.name())),
//     ));

//     object_meta_class.inherit(hooks, class_class);
//     object_meta_class.set_meta(class_class);

//     object_class.set_meta(object_meta_class);

//     Class::with_inheritance(hooks, hooks.manage_str(name), object_class)
//   }

//   pub fn test_fun(hooks: &GcHooks, module: Gc<Module>) -> GcObj<Fun> {
//     let fun = Fun::stub(
//       hooks,
//       hooks.manage_str("print"),
//       module,
//       SymbolicByteCode::Return,
//     );
//     hooks.manage_obj(fun)
//   }

//   fn dummy_module(hooks: &GcHooks) -> Gc<Module> {
//     let module_class = test_class(&hooks, "Module");
//     let object_class = module_class.super_class().expect("Expected Object");
//     hooks.push_root(module_class);
//     let mut module = hooks.manage(Module::new(module_class, 0));
//     hooks.pop_roots(1);
//     hooks.push_root(module);

//     let print = test_fun(hooks, module);
//     assert!(module.insert_symbol(print.name(), val!(print)).is_ok());
//     assert!(module
//       .insert_symbol(object_class.name(), val!(object_class))
//       .is_ok());
//     assert!(module.export_symbol(print.name()).is_ok());
//     assert!(module.export_symbol(object_class.name()).is_ok());

//     hooks.pop_roots(1);

//     module
//   }

//   fn test_compile(src: &str, context: &NoContext) -> Fun {
//     let hooks = &GcHooks::new(context);
//     let repl = false;

//     let src = hooks.manage_str(src);
//     hooks.push_root(src);
//     let source = Source::new(hooks.manage_str(src));
//     let (ast, line_offsets) = Parser::new(&source, 0).parse();
//     assert!(ast.is_ok(), "{}", src);
//     let mut ast = ast.unwrap();

//     let module = dummy_module(hooks);

//     let gc = context.replace_gc(Allocator::default());

//     assert!(Resolver::new(module, &gc, &source, 0, repl)
//       .resolve(&mut ast)
//       .is_ok());

//     let fake_vm_root: &NoGc = &NO_GC;
//     let compiler = Compiler::new(module, &line_offsets, 0, repl, fake_vm_root, gc);
//     #[cfg(feature = "debug")]
//     let compiler = compiler.with_io(io_native());

//     let (result, gc, _) = compiler.compile(&ast);
//     context.replace_gc(gc);

//     assert!(result.is_ok());
//     result.unwrap()
//   }

//   fn decode_byte_code(fun: &Fun) -> Vec<SymbolicByteCode> {
//     let bytes = &fun.chunk().instructions();
//     let mut decoded = Vec::new();
//     let mut offset = 0;

//     while offset < bytes.len() {
//       let (byte_code, new_offset) = SymbolicByteCode::decode(&bytes, offset);

//       match byte_code {
//         SymbolicByteCode::Closure(closure) => {
//           decoded.push(byte_code);
//           offset = decode_byte_code_closure(fun, &mut decoded, new_offset, closure)
//         },
//         SymbolicByteCode::GetProperty(_)
//         | SymbolicByteCode::SetProperty(_)
//         | SymbolicByteCode::Invoke(_)
//         | SymbolicByteCode::SuperInvoke(_) => {
//           decoded.push(byte_code);
//           offset = decode_byte_code_slot(fun, &mut decoded, new_offset)
//         },
//         _ => {
//           decoded.push(byte_code);
//           offset = new_offset;
//         },
//       }
//     }

//     decoded
//   }

//   fn decode_byte_code_closure(
//     fun: &Fun,
//     decoded: &mut Vec<SymbolicByteCode>,
//     offset: usize,
//     slot: u16,
//   ) -> usize {
//     let inner_fun = fun.chunk().get_constant(slot as usize).to_obj().to_fun();
//     let mut current_offset = offset;

//     let byte_slice = &fun.chunk().instructions();
//     for _ in 0..inner_fun.capture_count() {
//       let scalar = decode_u16(&byte_slice[offset..offset + 2]);

//       let capture_index: CaptureIndex = unsafe { mem::transmute(scalar) };
//       decoded.push(SymbolicByteCode::CaptureIndex(capture_index));
//       current_offset = current_offset + 2;
//     }

//     current_offset
//   }

//   fn decode_byte_code_slot(fun: &Fun, decoded: &mut Vec<SymbolicByteCode>, offset: usize) -> usize {
//     let byte_slice = fun.chunk().instructions();
//     decoded.push(SymbolicByteCode::Slot(decode_u32(
//       &byte_slice[offset..offset + 4],
//     )));

//     offset + 4
//   }

//   fn assert_simple_bytecode(fun: &Fun, max_slots: usize, code: &[SymbolicByteCode]) {
//     let stdio_container = StdioTestContainer::default();
//     let mut stdio = Stdio::new(Box::new(stdio_container.make_stdio()));

//     if let Err(_) = disassemble_chunk(&mut stdio, &fun.chunk(), "test") {
//       stdio_container.log_stdio();
//       assert!(false)
//     }
//     stdio_container.log_stdio();
//     let decoded_byte_code = decode_byte_code(fun);

//     decoded_byte_code
//       .iter()
//       .zip(code.iter())
//       .enumerate()
//       .for_each(|(index, (actual, expected))| {
//         assert_eq!(
//           actual, expected,
//           "compiled {:?} but expected {:?} at aligned instruction {}",
//           actual, expected, index
//         )
//       });

//     assert_eq!(decoded_byte_code.len(), code.len());
//     assert_eq!(fun.max_slots(), max_slots);
//   }

//   fn assert_fun_bytecode(fun: &Fun, max_slots: usize, code: &[ByteCodeTest]) {
//     let stdio_container = StdioTestContainer::default();
//     let mut stdio = Stdio::new(Box::new(stdio_container.make_stdio()));

//     assert!(disassemble_chunk(&mut stdio, &fun.chunk(), &*fun.name()).is_ok());
//     stdio_container.log_stdio();

//     let decoded_byte_code = decode_byte_code(fun);

//     for i in 0..code.len() {
//       match decoded_byte_code[i] {
//         SymbolicByteCode::Closure(index) => {
//           let fun = fun.chunk().get_constant(index as usize).to_obj().to_fun();

//           match &code[i] {
//             ByteCodeTest::Fun((expected_index, max_slots, inner)) => {
//               assert_eq!(*expected_index, index, "Function constant index not in expected spot. Found at index {} expected at index {}", expected_index, index);
//               assert_fun_bytecode(&*fun, *max_slots, &inner);
//             },
//             _ => assert!(false),
//           }
//         },
//         SymbolicByteCode::Constant(index) => {
//           let constant = fun.chunk().get_constant(index as usize);
//           if constant.is_obj_kind(ObjectKind::Fun) {
//             let fun = constant.to_obj().to_fun();

//             match &code[i] {
//               ByteCodeTest::Fun((expected, max_slots, inner)) => {
//                 assert_eq!(*expected, index as u16);
//                 assert_fun_bytecode(&*fun, *max_slots, &inner);
//               },
//               _ => assert!(false),
//             }
//           } else {
//             match &code[i] {
//               ByteCodeTest::Code(byte_code) => {
//                 assert_eq!(&decoded_byte_code[i], byte_code);
//               },
//               _ => assert!(false),
//             }
//           }
//         },
//         SymbolicByteCode::ConstantLong(index) => {
//           let constant = fun.chunk().get_constant(index as usize);

//           if constant.is_obj_kind(ObjectKind::Fun) {
//             let fun = constant.to_obj().to_fun();

//             match &code[i] {
//               ByteCodeTest::Fun((expected, max_slots, inner)) => {
//                 assert_eq!(*expected, index);
//                 assert_fun_bytecode(&*fun, *max_slots, &inner);
//               },
//               _ => assert!(false),
//             }
//           } else {
//             match &code[i] {
//               ByteCodeTest::Code(byte_code) => {
//                 assert_eq!(&decoded_byte_code[i], byte_code);
//               },
//               _ => assert!(false),
//             }
//           }
//         },
//         _ => match &code[i] {
//           ByteCodeTest::Code(byte_code) => {
//             assert_eq!(&decoded_byte_code[i], byte_code);
//           },
//           _ => assert!(false),
//         },
//       }
//     }

//     assert_eq!(
//       decoded_byte_code.len(),
//       code.len(),
//       "for fn {} expected instruction length to be {} but recieved {}",
//       fun.name(),
//       code.len(),
//       decoded_byte_code.len()
//     );
//     assert_eq!(
//       fun.max_slots(),
//       max_slots,
//       "for fn {} expected max slots to be {} but recieved {}.",
//       fun.name(),
//       max_slots,
//       fun.max_slots()
//     );
//   }

//   #[test]
//   fn import() {
//     let example = r#"
//       import std.time;
//     "#;

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       2,
//       &vec![
//         SymbolicByteCode::Import(0),
//         SymbolicByteCode::DefineGlobal(1),
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn export_variable() {
//     let example = "
//       export let x = 10;
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       2,
//       &vec![
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::DefineGlobal(0),
//         SymbolicByteCode::Export(0),
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn export_fun() {
//     let example = "
//       export fn example() {}
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_fun_bytecode(
//       &fun,
//       2,
//       &vec![
//         ByteCodeTest::Fun((
//           1,
//           2,
//           vec![
//             ByteCodeTest::Code(SymbolicByteCode::Nil),
//             ByteCodeTest::Code(SymbolicByteCode::Return),
//           ],
//         )),
//         ByteCodeTest::Code(SymbolicByteCode::DefineGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::Export(0)),
//         ByteCodeTest::Code(SymbolicByteCode::Nil),
//         ByteCodeTest::Code(SymbolicByteCode::Return),
//       ],
//     );
//   }

//   #[test]
//   fn export_class() {
//     let example = "
//       export class example {}
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       3,
//       &vec![
//         SymbolicByteCode::Class(0),
//         SymbolicByteCode::DefineGlobal(0),
//         SymbolicByteCode::GetGlobal(1),
//         SymbolicByteCode::GetGlobal(0),
//         SymbolicByteCode::Inherit,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Export(0),
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn empty_try_catch() {
//     let example = "
//       try {

//       } catch {

//       }
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       2,
//       &vec![
//         SymbolicByteCode::Jump(0),
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );

//     assert_eq!(fun.has_catch_jump(0), Some((3, 1)));
//   }

//   #[test]
//   fn filled_try_catch() {
//     let example = r#"
//       try {
//         let empty = {};
//         empty["missing"];
//       } catch {
//         print("no!");
//       }
//     "#;

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       4,
//       &vec![
//         SymbolicByteCode::Map(0),         // 0
//         SymbolicByteCode::GetLocal(1),    // 3
//         SymbolicByteCode::Constant(1),    // 5
//         SymbolicByteCode::Invoke((2, 1)), // 7
//         SymbolicByteCode::Slot(0),        // 7
//         SymbolicByteCode::Drop,           // 8
//         SymbolicByteCode::Drop,           // 9
//         SymbolicByteCode::Jump(8),        // 10
//         SymbolicByteCode::GetGlobal(3),   // 13
//         SymbolicByteCode::Constant(4),    // 16
//         SymbolicByteCode::Call(1),        // 18
//         SymbolicByteCode::Drop,           // 20
//         SymbolicByteCode::Nil,            // 21
//         SymbolicByteCode::Return,         // 22
//       ],
//     );

//     assert_eq!(fun.has_catch_jump(0), Some((20, 1)));
//   }

//   #[test]
//   fn nested_try_catch() {
//     let example = r#"
//       try {
//         [][3];
//         try {
//           [][1];
//         } catch {
//           print("woops!");
//         }
//       } catch {
//         print("no!");
//       }
//     "#;

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       3,
//       &vec![
//         SymbolicByteCode::List(0),        // 0
//         SymbolicByteCode::Constant(0),    // 3
//         SymbolicByteCode::Invoke((1, 1)), // 5
//         SymbolicByteCode::Slot(0),        // 9
//         SymbolicByteCode::Drop,           // 13
//         SymbolicByteCode::List(0),        // 16
//         SymbolicByteCode::Constant(2),    // 19
//         SymbolicByteCode::Invoke((1, 1)), // 22
//         SymbolicByteCode::Slot(1),        // 26
//         SymbolicByteCode::Drop,           // 31
//         SymbolicByteCode::Jump(8),        // 32
//         SymbolicByteCode::GetGlobal(3),   // 35
//         SymbolicByteCode::Constant(4),    // 38
//         SymbolicByteCode::Call(1),        // 40
//         SymbolicByteCode::Drop,           // 42
//         SymbolicByteCode::Jump(8),        // 43
//         SymbolicByteCode::GetGlobal(3),   // 46
//         SymbolicByteCode::Constant(5),    // 49
//         SymbolicByteCode::Call(1),        // 52
//         SymbolicByteCode::Drop,           // 54
//         SymbolicByteCode::Nil,            // 55
//         SymbolicByteCode::Return,         // 56
//       ],
//     );

//     assert_eq!(fun.has_catch_jump(5), Some((42, 1)));
//     assert_eq!(fun.has_catch_jump(31), Some((42, 1)));
//     assert_eq!(fun.has_catch_jump(19), Some((31, 1)));
//   }

//   #[test]
//   fn class_with_inherit() {
//     let example = "
//       class A {}

//       class B : A {}
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       3,
//       &vec![
//         SymbolicByteCode::Class(0),
//         SymbolicByteCode::DefineGlobal(0),
//         SymbolicByteCode::GetGlobal(1),
//         SymbolicByteCode::GetGlobal(0),
//         SymbolicByteCode::Inherit,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Class(2),
//         SymbolicByteCode::DefineGlobal(2),
//         SymbolicByteCode::GetGlobal(0),
//         SymbolicByteCode::GetGlobal(2),
//         SymbolicByteCode::Inherit,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn class_empty() {
//     let example = "
//       class A {}
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       3,
//       &vec![
//         SymbolicByteCode::Class(0),
//         SymbolicByteCode::DefineGlobal(0),
//         SymbolicByteCode::GetGlobal(1),
//         SymbolicByteCode::GetGlobal(0),
//         SymbolicByteCode::Inherit,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn class_with_methods() {
//     let example = "
//       class A {
//         init() {
//           self.field = true;
//         }

//         getField() {
//           return self.field;
//         }

//         getGetField() {
//           return self.getField();
//         }
//       }
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_fun_bytecode(
//       &fun,
//       4,
//       &vec![
//         ByteCodeTest::Code(SymbolicByteCode::Class(0)),
//         ByteCodeTest::Code(SymbolicByteCode::DefineGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::GetGlobal(1)),
//         ByteCodeTest::Code(SymbolicByteCode::GetGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::Inherit),
//         ByteCodeTest::Fun((
//           3,
//           3,
//           vec![
//             ByteCodeTest::Code(SymbolicByteCode::GetLocal(0)),
//             ByteCodeTest::Code(SymbolicByteCode::True),
//             ByteCodeTest::Code(SymbolicByteCode::SetProperty(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Slot(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Drop),
//             ByteCodeTest::Code(SymbolicByteCode::GetLocal(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Return),
//           ],
//         )),
//         ByteCodeTest::Code(SymbolicByteCode::Method(2)),
//         ByteCodeTest::Code(SymbolicByteCode::Field(4)),
//         ByteCodeTest::Fun((
//           6,
//           2,
//           vec![
//             ByteCodeTest::Code(SymbolicByteCode::GetLocal(0)),
//             ByteCodeTest::Code(SymbolicByteCode::GetProperty(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Slot(1)),
//             ByteCodeTest::Code(SymbolicByteCode::Return),
//           ],
//         )),
//         ByteCodeTest::Code(SymbolicByteCode::Method(5)),
//         ByteCodeTest::Fun((
//           8,
//           2,
//           vec![
//             ByteCodeTest::Code(SymbolicByteCode::GetLocal(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Invoke((0, 0))),
//             ByteCodeTest::Code(SymbolicByteCode::Slot(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Return),
//           ],
//         )),
//         ByteCodeTest::Code(SymbolicByteCode::Method(7)),
//         ByteCodeTest::Code(SymbolicByteCode::Drop),
//         ByteCodeTest::Code(SymbolicByteCode::Drop),
//         ByteCodeTest::Code(SymbolicByteCode::Nil),
//         ByteCodeTest::Code(SymbolicByteCode::Return),
//       ],
//     );
//   }

//   #[test]
//   fn class_with_methods_implicit() {
//     let example = "
//       class A {
//         init() {
//           self.field = true;
//         }

//         getField() {
//           self.field
//         }

//         getGetField() {
//           self.getField()
//         }
//       }
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_fun_bytecode(
//       &fun,
//       4,
//       &vec![
//         ByteCodeTest::Code(SymbolicByteCode::Class(0)),
//         ByteCodeTest::Code(SymbolicByteCode::DefineGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::GetGlobal(1)),
//         ByteCodeTest::Code(SymbolicByteCode::GetGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::Inherit),
//         ByteCodeTest::Fun((
//           3,
//           3,
//           vec![
//             ByteCodeTest::Code(SymbolicByteCode::GetLocal(0)),
//             ByteCodeTest::Code(SymbolicByteCode::True),
//             ByteCodeTest::Code(SymbolicByteCode::SetProperty(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Slot(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Drop),
//             ByteCodeTest::Code(SymbolicByteCode::GetLocal(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Return),
//           ],
//         )),
//         ByteCodeTest::Code(SymbolicByteCode::Method(2)),
//         ByteCodeTest::Code(SymbolicByteCode::Field(4)),
//         ByteCodeTest::Fun((
//           6,
//           2,
//           vec![
//             ByteCodeTest::Code(SymbolicByteCode::GetLocal(0)),
//             ByteCodeTest::Code(SymbolicByteCode::GetProperty(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Slot(1)),
//             ByteCodeTest::Code(SymbolicByteCode::Return),
//           ],
//         )),
//         ByteCodeTest::Code(SymbolicByteCode::Method(5)),
//         ByteCodeTest::Fun((
//           8,
//           2,
//           vec![
//             ByteCodeTest::Code(SymbolicByteCode::GetLocal(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Invoke((0, 0))),
//             ByteCodeTest::Code(SymbolicByteCode::Slot(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Return),
//           ],
//         )),
//         ByteCodeTest::Code(SymbolicByteCode::Method(7)),
//         ByteCodeTest::Code(SymbolicByteCode::Drop),
//         ByteCodeTest::Code(SymbolicByteCode::Drop),
//         ByteCodeTest::Code(SymbolicByteCode::Nil),
//         ByteCodeTest::Code(SymbolicByteCode::Return),
//       ],
//     );
//   }

//   #[test]
//   fn class_with_instance_access() {
//     let example = "
//       class A {
//         init() {
//           @field = true;
//         }

//         getField() {
//           @field
//         }

//         getGetField() {
//           self.getField()
//         }
//       }
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_fun_bytecode(
//       &fun,
//       4,
//       &vec![
//         ByteCodeTest::Code(SymbolicByteCode::Class(0)),
//         ByteCodeTest::Code(SymbolicByteCode::DefineGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::GetGlobal(1)),
//         ByteCodeTest::Code(SymbolicByteCode::GetGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::Inherit),
//         ByteCodeTest::Fun((
//           3,
//           3,
//           vec![
//             ByteCodeTest::Code(SymbolicByteCode::GetLocal(0)),
//             ByteCodeTest::Code(SymbolicByteCode::True),
//             ByteCodeTest::Code(SymbolicByteCode::SetProperty(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Slot(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Drop),
//             ByteCodeTest::Code(SymbolicByteCode::GetLocal(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Return),
//           ],
//         )),
//         ByteCodeTest::Code(SymbolicByteCode::Method(2)),
//         ByteCodeTest::Code(SymbolicByteCode::Field(4)),
//         ByteCodeTest::Fun((
//           6,
//           2,
//           vec![
//             ByteCodeTest::Code(SymbolicByteCode::GetLocal(0)),
//             ByteCodeTest::Code(SymbolicByteCode::GetProperty(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Slot(1)),
//             ByteCodeTest::Code(SymbolicByteCode::Return),
//           ],
//         )),
//         ByteCodeTest::Code(SymbolicByteCode::Method(5)),
//         ByteCodeTest::Fun((
//           8,
//           2,
//           vec![
//             ByteCodeTest::Code(SymbolicByteCode::GetLocal(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Invoke((0, 0))),
//             ByteCodeTest::Code(SymbolicByteCode::Slot(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Return),
//           ],
//         )),
//         ByteCodeTest::Code(SymbolicByteCode::Method(7)),
//         ByteCodeTest::Code(SymbolicByteCode::Drop),
//         ByteCodeTest::Code(SymbolicByteCode::Drop),
//         ByteCodeTest::Code(SymbolicByteCode::Nil),
//         ByteCodeTest::Code(SymbolicByteCode::Return),
//       ],
//     );
//   }

//   #[test]
//   fn class_property_assign_set() {
//     let example = "
//     class A {
//       init() {
//         self.a = 10;
//         self.a /= 5;
//       }
//     }
//   ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_fun_bytecode(
//       &fun,
//       4,
//       &vec![
//         ByteCodeTest::Code(SymbolicByteCode::Class(0)),
//         ByteCodeTest::Code(SymbolicByteCode::DefineGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::GetGlobal(1)),
//         ByteCodeTest::Code(SymbolicByteCode::GetGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::Inherit),
//         ByteCodeTest::Fun((
//           3,
//           4,
//           vec![
//             ByteCodeTest::Code(SymbolicByteCode::GetLocal(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Constant(1)),
//             ByteCodeTest::Code(SymbolicByteCode::SetProperty(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Slot(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Drop),
//             ByteCodeTest::Code(SymbolicByteCode::GetLocal(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Dup),
//             ByteCodeTest::Code(SymbolicByteCode::GetProperty(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Slot(1)),
//             ByteCodeTest::Code(SymbolicByteCode::Constant(2)),
//             ByteCodeTest::Code(SymbolicByteCode::Divide),
//             ByteCodeTest::Code(SymbolicByteCode::SetProperty(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Slot(2)),
//             ByteCodeTest::Code(SymbolicByteCode::Drop),
//             ByteCodeTest::Code(SymbolicByteCode::GetLocal(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Return),
//           ],
//         )),
//         ByteCodeTest::Code(SymbolicByteCode::Method(2)),
//         ByteCodeTest::Code(SymbolicByteCode::Field(4)),
//         ByteCodeTest::Code(SymbolicByteCode::Drop),
//         ByteCodeTest::Code(SymbolicByteCode::Drop),
//         ByteCodeTest::Code(SymbolicByteCode::Nil),
//         ByteCodeTest::Code(SymbolicByteCode::Return),
//       ],
//     );
//   }

//   #[test]
//   fn class_instance_access_assign_set() {
//     let example = "
//     class A {
//       init() {
//         @a = 10;
//         @a /= 5;
//       }
//     }
//   ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_fun_bytecode(
//       &fun,
//       4,
//       &vec![
//         ByteCodeTest::Code(SymbolicByteCode::Class(0)),
//         ByteCodeTest::Code(SymbolicByteCode::DefineGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::GetGlobal(1)),
//         ByteCodeTest::Code(SymbolicByteCode::GetGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::Inherit),
//         ByteCodeTest::Fun((
//           3,
//           4,
//           vec![
//             ByteCodeTest::Code(SymbolicByteCode::GetLocal(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Constant(1)),
//             ByteCodeTest::Code(SymbolicByteCode::SetProperty(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Slot(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Drop),
//             ByteCodeTest::Code(SymbolicByteCode::GetLocal(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Dup),
//             ByteCodeTest::Code(SymbolicByteCode::GetProperty(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Slot(1)),
//             ByteCodeTest::Code(SymbolicByteCode::Constant(2)),
//             ByteCodeTest::Code(SymbolicByteCode::Divide),
//             ByteCodeTest::Code(SymbolicByteCode::SetProperty(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Slot(2)),
//             ByteCodeTest::Code(SymbolicByteCode::Drop),
//             ByteCodeTest::Code(SymbolicByteCode::GetLocal(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Return),
//           ],
//         )),
//         ByteCodeTest::Code(SymbolicByteCode::Method(2)),
//         ByteCodeTest::Code(SymbolicByteCode::Field(4)),
//         ByteCodeTest::Code(SymbolicByteCode::Drop),
//         ByteCodeTest::Code(SymbolicByteCode::Drop),
//         ByteCodeTest::Code(SymbolicByteCode::Nil),
//         ByteCodeTest::Code(SymbolicByteCode::Return),
//       ],
//     );
//   }

//   #[test]
//   fn class_with_static_methods() {
//     let example = "
//       class A {
//         static sayHi() {
//           return 'hi';
//         }

//         static sayBye() {
//           return 'bye';
//         }
//       }
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_fun_bytecode(
//       &fun,
//       4,
//       &vec![
//         ByteCodeTest::Code(SymbolicByteCode::Class(0)),
//         ByteCodeTest::Code(SymbolicByteCode::DefineGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::GetGlobal(1)),
//         ByteCodeTest::Code(SymbolicByteCode::GetGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::Inherit),
//         ByteCodeTest::Fun((
//           3,
//           2,
//           vec![
//             ByteCodeTest::Code(SymbolicByteCode::Constant(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Return),
//           ],
//         )),
//         ByteCodeTest::Code(SymbolicByteCode::StaticMethod(2)),
//         ByteCodeTest::Fun((
//           5,
//           2,
//           vec![
//             ByteCodeTest::Code(SymbolicByteCode::Constant(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Return),
//           ],
//         )),
//         ByteCodeTest::Code(SymbolicByteCode::StaticMethod(4)),
//         ByteCodeTest::Code(SymbolicByteCode::Drop),
//         ByteCodeTest::Code(SymbolicByteCode::Drop),
//         ByteCodeTest::Code(SymbolicByteCode::Nil),
//         ByteCodeTest::Code(SymbolicByteCode::Return),
//       ],
//     );
//   }

//   #[test]
//   fn class_with_captures() {
//     let example = "
//       class A {
//         foo() {
//           fn bar(a) {
//             return [self, a];
//           }

//           return bar;
//         }
//       }
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_fun_bytecode(
//       &fun,
//       4,
//       &vec![
//         ByteCodeTest::Code(SymbolicByteCode::Class(0)),
//         ByteCodeTest::Code(SymbolicByteCode::DefineGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::GetGlobal(1)),
//         ByteCodeTest::Code(SymbolicByteCode::GetGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::Inherit),
//         ByteCodeTest::Fun((
//           3,
//           3,
//           vec![
//             ByteCodeTest::Code(SymbolicByteCode::Box(0)),
//             ByteCodeTest::Fun((
//               0,
//               3,
//               vec![
//                 ByteCodeTest::Code(SymbolicByteCode::GetCapture(0)),
//                 ByteCodeTest::Code(SymbolicByteCode::GetLocal(1)),
//                 ByteCodeTest::Code(SymbolicByteCode::List(2)),
//                 ByteCodeTest::Code(SymbolicByteCode::Return),
//               ],
//             )),
//             ByteCodeTest::Code(SymbolicByteCode::CaptureIndex(CaptureIndex::Local(0))),
//             ByteCodeTest::Code(SymbolicByteCode::GetLocal(1)),
//             ByteCodeTest::Code(SymbolicByteCode::Return),
//           ],
//         )),
//         ByteCodeTest::Code(SymbolicByteCode::Method(2)),
//         ByteCodeTest::Code(SymbolicByteCode::Drop),
//         ByteCodeTest::Code(SymbolicByteCode::Drop),
//         ByteCodeTest::Code(SymbolicByteCode::Nil),
//         ByteCodeTest::Code(SymbolicByteCode::Return),
//       ],
//     );
//   }

//   #[test]
//   fn class_instance_access_with_captures() {
//     let example = "
//       class A {
//         init() {
//           @a = true;
//         }

//         foo() {
//           fn bar() { @a }

//           return bar;
//         }
//       }
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_fun_bytecode(
//       &fun,
//       4,
//       &vec![
//         ByteCodeTest::Code(SymbolicByteCode::Class(0)),
//         ByteCodeTest::Code(SymbolicByteCode::DefineGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::GetGlobal(1)),
//         ByteCodeTest::Code(SymbolicByteCode::GetGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::Inherit),
//         ByteCodeTest::Fun((
//           3,
//           3,
//           vec![
//             ByteCodeTest::Code(SymbolicByteCode::GetLocal(0)),
//             ByteCodeTest::Code(SymbolicByteCode::True),
//             ByteCodeTest::Code(SymbolicByteCode::SetProperty(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Slot(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Drop),
//             ByteCodeTest::Code(SymbolicByteCode::GetLocal(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Return),
//           ],
//         )),
//         ByteCodeTest::Code(SymbolicByteCode::Method(2)),
//         ByteCodeTest::Code(SymbolicByteCode::Field(4)),
//         ByteCodeTest::Fun((
//           6,
//           3,
//           vec![
//             ByteCodeTest::Code(SymbolicByteCode::Box(0)),
//             ByteCodeTest::Fun((
//               0,
//               2,
//               vec![
//                 ByteCodeTest::Code(SymbolicByteCode::GetCapture(0)),
//                 ByteCodeTest::Code(SymbolicByteCode::GetProperty(0)),
//                 ByteCodeTest::Code(SymbolicByteCode::Slot(1)),
//                 ByteCodeTest::Code(SymbolicByteCode::Return),
//               ],
//             )),
//             ByteCodeTest::Code(SymbolicByteCode::CaptureIndex(CaptureIndex::Local(0))),
//             ByteCodeTest::Code(SymbolicByteCode::GetLocal(1)),
//             ByteCodeTest::Code(SymbolicByteCode::Return),
//           ],
//         )),
//         ByteCodeTest::Code(SymbolicByteCode::Method(5)),
//         ByteCodeTest::Code(SymbolicByteCode::Drop),
//         ByteCodeTest::Code(SymbolicByteCode::Drop),
//         ByteCodeTest::Code(SymbolicByteCode::Nil),
//         ByteCodeTest::Code(SymbolicByteCode::Return),
//       ],
//     );
//   }

//   #[test]
//   fn launch() {
//     let example = "
//       launch print();
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       2,
//       &vec![
//         SymbolicByteCode::GetGlobal(0),
//         SymbolicByteCode::Launch(0),
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn channel_send_index() {
//     let example = "
//       let a = [chan()];
//       a[0] <- 5;
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       4,
//       &vec![
//         SymbolicByteCode::Channel,
//         SymbolicByteCode::List(1),
//         SymbolicByteCode::DefineGlobal(0),
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::GetGlobal(0),
//         SymbolicByteCode::Constant(2),
//         SymbolicByteCode::Invoke((3, 1)),
//         SymbolicByteCode::Slot(0),
//         SymbolicByteCode::Send,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn channel_send_property() {
//     let example = "
//       let b = 10;
//       b.b <- 5;
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       3,
//       &vec![
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::DefineGlobal(0),
//         SymbolicByteCode::Constant(2),
//         SymbolicByteCode::GetGlobal(0),
//         SymbolicByteCode::GetProperty(0),
//         SymbolicByteCode::Slot(0),
//         SymbolicByteCode::Send,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn channel_send_variable() {
//     let example = "
//       let b = chan();
//       b <- 5;
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       3,
//       &vec![
//         SymbolicByteCode::Channel,
//         SymbolicByteCode::DefineGlobal(0),
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::GetGlobal(0),
//         SymbolicByteCode::Send,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn list_index_set() {
//     let example = "
//       let a = [print, print, print];
//       a[1] = 5;
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       4,
//       &vec![
//         SymbolicByteCode::GetGlobal(1),
//         SymbolicByteCode::GetGlobal(1),
//         SymbolicByteCode::GetGlobal(1),
//         SymbolicByteCode::List(3),
//         SymbolicByteCode::DefineGlobal(0),
//         SymbolicByteCode::GetGlobal(0),
//         SymbolicByteCode::Constant(2),
//         SymbolicByteCode::Constant(3),
//         SymbolicByteCode::Invoke((4, 2)),
//         SymbolicByteCode::Slot(0),
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn list_index_get() {
//     let example = "
//       let a = [\"john\", \"joe\", \"jim\"];
//       print(a[1]);
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       4,
//       &vec![
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::Constant(2),
//         SymbolicByteCode::Constant(3),
//         SymbolicByteCode::List(3),
//         SymbolicByteCode::DefineGlobal(0),
//         SymbolicByteCode::GetGlobal(4),
//         SymbolicByteCode::GetGlobal(0),
//         SymbolicByteCode::Constant(5),
//         SymbolicByteCode::Invoke((6, 1)),
//         SymbolicByteCode::Slot(0),
//         SymbolicByteCode::Call(1),
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn list_index_assign_set() {
//     let example = "
//     let a = [1, 2, 3];
//     a[1] += 5;
//   ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       4,
//       &vec![
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::Constant(2),
//         SymbolicByteCode::Constant(3),
//         SymbolicByteCode::List(3),
//         SymbolicByteCode::DefineGlobal(0),
//         SymbolicByteCode::GetGlobal(0),
//         SymbolicByteCode::Dup,
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::Invoke((4, 1)),
//         SymbolicByteCode::Slot(0),
//         SymbolicByteCode::Constant(5),
//         SymbolicByteCode::Add,
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::Invoke((6, 2)),
//         SymbolicByteCode::Slot(1),
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn list_initializer() {
//     let example = "
//       let a = [1, 2, nil, false, \"cat\"];
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       6,
//       &vec![
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::Constant(2),
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::False,
//         SymbolicByteCode::Constant(3),
//         SymbolicByteCode::List(5),
//         SymbolicByteCode::DefineGlobal(0),
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn list_empty() {
//     let example = "
//       let a = [];
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       2,
//       &vec![
//         SymbolicByteCode::List(0),
//         SymbolicByteCode::DefineGlobal(0),
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn tuple_initializer() {
//     let example = "
//       let a = (1, 2, nil, false, \"cat\");
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       6,
//       &vec![
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::Constant(2),
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::False,
//         SymbolicByteCode::Constant(3),
//         SymbolicByteCode::Tuple(5),
//         SymbolicByteCode::DefineGlobal(0),
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn tuple_empty() {
//     let example = "
//       let a = ();
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       2,
//       &vec![
//         SymbolicByteCode::Tuple(0),
//         SymbolicByteCode::DefineGlobal(0),
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn map_initializer() {
//     let example = "
//       let a = {
//         \"key1\": 10,
//         \"key2\": nil,
//       };
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       5,
//       &vec![
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::Constant(2),
//         SymbolicByteCode::Constant(3),
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Map(2),
//         SymbolicByteCode::DefineGlobal(0),
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn map_empty() {
//     let example = "
//       let a = {};
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       2,
//       &vec![
//         SymbolicByteCode::Map(0),
//         SymbolicByteCode::DefineGlobal(0),
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn lambda_expression_body() {
//     let example = "
//     let example = || 10;
//     example();
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_fun_bytecode(
//       &fun,
//       2,
//       &vec![
//         ByteCodeTest::Fun((
//           // example
//           1,
//           2,
//           vec![
//             ByteCodeTest::Code(SymbolicByteCode::Constant(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Return),
//           ],
//         )),
//         ByteCodeTest::Code(SymbolicByteCode::DefineGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::GetGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::Call(0)),
//         ByteCodeTest::Code(SymbolicByteCode::Drop),
//         ByteCodeTest::Code(SymbolicByteCode::Nil),
//         ByteCodeTest::Code(SymbolicByteCode::Return),
//       ],
//     );
//   }

//   #[test]
//   fn lambda_block_body() {
//     let example = "
//     let example = || { return 10; };
//     example();
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_fun_bytecode(
//       &fun,
//       2,
//       &vec![
//         ByteCodeTest::Fun((
//           // example
//           1,
//           2,
//           vec![
//             ByteCodeTest::Code(SymbolicByteCode::Constant(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Return),
//           ],
//         )),
//         ByteCodeTest::Code(SymbolicByteCode::DefineGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::GetGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::Call(0)),
//         ByteCodeTest::Code(SymbolicByteCode::Drop),
//         ByteCodeTest::Code(SymbolicByteCode::Nil),
//         ByteCodeTest::Code(SymbolicByteCode::Return),
//       ],
//     );
//   }

//   #[test]
//   fn fn_with_variables() {
//     let example = "
//     fn example(a, b, c) {
//       return a + b + c;
//     }
//     example(1, 2, 3);
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_fun_bytecode(
//       &fun,
//       5,
//       &vec![
//         ByteCodeTest::Fun((
//           // example
//           1,
//           3,
//           vec![
//             ByteCodeTest::Code(SymbolicByteCode::GetLocal(1)),
//             ByteCodeTest::Code(SymbolicByteCode::GetLocal(2)),
//             ByteCodeTest::Code(SymbolicByteCode::Add),
//             ByteCodeTest::Code(SymbolicByteCode::GetLocal(3)),
//             ByteCodeTest::Code(SymbolicByteCode::Add),
//             ByteCodeTest::Code(SymbolicByteCode::Return),
//           ],
//         )),
//         ByteCodeTest::Code(SymbolicByteCode::DefineGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::GetGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::Constant(2)),
//         ByteCodeTest::Code(SymbolicByteCode::Constant(3)),
//         ByteCodeTest::Code(SymbolicByteCode::Constant(4)),
//         ByteCodeTest::Code(SymbolicByteCode::Call(3)),
//         ByteCodeTest::Code(SymbolicByteCode::Drop),
//         ByteCodeTest::Code(SymbolicByteCode::Nil),
//         ByteCodeTest::Code(SymbolicByteCode::Return),
//       ],
//     );
//   }

//   #[test]
//   fn open_capture() {
//     let example = "
//     fn example() {
//       let x = 0;
//       fn middle() {
//         fn inner() {
//           return x;
//         }

//         return inner();
//       }

//       return middle();
//     }
//     example();
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_fun_bytecode(
//       &fun,
//       2,
//       &vec![
//         ByteCodeTest::Fun((
//           // example
//           1,
//           4,
//           vec![
//             ByteCodeTest::Code(SymbolicByteCode::EmptyBox),
//             ByteCodeTest::Code(SymbolicByteCode::Constant(1)),
//             ByteCodeTest::Code(SymbolicByteCode::FillBox),
//             ByteCodeTest::Fun((
//               // middle
//               2,
//               3,
//               vec![
//                 ByteCodeTest::Fun((
//                   // inner
//                   0,
//                   2,
//                   vec![
//                     ByteCodeTest::Code(SymbolicByteCode::GetCapture(0)),
//                     ByteCodeTest::Code(SymbolicByteCode::Return),
//                   ],
//                 )),
//                 ByteCodeTest::Code(SymbolicByteCode::CaptureIndex(CaptureIndex::Enclosing(0))),
//                 ByteCodeTest::Code(SymbolicByteCode::GetLocal(1)),
//                 ByteCodeTest::Code(SymbolicByteCode::Call(0)),
//                 ByteCodeTest::Code(SymbolicByteCode::Return),
//               ],
//             )),
//             ByteCodeTest::Code(SymbolicByteCode::CaptureIndex(CaptureIndex::Local(1))), //
//             ByteCodeTest::Code(SymbolicByteCode::GetLocal(2)),
//             ByteCodeTest::Code(SymbolicByteCode::Call(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Return),
//           ],
//         )),
//         ByteCodeTest::Code(SymbolicByteCode::DefineGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::GetGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::Call(0)),
//         ByteCodeTest::Code(SymbolicByteCode::Drop),
//         ByteCodeTest::Code(SymbolicByteCode::Nil),
//         ByteCodeTest::Code(SymbolicByteCode::Return),
//       ],
//     );
//   }

//   #[test]
//   fn close_capture() {
//     let example = "
//     fn example() {
//       let a = 10;
//       fn inner() {
//         return a;
//       }
//       return inner;
//     }
//     let inner = example();
//     inner();
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_fun_bytecode(
//       &fun,
//       2,
//       &vec![
//         ByteCodeTest::Fun((
//           1,
//           4,
//           vec![
//             ByteCodeTest::Code(SymbolicByteCode::EmptyBox),
//             ByteCodeTest::Code(SymbolicByteCode::Constant(1)),
//             ByteCodeTest::Code(SymbolicByteCode::FillBox),
//             ByteCodeTest::Fun((
//               2,
//               2,
//               vec![
//                 ByteCodeTest::Code(SymbolicByteCode::GetCapture(0)),
//                 ByteCodeTest::Code(SymbolicByteCode::Return),
//               ],
//             )),
//             ByteCodeTest::Code(SymbolicByteCode::CaptureIndex(CaptureIndex::Local(1))),
//             ByteCodeTest::Code(SymbolicByteCode::GetLocal(2)),
//             ByteCodeTest::Code(SymbolicByteCode::Return),
//           ],
//         )),
//         ByteCodeTest::Code(SymbolicByteCode::DefineGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::GetGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::Call(0)),
//         ByteCodeTest::Code(SymbolicByteCode::DefineGlobal(2)),
//         ByteCodeTest::Code(SymbolicByteCode::GetGlobal(2)),
//         ByteCodeTest::Code(SymbolicByteCode::Call(0)),
//         ByteCodeTest::Code(SymbolicByteCode::Drop),
//         ByteCodeTest::Code(SymbolicByteCode::Nil),
//         ByteCodeTest::Code(SymbolicByteCode::Return),
//       ],
//     );
//   }

//   #[test]
//   fn empty_fun() {
//     let example = "fn example() {} example();";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_fun_bytecode(
//       &fun,
//       2,
//       &vec![
//         ByteCodeTest::Fun((
//           1,
//           2,
//           vec![
//             ByteCodeTest::Code(SymbolicByteCode::Nil),
//             ByteCodeTest::Code(SymbolicByteCode::Return),
//           ],
//         )),
//         ByteCodeTest::Code(SymbolicByteCode::DefineGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::GetGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::Call(0)),
//         ByteCodeTest::Code(SymbolicByteCode::Drop),
//         ByteCodeTest::Code(SymbolicByteCode::Nil),
//         ByteCodeTest::Code(SymbolicByteCode::Return),
//       ],
//     );
//   }

//   #[test]
//   fn param_fun() {
//     let example = "
//     fn example(a) {
//       return a;
//     }
//     let a = 1;
//     example(a);
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_fun_bytecode(
//       &fun,
//       3,
//       &vec![
//         ByteCodeTest::Fun((
//           1,
//           2,
//           vec![
//             ByteCodeTest::Code(SymbolicByteCode::GetLocal(1)),
//             ByteCodeTest::Code(SymbolicByteCode::Return),
//           ],
//         )),
//         ByteCodeTest::Code(SymbolicByteCode::DefineGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::Constant(3)),
//         ByteCodeTest::Code(SymbolicByteCode::DefineGlobal(2)),
//         ByteCodeTest::Code(SymbolicByteCode::GetGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::GetGlobal(2)),
//         ByteCodeTest::Code(SymbolicByteCode::Call(1)),
//         ByteCodeTest::Code(SymbolicByteCode::Drop),
//         ByteCodeTest::Code(SymbolicByteCode::Nil),
//         ByteCodeTest::Code(SymbolicByteCode::Return),
//       ],
//     );
//   }

//   #[test]
//   fn empty_fun_basic() {
//     let example = "fn example() { let a = 10; return a; } example();";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_fun_bytecode(
//       &fun,
//       2,
//       &vec![
//         ByteCodeTest::Fun((
//           1,
//           3,
//           vec![
//             ByteCodeTest::Code(SymbolicByteCode::Constant(1)),
//             ByteCodeTest::Code(SymbolicByteCode::GetLocal(1)),
//             ByteCodeTest::Code(SymbolicByteCode::Return),
//           ],
//         )),
//         ByteCodeTest::Code(SymbolicByteCode::DefineGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::GetGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::Call(0)),
//         ByteCodeTest::Code(SymbolicByteCode::Drop),
//         ByteCodeTest::Code(SymbolicByteCode::Nil),
//         ByteCodeTest::Code(SymbolicByteCode::Return),
//       ],
//     );
//   }

//   #[test]
//   fn implicit_return() {
//     let example = "fn example() { 10 } example();";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_fun_bytecode(
//       &fun,
//       2,
//       &vec![
//         ByteCodeTest::Fun((
//           1,
//           2,
//           vec![
//             ByteCodeTest::Code(SymbolicByteCode::Constant(0)),
//             ByteCodeTest::Code(SymbolicByteCode::Return),
//           ],
//         )),
//         ByteCodeTest::Code(SymbolicByteCode::DefineGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::GetGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::Call(0)),
//         ByteCodeTest::Code(SymbolicByteCode::Drop),
//         ByteCodeTest::Code(SymbolicByteCode::Nil),
//         ByteCodeTest::Code(SymbolicByteCode::Return),
//       ],
//     );
//   }

//   #[test]
//   fn map() {
//     let example = "let a = { \"cat\": \"bat\", 10: nil };";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       5,
//       &vec![
//         SymbolicByteCode::Constant(1),     // 1
//         SymbolicByteCode::Constant(2),     // 3
//         SymbolicByteCode::Constant(3),     // 5
//         SymbolicByteCode::Nil,             // 7
//         SymbolicByteCode::Map(2),          // 8
//         SymbolicByteCode::DefineGlobal(0), // 11
//         SymbolicByteCode::Nil,             // 13
//         SymbolicByteCode::Return,          // 14
//       ],
//     );
//   }

//   #[test]
//   fn list() {
//     let example = "let a = [1, 2, 3, \"cat\"];";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       5,
//       &vec![
//         SymbolicByteCode::Constant(1),     // 1
//         SymbolicByteCode::Constant(2),     // 3
//         SymbolicByteCode::Constant(3),     // 5
//         SymbolicByteCode::Constant(4),     // 7
//         SymbolicByteCode::List(4),         // 9
//         SymbolicByteCode::DefineGlobal(0), // 12
//         SymbolicByteCode::Nil,             // 14
//         SymbolicByteCode::Return,          // 15
//       ],
//     );
//   }

//   #[test]
//   fn channel() {
//     let example = "
//     let a = chan(5);
//     let b = chan();
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       2,
//       &vec![
//         SymbolicByteCode::Constant(1),     // 1
//         SymbolicByteCode::BufferedChannel, // 3
//         SymbolicByteCode::DefineGlobal(0), // 4
//         SymbolicByteCode::Channel,         // 6
//         SymbolicByteCode::DefineGlobal(2), // 7
//         SymbolicByteCode::Nil,             // 9
//         SymbolicByteCode::Return,          // 10
//       ],
//     );
//   }

//   #[test]
//   fn for_loop() {
//     let example = "for x in [1, 2, 3] { print(x); }";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       5,
//       &vec![
//         SymbolicByteCode::Constant(0),     // 1   local 1 = [1, 2, 3].iter()
//         SymbolicByteCode::Constant(1),     // 3   local 2 =
//         SymbolicByteCode::Constant(2),     // 5
//         SymbolicByteCode::List(3),         // 7
//         SymbolicByteCode::Invoke((3, 0)),  // 10  const 1 = 1
//         SymbolicByteCode::Slot(0),         // 10  const 1 = 1
//         SymbolicByteCode::Nil,             // 0
//         SymbolicByteCode::GetLocal(1),     // 13  const 2 = 2
//         SymbolicByteCode::IterNext(4),     // 15  const 3 = 3
//         SymbolicByteCode::JumpIfFalse(19), // 17  const 4 = "iter"
//         SymbolicByteCode::GetLocal(1),     // 21
//         SymbolicByteCode::IterCurrent(5),  // 23  const 6 = "current"
//         SymbolicByteCode::SetLocal(2),     // 25
//         SymbolicByteCode::Drop,            // 27
//         SymbolicByteCode::GetGlobal(6),
//         SymbolicByteCode::GetLocal(2), // 29
//         SymbolicByteCode::Call(1),
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Loop(27), // 32
//         SymbolicByteCode::DropN(2), // 36
//         SymbolicByteCode::Nil,      // 38
//         SymbolicByteCode::Return,   // 39
//       ],
//     );
//   }

//   #[test]
//   fn for_loop_with_locals() {
//     let example = "for x in [1, 2, 3] { let x = 10; let y = 10; }";
//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       5,
//       &vec![
//         SymbolicByteCode::Constant(0),     // 1   local 1 = [1, 2, 3].iter()
//         SymbolicByteCode::Constant(1),     // 3   local 2 =
//         SymbolicByteCode::Constant(2),     // 5
//         SymbolicByteCode::List(3),         // 7
//         SymbolicByteCode::Invoke((3, 0)),  // 10  const 1 = 1
//         SymbolicByteCode::Slot(0),         // 10  const 1 = 1
//         SymbolicByteCode::Nil,             // 0
//         SymbolicByteCode::GetLocal(1),     // 13  const 2 = 2
//         SymbolicByteCode::IterNext(4),     // 15  const 3 = 3
//         SymbolicByteCode::JumpIfFalse(17), // 17  const 4 = "iter"
//         SymbolicByteCode::GetLocal(1),     // 21
//         SymbolicByteCode::IterCurrent(5),  // 23  const 6 = "current"
//         SymbolicByteCode::SetLocal(2),     // 25
//         SymbolicByteCode::Drop,            // 27
//         SymbolicByteCode::Constant(7),
//         SymbolicByteCode::Constant(7), // 29
//         SymbolicByteCode::DropN(2),
//         SymbolicByteCode::Loop(25), // 32
//         SymbolicByteCode::DropN(2), // 36
//         SymbolicByteCode::Nil,      // 38
//         SymbolicByteCode::Return,   // 39
//       ],
//     );
//   }

//   #[test]
//   fn while_loop() {
//     let example = "while true { print(10); }";
//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       3,
//       &vec![
//         SymbolicByteCode::True,
//         SymbolicByteCode::JumpIfFalse(11),
//         SymbolicByteCode::GetGlobal(0),
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::Call(1),
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Loop(15),
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn while_loop_with_locals() {
//     let example = "while true { let x = 10; let y = 10; }";
//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       3,
//       &vec![
//         SymbolicByteCode::True,
//         SymbolicByteCode::JumpIfFalse(9),
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::DropN(2),
//         SymbolicByteCode::Loop(13),
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn break_() {
//     let example = "while true { break; print(10); }";
//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       2,
//       &vec![
//         SymbolicByteCode::True,
//         SymbolicByteCode::JumpIfFalse(6),
//         SymbolicByteCode::Jump(3),
//         SymbolicByteCode::Loop(10),
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn break_fun_nested() {
//     let example = "
//       fn f() {
//         while true { break; let x; }
//       }
//     ";
//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_fun_bytecode(
//       &fun,
//       2,
//       &vec![
//         ByteCodeTest::Fun((
//           1,
//           2,
//           vec![
//             ByteCodeTest::Code(SymbolicByteCode::True),
//             ByteCodeTest::Code(SymbolicByteCode::JumpIfFalse(6)),
//             ByteCodeTest::Code(SymbolicByteCode::Jump(3)),
//             ByteCodeTest::Code(SymbolicByteCode::Loop(10)),
//             ByteCodeTest::Code(SymbolicByteCode::Nil),
//             ByteCodeTest::Code(SymbolicByteCode::Return),
//           ],
//         )),
//         ByteCodeTest::Code(SymbolicByteCode::DefineGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::Nil),
//         ByteCodeTest::Code(SymbolicByteCode::Return),
//       ],
//     );
//   }

//   #[test]
//   fn continue_() {
//     let example = "while true { continue; print(10); }";
//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       2,
//       &vec![
//         SymbolicByteCode::True,
//         SymbolicByteCode::JumpIfFalse(6),
//         SymbolicByteCode::Loop(7),
//         SymbolicByteCode::Loop(10),
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn continue_fun_nested() {
//     let example = "
//       fn f() {
//         while true { continue; let x; }
//       }
//     ";
//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_fun_bytecode(
//       &fun,
//       2,
//       &vec![
//         ByteCodeTest::Fun((
//           1,
//           2,
//           vec![
//             ByteCodeTest::Code(SymbolicByteCode::True),
//             ByteCodeTest::Code(SymbolicByteCode::JumpIfFalse(6)),
//             ByteCodeTest::Code(SymbolicByteCode::Loop(7)),
//             ByteCodeTest::Code(SymbolicByteCode::Loop(10)),
//             ByteCodeTest::Code(SymbolicByteCode::Nil),
//             ByteCodeTest::Code(SymbolicByteCode::Return),
//           ],
//         )),
//         ByteCodeTest::Code(SymbolicByteCode::DefineGlobal(0)),
//         ByteCodeTest::Code(SymbolicByteCode::Nil),
//         ByteCodeTest::Code(SymbolicByteCode::Return),
//       ],
//     );
//   }

//   #[test]
//   fn and_operator() {
//     let example = "true and false;";
//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       2,
//       &vec![
//         SymbolicByteCode::True,
//         SymbolicByteCode::And(1),
//         SymbolicByteCode::False,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn or_operator() {
//     let example = "false or true;";
//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       2,
//       &vec![
//         SymbolicByteCode::False,
//         SymbolicByteCode::Or(1),
//         SymbolicByteCode::True,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn if_condition() {
//     let example = "if (3 < 10) { print(\"hi\"); }";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       3,
//       &vec![
//         SymbolicByteCode::Constant(0),
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::Less,
//         SymbolicByteCode::JumpIfFalse(8),
//         SymbolicByteCode::GetGlobal(2),
//         SymbolicByteCode::Constant(3),
//         SymbolicByteCode::Call(1),
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn if_else_condition() {
//     let example = "if (3 < 10) { print(\"hi\"); } else { print(\"bye\"); }";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       3,
//       &vec![
//         SymbolicByteCode::Constant(0),     // 0
//         SymbolicByteCode::Constant(1),     // 2
//         SymbolicByteCode::Less,            // 4
//         SymbolicByteCode::JumpIfFalse(11), // 5
//         SymbolicByteCode::GetGlobal(2),
//         SymbolicByteCode::Constant(3), // 9
//         SymbolicByteCode::Call(1),     // 11
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Jump(8), // 12
//         SymbolicByteCode::GetGlobal(2),
//         SymbolicByteCode::Constant(4), // 17
//         SymbolicByteCode::Call(1),
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,    // 19
//         SymbolicByteCode::Return, // 20
//       ],
//     );
//   }

//   #[test]
//   fn declare_local() {
//     let example = "if true { let x = 10; }";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_simple_bytecode(
//       &fun,
//       2,
//       &vec![
//         SymbolicByteCode::True,
//         SymbolicByteCode::JumpIfFalse(3),
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn op_get_local() {
//     let example = "if true { let x = 10; print(x); }";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_simple_bytecode(
//       &fun,
//       4,
//       &vec![
//         SymbolicByteCode::True,
//         SymbolicByteCode::JumpIfFalse(11),
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::GetGlobal(2),
//         SymbolicByteCode::GetLocal(1),
//         SymbolicByteCode::Call(1),
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn op_set_local() {
//     let example = "if true { let x = 10; x = 5; }";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       3,
//       &vec![
//         SymbolicByteCode::True,
//         SymbolicByteCode::JumpIfFalse(8),
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::Constant(2),
//         SymbolicByteCode::SetLocal(1),
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn op_define_global_nil() {
//     let example = "let x;";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       2,
//       &vec![
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::DefineGlobal(0),
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn op_define_global_val() {
//     let example = "let x = 10;";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);

//     assert_simple_bytecode(
//       &fun,
//       2,
//       &vec![
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::DefineGlobal(0),
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn op_get_global() {
//     let example = "
//       let x = 10;
//       print(x);
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_simple_bytecode(
//       &fun,
//       3,
//       &vec![
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::DefineGlobal(0),
//         SymbolicByteCode::GetGlobal(2),
//         SymbolicByteCode::GetGlobal(0),
//         SymbolicByteCode::Call(1),
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn op_set_global() {
//     let example = "
//       let x;
//       x = \"cat\";
//     ";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_simple_bytecode(
//       &fun,
//       2,
//       &vec![
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::DefineGlobal(0),
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::SetGlobal(0),
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn op_pop() {
//     let example = "false;";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_simple_bytecode(
//       &fun,
//       2,
//       &vec![
//         SymbolicByteCode::False,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn op_return() {
//     let example = "";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_simple_bytecode(
//       &fun,
//       2,
//       &vec![SymbolicByteCode::Nil, SymbolicByteCode::Return],
//     );
//   }

//   #[test]
//   fn op_number() {
//     let example = "5.18;";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_simple_bytecode(
//       &fun,
//       2,
//       &vec![
//         SymbolicByteCode::Constant(0),
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn op_string() {
//     let example = "\"example\";";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_simple_bytecode(
//       &fun,
//       2,
//       &vec![
//         SymbolicByteCode::Constant(0),
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn op_interpolate() {
//     let example = "\"${10} ${\"stuff\"}\";";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_simple_bytecode(
//       &fun,
//       6,
//       &vec![
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::Constant(2),
//         SymbolicByteCode::Invoke((0, 0)),
//         SymbolicByteCode::Slot(0),
//         SymbolicByteCode::Constant(3),
//         SymbolicByteCode::Constant(4),
//         SymbolicByteCode::Invoke((0, 0)),
//         SymbolicByteCode::Slot(1),
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::Interpolate(5),
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn op_false() {
//     let example = "false;";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_simple_bytecode(
//       &fun,
//       2,
//       &vec![
//         SymbolicByteCode::False,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn op_true() {
//     let example = "true;";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_simple_bytecode(
//       &fun,
//       2,
//       &vec![
//         SymbolicByteCode::True,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn op_channel() {
//     let example = "chan();";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_simple_bytecode(
//       &fun,
//       2,
//       &vec![
//         SymbolicByteCode::Channel,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn op_buffered_channel() {
//     let example = "chan(5);";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_simple_bytecode(
//       &fun,
//       2,
//       &vec![
//         SymbolicByteCode::Constant(0),
//         SymbolicByteCode::BufferedChannel,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn op_nil() {
//     let example = "nil;";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_simple_bytecode(
//       &fun,
//       2,
//       &vec![
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn op_not() {
//     let example = "!false;";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_simple_bytecode(
//       &fun,
//       2,
//       &vec![
//         SymbolicByteCode::False,
//         SymbolicByteCode::Not,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn op_negate() {
//     let example = "-(15);";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_simple_bytecode(
//       &fun,
//       2,
//       &vec![
//         SymbolicByteCode::Constant(0),
//         SymbolicByteCode::Negate,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn ternary() {
//     let example = "10 > 5 ? \"example\" : nil;";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_simple_bytecode(
//       &fun,
//       3,
//       &vec![
//         SymbolicByteCode::Constant(0),
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::Greater,
//         SymbolicByteCode::JumpIfFalse(5),
//         SymbolicByteCode::Constant(2),
//         SymbolicByteCode::Jump(1),
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn op_receive() {
//     let example = "<- true;";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_simple_bytecode(
//       &fun,
//       2,
//       &vec![
//         SymbolicByteCode::True,
//         SymbolicByteCode::Receive,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn op_add() {
//     let example = "10 + 4;";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_simple_bytecode(
//       &fun,
//       3,
//       &vec![
//         SymbolicByteCode::Constant(0),
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::Add,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn op_subtract() {
//     let example = "10 - 4;";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_simple_bytecode(
//       &fun,
//       3,
//       &vec![
//         SymbolicByteCode::Constant(0),
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::Subtract,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn op_divide() {
//     let example = "10 / 4;";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_simple_bytecode(
//       &fun,
//       3,
//       &vec![
//         SymbolicByteCode::Constant(0),
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::Divide,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn op_multi() {
//     let example = "10 * 4;";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_simple_bytecode(
//       &fun,
//       3,
//       &vec![
//         SymbolicByteCode::Constant(0),
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::Multiply,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn op_equal() {
//     let example = "true == nil;";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_simple_bytecode(
//       &fun,
//       3,
//       &vec![
//         SymbolicByteCode::True,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Equal,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn op_not_equal() {
//     let example = "true != nil;";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_simple_bytecode(
//       &fun,
//       3,
//       &vec![
//         SymbolicByteCode::True,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::NotEqual,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn op_less() {
//     let example = "3 < 5;";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_simple_bytecode(
//       &fun,
//       3,
//       &vec![
//         SymbolicByteCode::Constant(0),
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::Less,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn op_less_equal() {
//     let example = "3 <= 5;";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_simple_bytecode(
//       &fun,
//       3,
//       &vec![
//         SymbolicByteCode::Constant(0),
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::LessEqual,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn op_greater() {
//     let example = "3 > 5;";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_simple_bytecode(
//       &fun,
//       3,
//       &vec![
//         SymbolicByteCode::Constant(0),
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::Greater,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }

//   #[test]
//   fn op_greater_equal() {
//     let example = "3 >= 5;";

//     let context = NoContext::default();
//     let fun = test_compile(example, &context);
//     assert_simple_bytecode(
//       &fun,
//       3,
//       &vec![
//         SymbolicByteCode::Constant(0),
//         SymbolicByteCode::Constant(1),
//         SymbolicByteCode::GreaterEqual,
//         SymbolicByteCode::Drop,
//         SymbolicByteCode::Nil,
//         SymbolicByteCode::Return,
//       ],
//     );
//   }
// }
