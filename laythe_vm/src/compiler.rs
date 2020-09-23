use crate::{
  ast::{self, Decl, Expr, Primary, Stmt, Symbol},
  token::{Token, TokenKind},
};
use ast::{BinaryOp, FunBody, List, Map, Ranged, Trailer, UnaryOp};
use laythe_core::{
  chunk::{AlignedByteCode, Chunk, UpvalueIndex},
  constants::{ITER, ITER_VAR, SCRIPT, SELF, SUPER},
  hooks::GcHooks,
  module, object,
  object::FunKind,
  signature::Arity,
  val,
  value::Value,
};
use laythe_env::{
  io::Io,
  managed::{Manage, Managed, Trace},
  stdio::Stdio,
};
use object::{Fun, TryBlock};
use smol_str::SmolStr;
use std::{convert::TryInto, mem, ptr::NonNull};

#[cfg(feature = "debug")]
use crate::debug::disassemble_chunk;

/// The result of a compilation
pub type CompilerResult = Result<Managed<Fun>, ()>;

const UNINITIALIZED: i16 = -1;

#[derive(Debug, Clone)]
pub struct Local {
  /// name of the local
  name: Option<SmolStr>,

  /// depth of the local
  depth: i16,

  /// is this local captured
  is_captured: bool,
}

#[derive(Debug, Clone)]
pub struct ClassInfo {
  enclosing: Option<Managed<ClassInfo>>,
  fun_kind: Option<FunKind>,
  fields: Vec<SmolStr>,
  has_super_class: bool,
  name: Option<Token>,
}

impl Trace for ClassInfo {
  fn trace(&self) -> bool {
    if let Some(enclosing) = self.enclosing {
      enclosing.trace();
    };
    true
  }

  fn trace_debug(&self, stdio: &mut Stdio) -> bool {
    if let Some(enclosing) = self.enclosing {
      enclosing.trace_debug(stdio);
    };
    true
  }
}

impl Manage for ClassInfo {
  fn alloc_type(&self) -> &str {
    "class compiler"
  }

  fn debug(&self) -> String {
    format!("{:?}", self)
  }

  fn debug_free(&self) -> String {
    format!(
      "ClassCompiler: {{ has_super_class: {:?}, name: {:?}, enclosing: {{...}} }}",
      self.has_super_class, self.name
    )
  }

  fn size(&self) -> usize {
    mem::size_of::<Self>()
  }
}

pub struct Compiler<'a> {
  /// The environments io interface
  io: &'a Io,

  /// The current function
  fun: Managed<Fun>,

  /// The type the current function scope
  fun_kind: FunKind,

  /// The current module
  module: Managed<module::Module>,

  /// The parent compiler if it exists note uses
  /// unsafe pointer
  enclosing: Option<NonNull<Compiler<'a>>>,

  /// The current class class compiler
  current_class: Option<Managed<ClassInfo>>,

  /// hooks into the surround context. Used to allocate laythe objects
  hooks: &'a GcHooks<'a>,

  /// Number of locals
  local_count: usize,

  /// Current scope depth
  scope_depth: i16,

  /// locals in this function
  locals: Vec<Local>,

  /// upvalues in this function
  upvalues: Vec<UpvalueIndex>,

  /// A set of constants used in the current function
  constants: object::Map<Value, usize>,

  /// Was an error encountered during compilation
  had_error: bool,
}

impl<'a> Compiler<'a> {
  /// Create a new compiler at the module scope level. This struct will take a an ast
  /// produced by the parser and emit Laythe bytecode.
  ///
  /// # Examples
  /// ```
  /// use laythe_vm::compiler::Compiler;
  /// use laythe_core::module::Module;
  /// use laythe_core::object::Class;
  /// use laythe_core::hooks::support::TestContext;
  /// use laythe_core::hooks::GcHooks;
  /// use laythe_env::io::Io;
  /// use laythe_env::memory::Gc;
  /// use std::path::PathBuf;
  ///
  /// let context = TestContext::default();
  /// let hooks = GcHooks::new(&context);
  /// let io = Io::default();
  /// let module = hooks.manage(Module::new(
  ///  hooks.manage(Class::bare(hooks.manage_str("module"))),
  ///  hooks.manage(PathBuf::from("./module.ly"))
  /// ));
  ///
  /// let compiler = Compiler::new(module, &io, &hooks);
  /// ```
  pub fn new(module: Managed<module::Module>, io: &'a Io, hooks: &'a GcHooks<'a>) -> Self {
    let fun = hooks.manage(object::Fun::new(hooks.manage_str(SCRIPT), module));

    Self {
      fun,
      io,
      module,
      fun_kind: FunKind::Script,
      scope_depth: 0,
      current_class: None,
      hooks,
      enclosing: None,
      local_count: 1,
      locals: vec![
        Local {
          name: Option::None,
          depth: UNINITIALIZED,
          is_captured: false,
        };
        std::u8::MAX as usize
      ],
      upvalues: vec![UpvalueIndex::Local(0); std::u8::MAX as usize],
      constants: object::Map::default(),
      had_error: false,
    }
  }

  /// Compile the provided ast into managed function objects that
  /// contain the vm bytecode
  pub fn compile(mut self, module: &ast::Module) -> CompilerResult {
    for decl in &module.decls {
      self.decl(decl);
    }

    self.end_compiler(module.end());
    if !self.had_error {
      Ok(self.fun)
    } else {
      Err(())
    }
  }

  // create a child compiler to compile a function inside the enclosing module
  fn child(name: Managed<SmolStr>, fun_kind: FunKind, enclosing: &mut Compiler<'a>) -> Self {
    let mut child = Self {
      fun: enclosing.fun,
      io: enclosing.io,
      module: enclosing.module,
      fun_kind,
      scope_depth: 0,
      current_class: enclosing.current_class,
      hooks: enclosing.hooks,
      enclosing: Some(NonNull::from(enclosing)),
      local_count: 1,
      locals: vec![
        Local {
          name: Option::None,
          depth: UNINITIALIZED,
          is_captured: false,
        };
        std::u8::MAX as usize
      ],
      upvalues: vec![UpvalueIndex::Local(0); std::u8::MAX as usize],
      constants: object::Map::default(),
      had_error: false,
    };

    child.fun = child.hooks.manage(Fun::new(name, child.module));
    child.locals[0] = first_local(fun_kind);
    child
  }

  /// End this compilers compilation emitting a final return
  /// and shrinking the function to the correct size
  fn end_compiler(&mut self, line: u32) {
    self.emit_return(line);
    self.fun.shrink_to_fit(self.hooks);

    #[cfg(feature = "debug")]
    self.print_chunk();
  }

  /// Print the chunk if debug and an error occurred
  #[cfg(feature = "debug")]
  fn print_chunk(&mut self) {
    let mut stdio = &mut self.io.stdio();

    disassemble_chunk(&mut stdio, self.current_chunk(), &self.fun.name)
      .expect("could not write to stdio");
  }

  /// Emit byte code for a return
  fn emit_return(&mut self, line: u32) {
    match self.fun_kind {
      FunKind::Initializer => self.emit_byte(AlignedByteCode::GetLocal(0), line),
      _ => self.emit_byte(AlignedByteCode::Nil, line),
    }

    self.emit_byte(AlignedByteCode::Return, line);
  }

  /// scope the provide closure
  fn scope<T>(&mut self, end_line: u32, cb: impl FnOnce(&mut Self) -> T) -> T {
    self.begin_scope();
    let result = cb(self);
    self.end_scope(end_line);
    result
  }

  /// Increase the scope depth by 1
  fn begin_scope(&mut self) {
    self.scope_depth += 1;
  }

  /// Decrease the scope depth by 1
  fn end_scope(&mut self, end_line: u32) {
    self.scope_depth -= 1;

    let mut idx = self.local_count;
    let mut no_captures = true;

    while idx > 0 && self.locals[idx - 1].depth > self.scope_depth {
      no_captures = no_captures && !self.locals[idx - 1].is_captured;
      idx -= 1;
    }

    // if we didn't capture anything emit dropN instruction
    if no_captures {
      let dropped = self.local_count - idx;

      match dropped {
        0 => (),
        1 => self.emit_byte(AlignedByteCode::Drop, end_line),
        _ => self.emit_byte(AlignedByteCode::DropN(dropped as u8), end_line),
      }

      self.local_count = idx;
      return;
    }

    // otherwise emit normal drop and close upvalues
    while self.local_count > 0 && self.locals[self.local_count - 1].depth > self.scope_depth {
      if self.locals[self.local_count - 1].is_captured {
        self.emit_byte(AlignedByteCode::CloseUpvalue, end_line)
      } else {
        self.emit_byte(AlignedByteCode::Drop, end_line);
      }
      self.local_count -= 1;
    }
  }

  /// Mark a variable initialized
  fn mark_initialized(&mut self) {
    if self.scope_depth > 0 {
      self.locals[self.local_count - 1].depth = self.scope_depth;
    }
  }

  /// Add a local variable to the current scope
  fn add_local(&mut self, name: Token) {
    if self.local_count == std::u8::MAX as usize {
      self.error("Too many local variables in function.");
      return;
    }

    let local = &mut self.locals[self.local_count];
    self.local_count += 1;

    local.name = Some(name.lexeme);
    local.depth = -1;
  }

  ///  declare a variable
  fn declare_variable(&mut self, name: Token) {
    // if global exit
    if self.scope_depth == 0 {
      return;
    }

    for local in self.locals[..self.local_count].iter().rev() {
      // If we in a lower scope break
      if local.depth != UNINITIALIZED && local.depth < self.scope_depth {
        break;
      }

      // check that the same variable wasn't declared twice in the same scope
      if let Some(local_name) = &local.name {
        if &name.lexeme == local_name {
          self.error("Variable with this name already declared in this scope.");
          return;
        }
      }
    }

    self.add_local(name);
  }

  /// retrieve a named variable from either local or global scope
  fn variable(&mut self, name: &Token, can_assign: bool) {
    let index = self.resolve_local(&name);

    let (get_byte, set_byte) = match index {
      Some(local) => (
        AlignedByteCode::GetLocal(local),
        AlignedByteCode::SetLocal(local),
      ),
      None => match self.resolve_upvalue(&name) {
        Some(upvalue) => (
          AlignedByteCode::GetUpvalue(upvalue),
          AlignedByteCode::SetUpvalue(upvalue),
        ),
        None => {
          let global_index = self.identifier_constant(&name);
          (
            AlignedByteCode::GetGlobal(global_index),
            AlignedByteCode::SetGlobal(global_index),
          )
        }
      },
    };

    if can_assign {
      self.emit_byte(set_byte, name.end());
    } else {
      self.emit_byte(get_byte, name.end());
    }
  }

  /// resolve a token to a local if it exists
  fn resolve_local(&mut self, name: &Token) -> Option<u8> {
    for i in (0..self.local_count).rev() {
      let local = &self.locals[i];

      if let Some(local_name) = &local.name {
        if &name.lexeme == local_name {
          // handle the case were `let a = a;`
          if local.depth == UNINITIALIZED {
            self.error("Cannot read local variable in its own initializer.")
          }

          return Some(i as u8);
        }
      }
    }

    // not found
    Option::None
  }

  /// resolve a token to an upvalue in an enclosing scope if it exists
  fn resolve_upvalue(&mut self, name: &Token) -> Option<u8> {
    match &mut self.enclosing {
      Some(parent_ptr) => {
        let parent = unsafe { parent_ptr.as_mut() };

        match parent.resolve_local(name) {
          Some(local) => {
            parent.locals[local as usize].is_captured = true;
            Some(self.add_upvalue(UpvalueIndex::Local(local)) as u8)
          }
          None => parent
            .resolve_upvalue(name)
            .map(|upvalue| self.add_upvalue(UpvalueIndex::Upvalue(upvalue)) as u8),
        }
      }
      None => None,
    }
  }

  /// add an upvalue
  fn add_upvalue(&mut self, upvalue: UpvalueIndex) -> usize {
    let upvalue_count = self.fun.upvalue_count;

    // check for existing upvalues
    if let Some(i) = self.upvalues[0..upvalue_count]
      .iter()
      .position(|existing_upvalue| match (&existing_upvalue, &upvalue) {
        (UpvalueIndex::Local(existing), UpvalueIndex::Local(new)) => *existing == *new,
        _ => false,
      })
    {
      return i;
    }

    // prevent overflow
    if upvalue_count == std::u8::MAX as usize {
      self.error("Too many closure variable in function.");
      return 0;
    }

    self.upvalues[upvalue_count] = upvalue;
    self.fun.upvalue_count += 1;

    upvalue_count
  }

  /// Define a variable
  fn define_variable(&mut self, variable: u16, line: u32) {
    if self.scope_depth > 0 {
      self.mark_initialized();
      return;
    }

    self.emit_byte(AlignedByteCode::DefineGlobal(variable), line);
  }

  /// Emit a provided instruction
  fn emit_byte(&mut self, op_code: AlignedByteCode, line: u32) {
    self.write_instruction(op_code, line);
  }

  /// Emit a jump instruction
  fn emit_jump(&mut self, jump: AlignedByteCode, line: u32) -> usize {
    self.emit_byte(jump, line);

    self.current_chunk().instructions.len() - 2
  }

  /// Patch a jump instruction
  fn patch_jump(&mut self, offset: usize) {
    let jump_landing = self.calc_jump(offset);
    let buffer = jump_landing.to_ne_bytes();
    self.fun.replace_instruction(offset, buffer[0]);
    self.fun.replace_instruction(offset + 1, buffer[1]);
  }

  /// Calculate the jump once it's landing has been found
  fn calc_jump(&mut self, offset: usize) -> u16 {
    let jump = self.current_chunk().instructions.len() - offset - 2;

    if jump > std::u16::MAX.try_into().unwrap() {
      self.error("Too much code to jump over.");
    }

    jump as u16
  }

  /// Emit a loop instruction
  fn emit_loop(&mut self, loop_start: usize, line: u32) {
    let offset = self.current_chunk().instructions.len() - loop_start + 3;
    if offset > std::u16::MAX.try_into().unwrap() {
      self.error("Loop body too large.");
    }

    self.emit_byte(AlignedByteCode::Loop(offset as u16), line);
  }

  /// The current chunk
  fn current_chunk(&self) -> &Chunk {
    self.fun.chunk()
  }

  /// write instruction to the current function
  fn write_instruction(&mut self, op_code: AlignedByteCode, line: u32) {
    self.fun.write_instruction(self.hooks, op_code, line)
  }

  /// Parse a variable from the provided token return it's new constant
  /// identifer if an identifer was identified
  fn make_identifier(&mut self, name: &Token) -> u16 {
    self.declare_variable(name.clone());
    if self.scope_depth > 0 {
      return 0;
    }
    self.identifier_constant(name)
  }

  /// Generate a constant from the provided identifier token
  fn identifier_constant(&mut self, name: &Token) -> u16 {
    let identifer = self.hooks.manage_str(name.lexeme.as_str());
    self.make_constant(val!(identifer))
  }

  /// Add a constant to the current chunk
  fn make_constant(&mut self, value: Value) -> u16 {
    match self.constants.get(&value) {
      Some(index) => *index as u16,
      None => {
        let index = self.fun.add_constant(&self.hooks, value);
        if index > std::u16::MAX as usize {
          self.error("Too many constants in one chunk.");
          return 0;
        }

        self.constants.insert(value, index);
        index as u16
      }
    }
  }

  /// Emit byte code for a constant
  fn emit_constant(&mut self, value: Value, line: u32) {
    let index = self.make_constant(value);

    if index <= std::u8::MAX as u16 {
      self.emit_byte(AlignedByteCode::Constant(index as u8), line);
    } else {
      self.emit_byte(AlignedByteCode::ConstantLong(index), line);
    }
  }

  /// Indicate an error occurred at he current index
  fn error_at_current(&mut self, message: &str) {
    self.error_at(message);
  }

  /// Indicate an error occurred at the previous index
  pub fn error(&mut self, message: &str) {
    self.error_at(message);
  }

  /// Print an error to the console for a user to address
  fn error_at(&mut self, message: &str) {
    let mut stdio = self.io.stdio();
    let stderr = stdio.stderr();

    write!(stderr, "[line {}] Error", 0).expect("Unable to write to stderr.");
    write!(stderr, " at {}", "todo").expect("Unable to write to stderr.");
    writeln!(stderr, ": {}", message).expect("Unable to write to stderr.");
    self.had_error = true;
  }

  /// Compile a declaration
  fn decl(&mut self, decl: &Decl) {
    match decl {
      Decl::Symbol(symbol) => self.symbol(symbol),
      Decl::Export(export) => self.export(export),
      Decl::Stmt(stmt) => self.stmt(stmt),
      Decl::Error(error) => self.visit_error(error),
    }
  }

  /// Compile a statement
  fn stmt(&mut self, stmt: &Stmt) {
    match stmt {
      Stmt::Expr(expr) => {
        self.expr(expr);
        self.emit_byte(AlignedByteCode::Drop, expr.end());
      }
      Stmt::Import(import) => self.import(import),
      Stmt::For(for_) => self.for_(for_),
      Stmt::If(if_) => self.if_(if_),
      Stmt::Return(return_) => self.return_(return_),
      Stmt::While(while_) => self.while_(while_),
      Stmt::Try(try_) => self.try_(try_),
    }
  }

  /// Compile an expression
  fn expr(&mut self, expr: &Expr) {
    match expr {
      Expr::Assign(assign) => self.assign(assign),
      Expr::Binary(binary) => self.binary(binary),
      Expr::Unary(unary) => self.unary(unary),
      Expr::Atom(atom) => self.atom(atom),
    }
  }

  /// Compile a the base of an expression
  fn primary(&mut self, primary: &Primary, trailers: &[Trailer]) -> bool {
    match primary {
      Primary::AssignBlock(block) => self.assign_block(block),
      Primary::True(token) => self.true_(token),
      Primary::False(token) => self.false_(token),
      Primary::Nil(token) => self.nil(token),
      Primary::Number(token) => self.number(token),
      Primary::Grouping(expr) => {
        self.expr(expr);
        false
      }
      Primary::String(token) => self.string(token),
      Primary::Ident(token) => self.identifier(token),
      Primary::Self_(token) => self.self_(token),
      Primary::Super(token) => self.super_(token, trailers),
      Primary::Lambda(fun) => self.lambda(fun),
      Primary::List(items) => self.list(items),
      Primary::Map(kvps) => self.map(kvps),
    }
  }

  /// Compile a symbol declaration
  fn symbol(&mut self, symbol: &Symbol) {
    match symbol {
      Symbol::Class(class) => self.class(class),
      Symbol::Fun(fun) => self.fun(fun),
      Symbol::Let(let_) => self.let_(let_),
      _ => 0,
    };
  }

  /// Compile an export declaration
  fn export(&mut self, export: &Symbol) {
    let symbol = match &export {
      Symbol::Class(class) => Some(self.class(class)),
      Symbol::Fun(fun) => Some(self.fun(fun)),
      Symbol::Let(let_) => Some(self.let_(let_)),
      _ => None,
    };

    // emit error if not at module level
    if self.scope_depth == 0 {
      if let Some(symbol) = symbol {
        self.emit_byte(AlignedByteCode::Export(symbol), export.end());
      }
    } else {
      self.error_at_current("Can only export from the module scope.")
    }
  }

  /// Compile a class declaration
  fn class(&mut self, class: &ast::Class) -> u16 {
    // declare the class by name
    let name = class.name.clone().expect("Expected class name.");
    let name_constant = self.identifier_constant(&name);
    self.declare_variable(name.clone());

    self.emit_byte(AlignedByteCode::Class(name_constant), name.end());
    self.define_variable(name_constant, name.end());

    // set this class as the current class compiler
    let mut class_compiler = self.hooks.manage(ClassInfo {
      name: class.name.clone(),
      fun_kind: None,
      fields: vec![],
      has_super_class: false,
      enclosing: self.current_class,
    });
    self.current_class = Some(class_compiler);

    // handle the case where a super class exists
    if let Some(super_class) = &class.super_class {
      self.variable(&super_class.type_ref.name, false);

      // start a new scope with the super keyword present
      self.begin_scope();
      self.add_local(Token {
        kind: TokenKind::Super,
        lexeme: SmolStr::new_inline_from_ascii(SUPER.len(), SUPER.as_bytes()),
        line: super_class.end(),
      });
      self.define_variable(0, super_class.end());

      self.variable(&name, false);
      self.emit_byte(AlignedByteCode::Inherit, super_class.type_ref.start());

      // indicate this class is a subclass
      class_compiler.has_super_class = true;
    }

    self.variable(&name, false);

    // process the initializer
    let field_line = if let Some(init) = &class.init {
      self.method(&init, FunKind::Initializer);
      init.start()
    } else {
      class.start()
    };

    self.emit_fields(field_line);

    // process methods
    for method in &class.methods {
      self.method(&method, FunKind::Method);
    }

    // process static methods
    for static_method in &class.static_methods {
      self.static_method(&static_method);
    }

    self.emit_byte(AlignedByteCode::Drop, class.end());

    // if we have a super drop the extra scope with super
    if class_compiler.has_super_class {
      self.end_scope(class.end());
    }

    // restore the enclosing class compiler
    self.current_class = class_compiler.enclosing;
    name_constant
  }

  /// Emit field instructions
  fn emit_fields(&mut self, line: u32) {
    let class_info = self.current_class.expect("Current class unset");

    class_info.fields.iter().for_each(|f| {
      let value = val!(self.hooks.manage_str(f.as_str()));
      let constant = self.make_constant(value);
      self.emit_byte(AlignedByteCode::Field(constant), line)
    })
  }

  /// Compile a method
  fn method(&mut self, method: &ast::Fun, fun_kind: FunKind) {
    let constant = method
      .name
      .as_ref()
      .map(|name| self.identifier_constant(name))
      .expect("Expect method name");

    self.current_class.expect("Class compiler not set").fun_kind = Some(fun_kind);

    self.function(method, fun_kind);
    self.emit_byte(AlignedByteCode::Method(constant), method.end());
  }

  /// Compile a static method
  fn static_method(&mut self, static_method: &ast::Fun) {
    let constant = static_method
      .name
      .as_ref()
      .map(|name| self.make_identifier(name))
      .expect("Expected method name.");

    self.current_class.expect("Class compiler not set").fun_kind = Some(FunKind::StaticMethod);

    self.function(static_method, FunKind::StaticMethod);
    self.emit_byte(AlignedByteCode::StaticMethod(constant), static_method.end());
  }

  /// Compile a plain function
  fn fun(&mut self, fun: &ast::Fun) -> u16 {
    let constant = fun
      .name
      .as_ref()
      .map(|name| self.make_identifier(name))
      .expect("Expected function name");

    self.mark_initialized();
    self.function(fun, FunKind::Fun);
    self.define_variable(constant, fun.end());

    constant
  }

  /// Compile a let binding
  fn let_(&mut self, let_: &ast::Let) -> u16 {
    self.declare_variable(let_.name.clone());
    let variable = self.identifier_constant(&let_.name);

    match &let_.value {
      Some(v) => self.expr(v),
      None => self.emit_byte(AlignedByteCode::Nil, let_.name.end()),
    }

    self.define_variable(variable, let_.end());
    variable
  }

  /// Compile a function objects that presents, functions, methods
  /// and lambdas
  fn function(&mut self, fun: &ast::Fun, fun_kind: FunKind) {
    // manage name assume name "lambda" if none is provided
    let name = fun
      .name
      .as_ref()
      .map(|name| self.hooks.manage_str(name.lexeme.as_str()))
      .unwrap_or_else(|| self.hooks.manage_str("lambda"));

    // create a new child compiler for this function
    let mut compiler = Compiler::child(name, fun_kind, &mut *self);
    compiler.begin_scope();
    compiler.call_sig(&fun.call_sig);

    match &fun.body {
      FunBody::Block(block) => compiler.block(&block),
      FunBody::Expr(expr) => {
        compiler.expr(&expr);
        compiler.emit_byte(AlignedByteCode::Return, expr.end());
      }
    };

    let end_line = fun.end();

    // end compilation of function chunk
    compiler.end_compiler(end_line);
    let upvalue_count = compiler.fun.upvalue_count;

    let index = self.make_constant(val!(compiler.fun));
    self.emit_byte(AlignedByteCode::Closure(index), end_line);
    self.had_error = self.had_error || compiler.had_error;

    // emit upvalue index instructions
    compiler.upvalues[0..upvalue_count]
      .iter()
      .for_each(|upvalue| self.emit_byte(AlignedByteCode::UpvalueIndex(*upvalue), end_line));
  }

  /// Compile an import statement
  fn import(&mut self, import: &ast::Import) {
    let name = self.identifier_constant(&import.imported);
    let string = self.hooks.manage_str(import.path.lexeme.as_str());
    let value = val!(string);
    let path = self.make_constant(value);

    // emit error if not at module level
    if self.scope_depth == 0 {
      self.emit_byte(AlignedByteCode::Import(path), import.imported.start());
      self.emit_byte(AlignedByteCode::DefineGlobal(name), import.imported.start());
    } else {
      self.error_at_current("Can only import from the module scope.")
    }
  }

  /// Compile a for loop
  fn for_(&mut self, for_: &ast::For) {
    // new scope for full loop including loop variables
    self.scope(for_.end(), |self_| {
      let item = self_.make_identifier(&for_.item);
      let item_line = for_.item.end();

      self_.emit_byte(AlignedByteCode::Nil, item_line);
      self_.define_variable(item, item_line);
      self_.expr(&for_.iter);

      let expr_line = for_.iter.end();

      // token for hidden $iter variable
      let iterator_token = Token {
        lexeme: SmolStr::new_inline_from_ascii(ITER_VAR.len(), ITER_VAR.as_bytes()),
        kind: TokenKind::Identifier,
        line: expr_line,
      };

      // get constant for 'iter' method
      let iter_const = self_.identifier_constant(&Token {
        lexeme: SmolStr::new_inline_from_ascii(ITER.len(), ITER.as_bytes()),
        kind: TokenKind::Identifier,
        line: expr_line,
      });

      // declare the hidden local $iter variable
      let iterator_const = self_.identifier_constant(&iterator_token);
      self_.declare_variable(iterator_token.clone());
      self_.emit_byte(AlignedByteCode::Invoke((iter_const, 0)), expr_line);
      self_.define_variable(iterator_const, expr_line);

      // mark start of loop
      let loop_start = self_.current_chunk().instructions.len();

      // define iterator method constants
      let next_const = self_.identifier_constant(&Token {
        lexeme: SmolStr::new_inline_from_ascii(4, b"next"),
        kind: TokenKind::Identifier,
        line: for_.item.end(),
      });

      let current_const = self_.identifier_constant(&Token {
        lexeme: SmolStr::new_inline_from_ascii(6, b"current"),
        kind: TokenKind::Identifier,
        line: for_.item.end(),
      });

      // call next on iterator
      let iterator_variable = self_
        .resolve_local(&iterator_token)
        .expect("Iterator variable was not defined.");

      self_.emit_byte(AlignedByteCode::GetLocal(iterator_variable), expr_line);
      self_.emit_byte(AlignedByteCode::IterNext(next_const), expr_line);

      // check at end of iterator
      let exit_jump = self_.emit_jump(AlignedByteCode::JumpIfFalse(0), expr_line);
      self_.emit_byte(AlignedByteCode::Drop, expr_line);

      // assign $iter.current to loop variable
      let loop_variable = self_
        .resolve_local(&for_.item)
        .expect("Loop variable was not defined.");

      self_.emit_byte(AlignedByteCode::GetLocal(iterator_variable), expr_line);
      self_.emit_byte(AlignedByteCode::IterCurrent(current_const), expr_line);
      self_.emit_byte(AlignedByteCode::SetLocal(loop_variable), expr_line);
      self_.emit_byte(AlignedByteCode::Drop, expr_line);

      // loop body
      self_.scope(for_.body.end(), |self_| self_.block(&for_.body));
      self_.emit_loop(loop_start, for_.end());

      // loop back to top
      self_.patch_jump(exit_jump);
      self_.emit_byte(AlignedByteCode::Drop, for_.end());
    });
  }

  /// Compile a while loop
  fn while_(&mut self, while_: &ast::While) {
    let loop_start = self.current_chunk().instructions.len();
    self.expr(&while_.cond);

    let exit_jump = self.emit_jump(AlignedByteCode::JumpIfFalse(0), while_.cond.end());

    self.emit_byte(AlignedByteCode::Drop, while_.cond.end());
    self.scope(while_.end(), |self_| self_.block(&while_.body));

    self.emit_loop(loop_start, while_.end());

    self.patch_jump(exit_jump);
    self.emit_byte(AlignedByteCode::Drop, while_.end())
  }

  /// Compile a if statement
  fn if_(&mut self, if_: &ast::If) {
    self.expr(&if_.cond);

    // parse then branch
    let then_jump = self.emit_jump(AlignedByteCode::JumpIfFalse(0), if_.cond.end());
    self.emit_byte(AlignedByteCode::Drop, if_.cond.end());

    self.scope(if_.body.end(), |self_| self_.block(&if_.body));

    // emit else jump
    let else_jump = self.emit_jump(AlignedByteCode::Jump(0), if_.body.end());
    self.patch_jump(then_jump);
    self.emit_byte(AlignedByteCode::Drop, if_.body.end());

    if let Some(else_) = &if_.else_ {
      match else_ {
        ast::Else::If(if_) => self.if_(if_),
        ast::Else::Block(block) => self.scope(block.end(), |self_| self_.block(block)),
      }
    }

    self.patch_jump(else_jump);
  }

  /// Compile a return statement
  fn return_(&mut self, return_: &ast::Return) {
    match &return_.value {
      Some(v) => {
        self.expr(&v);
        self.emit_byte(AlignedByteCode::Return, v.end());
      }
      None => self.emit_return(return_.start()),
    }
  }

  /// Compile a try catch block
  fn try_(&mut self, try_: &ast::Try) {
    let start = self.current_chunk().instructions.len();

    self.scope(try_.block.end(), |self_| self_.block(&try_.block));

    let catch_jump = self.emit_jump(AlignedByteCode::Jump(0), try_.block.end());
    let end = self.current_chunk().instructions.len();

    self.scope(try_.catch.end(), |self_| self_.block(&try_.catch));

    self.patch_jump(catch_jump);
    self.fun.add_try(TryBlock::new(start as u16, end as u16));
  }

  /// Compile a block
  fn block(&mut self, block: &ast::Block) {
    for decl in &block.decls {
      self.decl(&decl);
    }
  }

  /// Compile an assignment expression
  fn assign(&mut self, assign: &ast::Assign) {
    match &assign.lhs {
      Expr::Atom(atom) => match atom.trailers.last() {
        // if we have trailers compile to last trailer and emit specialized
        // set instruction
        Some(last) => {
          let skip_first = self.primary(&atom.primary, &atom.trailers);
          self.apply_trailers(skip_first, &atom.trailers[..atom.trailers.len() - 1]);

          match last {
            Trailer::Index(index) => {
              self.expr(&index.index);
              self.expr(&assign.rhs);
              self.emit_byte(AlignedByteCode::SetIndex, assign.rhs.end())
            }
            Trailer::Access(access) => {
              if self.fun_kind == FunKind::Initializer && atom.trailers.len() == 1 {
                match atom.primary {
                  Primary::Self_(_) => {
                    let mut class_info = self.current_class.unwrap();

                    if !class_info.fields.iter().any(|f| *f == access.prop.lexeme) {
                      class_info.fields.push(access.prop.lexeme.clone());
                    }
                  }
                  _ => (),
                }
              }

              let name = self.identifier_constant(&access.prop);

              self.expr(&assign.rhs);
              self.emit_byte(AlignedByteCode::SetProperty(name), access.end())
            }
            Trailer::Call(_) => {
              unreachable!("Unexpected expression on left hand side of assignment.")
            }
          }
        }
        None => {
          if let Primary::Ident(name) = &atom.primary {
            self.expr(&assign.rhs);
            self.variable(name, true);
          } else {
            unreachable!("Unexpected expression on left hand side of assignment.");
          }
        }
      },
      _ => unreachable!("Unexpected expression on left hand side of assignment."),
    }
  }

  /// Compile a binary expression
  fn binary(&mut self, binary: &ast::Binary) {
    self.expr(&binary.lhs);

    // emit for rhs if we're not an "and" or "or"
    match &binary.op {
      BinaryOp::And | BinaryOp::Or => (),
      _ => self.expr(&binary.rhs),
    }

    // emit for binary operation
    match &binary.op {
      BinaryOp::Add => self.emit_byte(AlignedByteCode::Add, binary.rhs.end()),
      BinaryOp::Sub => self.emit_byte(AlignedByteCode::Subtract, binary.rhs.end()),
      BinaryOp::Multi => self.emit_byte(AlignedByteCode::Multiply, binary.rhs.end()),
      BinaryOp::Div => self.emit_byte(AlignedByteCode::Divide, binary.rhs.end()),
      BinaryOp::Lt => self.emit_byte(AlignedByteCode::Less, binary.rhs.end()),
      BinaryOp::LtEq => self.emit_byte(AlignedByteCode::LessEqual, binary.rhs.end()),
      BinaryOp::Gt => self.emit_byte(AlignedByteCode::Greater, binary.rhs.end()),
      BinaryOp::GtEq => self.emit_byte(AlignedByteCode::GreaterEqual, binary.rhs.end()),
      BinaryOp::Eq => self.emit_byte(AlignedByteCode::Equal, binary.rhs.end()),
      BinaryOp::Ne => self.emit_byte(AlignedByteCode::NotEqual, binary.rhs.end()),
      BinaryOp::And => {
        let end_jump = self.emit_jump(AlignedByteCode::JumpIfFalse(0), binary.lhs.end());

        self.emit_byte(AlignedByteCode::Drop, binary.lhs.end());
        self.expr(&binary.rhs);

        self.patch_jump(end_jump);
      }
      BinaryOp::Or => {
        let else_jump = self.emit_jump(AlignedByteCode::JumpIfFalse(0), binary.lhs.end());
        let end_jump = self.emit_jump(AlignedByteCode::Jump(0), binary.lhs.end());

        self.patch_jump(else_jump);
        self.emit_byte(AlignedByteCode::Drop, binary.lhs.end());

        self.expr(&binary.rhs);
        self.patch_jump(end_jump);
      }
    }
  }

  /// Compile a unary expression
  fn unary(&mut self, unary: &ast::Unary) {
    self.expr(&unary.expr);

    match &unary.op {
      UnaryOp::Not => self.emit_byte(AlignedByteCode::Not, unary.expr.end()),
      UnaryOp::Negate => self.emit_byte(AlignedByteCode::Negate, unary.expr.end()),
    }
  }

  /// Compile a call expression
  fn call(&mut self, call: &ast::Call) -> bool {
    for expr in &call.args {
      self.expr(expr);
    }

    self.emit_byte(AlignedByteCode::Call(call.args.len() as u8), call.end());
    false
  }

  /// Compile an indexing expression
  fn index(&mut self, index: &ast::Index) -> bool {
    self.expr(&index.index);
    self.emit_byte(AlignedByteCode::GetIndex, index.end());
    false
  }

  /// Compile an access expression
  fn access(&mut self, access: &ast::Access, trailers: &[Trailer]) -> bool {
    let name = self.identifier_constant(&access.prop);

    match trailers.first() {
      Some(trailer) => {
        if let Trailer::Call(call) = trailer {
          for expr in &call.args {
            self.expr(expr);
          }
          self.emit_byte(
            AlignedByteCode::Invoke((name, call.args.len() as u8)),
            trailer.end(),
          );
          true
        } else {
          self.emit_byte(AlignedByteCode::GetProperty(name), access.prop.end());
          false
        }
      }
      None => {
        self.emit_byte(AlignedByteCode::GetProperty(name), access.prop.end());
        false
      }
    }
  }

  /// Compile an atom expression
  fn atom(&mut self, atom: &ast::Atom) {
    let skip_first = self.primary(&atom.primary, &atom.trailers);
    self.apply_trailers(skip_first, &atom.trailers);
  }

  /// Compile trailers onto a base primary
  fn apply_trailers(&mut self, skip_first: bool, trailers: &[Trailer]) {
    let mut skip = skip_first;
    for (idx, trailer) in trailers.iter().enumerate() {
      if skip {
        skip = false;
        continue;
      }

      skip = match trailer {
        Trailer::Call(call) => self.call(&call),
        Trailer::Index(index) => self.index(&index),
        Trailer::Access(access) => self.access(&access, &trailers[(idx + 1)..]),
      }
    }
  }

  /// Compile an assignment block
  fn assign_block(&mut self, block: &ast::Block) -> bool {
    self.scope(block.end(), |self_| {
      self_.block(block);
    });
    self.emit_byte(AlignedByteCode::Nil, block.end());
    false
  }

  /// Compile a true token
  fn true_(&mut self, true_: &Token) -> bool {
    self.emit_byte(AlignedByteCode::True, true_.end());
    false
  }

  /// Compile a false token
  fn false_(&mut self, false_: &Token) -> bool {
    self.emit_byte(AlignedByteCode::False, false_.end());
    false
  }

  /// Compile a nil token
  fn nil(&mut self, nil: &Token) -> bool {
    self.emit_byte(AlignedByteCode::Nil, nil.end());
    false
  }

  /// Compile a number token
  fn number(&mut self, token: &Token) -> bool {
    let value = val!(token.lexeme.parse::<f64>().expect("Unable to parse float"));
    self.emit_constant(value, token.end());
    false
  }

  /// Compile a string token
  fn string(&mut self, token: &Token) -> bool {
    let value = val!(self.hooks.manage_str(token.lexeme.as_str()));
    self.emit_constant(value, token.end());
    false
  }

  /// Compile a identifer token
  fn identifier(&mut self, token: &Token) -> bool {
    self.variable(&token, false);
    false
  }

  /// Compile the self token
  fn self_(&mut self, self_: &Token) -> bool {
    self
      .current_class
      .map(|class_compiler| class_compiler.fun_kind)
      .and_then(|fun_kind| {
        fun_kind.and_then(|fun_kind| match fun_kind {
          FunKind::Method | FunKind::Initializer => {
            self.variable(&self_, false);
            Some(())
          }
          _ => None,
        })
      })
      .or_else(|| {
        self.error("Cannot use 'self' outside of class instance methods.");
        None
      });

    false
  }

  /// Compile the super token
  fn super_(&mut self, super_: &ast::Super, trailers: &[Trailer]) -> bool {
    match self.current_class {
      None => self.error("Cannot use 'super' outside of a class."),
      Some(class) => {
        if !class.has_super_class {
          self.error("Cannot use 'super' in a class with no superclass.");
        }
      }
    }

    let name = self.identifier_constant(&super_.access);

    // load self on top of stack
    self.variable(
      &Token {
        lexeme: SmolStr::new_inline_from_ascii(SELF.len(), SELF.as_bytes()),
        kind: TokenKind::Self_,
        line: super_.end(),
      },
      false,
    );

    match trailers.first() {
      Some(trailer) => match trailer {
        Trailer::Call(call) => {
          for arg in &call.args {
            self.expr(arg);
          }

          self.variable(&super_.super_, false);
          self.emit_byte(
            AlignedByteCode::SuperInvoke((name, call.args.len() as u8)),
            super_.access.end(),
          );
          true
        }
        _ => {
          self.variable(&super_.super_, false);
          self.emit_byte(AlignedByteCode::GetSuper(name), super_.end());
          false
        }
      },
      None => {
        self.variable(
          &Token {
            lexeme: SmolStr::new_inline_from_ascii(SUPER.len(), SUPER.as_bytes()),
            kind: TokenKind::Super,
            line: super_.super_.end(),
          },
          false,
        );

        self.emit_byte(AlignedByteCode::GetSuper(name), super_.access.end());
        false
      }
    }
  }

  /// Compile a lambda expression
  fn lambda(&mut self, fun: &ast::Fun) -> bool {
    self.function(fun, FunKind::Fun);
    false
  }

  /// Compile a list literal
  fn list(&mut self, list: &List) -> bool {
    self.emit_byte(AlignedByteCode::List, list.start());

    for item in list.items.iter() {
      self.expr(item);
    }

    if !list.items.is_empty() {
      self.emit_byte(
        AlignedByteCode::ListInit(list.items.len() as u16),
        list.end(),
      );
    }

    false
  }

  /// Compile a map literal
  fn map(&mut self, map: &Map) -> bool {
    self.emit_byte(AlignedByteCode::Map, map.start());

    for (key, value) in map.entries.iter() {
      self.expr(key);
      self.expr(value);
    }

    if !map.entries.is_empty() {
      self.emit_byte(
        AlignedByteCode::MapInit(map.entries.len() as u16),
        map.end(),
      );
    }

    false
  }

  /// Set the functions arity from the call signature
  fn call_sig(&mut self, call_sig: &ast::CallSignature) {
    for param in &call_sig.params {
      let param_constant = self.make_identifier(&param.name);
      self.define_variable(param_constant, param.name.end());
    }

    self.fun.arity = Arity::Fixed(call_sig.params.len() as u8);
  }

  /// Do nothing if we encounter an error token
  fn visit_error(&mut self, _: &[Token]) {}
}

/// Get the first local for a given function kind
fn first_local(fun_kind: FunKind) -> Local {
  match fun_kind {
    FunKind::Fun | FunKind::StaticMethod | FunKind::Script => Local {
      name: Option::None,
      depth: 0,
      is_captured: false,
    },
    FunKind::Method | FunKind::Initializer => Local {
      name: Some(SmolStr::new_inline_from_ascii(SELF.len(), SELF.as_bytes())),
      depth: 0,
      is_captured: false,
    },
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::{debug::disassemble_chunk, parser::Parser};
  use laythe_core::chunk::decode_u16;
  use laythe_core::hooks::{support::TestContext, GcContext};
  use laythe_env::stdio::support::{IoStdioTest, StdioTestContainer};
  use module::Module;
  use std::{path::PathBuf, rc::Rc};

  enum ByteCodeTest {
    Code(AlignedByteCode),
    Fun((u16, Vec<ByteCodeTest>)),
  }

  fn test_compile<'a>(src: &str, context: &dyn GcContext) -> Managed<Fun> {
    let mut stdio_container = Rc::new(StdioTestContainer::default());

    let stdio = Rc::new(IoStdioTest::new(&mut stdio_container));
    let io = Io::default().with_stdio(stdio);

    let ast = Parser::new(io.stdio(), src).parse();
    assert!(ast.is_ok());

    let hooks = &GcHooks::new(context);

    let module = hooks
      .manage(Module::from_path(&hooks, hooks.manage(PathBuf::from("path/module.ly"))).unwrap());

    let compiler = Compiler::new(module, &io, hooks);
    let result = compiler.compile(&ast.unwrap());

    assert_eq!(result.is_ok(), true);
    result.unwrap()
  }

  fn decode_byte_code(fun: Managed<Fun>) -> Vec<AlignedByteCode> {
    let bytes = &fun.chunk().instructions;
    let mut decoded = Vec::new();
    let mut offset = 0;

    while offset < bytes.len() {
      let (byte_code, new_offset) = AlignedByteCode::decode(&bytes, offset);

      match byte_code {
        AlignedByteCode::Closure(closure) => {
          decoded.push(byte_code);
          offset = decode_byte_code_closure(fun, &mut decoded, new_offset, closure)
        }
        _ => {
          decoded.push(byte_code);
          offset = new_offset;
        }
      }
    }

    decoded
  }

  fn decode_byte_code_closure(
    fun: Managed<Fun>,
    decoded: &mut Vec<AlignedByteCode>,
    offset: usize,
    slot: u16,
  ) -> usize {
    let inner_fun = fun.chunk().constants[slot as usize].to_fun();
    let mut current_offset = offset;

    let instructions = &fun.chunk().instructions;
    for _ in 0..inner_fun.upvalue_count {
      let scalar = decode_u16(&instructions[offset..offset + 2]);

      let upvalue_index: UpvalueIndex = unsafe { mem::transmute(scalar) };
      decoded.push(AlignedByteCode::UpvalueIndex(upvalue_index));
      current_offset = current_offset + 2;
    }

    current_offset
  }

  fn assert_simple_bytecode(fun: Managed<Fun>, code: &[AlignedByteCode]) {
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
  }

  fn assert_fun_bytecode(fun: Managed<Fun>, code: &[ByteCodeTest]) {
    let stdio_container = StdioTestContainer::default();
    let mut stdio = Stdio::new(Box::new(stdio_container.make_stdio()));

    assert!(disassemble_chunk(&mut stdio, &fun.chunk(), &*fun.name).is_ok());
    stdio_container.log_stdio();

    let decoded_byte_code = decode_byte_code(fun);
    assert_eq!(decoded_byte_code.len(), code.len(), "for fun {}", fun.name);

    for i in 0..code.len() {
      match decoded_byte_code[i] {
        AlignedByteCode::Closure(index) => {
          let fun = fun.chunk().constants[index as usize].to_fun();

          match &code[i] {
            ByteCodeTest::Fun((expected, inner)) => {
              assert_eq!(*expected, index);
              assert_fun_bytecode(fun, &inner);
            }
            _ => assert!(false),
          }
        }
        _ => match &code[i] {
          ByteCodeTest::Code(byte_code) => {
            assert_eq!(&decoded_byte_code[i], byte_code);
          }
          _ => assert!(false),
        },
      }
    }
  }

  #[test]
  fn import() {
    let example = r#"
      import time from "std/time";
    "#;

    let context = TestContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::Import(1),
        AlignedByteCode::DefineGlobal(0),
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      fun,
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);

    assert_fun_bytecode(
      fun,
      &vec![
        ByteCodeTest::Fun((
          1,
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::Class(0),
        AlignedByteCode::DefineGlobal(0),
        AlignedByteCode::GetGlobal(0),
        AlignedByteCode::Drop,
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

      } catch {

      }
    ";

    let context = TestContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::Jump(0),
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );

    assert_eq!(fun.has_catch_jump(0), Some(3));
  }

  #[test]
  fn filled_try_catch() {
    let example = r#"
      try {
        let empty = {};
        empty["missing"];
      } catch {
        print("no!");
      }
    "#;

    let context = TestContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::Map,          // 0
        AlignedByteCode::GetLocal(1),  // 1
        AlignedByteCode::Constant(1),  // 3
        AlignedByteCode::GetIndex,     // 5
        AlignedByteCode::Drop,         // 6
        AlignedByteCode::Drop,         // 7
        AlignedByteCode::Jump(8),      // 8
        AlignedByteCode::GetGlobal(2), // 13
        AlignedByteCode::Constant(3),  // 11
        AlignedByteCode::Call(1),      // 13
        AlignedByteCode::Drop,         //
        AlignedByteCode::Nil,          // 14
        AlignedByteCode::Return,       // 15
      ],
    );

    assert_eq!(fun.has_catch_jump(0), Some(11));
  }

  #[test]
  fn nested_try_catch() {
    let example = r#"
      try {
        [][3];
        try {
          [][1];
        } catch {
          print("woops!");
        }
      } catch {
        print("no!");
      }
    "#;

    let context = TestContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::List,         // 0
        AlignedByteCode::Constant(0),  // 1
        AlignedByteCode::GetIndex,     // 3
        AlignedByteCode::Drop,         // 4
        AlignedByteCode::List,         // 5
        AlignedByteCode::Constant(1),  // 6
        AlignedByteCode::GetIndex,     // 8
        AlignedByteCode::Drop,         // 9
        AlignedByteCode::Jump(8),      // 10
        AlignedByteCode::GetGlobal(2), //
        AlignedByteCode::Constant(3),  // 13
        AlignedByteCode::Call(1),      //
        AlignedByteCode::Drop,
        AlignedByteCode::Jump(8),      // 16
        AlignedByteCode::GetGlobal(2), // 21
        AlignedByteCode::Constant(4),  // 19
        AlignedByteCode::Call(1),      // 21
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,    // 22
        AlignedByteCode::Return, // 23
      ],
    );

    assert_eq!(fun.has_catch_jump(0), Some(24));
    assert_eq!(fun.has_catch_jump(13), Some(24));
    assert_eq!(fun.has_catch_jump(5), Some(13));
  }

  // #[test]
  // fn test() {
  //   let example = r#"
  //   class Base {
  //     foo() {
  //       print("Base.foo()");
  //     }
  //   }

  //   class Derived : Base {
  //     bar() {
  //       print("Derived.bar()");
  //       super.foo();
  //     }
  //   }

  //   Derived().bar();
  //   // expect: Derived.bar()
  //   // expect: Base.foo()

  //   "#;

  //   let context = TestContext::default();
  //   let fun = test_compile(example, &context);

  //   assert_simple_bytecode(
  //     fun,
  //     &vec![
  //       AlignedByteCode::Class(0),
  //       AlignedByteCode::DefineGlobal(0),
  //       AlignedByteCode::GetGlobal(0),
  //       AlignedByteCode::Drop,
  //       AlignedByteCode::Class(1),
  //       AlignedByteCode::DefineGlobal(1),
  //       AlignedByteCode::GetGlobal(0),
  //       AlignedByteCode::GetGlobal(1),
  //       AlignedByteCode::Inherit,
  //       AlignedByteCode::GetGlobal(1),
  //       AlignedByteCode::Drop,
  //       AlignedByteCode::Drop,
  //       AlignedByteCode::Nil,
  //       AlignedByteCode::Return,
  //     ],
  //   );
  // }

  #[test]
  fn class_with_inherit() {
    let example = "
      class A {}

      class B : A {}
    ";

    let context = TestContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::Class(0),
        AlignedByteCode::DefineGlobal(0),
        AlignedByteCode::GetGlobal(0),
        AlignedByteCode::Drop,
        AlignedByteCode::Class(1),
        AlignedByteCode::DefineGlobal(1),
        AlignedByteCode::GetGlobal(0),
        AlignedByteCode::GetGlobal(1),
        AlignedByteCode::Inherit,
        AlignedByteCode::GetGlobal(1),
        AlignedByteCode::Drop,
        AlignedByteCode::Drop,
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::Class(0),
        AlignedByteCode::DefineGlobal(0),
        AlignedByteCode::GetGlobal(0),
        AlignedByteCode::Drop,
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);

    assert_fun_bytecode(
      fun,
      &vec![
        ByteCodeTest::Code(AlignedByteCode::Class(0)),
        ByteCodeTest::Code(AlignedByteCode::DefineGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::GetGlobal(0)),
        ByteCodeTest::Fun((
          2,
          vec![
            ByteCodeTest::Code(AlignedByteCode::GetLocal(0)),
            ByteCodeTest::Code(AlignedByteCode::True),
            ByteCodeTest::Code(AlignedByteCode::SetProperty(0)),
            ByteCodeTest::Code(AlignedByteCode::Drop),
            ByteCodeTest::Code(AlignedByteCode::GetLocal(0)),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::Method(1)),
        ByteCodeTest::Code(AlignedByteCode::Field(3)),
        ByteCodeTest::Fun((
          5,
          vec![
            ByteCodeTest::Code(AlignedByteCode::GetLocal(0)),
            ByteCodeTest::Code(AlignedByteCode::GetProperty(0)),
            ByteCodeTest::Code(AlignedByteCode::Return),
            ByteCodeTest::Code(AlignedByteCode::Nil),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::Method(4)),
        ByteCodeTest::Fun((
          7,
          vec![
            ByteCodeTest::Code(AlignedByteCode::GetLocal(0)),
            ByteCodeTest::Code(AlignedByteCode::Invoke((0, 0))),
            ByteCodeTest::Code(AlignedByteCode::Return),
            ByteCodeTest::Code(AlignedByteCode::Nil),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::Method(6)),
        ByteCodeTest::Code(AlignedByteCode::Drop),
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);

    assert_fun_bytecode(
      fun,
      &vec![
        ByteCodeTest::Code(AlignedByteCode::Class(0)),
        ByteCodeTest::Code(AlignedByteCode::DefineGlobal(0)),
        ByteCodeTest::Code(AlignedByteCode::GetGlobal(0)),
        ByteCodeTest::Fun((
          2,
          vec![
            ByteCodeTest::Code(AlignedByteCode::Constant(0)),
            ByteCodeTest::Code(AlignedByteCode::Return),
            ByteCodeTest::Code(AlignedByteCode::Nil),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::StaticMethod(1)),
        ByteCodeTest::Fun((
          4,
          vec![
            ByteCodeTest::Code(AlignedByteCode::Constant(0)),
            ByteCodeTest::Code(AlignedByteCode::Return),
            ByteCodeTest::Code(AlignedByteCode::Nil),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::StaticMethod(3)),
        ByteCodeTest::Code(AlignedByteCode::Drop),
        ByteCodeTest::Code(AlignedByteCode::Nil),
        ByteCodeTest::Code(AlignedByteCode::Return),
      ],
    );
  }

  #[test]
  fn list_index_set() {
    let example = "
      let a = [clock, clock, clock];
      a[1] = 5;
    ";

    let context = TestContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::List,
        AlignedByteCode::GetGlobal(1),
        AlignedByteCode::GetGlobal(1),
        AlignedByteCode::GetGlobal(1),
        AlignedByteCode::ListInit(3),
        AlignedByteCode::DefineGlobal(0),
        AlignedByteCode::GetGlobal(0),
        AlignedByteCode::Constant(2),
        AlignedByteCode::Constant(3),
        AlignedByteCode::SetIndex,
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::List,
        AlignedByteCode::Constant(1),
        AlignedByteCode::Constant(2),
        AlignedByteCode::Constant(3),
        AlignedByteCode::ListInit(3),
        AlignedByteCode::DefineGlobal(0),
        AlignedByteCode::GetGlobal(4),
        AlignedByteCode::GetGlobal(0),
        AlignedByteCode::Constant(5),
        AlignedByteCode::GetIndex,
        AlignedByteCode::Call(1),
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::List,
        AlignedByteCode::Constant(1),
        AlignedByteCode::Constant(2),
        AlignedByteCode::Nil,
        AlignedByteCode::False,
        AlignedByteCode::Constant(3),
        AlignedByteCode::ListInit(5),
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::List,
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::Map,
        AlignedByteCode::Constant(1),
        AlignedByteCode::Constant(2),
        AlignedByteCode::Constant(3),
        AlignedByteCode::Nil,
        AlignedByteCode::MapInit(2),
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::Map,
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_fun_bytecode(
      fun,
      &vec![
        ByteCodeTest::Fun((
          // example
          1,
          vec![
            ByteCodeTest::Code(AlignedByteCode::Constant(0)),
            ByteCodeTest::Code(AlignedByteCode::Return),
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
  fn lambda_block_body() {
    let example = "
    let example = || { return 10; };
    example();
    ";

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_fun_bytecode(
      fun,
      &vec![
        ByteCodeTest::Fun((
          // example
          1,
          vec![
            ByteCodeTest::Code(AlignedByteCode::Constant(0)),
            ByteCodeTest::Code(AlignedByteCode::Return),
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
  fn fn_with_variables() {
    let example = "
    fn example(a, b, c) {
      return a + b + c; 
    }
    example(1, 2, 3);
    ";

    let context = TestContext::default();
    let fun = test_compile(example, &context);

    assert_fun_bytecode(
      fun,
      &vec![
        ByteCodeTest::Fun((
          // example
          1,
          vec![
            ByteCodeTest::Code(AlignedByteCode::GetLocal(1)),
            ByteCodeTest::Code(AlignedByteCode::GetLocal(2)),
            ByteCodeTest::Code(AlignedByteCode::Add),
            ByteCodeTest::Code(AlignedByteCode::GetLocal(3)),
            ByteCodeTest::Code(AlignedByteCode::Add),
            ByteCodeTest::Code(AlignedByteCode::Return),
            ByteCodeTest::Code(AlignedByteCode::Nil),
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
  fn open_upvalue() {
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_fun_bytecode(
      fun,
      &vec![
        ByteCodeTest::Fun((
          // example
          1,
          vec![
            ByteCodeTest::Code(AlignedByteCode::Constant(1)),
            ByteCodeTest::Fun((
              // middle
              2,
              vec![
                ByteCodeTest::Fun((
                  // inner
                  0,
                  vec![
                    ByteCodeTest::Code(AlignedByteCode::GetUpvalue(0)),
                    ByteCodeTest::Code(AlignedByteCode::Return),
                    ByteCodeTest::Code(AlignedByteCode::Nil),
                    ByteCodeTest::Code(AlignedByteCode::Return),
                  ],
                )),
                ByteCodeTest::Code(AlignedByteCode::UpvalueIndex(UpvalueIndex::Upvalue(0))),
                ByteCodeTest::Code(AlignedByteCode::GetLocal(1)),
                ByteCodeTest::Code(AlignedByteCode::Call(0)),
                ByteCodeTest::Code(AlignedByteCode::Return),
                ByteCodeTest::Code(AlignedByteCode::Nil),
                ByteCodeTest::Code(AlignedByteCode::Return),
              ],
            )),
            ByteCodeTest::Code(AlignedByteCode::UpvalueIndex(UpvalueIndex::Local(1))), //
            ByteCodeTest::Code(AlignedByteCode::GetLocal(2)),
            ByteCodeTest::Code(AlignedByteCode::Call(0)),
            ByteCodeTest::Code(AlignedByteCode::Return),
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
  fn close_upvalue() {
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_fun_bytecode(
      fun,
      &vec![
        ByteCodeTest::Fun((
          1,
          vec![
            ByteCodeTest::Code(AlignedByteCode::Constant(1)),
            ByteCodeTest::Fun((
              2,
              vec![
                ByteCodeTest::Code(AlignedByteCode::GetUpvalue(0)),
                ByteCodeTest::Code(AlignedByteCode::Return),
                ByteCodeTest::Code(AlignedByteCode::Nil),
                ByteCodeTest::Code(AlignedByteCode::Return),
              ],
            )),
            ByteCodeTest::Code(AlignedByteCode::UpvalueIndex(UpvalueIndex::Local(1))),
            ByteCodeTest::Code(AlignedByteCode::GetLocal(2)),
            ByteCodeTest::Code(AlignedByteCode::Return),
            ByteCodeTest::Code(AlignedByteCode::Nil),
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_fun_bytecode(
      fun,
      &vec![
        ByteCodeTest::Fun((
          1,
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_fun_bytecode(
      fun,
      &vec![
        ByteCodeTest::Fun((
          1,
          vec![
            ByteCodeTest::Code(AlignedByteCode::GetLocal(1)),
            ByteCodeTest::Code(AlignedByteCode::Return),
            ByteCodeTest::Code(AlignedByteCode::Nil),
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_fun_bytecode(
      fun,
      &vec![
        ByteCodeTest::Fun((
          1,
          vec![
            ByteCodeTest::Code(AlignedByteCode::Constant(1)),
            ByteCodeTest::Code(AlignedByteCode::GetLocal(1)),
            ByteCodeTest::Code(AlignedByteCode::Return),
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
  fn map() {
    let example = "let a = { \"cat\": \"bat\", 10: nil };";

    let context = TestContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::Map,             // 0
        AlignedByteCode::Constant(1),     // 1
        AlignedByteCode::Constant(2),     // 3
        AlignedByteCode::Constant(3),     // 5
        AlignedByteCode::Nil,             // 7
        AlignedByteCode::MapInit(2),      // 8
        AlignedByteCode::DefineGlobal(0), // 11
        AlignedByteCode::Nil,             // 13
        AlignedByteCode::Return,          // 14
      ],
    );
  }

  #[test]
  fn list() {
    let example = "let a = [1, 2, 3, \"cat\"];";

    let context = TestContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::List,            // 0
        AlignedByteCode::Constant(1),     // 1
        AlignedByteCode::Constant(2),     // 3
        AlignedByteCode::Constant(3),     // 5
        AlignedByteCode::Constant(4),     // 7
        AlignedByteCode::ListInit(4),     // 9
        AlignedByteCode::DefineGlobal(0), // 12
        AlignedByteCode::Nil,             // 14
        AlignedByteCode::Return,          // 15
      ],
    );
  }

  #[test]
  fn for_loop() {
    let example = "for x in [1, 2, 3] { print(x); }";

    let context = TestContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::Nil,             // 0
        AlignedByteCode::List,            // 1   local 1 = x
        AlignedByteCode::Constant(0),     // 2   local 2 = [1, 2, 3].iter()
        AlignedByteCode::Constant(1),     // 4   local 3 =
        AlignedByteCode::Constant(2),     // 6
        AlignedByteCode::ListInit(3),     // 8
        AlignedByteCode::Invoke((3, 0)),  // 11  const 1 = 1
        AlignedByteCode::GetLocal(2),     // 14  const 2 = 2
        AlignedByteCode::IterNext(5),     // 16  const 3 = 3
        AlignedByteCode::JumpIfFalse(20), // 18  const 4 = "iter"
        AlignedByteCode::Drop,            // 21  const 5 = "next"
        AlignedByteCode::GetLocal(2),     // 22
        AlignedByteCode::IterCurrent(6),  // 24  const 6 = "current"
        AlignedByteCode::SetLocal(1),     // 26
        AlignedByteCode::Drop,            // 28
        AlignedByteCode::GetGlobal(7),
        AlignedByteCode::GetLocal(1), // 29
        AlignedByteCode::Call(1),
        AlignedByteCode::Drop,
        AlignedByteCode::Loop(28), // 32
        AlignedByteCode::Drop,     // 35
        AlignedByteCode::DropN(2), // 36
        AlignedByteCode::Nil,      // 38
        AlignedByteCode::Return,   // 39
      ],
    );
  }

  #[test]
  fn while_loop() {
    let example = "while true { print(10); }";
    let context = TestContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::True,
        AlignedByteCode::JumpIfFalse(12),
        AlignedByteCode::Drop,
        AlignedByteCode::GetGlobal(0),
        AlignedByteCode::Constant(1),
        AlignedByteCode::Call(1),
        AlignedByteCode::Drop,
        AlignedByteCode::Loop(16),
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn and_operator() {
    let example = "true and false;";
    let context = TestContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::True,
        AlignedByteCode::JumpIfFalse(2),
        AlignedByteCode::Drop,
        AlignedByteCode::False,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn or_operator() {
    let example = "false or true;";
    let context = TestContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::False,
        AlignedByteCode::JumpIfFalse(3),
        AlignedByteCode::Jump(2),
        AlignedByteCode::Drop,
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::Constant(0),
        AlignedByteCode::Constant(1),
        AlignedByteCode::Less,
        AlignedByteCode::JumpIfFalse(12),
        AlignedByteCode::Drop,
        AlignedByteCode::GetGlobal(2),
        AlignedByteCode::Constant(3),
        AlignedByteCode::Call(1),
        AlignedByteCode::Drop,
        AlignedByteCode::Jump(1),
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn if_else_condition() {
    let example = "if (3 < 10) { print(\"hi\"); } else { print(\"bye\"); }";

    let context = TestContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::Constant(0),     // 0
        AlignedByteCode::Constant(1),     // 2
        AlignedByteCode::Less,            // 4
        AlignedByteCode::JumpIfFalse(12), // 5
        AlignedByteCode::Drop,            // 8
        AlignedByteCode::GetGlobal(2),
        AlignedByteCode::Constant(3), // 9
        AlignedByteCode::Call(1),     // 11
        AlignedByteCode::Drop,
        AlignedByteCode::Jump(9), // 12
        AlignedByteCode::Drop,    // 15
        AlignedByteCode::GetGlobal(2),
        AlignedByteCode::Constant(4), // 17
        AlignedByteCode::Call(1),
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,    // 19
        AlignedByteCode::Return, // 20
      ],
    );
  }

  #[test]
  fn declare_local() {
    let example = ":{ let x = 10; };";

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::Constant(1),
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_get_local() {
    let example = ":{ let x = 10; print(x); };";

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::Constant(1),
        AlignedByteCode::GetGlobal(2),
        AlignedByteCode::GetLocal(1),
        AlignedByteCode::Call(1),
        AlignedByteCode::Drop,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_set_local() {
    let example = ":{ let x = 10; x = 5; };";

    let context = TestContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::Constant(1),
        AlignedByteCode::Constant(2),
        AlignedByteCode::SetLocal(1),
        AlignedByteCode::Drop,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_define_global_nil() {
    let example = "let x;";

    let context = TestContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      fun,
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);

    assert_simple_bytecode(
      fun,
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
    let example = "print(x);";

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::GetGlobal(0),
        AlignedByteCode::GetGlobal(1),
        AlignedByteCode::Call(1),
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_set_global() {
    let example = "x = \"cat\";";

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::Constant(0),
        AlignedByteCode::SetGlobal(1),
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_pop() {
    let example = "false;";

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      fun,
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(fun, &vec![AlignedByteCode::Nil, AlignedByteCode::Return]);
  }

  #[test]
  fn op_number() {
    let example = "5.18;";

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      fun,
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::Constant(0),
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_false() {
    let example = "false;";

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      fun,
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::True,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_nil() {
    let example = "nil;";

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      fun,
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      fun,
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
    let example = "-15;";

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      fun,
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
  fn op_add() {
    let example = "10 + 4;";

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      fun,
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      fun,
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      fun,
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      fun,
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      fun,
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      fun,
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      fun,
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      fun,
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      fun,
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

    let context = TestContext::default();
    let fun = test_compile(example, &context);
    assert_simple_bytecode(
      fun,
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
