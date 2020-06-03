use crate::scanner::Scanner;
use spacelox_core::chunk::{AlignedByteCode, Chunk, UpvalueIndex};
use spacelox_core::token::{Token, TokenKind};
use spacelox_core::utils::{copy_string, do_if_some};
use spacelox_core::{
  arity::ArityKind,
  constants::{INIT, ITER, ITER_VAR, SCRIPT, SUPER, THIS},
  hooks::Hooks,
  module::Module,
  object::SlHashMap,
  object::{Fun, FunKind},
  value::Value,
};
use spacelox_env::{
  io::Io,
  managed::{Manage, Managed, Trace},
  stdio::StdIo,
};
use std::convert::TryInto;
use std::mem;

#[cfg(feature = "debug")]
use crate::debug::disassemble_chunk;

/// The result of a compilation
pub struct CompilerResult {
  /// Was an error encountered while this chunk was compiled
  pub success: bool,

  /// The chunk that was compiled
  pub fun: Managed<Fun>,
}

const UNINITIALIZED: i16 = -1;

#[derive(Debug, Clone)]
pub struct Local {
  /// name of the local
  name: Option<String>,

  /// depth of the local
  depth: i16,

  /// is this local captured
  is_captured: bool,
}

pub enum ForLoop {
  Range(Token),
  CStyle,
}

/// The spacelox compiler for converting tokens to bytecode
pub struct Compiler<'a, 's, I: Io + 'static> {
  /// The current function
  fun: Managed<Fun>,

  /// The type the current function scope
  fun_kind: FunKind,

  /// The current module
  module: Managed<Module>,

  /// The parent compiler if it exists note uses
  /// unsafe pointer
  enclosing: Option<*mut Compiler<'a, 's, I>>,

  /// The parser in charge incrementing the scanner and
  /// expecting / consuming tokens
  parser: &'a mut Parser<'s, I::StdIo>,

  /// The current class class compiler
  current_class: Option<Managed<ClassCompiler>>,

  /// hooks into the surround context. Used to allocate spacelox objects
  hooks: &'a Hooks<'a>,

  /// The environments standard io access
  io: I,

  /// Number of locals
  local_count: usize,

  /// Current scope depth
  scope_depth: i16,

  /// locals in this function
  locals: Vec<Local>,

  /// upvalues in this function
  upvalues: Vec<UpvalueIndex>,

  /// A set of constants used in the current function
  constants: SlHashMap<Value, usize>,
}

impl<'a, 's, I: Io + Clone> Compiler<'a, 's, I> {
  /// Create a new instance of the spacelox compiler.
  /// The compiler write a sequence of op codes to the chunk
  /// to be executed
  ///
  /// # Examples
  /// ```
  /// use spacelox_vm::compiler::{Compiler, Parser};
  /// use spacelox_core::module::Module;
  /// use spacelox_core::hooks::{Hooks, NoContext};
  /// use spacelox_env::io::NativeIo;
  /// use spacelox_env::stdio::NativeStdIo;
  /// use spacelox_env::memory::Gc;
  /// use std::path::PathBuf;
  ///
  /// // an expression
  /// let source = "10 + 3".to_string();
  ///
  /// let gc = Gc::default();
  /// let mut context = NoContext::new(&gc);
  /// let mut hooks = Hooks::new(&mut context);
  /// let mut parser = Parser::new(NativeStdIo::new(), &source);
  /// let module = hooks.manage(Module::new(
  ///  hooks.manage_str("module".to_string()),
  ///  hooks.manage(PathBuf::from("./module.lox"))
  /// ));
  ///
  /// let compiler = Compiler::new(module, NativeIo::default(), &mut parser, &mut hooks);
  /// ```
  pub fn new(
    module: Managed<Module>,
    io: I,
    parser: &'a mut Parser<'s, I::StdIo>,
    hooks: &'a Hooks<'a>,
  ) -> Self {
    let fun = hooks.manage(Fun::new(hooks.manage_str(String::from(SCRIPT)), module));

    let mut compiler = Self {
      fun,
      fun_kind: FunKind::Script,
      module,
      current_class: None,
      hooks,
      io,
      parser,
      enclosing: None,
      local_count: 1,
      scope_depth: 0,
      locals: vec![
        Local {
          name: Option::None,
          depth: UNINITIALIZED,
          is_captured: false,
        };
        std::u8::MAX as usize
      ],
      upvalues: vec![UpvalueIndex::Local(0); std::u8::MAX as usize],
      constants: SlHashMap::default(),
    };

    compiler.locals[0] = first_local(FunKind::Script);
    compiler
  }

  /// Construct an inner compiler used to compile functions inside of a script
  fn child(name: Managed<String>, fun_kind: FunKind, enclosing: *mut Compiler<'a, 's, I>) -> Self {
    let mut child = Self {
      fun: unsafe { (*enclosing).fun },
      fun_kind: fun_kind.clone(),
      module: unsafe { (*enclosing).module },
      current_class: unsafe { (*enclosing).current_class },
      hooks: unsafe { (*enclosing).hooks },
      io: unsafe { (*enclosing).io },
      parser: unsafe { (*enclosing).parser },
      enclosing: Some(enclosing),
      local_count: 1,
      scope_depth: 0,
      locals: vec![
        Local {
          name: Option::None,
          depth: UNINITIALIZED,
          is_captured: false,
        };
        std::u8::MAX as usize
      ],
      upvalues: vec![UpvalueIndex::Local(0); std::u8::MAX as usize],
      constants: SlHashMap::default(),
    };

    child.fun = child.hooks.manage(Fun::new(name, child.module));

    child.locals[0] = first_local(fun_kind);

    child
  }

  /// Compile the provided source code onto the chunk
  /// returns true if the compiler ran without parser errors
  ///
  /// # Examples
  /// ```
  /// use spacelox_vm::compiler::{Compiler, Parser};
  /// use spacelox_core::module::Module;
  /// use spacelox_core::hooks::{Hooks, NoContext};
  /// use spacelox_env::io::NativeIo;
  /// use spacelox_env::stdio::NativeStdIo;
  /// use spacelox_env::memory::Gc;
  /// use std::path::PathBuf;
  ///
  /// // an expression
  /// let source = "3 / 2 + 10;".to_string();
  ///
  /// let gc = Gc::new(Box::new(NativeStdIo::new()));
  /// let mut context = NoContext::new(&gc);
  /// let mut hooks = Hooks::new(&mut context);
  /// let mut parser = Parser::new(NativeStdIo::new(), &source);
  /// let module = hooks.manage(Module::new(
  ///  hooks.manage_str("module".to_string()),
  ///  hooks.manage(PathBuf::from("./module.lox"))
  /// ));
  ///
  /// let compiler = Compiler::new(module, NativeIo::default(), &mut parser, &mut hooks);
  /// let result = compiler.compile();
  /// assert_eq!(result.success, true);
  /// ```
  pub fn compile(mut self) -> CompilerResult {
    self.parser.advance();

    // early exit if ""
    if let TokenKind::Eof = self.parser.current.kind {
      self.end_compiler();

      self.fun.shrink_to_fit(self.hooks);
      return CompilerResult {
        success: !self.parser.had_error,
        fun: self.fun,
      };
    }

    while !self.parser.match_kind(TokenKind::Eof) {
      self.declaration()
    }

    self
      .parser
      .consume(TokenKind::Eof, "Expected end of expression.");

    self.end_compiler();
    self.fun.shrink_to_fit(self.hooks);

    CompilerResult {
      success: !self.parser.had_error,
      fun: self.fun,
    }
  }

  /// The current chunk
  fn current_chunk(&mut self) -> &Chunk {
    self.fun.chunk()
  }

  /// write instruction to the current function
  fn write_instruction(&mut self, op_code: AlignedByteCode, line: u32) {
    self.fun.write_instruction(self.hooks, op_code, line)
  }

  /// Parse a declaration
  fn declaration(&mut self) {
    match self.parser.current.kind {
      TokenKind::Class => {
        self.parser.advance();
        self.class_declaration();
      }
      TokenKind::Fun => {
        self.parser.advance();
        self.fun_declaration();
      }
      TokenKind::Var => {
        self.parser.advance();
        self.var_declaration();
      }
      TokenKind::Export => {
        self.parser.advance();
        self.export_declaration();
      }
      _ => {
        self.statement();
      }
    }

    if self.parser.panic_mode {
      self.synchronize(None);
    }
  }

  /// Parse a symbol declaration
  fn export_declaration(&mut self) {
    let symbol = match self.parser.current.kind {
      TokenKind::Class => {
        self.parser.advance();
        Some(self.class_declaration())
      }
      TokenKind::Fun => {
        self.parser.advance();
        Some(self.fun_declaration())
      }
      TokenKind::Var => {
        self.parser.advance();
        Some(self.var_declaration())
      }
      _ => None,
    };

    if self.scope_depth == 0 {
      match symbol {
        Some(valid_symbol) => {
          self.emit_byte(AlignedByteCode::Export(valid_symbol));
        }
        None => self
          .parser
          .error_at_current("Can only export variable, function or class declarations."),
      }
    } else {
      self
        .parser
        .error_at_current("Can only export from the module scope.")
    }
  }

  /// Parse a statement
  fn statement(&mut self) {
    match self.parser.current.kind {
      TokenKind::Print => {
        self.parser.advance();
        self.print_statement();
      }
      TokenKind::For => {
        self.parser.advance();
        self.for_statement();
      }
      TokenKind::If => {
        self.parser.advance();
        self.if_statement();
      }
      TokenKind::Return => {
        self.parser.advance();
        self.return_statement();
      }
      TokenKind::While => {
        self.parser.advance();
        self.while_statement();
      }
      TokenKind::Import => {
        self.parser.advance();
        self.import_statement();
      }
      TokenKind::LeftBrace => {
        self.parser.advance();
        self.begin_scope();
        self.block();
        self.end_scope();
      }
      _ => {
        self.expression_statement();
      }
    }
  }

  /// Parse an expression
  fn expression(&mut self) {
    self.parse_precedence(Precedence::Assignment)
  }

  /// Parse a block statement
  fn block(&mut self) {
    while !self.parser.check(TokenKind::RightBrace) && !self.parser.check(TokenKind::Eof) {
      self.declaration();
    }

    self
      .parser
      .consume(TokenKind::RightBrace, "Expect '}' after block.")
  }

  /// Parse a class declaration
  fn class_declaration(&mut self) -> u16 {
    self
      .parser
      .consume(TokenKind::Identifier, "Expect class name.");

    let class_name = self.parser.previous.clone();
    let name_constant = self.identifer_constant(self.parser.previous.clone());
    self.declare_variable(self.parser.previous.clone());

    self.emit_byte(AlignedByteCode::Class(name_constant));
    self.define_variable(name_constant);

    let mut class_compiler = self.hooks.manage(ClassCompiler {
      name: self.parser.previous.clone(),
      has_super_class: false,
      enclosing: self.current_class,
    });
    self.current_class = Some(class_compiler);

    if self.parser.match_kind(TokenKind::Less) {
      self
        .parser
        .consume(TokenKind::Identifier, "Expect superclass name.");
      self.variable(false);

      if class_name.lexeme == self.parser.previous.lexeme {
        self.parser.error("A class cannot inherit from itself.");
      }

      self.begin_scope();
      self.add_local(Token {
        kind: TokenKind::Super,
        lexeme: "super".to_string(),
        line: class_name.line,
      });
      self.define_variable(0);

      self.named_variable(class_name.clone(), false);
      self.emit_byte(AlignedByteCode::Inherit);

      class_compiler.has_super_class = true;
    }

    self.named_variable(class_name, false);

    self
      .parser
      .consume(TokenKind::LeftBrace, "Expect '{' before class body.");
    while !self.parser.check(TokenKind::RightBrace) && !self.parser.check(TokenKind::Eof) {
      self.method();
    }

    self
      .parser
      .consume(TokenKind::RightBrace, "Expect '}' after class body.");
    self.emit_byte(AlignedByteCode::Drop);

    if class_compiler.has_super_class {
      self.end_scope();
    }

    self.current_class = class_compiler.enclosing;
    name_constant
  }

  /// Parse a function declaration
  fn fun_declaration(&mut self) -> u16 {
    let global = self.parse_variable("Expect variable name.");

    self.mark_initialized();
    self.function(FunKind::Fun);
    self.define_variable(global);
    global
  }

  /// Parse a function declaration and body
  fn function(&mut self, fun_kind: FunKind) {
    let name = self
      .hooks
      .manage_str(self.parser.previous.lexeme.to_string());

    let mut fun_compiler = Compiler::child(name, fun_kind, &mut *self);
    fun_compiler.begin_scope();

    fun_compiler
      .parser
      .consume(TokenKind::LeftParen, "Expect '(' after function name.");

    // parse function parameters
    if !fun_compiler.parser.check(TokenKind::RightParen) {
      fun_compiler.function_arity();
    }

    fun_compiler
      .parser
      .consume(TokenKind::RightParen, "Expect ')' after parameters.");

    fun_compiler
      .parser
      .consume(TokenKind::LeftBrace, "Expect '{' before function body.");
    fun_compiler.block();

    // end compilation of function chunk
    fun_compiler.end_compiler();
    let upvalue_count = fun_compiler.fun.upvalue_count;

    let index = self.make_constant(Value::from(fun_compiler.fun));
    self.emit_byte(AlignedByteCode::Closure(index));

    // emit upvalue index instructions
    fun_compiler.upvalues[0..upvalue_count]
      .iter()
      .for_each(|upvalue| self.emit_byte(AlignedByteCode::UpvalueIndex(*upvalue)));
  }

  fn method(&mut self) {
    self
      .parser
      .consume(TokenKind::Identifier, "Expect method name.");
    let constant = self.identifer_constant(self.parser.previous.clone());

    let fun_kind = if INIT == self.parser.previous.lexeme {
      FunKind::Initializer
    } else {
      FunKind::Method
    };

    self.function(fun_kind);
    self.emit_byte(AlignedByteCode::Method(constant));
  }

  /// Parse a variable declaration
  fn var_declaration(&mut self) -> u16 {
    let variable = self.parse_variable("Expect variable name.");

    if self.parser.match_kind(TokenKind::Equal) {
      self.expression();
    } else {
      self.emit_byte(AlignedByteCode::Nil);
    }

    self.parser.consume(
      TokenKind::Semicolon,
      "Expect ';' after variable declaration.",
    );

    self.define_variable(variable);
    variable
  }

  /// Parse an expression statement
  fn expression_statement(&mut self) {
    self.expression();
    self
      .parser
      .consume(TokenKind::Semicolon, "Expected ';' after expression.");
    self.emit_byte(AlignedByteCode::Drop)
  }

  /// Parse for loop
  fn for_statement(&mut self) {
    self.begin_scope();
    self
      .parser
      .consume(TokenKind::LeftParen, "Expected '(' after 'for'.");

    // parse an initializer
    let style: ForLoop = if self.parser.match_kind(TokenKind::Semicolon) {
      ForLoop::CStyle
    } else if self.parser.match_kind(TokenKind::Var) {
      self.for_var_declaration()
    } else {
      self.expression_statement();
      ForLoop::CStyle
    };

    match style {
      ForLoop::CStyle => self.for_cstyle_body(),
      ForLoop::Range(variable) => self.for_range_body(variable),
    }

    self.end_scope();
  }

  /// Parse a variable declaration in a for loop
  fn for_var_declaration(&mut self) -> ForLoop {
    let variable = self.parse_variable("Expect variable name.");
    let variable_token = self.parser.previous.clone();

    let style = if self.parser.match_kind(TokenKind::Equal) {
      // assignment indicates a c style
      self.expression();
      ForLoop::CStyle
    } else {
      self.emit_byte(AlignedByteCode::Nil);

      // the in keyword indicates a range for loop
      if self.parser.match_kind(TokenKind::In) {
        ForLoop::Range(variable_token)
      } else {
        ForLoop::CStyle
      }
    };

    if let ForLoop::CStyle = style {
      self.parser.consume(
        TokenKind::Semicolon,
        "Expect ';' after variable declaration.",
      );
    }

    self.define_variable(variable);
    style
  }

  /// Parse the body of a c style for loop
  fn for_cstyle_body(&mut self) {
    let mut loop_start = self.current_chunk().instructions.len();

    // parse loop condition
    let mut exit_jump: Option<usize> = Option::None;
    if !self.parser.match_kind(TokenKind::Semicolon) {
      self.expression();
      self
        .parser
        .consume(TokenKind::Semicolon, "Expected ';' after loop condition");
      exit_jump = Some(self.emit_jump(AlignedByteCode::JumpIfFalse(0)));
      self.emit_byte(AlignedByteCode::Drop);
    }

    // parse incrementor
    if !self.parser.match_kind(TokenKind::RightParen) {
      let body_jump = self.emit_jump(AlignedByteCode::Jump(0));

      let increment_start = self.current_chunk().instructions.len();
      self.expression();
      self.emit_byte(AlignedByteCode::Drop);
      self
        .parser
        .consume(TokenKind::RightParen, "Expect ')' after for clauses.");

      self.emit_loop(loop_start);
      loop_start = increment_start;

      self.patch_jump(body_jump);
    }

    self.statement();
    self.emit_loop(loop_start);

    // patch exit jump
    if let Some(jump) = exit_jump {
      self.patch_jump(jump);
      self.emit_byte(AlignedByteCode::Drop);
    }
  }

  /// Parse the body of a range style for loop
  fn for_range_body(&mut self, loop_token: Token) {
    // push collection onto stack
    self.expression();

    let iterator_token = Token {
      lexeme: ITER_VAR.to_string(),
      kind: TokenKind::Identifier,
      line: self.parser.previous.line,
    };

    // get constant for 'iter' method
    let iter_const = self.identifer_constant(Token {
      lexeme: ITER.to_string(),
      kind: TokenKind::Identifier,
      line: self.parser.previous.line,
    });

    // declare the hidden local $iter variable
    let iterator_const = self.identifer_constant(iterator_token.clone());
    self.declare_variable(iterator_token.clone());
    self.emit_byte(AlignedByteCode::Invoke((iter_const, 0)));
    self.define_variable(iterator_const);

    self
      .parser
      .consume(TokenKind::RightParen, "Expected ')' after iterable.");

    // mark start of loop
    let loop_start = self.current_chunk().instructions.len();

    // define iterator method constants
    let next_const = self.identifer_constant(Token {
      lexeme: "next".to_string(),
      kind: TokenKind::Identifier,
      line: self.parser.previous.line,
    });

    let current_const = self.identifer_constant(Token {
      lexeme: "current".to_string(),
      kind: TokenKind::Identifier,
      line: self.parser.previous.line,
    });

    // call next on iterator
    let iterator_variable = self
      .resolve_local(&iterator_token)
      .expect("Iterator variable was not defined.");
    self.emit_byte(AlignedByteCode::GetLocal(iterator_variable));
    self.emit_byte(AlignedByteCode::IterNext(next_const));

    // check at end of iterator
    let exit_jump = self.emit_jump(AlignedByteCode::JumpIfFalse(0));
    self.emit_byte(AlignedByteCode::Drop);

    // assign $iter.current to loop variable
    let loop_variable = self
      .resolve_local(&loop_token)
      .expect("Loop variable was not defined.");
    self.emit_byte(AlignedByteCode::GetLocal(iterator_variable));
    self.emit_byte(AlignedByteCode::IterCurrent(current_const));
    self.emit_byte(AlignedByteCode::SetLocal(loop_variable));
    self.emit_byte(AlignedByteCode::Drop);

    // loop body
    self.statement();
    self.emit_loop(loop_start);

    // loop back to top
    self.patch_jump(exit_jump);
    self.emit_byte(AlignedByteCode::Drop);
  }

  /// Parse while statement
  fn while_statement(&mut self) {
    let loop_start = self.current_chunk().instructions.len();

    self
      .parser
      .consume(TokenKind::LeftParen, "Expected '(' after 'while'.");
    self.expression();
    self
      .parser
      .consume(TokenKind::RightParen, "Expected ')' after condition.");

    let exit_jump = self.emit_jump(AlignedByteCode::JumpIfFalse(0));

    self.emit_byte(AlignedByteCode::Drop);
    self.statement();

    self.emit_loop(loop_start);

    self.patch_jump(exit_jump);
    self.emit_byte(AlignedByteCode::Drop)
  }

  /// Compile a if statement
  fn if_statement(&mut self) {
    // parse condition
    self
      .parser
      .consume(TokenKind::LeftParen, "Expected '(' after 'if'.");
    self.expression();
    self
      .parser
      .consume(TokenKind::RightParen, "Expected ')' after condition.");

    // parse then branch
    let then_jump = self.emit_jump(AlignedByteCode::JumpIfFalse(0));
    self.emit_byte(AlignedByteCode::Drop);
    self.statement();

    // emit else jump
    let else_jump = self.emit_jump(AlignedByteCode::Jump(0));
    self.patch_jump(then_jump);
    self.emit_byte(AlignedByteCode::Drop);

    // parse else branch if it exists
    if self.parser.match_kind(TokenKind::Else) {
      self.statement();
    }

    self.patch_jump(else_jump);
  }

  /// Parse an import statement
  fn import_statement(&mut self) {
    self
      .parser
      .consume(TokenKind::Identifier, "Expected name following import.");
    let name = self.identifer_constant(self.parser.previous.clone());

    self
      .parser
      .consume(TokenKind::From, "Expected 'from' following import name.");

    self
      .parser
      .consume(TokenKind::String, "Expected path string after 'from'.");
    let string = self.hooks.manage_str(copy_string(&self.parser.previous));
    let value = Value::from(string);
    let path = self.make_constant(value);
    self
      .parser
      .consume(TokenKind::Semicolon, "Expect ';' after value.");

    if self.scope_depth == 0 {
      self.emit_byte(AlignedByteCode::Import(path));
      self.emit_byte(AlignedByteCode::DefineGlobal(name));
    } else {
      self
        .parser
        .error_at_current("Can only import from the module scope.")
    }
  }

  /// Parse print statement
  fn print_statement(&mut self) {
    self.expression();
    self
      .parser
      .consume(TokenKind::Semicolon, "Expect ';' after value.");
    self.emit_byte(AlignedByteCode::Print)
  }

  fn return_statement(&mut self) {
    if self.fun_kind == FunKind::Script {
      self.parser.error("Cannot return from top-level code.");
    }

    if self.parser.match_kind(TokenKind::Semicolon) {
      self.emit_return();
    } else {
      if let FunKind::Initializer = self.fun_kind {
        self
          .parser
          .error("Cannot return a value from an initializer.");
      }

      self.expression();
      self
        .parser
        .consume(TokenKind::Semicolon, "Expect ',' after return value.");
      self.emit_byte(AlignedByteCode::Return);
    }
  }

  /// Synchronize the compiler to a sentinel token
  fn synchronize(&mut self, optional_stop: Option<TokenKind>) {
    self.parser.panic_mode = false;

    while self.parser.current.kind != TokenKind::Eof {
      if self.parser.previous.kind == TokenKind::Semicolon {
        return;
      }

      match self.parser.current.kind {
        TokenKind::Class
        | TokenKind::Fun
        | TokenKind::Var
        | TokenKind::For
        | TokenKind::If
        | TokenKind::While
        | TokenKind::Print
        | TokenKind::Return => {
          return;
        }
        _ => {}
      }

      if let Some(stop) = optional_stop {
        if self.parser.current.kind == stop {
          return;
        }
      }

      self.parser.advance();
    }
  }

  /// End the compilation at eof
  fn end_compiler(&mut self) {
    self.emit_return();

    #[cfg(feature = "debug")]
    self.print_chunk();
  }

  /// Increase the scope depth by 1
  fn begin_scope(&mut self) {
    self.scope_depth += 1;
  }

  /// Decrease the scope depth by 1
  fn end_scope(&mut self) {
    self.scope_depth -= 1;

    while self.local_count > 0 && self.locals[self.local_count - 1].depth > self.scope_depth {
      if self.locals[self.local_count - 1].is_captured {
        self.emit_byte(AlignedByteCode::CloseUpvalue)
      } else {
        self.emit_byte(AlignedByteCode::Drop);
      }
      self.local_count -= 1;
    }
  }

  /// Print the chunk if debug and an error occurred
  #[cfg(feature = "debug")]
  fn print_chunk(&self) {
    disassemble_chunk(&self.io.stdio(), self.fun.chunk(), &self.fun.name)
  }

  /// Compiles a binary expression into it's equivalent bytecodes
  ///
  /// # Panics
  /// This method will panic if an invalid binary operator is passed
  fn binary(&mut self) {
    // Remember the operator
    let operator_kind = self.parser.previous.kind;
    let precedence = get_rule(operator_kind).precedence.higher();
    self.parse_precedence(precedence);

    match operator_kind {
      TokenKind::BangEqual => self.emit_bytes(AlignedByteCode::Equal, AlignedByteCode::Not),
      TokenKind::EqualEqual => self.emit_byte(AlignedByteCode::Equal),
      TokenKind::Greater => self.emit_byte(AlignedByteCode::Greater),
      TokenKind::GreaterEqual => self.emit_bytes(AlignedByteCode::Less, AlignedByteCode::Not),
      TokenKind::Less => self.emit_byte(AlignedByteCode::Less),
      TokenKind::LessEqual => self.emit_bytes(AlignedByteCode::Greater, AlignedByteCode::Not),
      TokenKind::Plus => self.emit_byte(AlignedByteCode::Add),
      TokenKind::Minus => self.emit_byte(AlignedByteCode::Subtract),
      TokenKind::Star => self.emit_byte(AlignedByteCode::Multiply),
      TokenKind::Slash => self.emit_byte(AlignedByteCode::Divide),
      _ => panic!("Invalid operator"),
    }
  }

  /// Compile a call invocation
  fn call(&mut self) {
    let arg_count = self.call_arguments();
    self.emit_byte(AlignedByteCode::Call(arg_count));
  }

  /// Compile a call invocation
  fn list(&mut self) {
    self.emit_byte(AlignedByteCode::List);
    let arg_count = self.list_arguments();

    if arg_count > 0 {
      self.emit_byte(AlignedByteCode::ListInit(arg_count));
    }
  }

  /// Compile an index
  fn index(&mut self, can_assign: bool) {
    self.expression();
    self
      .parser
      .consume(TokenKind::RightBracket, "Expected ']' after index");

    if can_assign && self.parser.match_kind(TokenKind::Equal) {
      self.expression();
      self.emit_byte(AlignedByteCode::SetIndex);
    } else {
      self.emit_byte(AlignedByteCode::GetIndex);
    }
  }

  /// Compile a map literal
  fn map(&mut self) {
    self.emit_byte(AlignedByteCode::Map);
    let mut entries: usize = 0;

    while !self.parser.check(TokenKind::RightBrace) {
      self.expression();
      self
        .parser
        .consume(TokenKind::Colon, "Expected ':' after map key");
      self.expression();

      if entries == std::u16::MAX as usize {
        self.parser.error(&format!(
          "Cannot have more than {} key value pairs in map literal",
          entries
        ));
      }
      entries += 1;

      if !self.parser.match_kind(TokenKind::Comma) {
        break;
      }
    }

    self
      .parser
      .consume(TokenKind::RightBrace, "Expected '}' after map");

    if entries > 0 {
      self.emit_byte(AlignedByteCode::MapInit(entries as u16))
    }
  }

  /// Compile a lambda expression
  fn lambda(&mut self) {
    let name = self.hooks.manage_str("lambda".to_string());

    let mut fun_compiler = Compiler::child(name, FunKind::Fun, &mut *self);
    fun_compiler.begin_scope();

    // parse function parameters
    if !fun_compiler.parser.check(TokenKind::Pipe) {
      fun_compiler.function_arity();
    }

    fun_compiler
      .parser
      .consume(TokenKind::Pipe, "Expect '|' after lambda parameters.");

    if fun_compiler.parser.match_kind(TokenKind::LeftBrace) {
      fun_compiler.block();
    } else {
      // implicitly return expression lambdas
      fun_compiler.expression();
      fun_compiler.emit_byte(AlignedByteCode::Return)
    }

    // end compilation of function chunk
    fun_compiler.end_compiler();
    let upvalue_count = fun_compiler.fun.upvalue_count;
    // let boxed_fun = Box::new(fun_compiler.fun);

    let index = self.make_constant(Value::from(fun_compiler.fun));
    self.emit_byte(AlignedByteCode::Closure(index));

    // emit upvalue index instructions
    fun_compiler.upvalues[0..upvalue_count]
      .iter()
      .for_each(|upvalue| self.emit_byte(AlignedByteCode::UpvalueIndex(*upvalue)));
  }

  /// Compile an dot operator
  fn dot(&mut self, can_assign: bool) {
    self
      .parser
      .consume(TokenKind::Identifier, "Expect property name after '.'.");
    let name = self.identifer_constant(self.parser.previous.clone());

    if can_assign && self.parser.match_kind(TokenKind::Equal) {
      self.expression();
      self.emit_byte(AlignedByteCode::SetProperty(name));
    } else if self.parser.match_kind(TokenKind::LeftParen) {
      let arg_count = self.call_arguments();
      self.emit_byte(AlignedByteCode::Invoke((name, arg_count)));
    } else {
      self.emit_byte(AlignedByteCode::GetProperty(name));
    }
  }

  /// Compile a unary expression into it's equivalent bytecode
  ///
  /// # Panics
  /// This method will panic if an invalid unary operator is attempted to compile
  fn unary(&mut self) {
    let operator_kind = self.parser.previous.kind;

    // Compile the operand
    self.parse_precedence(Precedence::Unary);

    // Emit the operator instruction
    match operator_kind {
      TokenKind::Minus => self.emit_byte(AlignedByteCode::Negate),
      TokenKind::Bang => self.emit_byte(AlignedByteCode::Not),
      _ => panic!(),
    }
  }

  /// Compile a grouping expression
  fn grouping(&mut self) {
    self.expression();

    self
      .parser
      .consume(TokenKind::RightParen, "Expected ')' after expression")
  }

  /// Compile a number literal
  fn number(&mut self) {
    let value = Value::from(
      self
        .parser
        .previous
        .lexeme
        .parse::<f64>()
        .expect("Unable to parse float"),
    );
    self.emit_constant(value);
  }

  /// Compile a variable statement
  fn variable(&mut self, can_assign: bool) {
    self.named_variable(self.parser.previous.clone(), can_assign);
  }

  /// Compile a string literal
  fn string(&mut self) {
    let string = self.hooks.manage_str(copy_string(&self.parser.previous));
    let value = Value::from(string);
    self.emit_constant(value)
  }

  /// retrieve a named variable from either local or global scope
  fn named_variable(&mut self, name: Token, can_assign: bool) {
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
          let global_index = self.identifer_constant(name);
          (
            AlignedByteCode::GetGlobal(global_index),
            AlignedByteCode::SetGlobal(global_index),
          )
        }
      },
    };

    if can_assign && self.parser.match_kind(TokenKind::Equal) {
      self.expression();
      self.emit_byte(set_byte);
    } else {
      self.emit_byte(get_byte);
    }
  }

  /// Compile a literal
  fn literal(&mut self) {
    match self.parser.previous.kind {
      TokenKind::True => self.emit_byte(AlignedByteCode::True),
      TokenKind::False => self.emit_byte(AlignedByteCode::False),
      TokenKind::Nil => self.emit_byte(AlignedByteCode::Nil),
      _ => panic!(format!(
        "Unexpected token kind {:?}",
        self.parser.previous.kind
      )),
    }
  }

  /// Compile an expression an a provided precedences
  fn parse_precedence(&mut self, precedence: Precedence) {
    self.parser.advance();

    let can_assign = precedence <= Precedence::Assignment;
    let prefix_fn = get_rule(self.parser.previous.kind).prefix.clone();

    match prefix_fn {
      Some(prefix) => self.execute_action(prefix, can_assign),
      None => {
        self.parser.error("Expected expression.");
        return;
      }
    }

    while precedence <= get_rule(self.parser.current.kind).precedence {
      self.parser.advance();
      let infix_fn = get_rule(self.parser.previous.kind).infix.clone();

      self.execute_action(infix_fn.expect("Failure"), can_assign);
    }

    if can_assign && self.parser.match_kind(TokenKind::Equal) {
      self.parser.error("Invalid assignment target.")
    }
  }

  /// Define a variable
  fn define_variable(&mut self, variable: u16) {
    if self.scope_depth > 0 {
      self.mark_initialized();
      return;
    }

    self.emit_byte(AlignedByteCode::DefineGlobal(variable));
  }

  /// Parse the current functions arity
  fn function_arity(&mut self) {
    let mut arity: u16 = 0;

    loop {
      arity += 1;
      if arity == std::u8::MAX as u16 {
        self
          .parser
          .error_at_current("Cannot have more than 255 parameters.");
      }

      let param_constant = self.parse_variable("Expect parameter name.");
      self.define_variable(param_constant);

      if !self.parser.match_kind(TokenKind::Comma) {
        break;
      }
    }

    self.fun.arity = ArityKind::Fixed(arity as u8);
  }

  /// Parse a list of argument to a function
  fn call_arguments(&mut self) -> u8 {
    let arg_count = self.consume_arguments(TokenKind::RightParen, std::u8::MAX as usize);
    self
      .parser
      .consume(TokenKind::RightParen, "Expect ')' after arguments");
    arg_count as u8
  }

  /// Parse a list of argument defining a list
  fn list_arguments(&mut self) -> u16 {
    let arg_count = self.consume_arguments(TokenKind::RightBracket, std::u16::MAX as usize);
    self
      .parser
      .consume(TokenKind::RightBracket, "Expect ']' after arguments");
    arg_count as u16
  }

  /// Consume a comma separated set of arguments for calls and lists
  fn consume_arguments(&mut self, stop_token: TokenKind, max: usize) -> usize {
    let mut arg_count: usize = 0;

    while !self.parser.check(stop_token) {
      self.expression();

      if arg_count == max {
        self
          .parser
          .error(&format!("Cannot have more than {} arguments", max));
        return arg_count;
      }
      arg_count += 1;

      if !self.parser.match_kind(TokenKind::Comma) {
        break;
      }
    }

    arg_count
  }

  /// Emit instruction for a short circuited and
  fn and(&mut self) {
    let end_jump = self.emit_jump(AlignedByteCode::JumpIfFalse(0));

    self.emit_byte(AlignedByteCode::Drop);
    self.parse_precedence(Precedence::And);

    self.patch_jump(end_jump);
  }

  /// Emit instruction for a short circuited and
  fn or(&mut self) {
    let else_jump = self.emit_jump(AlignedByteCode::JumpIfFalse(0));
    let end_jump = self.emit_jump(AlignedByteCode::Jump(0));

    self.patch_jump(else_jump);
    self.emit_byte(AlignedByteCode::Drop);

    self.parse_precedence(Precedence::Or);
    self.patch_jump(end_jump);
  }

  /// Parse a class's this identifier
  fn this(&mut self) {
    if self.current_class.is_none() {
      self.parser.error("Cannot use 'this' outside of class.");
      return;
    }

    self.variable(false);
  }

  fn super_(&mut self) {
    match self.current_class {
      None => self.parser.error("Cannot use 'super' outside of a class."),
      Some(class) => {
        if !class.has_super_class {
          self
            .parser
            .error("Cannot use 'super' in a class with no superclass.");
        }
      }
    }

    self
      .parser
      .consume(TokenKind::Dot, "Expect '.' after 'super'.");
    self
      .parser
      .consume(TokenKind::Identifier, "Expect superclass method name.");
    let name = self.identifer_constant(self.parser.previous.clone());

    self.named_variable(
      Token {
        lexeme: THIS.to_string(),
        kind: TokenKind::This,
        line: self.parser.previous.line,
      },
      false,
    );

    if self.parser.match_kind(TokenKind::LeftParen) {
      let arg_count = self.call_arguments();
      self.named_variable(
        Token {
          lexeme: SUPER.to_string(),
          kind: TokenKind::Super,
          line: self.parser.previous.line,
        },
        false,
      );
      self.emit_byte(AlignedByteCode::SuperInvoke((name, arg_count)));
    } else {
      self.named_variable(
        Token {
          lexeme: SUPER.to_string(),
          kind: TokenKind::Super,
          line: self.parser.previous.line,
        },
        false,
      );

      self.emit_byte(AlignedByteCode::GetSuper(name));
    }
  }

  /// Parse a variable from the provided token return it's new constant
  /// identifer if an identifer was identified
  fn parse_variable(&mut self, error_message: &str) -> u16 {
    self.parser.consume(TokenKind::Identifier, error_message);
    self.declare_variable(self.parser.previous.clone());
    if self.scope_depth > 0 {
      return 0;
    }
    self.identifer_constant(self.parser.previous.clone())
  }

  /// Mark a variable initialized
  fn mark_initialized(&mut self) {
    if self.scope_depth > 0 {
      self.locals[self.local_count - 1].depth = self.scope_depth;
    }
  }

  /// Generate a constant from the provided identifier token
  fn identifer_constant(&mut self, name: Token) -> u16 {
    let identifer = self.hooks.manage_str(name.lexeme);
    self.make_constant(Value::from(identifer))
  }

  fn add_local(&mut self, name: Token) {
    if self.local_count == std::u8::MAX as usize {
      self.parser.error("Too many local variables in function.");
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

    for i in (0..self.local_count).rev() {
      let local = &self.locals[i];

      // If we in a lower scope break
      if local.depth != UNINITIALIZED && local.depth < self.scope_depth {
        break;
      }

      // check that the same variable wasn't declared twice in the same scope
      if let Some(local_name) = &local.name {
        if &name.lexeme == local_name {
          self
            .parser
            .error("Variable with this name already declared in this scope.");
        }
      }
    }

    self.add_local(name);
  }

  /// resolve a token to a local if it exists
  fn resolve_local(&mut self, name: &Token) -> Option<u8> {
    for i in (0..self.local_count).rev() {
      let local = &self.locals[i];

      if let Some(local_name) = &local.name {
        if &name.lexeme == local_name {
          // handle the case were `var a = a;`
          if local.depth == UNINITIALIZED {
            self
              .parser
              .error("Cannot read local variable in its own initializer.")
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
    match self.enclosing {
      Some(parent_ptr) => {
        let parent = unsafe { &mut *parent_ptr };

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
      self.parser.error("Too many closure variable in function.");
      return 0;
    }

    self.upvalues[upvalue_count] = upvalue;
    self.fun.upvalue_count += 1;

    upvalue_count
  }

  /// Execute a provided action
  fn execute_action(&mut self, action: Act, can_assign: bool) {
    match action {
      Act::And => self.and(),
      Act::Binary => self.binary(),
      Act::Call => self.call(),
      Act::List => self.list(),
      Act::Map => self.map(),
      Act::Lambda => self.lambda(),
      Act::Index => self.index(can_assign),
      Act::Dot => self.dot(can_assign),
      Act::Grouping => self.grouping(),
      Act::Literal => self.literal(),
      Act::Number => self.number(),
      Act::Or => self.or(),
      Act::String => self.string(),
      Act::Super => self.super_(),
      Act::This => self.this(),
      Act::Unary => self.unary(),
      Act::Variable => self.variable(can_assign),
    }
  }

  /// Emit byte code for a return
  fn emit_return(&mut self) {
    match self.fun_kind {
      FunKind::Initializer => self.emit_byte(AlignedByteCode::GetLocal(0)),
      _ => self.emit_byte(AlignedByteCode::Nil),
    }

    self.emit_byte(AlignedByteCode::Return);
  }

  /// Add a constant to the current chunk
  fn make_constant(&mut self, value: Value) -> u16 {
    match self.constants.get(&value) {
      Some(index) => *index as u16,
      None => {
        let index = self.fun.add_constant(&self.hooks, value);
        if index > std::u16::MAX as usize {
          self.parser.error("Too many constants in one chunk.");
          return 0;
        }

        self.constants.insert(value, index);
        index as u16
      }
    }
  }

  /// Emit byte code for a constant
  fn emit_constant(&mut self, value: Value) {
    let index = self.make_constant(value);

    if index <= std::u8::MAX as u16 {
      self.emit_byte(AlignedByteCode::Constant(index as u8));
    } else {
      self.emit_byte(AlignedByteCode::ConstantLong(index));
    }
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
      self.parser.error("Too much code to jump over.");
    }

    jump as u16
  }

  /// Emit a loop instruction
  fn emit_loop(&mut self, loop_start: usize) {
    let offset = self.current_chunk().instructions.len() - loop_start + 3;
    if offset > std::u16::MAX.try_into().unwrap() {
      self.parser.error("Loop body too large.");
    }

    self.emit_byte(AlignedByteCode::Loop(offset as u16));
  }

  /// Emit two provided instruction
  fn emit_bytes(&mut self, op_code1: AlignedByteCode, op_code2: AlignedByteCode) {
    let line = self.parser.previous.line;
    self.write_instruction(op_code1, line);
    self.write_instruction(op_code2, line);
  }

  /// Emit a provided instruction
  fn emit_byte(&mut self, op_code: AlignedByteCode) {
    let line = self.parser.previous.line;
    self.write_instruction(op_code, line);
  }

  /// Emit a jump instruction
  fn emit_jump(&mut self, jump: AlignedByteCode) -> usize {
    self.emit_byte(jump);

    self.current_chunk().instructions.len() - 2
  }
}

/// Get the first local for a given function kind
fn first_local(fun_kind: FunKind) -> Local {
  match fun_kind {
    FunKind::Fun => Local {
      name: Option::None,
      depth: 0,
      is_captured: false,
    },
    _ => Local {
      name: Some("this".to_string()),
      depth: 0,
      is_captured: false,
    },
  }
}

/// The rules for infix and prefix operators
const RULES_TABLE: [ParseRule; 48] = [
  ParseRule::new(Some(Act::Grouping), Some(Act::Call), Precedence::Call),
  // TOKEN_LEFT_PAREN
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_RIGHT_PAREN
  ParseRule::new(Some(Act::Map), None, Precedence::Call),
  // TOKEN_LEFT_BRACE
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_RIGHT_BRACE
  ParseRule::new(Some(Act::List), Some(Act::Index), Precedence::Call),
  // TOKEN_LEFT_BRACKET
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_RIGHT_BRACKET
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_COMMA
  ParseRule::new(None, Some(Act::Dot), Precedence::Call),
  // TOKEN_DOT
  ParseRule::new(Some(Act::Unary), Some(Act::Binary), Precedence::Term),
  // TOKEN_MINUS
  ParseRule::new(None, Some(Act::Binary), Precedence::Term),
  // TOKEN_PLUS
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_COLON
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_SEMICOLON
  ParseRule::new(Some(Act::Lambda), None, Precedence::Call),
  // TOKEN_PIPE
  ParseRule::new(None, Some(Act::Binary), Precedence::Factor),
  // TOKEN_SLASH
  ParseRule::new(None, Some(Act::Binary), Precedence::Factor),
  // TOKEN_STAR
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_EXPORT
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_IMPORT
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_FROM
  ParseRule::new(Some(Act::Unary), None, Precedence::None),
  // TOKEN_BANG
  ParseRule::new(None, Some(Act::Binary), Precedence::Equality),
  // TOKEN_BANG_EQUAL
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_EQUAL
  ParseRule::new(None, Some(Act::Binary), Precedence::Equality),
  // TOKEN_EQUAL_EQUAL
  ParseRule::new(None, Some(Act::Binary), Precedence::Comparison),
  // TOKEN_GREATER
  ParseRule::new(None, Some(Act::Binary), Precedence::Comparison),
  // TOKEN_GREATER_EQUAL
  ParseRule::new(None, Some(Act::Binary), Precedence::Comparison),
  // TOKEN_LESS
  ParseRule::new(None, Some(Act::Binary), Precedence::Comparison),
  // TOKEN_LESS_EQUAL
  ParseRule::new(Some(Act::Variable), None, Precedence::None),
  // TOKEN_IDENTIFIER
  ParseRule::new(Some(Act::String), None, Precedence::None),
  // TOKEN_STRING
  ParseRule::new(Some(Act::Number), None, Precedence::None),
  // TOKEN_NUMBER
  ParseRule::new(None, Some(Act::And), Precedence::And),
  // TOKEN_AND
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_CLASS
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_ELSE
  ParseRule::new(Some(Act::Literal), None, Precedence::None),
  // TOKEN_FALSE
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_FOR
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_FUN
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_IF
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_IN
  ParseRule::new(Some(Act::Literal), None, Precedence::None),
  // TOKEN_NIL
  ParseRule::new(None, Some(Act::Or), Precedence::Or),
  // TOKEN_OR
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_PRINT
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_RETURN
  ParseRule::new(Some(Act::Super), None, Precedence::None),
  // TOKEN_SUPER
  ParseRule::new(Some(Act::This), None, Precedence::None),
  // TOKEN_THIS
  ParseRule::new(Some(Act::Literal), None, Precedence::None),
  // TOKEN_TRUE
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_VAR
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_WHILE
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_ERROR
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_EOF
];

/// Get a rule from the rules table
const fn get_rule(kind: TokenKind) -> &'static ParseRule {
  &RULES_TABLE[kind as usize]
}

#[derive(Debug, Clone)]
pub struct ClassCompiler {
  enclosing: Option<Managed<ClassCompiler>>,
  has_super_class: bool,
  name: Token,
}

impl Trace for ClassCompiler {
  fn trace(&self) -> bool {
    do_if_some(self.enclosing, |enclosing| {
      enclosing.trace();
    });
    true
  }

  fn trace_debug(&self, stdio: &dyn StdIo) -> bool {
    do_if_some(self.enclosing, |enclosing| {
      enclosing.trace_debug(stdio);
    });
    true
  }
}

impl Manage for ClassCompiler {
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

/// The space lox parser. This struct is responsible for
/// advancing the scanner and checking for specific conditions
pub struct Parser<'a, S: StdIo> {
  /// The current token
  current: Token,

  /// The previous token
  previous: Token,

  /// Has the parser encountered an error
  had_error: bool,

  /// Is the parser in panic mode
  panic_mode: bool,

  /// Help reference to the backing scanner
  scanner: Scanner<'a>,

  /// The environments standard io access
  stdio: S,
}

impl<'a, S: StdIo> Parser<'a, S> {
  /// Create a new instance of the parser from a source str
  pub fn new(stdio: S, source: &'a str) -> Self {
    Self {
      scanner: Scanner::new(source),
      stdio,
      had_error: false,
      panic_mode: false,
      previous: Token {
        lexeme: "error".to_string(),
        line: 0,
        kind: TokenKind::Error,
      },
      current: Token {
        lexeme: "error".to_string(),
        line: 0,
        kind: TokenKind::Error,
      },
    }
  }

  /// Does the provided token kind match if so advance the
  /// token index
  pub fn match_kind(&mut self, kind: TokenKind) -> bool {
    if !self.check(kind) {
      return false;
    }
    self.advance();
    true
  }

  /// Does the provided token kind match the current kind
  pub fn check(&self, kind: TokenKind) -> bool {
    self.current.kind == kind
  }

  /// Advance the parser a token forward
  pub fn advance(&mut self) {
    self.previous = self.current.clone();
    loop {
      self.current = self.scanner.scan_token();
      if self.current.kind != TokenKind::Error {
        break;
      }

      self.error_at_current(&self.current.lexeme.to_string())
    }
  }

  /// Consume a token and advance the current token index
  fn consume(&mut self, kind: TokenKind, message: &str) {
    if self.current.kind == kind {
      self.advance();
      return;
    }

    self.error_at_current(message)
  }

  /// Indicate an error occurred at he current index
  fn error_at_current(&mut self, message: &str) {
    let token = self.current.clone();
    self.error_at(token, message);
  }

  /// Indicate an error occurred at the previous index
  pub fn error(&mut self, message: &str) {
    let token = self.previous.clone();
    self.error_at(token, message);
  }

  /// Print an error to the console for a user to address
  fn error_at(&mut self, token: Token, message: &str) {
    if self.panic_mode {
      return;
    }

    self.panic_mode = true;
    self.stdio.eprint(&format!("[line {}] Error", token.line));

    match token.kind {
      TokenKind::Eof => self.stdio.eprint(" at end"),
      TokenKind::Error => (),
      _ => self.stdio.eprint(&format!(" at {}", token.lexeme)),
    }

    self.stdio.eprintln(&format!(": {}", message));
    self.had_error = true;
  }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
enum Precedence {
  None,
  Assignment,
  Or,
  And,
  Equality,
  Comparison,
  Term,
  Factor,
  Unary,
  Call,
  Primary,
}

impl Precedence {
  pub fn higher(&self) -> Precedence {
    match self {
      Precedence::None => Precedence::Assignment,
      Precedence::Assignment => Precedence::Or,
      Precedence::Or => Precedence::And,
      Precedence::And => Precedence::Equality,
      Precedence::Equality => Precedence::Comparison,
      Precedence::Comparison => Precedence::Term,
      Precedence::Term => Precedence::Factor,
      Precedence::Factor => Precedence::Unary,
      Precedence::Unary => Precedence::Call,
      Precedence::Call => Precedence::Primary,
      Precedence::Primary => panic!("Primary is highest precedence"),
    }
  }
}

struct ParseRule {
  prefix: Option<Act>,
  infix: Option<Act>,
  precedence: Precedence,
}

impl ParseRule {
  const fn new(prefix: Option<Act>, infix: Option<Act>, precedence: Precedence) -> Self {
    Self {
      prefix,
      infix,
      precedence,
    }
  }
}

#[derive(Clone)]
enum Act {
  And,
  Binary,
  Call,
  Index,
  List,
  Map,
  Dot,
  Lambda,
  Grouping,
  Literal,
  Number,
  Or,
  String,
  Super,
  This,
  Unary,
  Variable,
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::debug::disassemble_chunk;
  use spacelox_core::chunk::decode_u16;
  use spacelox_core::hooks::NoContext;
  use spacelox_env::{io::NativeIo, memory::Gc, stdio::NativeStdIo};
  use std::path::PathBuf;

  enum ByteCodeTest {
    Code(AlignedByteCode),
    Fun((u16, Vec<ByteCodeTest>)),
  }

  fn test_compile<'a>(src: String, gc: &mut Gc) -> Managed<Fun> {
    let io = NativeIo();
    let mut parser = Parser::new(io.stdio(), &src);

    let mut context = NoContext::new(gc);
    let hooks = &Hooks::new(&mut context);

    let module = hooks.manage(Module::new(
      hooks.manage_str("module".to_string()),
      hooks.manage(PathBuf::from("path/module.lox")),
    ));
    let compiler = Compiler::new(module, io, &mut parser, &hooks);
    let result = compiler.compile();
    assert_eq!(result.success, true);

    result.fun
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
    disassemble_chunk(&NativeStdIo::new(), &fun.chunk(), "test");
    let decoded_byte_code = decode_byte_code(fun);
    assert_eq!(decoded_byte_code.len(), code.len());

    decoded_byte_code
      .iter()
      .zip(code.iter())
      .for_each(|(actual, expect)| assert_eq!(actual, expect));
  }

  fn assert_fun_bytecode(fun: Managed<Fun>, code: &[ByteCodeTest]) {
    disassemble_chunk(&NativeStdIo::new(), &fun.chunk(), &*fun.name);
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
  fn op_print() {
    let example = "print 10;".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::Constant(0),
        AlignedByteCode::Print,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn import() {
    let example = r#"
      import time from "std/time";
    "#
    .to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);

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
      export var x = 10;
    "
    .to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);

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
      export fun example() {}
    "
    .to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);

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
    "
    .to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);

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
  fn class_empty() {
    let example = "
      class A {}
    "
    .to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);

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
          this.field = true;
        }

        getField() {
          return this.field;
        }

        getGetField() {
          return this.getField();
        }
      }
    "
    .to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);

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
        ByteCodeTest::Fun((
          4,
          vec![
            ByteCodeTest::Code(AlignedByteCode::GetLocal(0)),
            ByteCodeTest::Code(AlignedByteCode::GetProperty(0)),
            ByteCodeTest::Code(AlignedByteCode::Return),
            ByteCodeTest::Code(AlignedByteCode::Nil),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::Method(3)),
        ByteCodeTest::Fun((
          6,
          vec![
            ByteCodeTest::Code(AlignedByteCode::GetLocal(0)),
            ByteCodeTest::Code(AlignedByteCode::Invoke((0, 0))),
            ByteCodeTest::Code(AlignedByteCode::Return),
            ByteCodeTest::Code(AlignedByteCode::Nil),
            ByteCodeTest::Code(AlignedByteCode::Return),
          ],
        )),
        ByteCodeTest::Code(AlignedByteCode::Method(5)),
        ByteCodeTest::Code(AlignedByteCode::Drop),
        ByteCodeTest::Code(AlignedByteCode::Nil),
        ByteCodeTest::Code(AlignedByteCode::Return),
      ],
    );
  }

  #[test]
  fn list_index_set() {
    let example = "
      var a = [clock, clock, clock];
      a[1] = 5;
    "
    .to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);

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
      var a = [\"john\", \"joe\", \"jim\"];
      print a[1];
    "
    .to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);

    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::List,
        AlignedByteCode::Constant(1),
        AlignedByteCode::Constant(2),
        AlignedByteCode::Constant(3),
        AlignedByteCode::ListInit(3),
        AlignedByteCode::DefineGlobal(0),
        AlignedByteCode::GetGlobal(0),
        AlignedByteCode::Constant(4),
        AlignedByteCode::GetIndex,
        AlignedByteCode::Print,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn list_initializer() {
    let example = "
      var a = [1, 2, nil, false, \"cat\"];
    "
    .to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);

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
      var a = [];
    "
    .to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);

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
  fn map_intializer() {
    let example = "
      var a = {
        \"key1\": 10,
        \"key2\": nil,
      };
    "
    .to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);

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
      var a = {};
    "
    .to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);

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
    var example = || 10;
    example();
    "
    .to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
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
    var example = || { return 10; };
    example();
    "
    .to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
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
  fn open_upvalue() {
    let example = "
    fun example() {
      var x = 0;
      fun middle() {
        fun inner() {
          return x;
        }

        return inner();
      }

      return middle();
    }
    example();
    "
    .to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
    assert_fun_bytecode(
      fun,
      &vec![
        ByteCodeTest::Fun((
          // example
          1,
          vec![
            ByteCodeTest::Code(AlignedByteCode::Constant(0)),
            ByteCodeTest::Fun((
              // middle
              1,
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
            ByteCodeTest::Code(AlignedByteCode::UpvalueIndex(UpvalueIndex::Local(1))),
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
    fun example() {
      var a = 10;
      fun inner() {
        return a;
      }
      return inner;
    }
    var inner = example();
    inner();
    "
    .to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
    assert_fun_bytecode(
      fun,
      &vec![
        ByteCodeTest::Fun((
          1,
          vec![
            ByteCodeTest::Code(AlignedByteCode::Constant(0)),
            ByteCodeTest::Fun((
              1,
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
    let example = "fun example() {} example();".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
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
    fun example(a) {
      return a;
    }
    var a = 1;
    example(a);
    "
    .to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
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
    let example = "fun example() { var a = 10; return a; } example();".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
    assert_fun_bytecode(
      fun,
      &vec![
        ByteCodeTest::Fun((
          1,
          vec![
            ByteCodeTest::Code(AlignedByteCode::Constant(0)),
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
    let example = "var a = { \"cat\": \"bat\", 10: nil };".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);

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
    let example = "var a = [1, 2, 3, \"cat\"];".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);

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
    let example = "for (var x = 0; x < 10; x = x + 1) { print(x); }".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);

    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::Constant(0),     // 0
        AlignedByteCode::GetLocal(1),     // 2
        AlignedByteCode::Constant(1),     // 4
        AlignedByteCode::Less,            // 6
        AlignedByteCode::JumpIfFalse(21), // 7
        AlignedByteCode::Drop,            // 10
        AlignedByteCode::Jump(11),        // 11
        AlignedByteCode::GetLocal(1),     // 14
        AlignedByteCode::Constant(2),     // 16
        AlignedByteCode::Add,             // 18
        AlignedByteCode::SetLocal(1),     // 19
        AlignedByteCode::Drop,            // 21
        AlignedByteCode::Loop(23),        // 22
        AlignedByteCode::GetLocal(1),     // 25
        AlignedByteCode::Print,           // 27
        AlignedByteCode::Loop(17),        // 28
        AlignedByteCode::Drop,            // 31
        AlignedByteCode::Drop,            // 32
        AlignedByteCode::Nil,             // 33
        AlignedByteCode::Return,          // 34
      ],
    );
  }

  #[test]
  fn for_range_loop() {
    let example = "for (var x in [1, 2, 3]) { print x; }".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);

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
        AlignedByteCode::JumpIfFalse(15), // 18  const 4 = "iter"
        AlignedByteCode::Drop,            // 21  const 5 = "next"
        AlignedByteCode::GetLocal(2),     // 22
        AlignedByteCode::IterCurrent(6),  // 24  const 6 = "current"
        AlignedByteCode::SetLocal(1),     // 26
        AlignedByteCode::Drop,            // 28
        AlignedByteCode::GetLocal(1),     // 29
        AlignedByteCode::Print,           // 31
        AlignedByteCode::Loop(23),        // 32
        AlignedByteCode::Drop,            // 35
        AlignedByteCode::Drop,            // 36
        AlignedByteCode::Drop,            // 37
        AlignedByteCode::Nil,             // 38
        AlignedByteCode::Return,          // 39
      ],
    );
  }

  #[test]
  fn while_loop() {
    let example = "while (true) { print 10; }".to_string();
    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);

    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::True,
        AlignedByteCode::JumpIfFalse(7),
        AlignedByteCode::Drop,
        AlignedByteCode::Constant(0),
        AlignedByteCode::Print,
        AlignedByteCode::Loop(11),
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn or_operator() {
    let example = "print false or true;".to_string();
    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);

    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::False,
        AlignedByteCode::JumpIfFalse(3),
        AlignedByteCode::Jump(2),
        AlignedByteCode::Drop,
        AlignedByteCode::True,
        AlignedByteCode::Print,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn if_condition() {
    let example = "if (3 < 10) { print \"hi\"; }".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);

    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::Constant(0),
        AlignedByteCode::Constant(1),
        AlignedByteCode::Less,
        AlignedByteCode::JumpIfFalse(7),
        AlignedByteCode::Drop,
        AlignedByteCode::Constant(2),
        AlignedByteCode::Print,
        AlignedByteCode::Jump(1),
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn if_else_condition() {
    let example = "if (3 < 10) { print \"hi\"; } else { print \"bye\"; }".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);

    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::Constant(0),    // 0
        AlignedByteCode::Constant(1),    // 2
        AlignedByteCode::Less,           // 4
        AlignedByteCode::JumpIfFalse(7), // 5
        AlignedByteCode::Drop,           // 8
        AlignedByteCode::Constant(2),    // 9
        AlignedByteCode::Print,          // 11
        AlignedByteCode::Jump(4),        // 12
        AlignedByteCode::Drop,           // 15
        AlignedByteCode::Constant(3),    // 17
        AlignedByteCode::Print,          // 18
        AlignedByteCode::Nil,            // 19
        AlignedByteCode::Return,         // 20
      ],
    );
  }

  #[test]
  fn declare_local() {
    let example = "{ var x = 10; }".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
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
  fn op_get_local() {
    let example = "{ var x = 10; print(x); }".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::Constant(0),
        AlignedByteCode::GetLocal(1),
        AlignedByteCode::Print,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_set_local() {
    let example = "{ var x = 10; x = 5; }".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);

    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::Constant(0),
        AlignedByteCode::Constant(1),
        AlignedByteCode::SetLocal(1),
        AlignedByteCode::Drop,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_define_global_nil() {
    let example = "var x;".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);

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
    let example = "var x = 10;".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);

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
    let example = "print x;".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::GetGlobal(0),
        AlignedByteCode::Print,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_set_global() {
    let example = "x = \"cat\";".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
    assert_simple_bytecode(
      fun,
      &vec![
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
    let example = "false;".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
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
    let example = "".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
    assert_simple_bytecode(fun, &vec![AlignedByteCode::Nil, AlignedByteCode::Return]);
  }

  #[test]
  fn op_number() {
    let example = "5.18;".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
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
    let example = "\"example\";".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
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
    let example = "false;".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
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
    let example = "true;".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
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
    let example = "nil;".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
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
    let example = "!false;".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
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
    let example = "-15;".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
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
    let example = "10 + 4;".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
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
    let example = "10 - 4;".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
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
    let example = "10 / 4;".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
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
    let example = "10 * 4;".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
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
    let example = "true == nil;".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
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
    let example = "true != nil;".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::True,
        AlignedByteCode::Nil,
        AlignedByteCode::Equal,
        AlignedByteCode::Not,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_less() {
    let example = "3 < 5;".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
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
    let example = "3 <= 5;".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::Constant(0),
        AlignedByteCode::Constant(1),
        AlignedByteCode::Greater,
        AlignedByteCode::Not,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }

  #[test]
  fn op_greater() {
    let example = "3 > 5;".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
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
    let example = "3 >= 5;".to_string();

    let mut gc = Gc::new(Box::new(NativeStdIo::new()));
    let fun = test_compile(example, &mut gc);
    assert_simple_bytecode(
      fun,
      &vec![
        AlignedByteCode::Constant(0),
        AlignedByteCode::Constant(1),
        AlignedByteCode::Less,
        AlignedByteCode::Not,
        AlignedByteCode::Drop,
        AlignedByteCode::Nil,
        AlignedByteCode::Return,
      ],
    );
  }
}
