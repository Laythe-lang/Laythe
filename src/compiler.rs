use crate::chunk::{ByteCode, Chunk};
use crate::debug::disassemble_chunk;
use crate::object::{copy_string, Obj, ObjValue};
use crate::scanner::{Scanner, Token, TokenKind};
use crate::value::Value;
use std::convert::TryInto;

/// The result of a compilation
pub struct CompilerResult<'c> {
  /// Was an error encountered while this chunk was compiled
  pub success: bool,

  /// The chunk that was compiled
  pub chunk: Chunk<'c>,
}

pub struct CompilerAnalytics<'a, 'c: 'a> {
  /// provide a side effect when the compiler allocates an object
  pub allocate: &'a dyn Fn(ObjValue) -> Obj<'c>,

  /// provide a mechanism to intern a string
  pub intern: &'a dyn Fn(String) -> String,
}

const UNINITIALIZED: i16 = -1;

#[derive(Debug, Clone)]
pub struct Local {
  name: Option<Token>,

  depth: i16,
}

/// The spacelox compiler for converting tokens to bytecode
pub struct Compiler<'a, 'c: 'a> {
  /// The current chunk this compiler points to
  chunk: Chunk<'c>,

  /// The parser in charge incrementing the scanner and
  /// expecting / consuming tokens
  parser: Parser,

  /// Analytics for the compiler
  analytics: CompilerAnalytics<'a, 'c>,

  /// Number of locals
  local_count: usize,

  /// Current scope depth
  scope_depth: i16,

  /// The in scope locals
  locals: Vec<Local>,
}

impl<'a, 'c: 'a> Compiler<'a, 'c> {
  /// Create a new instance of the spacelox compiler.
  /// The compiler write a sequence of op codes to the chunk
  /// to be executed
  ///
  /// # Examples
  /// ```
  /// use lox_runtime::chunk::{Chunk};
  /// use lox_runtime::compiler::{Compiler, CompilerAnalytics};
  /// use lox_runtime::object::{ObjValue, Obj};
  ///
  /// // an expression
  /// let source = "10 + 3".to_string();
  /// let chunk = Chunk::default();
  ///
  /// let allocate = |value: ObjValue| Obj::new(value);
  /// let intern = |string: String| string;
  /// let analytics = CompilerAnalytics { allocate: &allocate, intern: &intern };
  ///
  /// let compiler = Compiler::new(source, chunk, analytics);
  /// ```
  pub fn new(source: String, chunk: Chunk<'c>, analytics: CompilerAnalytics<'a, 'c>) -> Self {
    Self {
      chunk,
      analytics,
      parser: Parser::new(source),
      local_count: 0,
      scope_depth: 0,
      locals: vec![
        Local {
          name: Option::None,
          depth: UNINITIALIZED,
        };
        std::u8::MAX as usize
      ],
    }
  }

  /// Compile the provided source code onto the chunk
  /// returns true if the compiler ran without parser errors
  ///
  /// # Examples
  /// ```
  /// use lox_runtime::chunk::{Chunk};
  /// use lox_runtime::compiler::{Compiler, CompilerAnalytics};
  /// use lox_runtime::object::{ObjValue, Obj};
  ///
  /// // an expression
  /// let source = "3 / 2 + 10".to_string();
  /// let chunk = Chunk::default();
  ///
  /// let allocate = |value: ObjValue| Obj::new(value);
  /// let intern = |string: String| string;
  /// let analytics = CompilerAnalytics { allocate: &allocate, intern: &intern };
  ///
  /// let compiler = Compiler::new(source, chunk, analytics);
  /// let result = compiler.compile();
  /// assert_eq!(result.success, true);
  /// ```
  pub fn compile(mut self) -> CompilerResult<'c> {
    self.parser.advance();

    // early exit if ""
    if let TokenKind::Eof = self.parser.current.kind {
      self.end_compiler();

      return CompilerResult {
        success: !self.parser.had_error,
        chunk: self.chunk,
      };
    }

    loop {
      if self.parser.match_kind(TokenKind::Eof) {
        break;
      }

      self.declaration()
    }

    self
      .parser
      .consume(TokenKind::Eof, "Expected end of expression.");
    self.end_compiler();

    CompilerResult {
      success: !self.parser.had_error,
      chunk: self.chunk,
    }
  }

  /// Parse a declaration
  fn declaration(&mut self) {
    if self.parser.match_kind(TokenKind::Var) {
      self.var_declaration();
    } else {
      self.statement();
    }

    if self.parser.panic_mode {
      self.synchronize();
    }
  }

  /// Parse a statement
  fn statement(&mut self) {
    if self.parser.match_kind(TokenKind::Print) {
      self.print_statement();
    } else if self.parser.match_kind(TokenKind::For) {
      self.for_statement();
    } else if self.parser.match_kind(TokenKind::If) {
      self.if_statement();
    } else if self.parser.match_kind(TokenKind::While) {
      self.while_statement();
    } else if self.parser.match_kind(TokenKind::LeftBrace) {
      self.begin_scope();
      self.block();
      self.end_scope();
    } else {
      self.expression_statement();
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

  /// Parse a variable declaration
  fn var_declaration(&mut self) {
    let global = self.parse_variable("Expect variable name.");

    if self.parser.match_kind(TokenKind::Equal) {
      self.expression();
    } else {
      self.emit_byte(ByteCode::Nil);
    }

    self.parser.consume(
      TokenKind::Semicolon,
      "Expect ';' after variable declaration.",
    );

    self.define_variable(global);
  }

  /// Parse an expression statement
  fn expression_statement(&mut self) {
    self.expression();
    self
      .parser
      .consume(TokenKind::Semicolon, "Expected ';' after expression.");
    self.emit_byte(ByteCode::Pop)
  }

  fn for_statement(&mut self) {
    self.begin_scope();
    self.parser.consume(TokenKind::LeftParen, "Expected '(' after 'for'.");
    
    // parse an initializer
    if self.parser.match_kind(TokenKind::Semicolon) {
    } else if self.parser.match_kind(TokenKind::Var) {
      self.var_declaration();
    } else {
      self.expression_statement();
    }

    let mut loop_start = self.chunk.instructions.len();
    println!("loop_start: {}", loop_start);

    // parse loop condition
    let mut exit_jump: Option<usize> = Option::None;
    if !self.parser.match_kind(TokenKind::Semicolon) {
      self.expression();
      self.parser.consume(TokenKind::Semicolon, "Expected ';' after loop condition");
    
      exit_jump = Some(self.emit_jump());
      self.emit_byte(ByteCode::Pop);
    }

    // parse incrementor
    if !self.parser.match_kind(TokenKind::RightParen) {
      let body_jump = self.emit_jump();

      let increment_start = self.chunk.instructions.len();
      self.expression();
      self.emit_byte(ByteCode::Pop);
      self.parser.consume(TokenKind::RightParen, "Expect ')' after for clauses.");

      self.emit_loop(loop_start);
      loop_start = increment_start;
      println!("increment_start: {}", loop_start);

      self.chunk.instructions[body_jump] = ByteCode::Jump(self.patch_jump(body_jump));
    }

    self.statement();
    self.emit_loop(loop_start);

    // patch exit jump
    if let Some(jump) = exit_jump {
      self.chunk.instructions[jump] = ByteCode::JumpIfFalse(self.patch_jump(jump));
      self.emit_byte(ByteCode::Pop);
    }

    self.end_scope();
  }

  /// Compile a while statement
  fn if_statement(&mut self) {
    self.parser.consume(TokenKind::LeftParen, "Expected '(' after 'if'.");
    self.expression();
    self.parser.consume(TokenKind::RightParen, "Expected ')' after condition.");

    let then_jump = self.emit_jump();
    self.emit_byte(ByteCode::Pop);
    self.statement();

    let else_jump = self.emit_jump();
    self.chunk.instructions[then_jump] = ByteCode::JumpIfFalse(self.patch_jump(then_jump));
    self.emit_byte(ByteCode::Pop);

    if self.parser.match_kind(TokenKind::Else) {
      self.statement();
    }

    self.chunk.instructions[else_jump] = ByteCode::Jump(self.patch_jump(else_jump));
  }

  fn while_statement(&mut self) {
    let loop_start = self.chunk.instructions.len();

    self.parser.consume(TokenKind::LeftParen, "Expected '(' after 'while'.");
    self.expression();
    self.parser.consume(TokenKind::RightParen, "Expected ')' after condition.");

    let exit_jump = self.emit_jump();

    self.emit_byte(ByteCode::Pop);
    self.statement();

    self.emit_loop(loop_start);

    self.chunk.instructions[exit_jump] = ByteCode::JumpIfFalse(self.patch_jump(exit_jump));
    self.emit_byte(ByteCode::Pop)
  }

  /// Parse print statement
  fn print_statement(&mut self) {
    self.expression();
    self
      .parser
      .consume(TokenKind::Semicolon, "Expect ';' after value.");
    self.emit_byte(ByteCode::Print)
  }

  /// Synchronize the compiler to a sentinel token
  fn synchronize(&mut self) {
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

      self.parser.advance();
    }
  }

  /// End the compilation at eof
  fn end_compiler(&mut self) {
    self.emit_return();

    #[cfg(debug_assertions)]
    self.print_chunk()
  }

  /// Increase the scope depth by 1
  fn begin_scope(&mut self) {
    self.scope_depth += 1;
  }

  /// Decrease the scope depth by 1
  fn end_scope(&mut self) {
    self.scope_depth -= 1;

    while self.local_count > 0 && self.locals[self.local_count - 1].depth > self.scope_depth {
      self.emit_byte(ByteCode::Pop);
      self.local_count -= 1;
    }
  }

  /// Print the chunk if debug and an error occurred
  fn print_chunk(&self) {
    if self.parser.had_error {
      disassemble_chunk(&self.chunk, "code")
    }
  }

  /// Compiles a binary expression into it's equivalent bytecodes
  ///
  /// # Panics
  /// This method will panic if an invalid binary operator is passed
  pub fn binary(&mut self) {
    // Remember the operator
    let operator_kind = self.parser.previous.kind.clone();
    let precedence = get_rule(operator_kind.clone()).precedence.higher();
    self.parse_precedence(precedence);

    match operator_kind {
      TokenKind::BangEqual => self.emit_bytes(ByteCode::Equal, ByteCode::Not),
      TokenKind::EqualEqual => self.emit_byte(ByteCode::Equal),
      TokenKind::Greater => self.emit_byte(ByteCode::Greater),
      TokenKind::GreaterEqual => self.emit_bytes(ByteCode::Less, ByteCode::Not),
      TokenKind::Less => self.emit_byte(ByteCode::Less),
      TokenKind::LessEqual => self.emit_bytes(ByteCode::Greater, ByteCode::Not),
      TokenKind::Plus => self.emit_byte(ByteCode::Add),
      TokenKind::Minus => self.emit_byte(ByteCode::Subtract),
      TokenKind::Star => self.emit_byte(ByteCode::Multiply),
      TokenKind::Slash => self.emit_byte(ByteCode::Divide),
      _ => panic!("Invalid operator"),
    }
  }
  /// Compile a unary expression into it's equivalent bytecode
  ///
  /// # Panics
  /// This method will panic if an invalid unary operator is attempted to compile
  fn unary(&mut self) {
    let operator_kind = self.parser.previous.kind.clone();

    // Compile the operand
    self.parse_precedence(Precedence::Unary);

    // Emit the operator instruction
    match operator_kind {
      TokenKind::Minus => self.emit_byte(ByteCode::Negate),
      TokenKind::Bang => self.emit_byte(ByteCode::Not),
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
    let value = Value::Number(
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
    let string = (self.analytics.intern)(copy_string(&self.parser.previous));
    let obj = (self.analytics.allocate)(ObjValue::String(string));
    let value = Value::Obj(obj);
    self.emit_constant(value)
  }

  /// retrieve a named variable from either local or global scope
  fn named_variable(&mut self, name: Token, can_assign: bool) {
    let index = self.resolve_local(&name);

    let (get_byte, set_byte) = match index {
      Some(index) => (ByteCode::GetLocal(index), ByteCode::SetLocal(index)),
      None => {
        let global_index = self.identifer_constant(name);
        (
          ByteCode::GetGlobal(global_index),
          ByteCode::SetGlobal(global_index),
        )
      }
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
      TokenKind::True => self.emit_byte(ByteCode::True),
      TokenKind::False => self.emit_byte(ByteCode::False),
      TokenKind::Nil => self.emit_byte(ByteCode::Nil),
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
    let prefix_fn = get_rule(self.parser.previous.kind.clone()).prefix.clone();
    self.execute_action(prefix_fn.expect("Expected expression."), can_assign);

    while precedence <= get_rule(self.parser.current.kind.clone()).precedence {
      self.parser.advance();
      let infix_fn = get_rule(self.parser.previous.kind.clone()).infix.clone();

      self.execute_action(infix_fn.expect("Failure"), can_assign);
    }

    if can_assign && self.parser.match_kind(TokenKind::Equal) {
      self.parser.error("Invalid assignment target.")
    }
  }

  /// Define a variable
  fn define_variable(&mut self, global: u8) {
    if self.scope_depth > 0 {
      self.mark_initialized();
      return;
    }

    self.emit_byte(ByteCode::DefineGlobal(global));
  }

  /// Emit instruction for a short circuited and
  fn and(&mut self) {
    let end_jump = self.emit_jump();

    self.emit_byte(ByteCode::Pop);
    self.parse_precedence(Precedence::And);

    self.chunk.instructions[end_jump] = ByteCode::JumpIfFalse(self.patch_jump(end_jump));
  }

  /// Emit instruction for a short circuited and
  fn or(&mut self) {
    let else_jump = self.emit_jump();
    let end_jump = self.emit_jump();

    self.chunk.instructions[else_jump] = ByteCode::JumpIfFalse(self.patch_jump(else_jump));
    self.emit_byte(ByteCode::Pop);

    self.parse_precedence(Precedence::Or);
    self.chunk.instructions[end_jump] = ByteCode::Jump(self.patch_jump(end_jump));
  }

  /// Parse a variable from the provided token return it's new constant
  /// identifer if an identifer was identified
  fn parse_variable(&mut self, error_message: &str) -> u8 {
    self.parser.consume(TokenKind::Identifier, error_message);
    self.declare_variable();
    if self.scope_depth > 0 {
      return 0;
    }
    self.identifer_constant(self.parser.previous.clone())
  }

  /// Mark a variable initialized
  fn mark_initialized(&mut self) {
    self.locals[self.local_count - 1].depth = self.scope_depth;
  }

  /// Generate a constant from the provided identifier token
  fn identifer_constant(&mut self, name: Token) -> u8 {
    let identifer = (self.analytics.allocate)(ObjValue::String(name.lexeme));
    self.make_constant(Value::Obj(identifer))
  }

  fn add_local(&mut self, name: Token) {
    if self.local_count == std::u8::MAX as usize {
      self.parser.error("Too many local variables in function.");
      return;
    }

    let local = &mut self.locals[self.local_count];
    self.local_count += 1;

    local.name = Some(name);
    local.depth = self.scope_depth;
  }

  ///  declare a variable
  fn declare_variable(&mut self) {
    // if global exit
    if self.scope_depth == 0 {
      return;
    }

    let name = self.parser.previous.clone();
    for i in (0..self.local_count).rev() {
      let local = &self.locals[i];

      // If we in a lower scope break
      if local.depth != UNINITIALIZED && local.depth < self.scope_depth {
        break;
      }

      // check that the same variable wasn't declared twice in the same scope
      match &local.name {
        Some(local_name) => {
          if identifers_equal(&name, local_name) {
            self
              .parser
              .error("Variable with this name already declared in this scope.");
          }
        }
        None => panic!("local not set!"),
      }
    }

    self.add_local(name);
  }

  /// resolve a token to a local if it exists
  fn resolve_local(&mut self, name: &Token) -> Option<u8> {
    for i in (0..self.local_count).rev() {
      let local = &self.locals[i];

      match &local.name {
        Some(local_name) => {
          if identifers_equal(&name, local_name) {
            // handle the case were `var a = a;`
            if local.depth == UNINITIALIZED {
              self
                .parser
                .error("Cannot read local variable in its own initializer.")
            }

            return Some(i as u8);
          }
        }
        None => panic!("local not set!"),
      }
    }

    // not found
    Option::None
  }

  /// Execute a provided action
  fn execute_action(&mut self, action: Act, can_assign: bool) {
    match action {
      Act::Literal => self.literal(),
      Act::And => self.and(),
      Act::Or => self.or(),
      Act::Binary => self.binary(),
      Act::Unary => self.unary(),
      Act::Variable => self.variable(can_assign),
      Act::Grouping => self.grouping(),
      Act::Number => self.number(),
      Act::String => self.string(),
    }
  }

  /// Emit byte code for a return
  fn emit_return(&mut self) {
    self.emit_byte(ByteCode::Return)
  }

  /// Add a constant to the current chunk
  fn make_constant(&mut self, value: Value<'c>) -> u8 {
    let index = self.chunk.add_constant(value);
    if index > std::u8::MAX as usize {
      self.parser.error("Too many constants in one chunk.");
      return 0;
    }

    index as u8
  }

  /// Emit byte code for a constant
  fn emit_constant(&mut self, value: Value<'c>) {
    let index = self.make_constant(value);
    self.emit_byte(ByteCode::Constant(index));
  }


  fn patch_jump(&mut self, offset: usize) -> u16 {
    let jump = self.chunk.instructions.len() - offset - 1;

    if jump > std::u16::MAX.try_into().unwrap() {
      self.parser.error("Too much code to jump over.");
    }

    jump as u16
    // self.chunk.instructions[offset] = ByteCode::JumpIfFalse(jump as u16)
  }

  fn emit_loop(&mut self, loop_start: usize) {
    let offset = self.chunk.instructions.len() - loop_start + 1;
    if offset > std::u16::MAX.try_into().unwrap() {
      self.parser.error("Loop body too large.");
    }

    self.emit_byte(ByteCode::Loop(offset as u16));
  }

  /// Emit two provided instruction
  fn emit_bytes(&mut self, op_code1: ByteCode, op_code2: ByteCode) {
    let line = self.parser.current.line;
    self.chunk.write_instruction(op_code1, line);
    self.chunk.write_instruction(op_code2, line);
  }

  /// Emit a provided instruction
  fn emit_byte(&mut self, op_code: ByteCode) {
    let line = self.parser.current.line;
    self.chunk.write_instruction(op_code, line);
  }

  /// Emit a jump instruction
  fn emit_jump(&mut self) -> usize {
    self.emit_byte(ByteCode::Noop);

    return self.chunk.instructions.len() - 1;
  }
}

/// The rules for infix and prefix operators
const RULES_TABLE: [ParseRule; 40] = [
  ParseRule::new(Some(Act::Grouping), None, Precedence::None),
  // TOKEN_LEFT_PAREN
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_RIGHT_PAREN
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_LEFT_BRACE
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_RIGHT_BRACE
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_COMMA
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_DOT
  ParseRule::new(Some(Act::Unary), Some(Act::Binary), Precedence::Term),
  // TOKEN_MINUS
  ParseRule::new(None, Some(Act::Binary), Precedence::Term),
  // TOKEN_PLUS
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_SEMICOLON
  ParseRule::new(None, Some(Act::Binary), Precedence::Factor),
  // TOKEN_SLASH
  ParseRule::new(None, Some(Act::Binary), Precedence::Factor),
  // TOKEN_STAR
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
  ParseRule::new(Some(Act::Literal), None, Precedence::None),
  // TOKEN_NIL
  ParseRule::new(None, Some(Act::Or), Precedence::Or),
  // TOKEN_OR
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_PRINT
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_RETURN
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_SUPER
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_THIS
  ParseRule::new(Some(Act::Literal), None, Precedence::None),
  // TOKEN_TRUE
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_VAR
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_WHILE
  ParseRule::new(None, None, Precedence::None),
  // TOKEN_ERROR
  ParseRule::new(None, None, Precedence::None), // TOKEN_EOF
];

/// Get a rule from the rules table
const fn get_rule(kind: TokenKind) -> &'static ParseRule {
  &RULES_TABLE[kind as usize]
}

/// Are the two provided identifiers equal
fn identifers_equal(a: &Token, b: &Token) -> bool {
  a.lexeme == b.lexeme
}

/// The space lox parser. This struct is responsible for
/// advancing the scanner and checking for specific conditions
struct Parser {
  /// The current token
  current: Token,

  /// The previous token
  previous: Token,

  /// Has the parser encountered an error
  had_error: bool,

  /// Is the parser in panic mode
  panic_mode: bool,

  /// Help reference to the backing scanner
  scanner: Scanner,
}

impl Parser {
  /// Create a new instance of the parser from a source str
  pub fn new(source: String) -> Self {
    Self {
      scanner: Scanner::new(source),
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
    eprint!("[line {}] Error", token.line);

    match token.kind {
      TokenKind::Eof => eprint!(" at end"),
      TokenKind::Error => (),
      _ => eprint!(" at {}", token.lexeme),
    }

    eprintln!(": {}", message);
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
  Or,
  Grouping,
  Binary,
  Unary,
  Number,
  String,
  Literal,
  Variable,
}

#[cfg(test)]
mod test {
  use super::*;
  // use std::cell::Cell;

  // struct TestCalled {
  //   allocate: Cell<Option<ObjValue>>,
  //   intern: Cell<Option<String>>,
  // }

  // struct TestSetup<'a> {
  //   analytics: CompilerAnalytics<'a, 'a>,
  //   allocate: Box<dyn Fn(ObjValue) -> Obj<'a> + 'a>,
  //   intern: Box<dyn Fn(String) -> String + 'a>,
  // }

  // impl TestCalled {
  //   pub fn new() -> TestCalled {
  //     TestCalled {
  //       allocate: Cell::new(Option::None),
  //       intern: Cell::new(Option::None),
  //     }
  //   }
  // }

  // fn test_analytics<'a>(
  //   allocate: &'a dyn Fn(ObjValue) -> Obj<'a>,
  //   intern: &'a dyn Fn(String) -> String,
  // ) -> CompilerAnalytics<'a, 'a> {
  //   CompilerAnalytics {
  //     allocate: allocate,
  //     intern: intern,
  //   }
  // }

  // fn test_setup<'a>() -> (TestSetup<'a>, Box<TestCalled>) {
  //   let test_called = Box::new(TestCalled::new());

  //   let allocate = Box::new(|value: ObjValue| {
  //     let value_copy = value.clone();
  //     test_called.allocate.set(Some(value_copy));
  //     Obj::new(value)
  //   });

  //   let intern = Box::new(|string: String| {
  //     test_called.intern.set(Some(string.clone()));
  //     string
  //   });

  //   (
  //     TestSetup {
  //       analytics: test_analytics(&allocate, &intern),
  //       allocate,
  //       intern,
  //     },
  //     test_called,
  //   )
  // }

  fn test_compile(src: String) -> Vec<ByteCode> {
    let allocate = |value: ObjValue| Obj::new(value);
    let intern = |string: String| string;
    let compiler = Compiler::new(
      src,
      Chunk::default(),
      CompilerAnalytics {
        allocate: &allocate,
        intern: &intern,
      },
    );
    let result = compiler.compile();
    assert_eq!(result.success, true);

    result.chunk.instructions
  }

  #[test]
  fn op_print() {
    let example = "print 10;".to_string();

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 3);
    assert_eq!(instructions[0].clone(), ByteCode::Constant(0));
    assert_eq!(instructions[1].clone(), ByteCode::Print);
    assert_eq!(instructions[2], ByteCode::Return);
  }

  #[test]
  fn for_loop() {
    let example = "for (var x = 0; x < 10; x = x + 1) { print(x); }".to_string();

    let instructions = test_compile(example);
    assert_eq!(instructions.len(), 19);
    assert_eq!(instructions[0], ByteCode::Constant(0));
    assert_eq!(instructions[1], ByteCode::GetLocal(0));
    assert_eq!(instructions[2], ByteCode::Constant(1));
    assert_eq!(instructions[3], ByteCode::Less);
    assert_eq!(instructions[4], ByteCode::JumpIfFalse(11));
    assert_eq!(instructions[5], ByteCode::Pop);
    assert_eq!(instructions[6], ByteCode::Jump(6));
    assert_eq!(instructions[7], ByteCode::GetLocal(0));
    assert_eq!(instructions[8], ByteCode::Constant(2));
    assert_eq!(instructions[9], ByteCode::Add);
    assert_eq!(instructions[10], ByteCode::SetLocal(0));
    assert_eq!(instructions[11], ByteCode::Pop);
    assert_eq!(instructions[12], ByteCode::Loop(12));
    assert_eq!(instructions[13], ByteCode::GetLocal(0));
    assert_eq!(instructions[14], ByteCode::Print);
    assert_eq!(instructions[15], ByteCode::Loop(9));
    assert_eq!(instructions[16], ByteCode::Pop);
    assert_eq!(instructions[17], ByteCode::Pop);
    assert_eq!(instructions[18], ByteCode::Return);
  }

  #[test]
  fn while_loop() {
    let example = "while (true) { print 10; }".to_string();

    let instructions = test_compile(example);
    assert_eq!(instructions.len(), 8);
    assert_eq!(instructions[0], ByteCode::True);
    assert_eq!(instructions[1], ByteCode::JumpIfFalse(4));
    assert_eq!(instructions[2], ByteCode::Pop);
    assert_eq!(instructions[3], ByteCode::Constant(0));
    assert_eq!(instructions[4], ByteCode::Print);
    assert_eq!(instructions[5], ByteCode::Loop(6));
    assert_eq!(instructions[6], ByteCode::Pop);
    assert_eq!(instructions[7], ByteCode::Return);
  }

  #[test]
  fn if_condition() {
    let example = "if (3 < 10) { print \"hi\"; }".to_string();

    let instructions = test_compile(example);
    assert_eq!(instructions.len(), 10);
    assert_eq!(instructions[0], ByteCode::Constant(0));
    assert_eq!(instructions[1], ByteCode::Constant(1));
    assert_eq!(instructions[2], ByteCode::Less);
    assert_eq!(instructions[3], ByteCode::JumpIfFalse(4));
    assert_eq!(instructions[4], ByteCode::Pop);
    assert_eq!(instructions[5], ByteCode::Constant(2));
    assert_eq!(instructions[6], ByteCode::Print);
    assert_eq!(instructions[7], ByteCode::Jump(1));
    assert_eq!(instructions[8], ByteCode::Pop);
    assert_eq!(instructions[9], ByteCode::Return);
  }

  #[test]
  fn if_else_condition() {
    let example = "if (3 < 10) { print \"hi\"; } else { print \"bye\" }".to_string();

    let instructions = test_compile(example);
    assert_eq!(instructions.len(), 12);
    assert_eq!(instructions[0], ByteCode::Constant(0));
    assert_eq!(instructions[1], ByteCode::Constant(1));
    assert_eq!(instructions[2], ByteCode::Less);
    assert_eq!(instructions[3], ByteCode::JumpIfFalse(4));
    assert_eq!(instructions[4], ByteCode::Pop);
    assert_eq!(instructions[5], ByteCode::Constant(2));
    assert_eq!(instructions[6], ByteCode::Print);
    assert_eq!(instructions[7], ByteCode::Jump(3));
    assert_eq!(instructions[8], ByteCode::Pop);
    assert_eq!(instructions[9], ByteCode::Constant(3));
    assert_eq!(instructions[10], ByteCode::Print);
    assert_eq!(instructions[11], ByteCode::Return);
  }

  #[test]
  fn declare_local() {
    let example = "{ var x = 10; }".to_string();

    let instructions = test_compile(example);
    assert_eq!(instructions.len(), 3);
    assert_eq!(instructions[0], ByteCode::Constant(0));
    assert_eq!(instructions[1], ByteCode::Pop);
    assert_eq!(instructions[2], ByteCode::Return);
  }

  #[test]
  fn op_get_local() {
    let example = "{ var x = 10; print(x); }".to_string();

    let instructions = test_compile(example);
    assert_eq!(instructions.len(), 5);
    assert_eq!(instructions[0], ByteCode::Constant(0));
    assert_eq!(instructions[1], ByteCode::GetLocal(0));
    assert_eq!(instructions[2], ByteCode::Print);
    assert_eq!(instructions[3], ByteCode::Pop);
    assert_eq!(instructions[4], ByteCode::Return);
  }

  #[test]
  fn op_set_local() {
    let example = "{ var x = 10; x = 5; }".to_string();

    let instructions = test_compile(example);
    assert_eq!(instructions.len(), 6);
    assert_eq!(instructions[0], ByteCode::Constant(0));
    assert_eq!(instructions[1], ByteCode::Constant(1));
    assert_eq!(instructions[2], ByteCode::SetLocal(0));
    assert_eq!(instructions[3], ByteCode::Pop);
    assert_eq!(instructions[4], ByteCode::Pop);
    assert_eq!(instructions[5], ByteCode::Return);
  }

  #[test]
  fn op_define_global_nil() {
    let example = "var x;".to_string();

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 3);
    assert_eq!(instructions[0], ByteCode::Nil);
    assert_eq!(instructions[1], ByteCode::DefineGlobal(0));
    assert_eq!(instructions[2], ByteCode::Return);
  }

  #[test]
  fn op_define_global_val() {
    let example = "var x = 10;".to_string();

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 3);
    assert_eq!(instructions[0], ByteCode::Constant(1));
    assert_eq!(instructions[1], ByteCode::DefineGlobal(0));
    assert_eq!(instructions[2], ByteCode::Return);
  }

  #[test]
  fn op_get_global() {
    let example = "print x;".to_string();

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 3);
    assert_eq!(instructions[0], ByteCode::GetGlobal(0));
    assert_eq!(instructions[1], ByteCode::Print);
    assert_eq!(instructions[2], ByteCode::Return);
  }

  #[test]
  fn op_set_global() {
    let example = "x = \"cat\";".to_string();

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 4);
    assert_eq!(instructions[0], ByteCode::Constant(1));
    assert_eq!(instructions[1], ByteCode::SetGlobal(0));
    assert_eq!(instructions[2], ByteCode::Pop);
    assert_eq!(instructions[3], ByteCode::Return);
  }

  #[test]
  fn op_pop() {
    let example = "false;".to_string();

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 3);
    assert_eq!(instructions[0], ByteCode::False);
    assert_eq!(instructions[1], ByteCode::Pop);
    assert_eq!(instructions[2], ByteCode::Return);
  }

  #[test]
  fn op_return() {
    let example = "".to_string();

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 1);
    assert_eq!(instructions[0], ByteCode::Return);
  }

  #[test]
  fn op_number() {
    let example = "5.18;".to_string();

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 3);
    assert_eq!(instructions[0].clone(), ByteCode::Constant(0));
    assert_eq!(instructions[1].clone(), ByteCode::Pop);
    assert_eq!(instructions[2].clone(), ByteCode::Return);
  }

  #[test]
  fn op_string() {
    let example = "\"example\";".to_string();

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 3);
    assert_eq!(instructions[0].clone(), ByteCode::Constant(0));
    assert_eq!(instructions[1].clone(), ByteCode::Pop);
    assert_eq!(instructions[2].clone(), ByteCode::Return);
  }

  #[test]
  fn op_false() {
    let example = "false;".to_string();

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 3);
    assert_eq!(instructions[0], ByteCode::False);
    assert_eq!(instructions[1], ByteCode::Pop);
    assert_eq!(instructions[2], ByteCode::Return);
  }

  #[test]
  fn op_true() {
    let example = "true;".to_string();

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 3);
    assert_eq!(instructions[0], ByteCode::True);
    assert_eq!(instructions[1], ByteCode::Pop);
    assert_eq!(instructions[2], ByteCode::Return);
  }

  #[test]
  fn op_nil() {
    let example = "nil;".to_string();

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 3);
    assert_eq!(instructions[0], ByteCode::Nil);
    assert_eq!(instructions[1], ByteCode::Pop);
    assert_eq!(instructions[2], ByteCode::Return);
  }

  #[test]
  fn op_not() {
    let example = "!false;".to_string();

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 4);
    assert_eq!(instructions[0], ByteCode::False);
    assert_eq!(instructions[1], ByteCode::Not);
    assert_eq!(instructions[2], ByteCode::Pop);
    assert_eq!(instructions[3], ByteCode::Return);
  }

  #[test]
  fn op_negate() {
    let example = "-15;".to_string();

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 4);
    assert_eq!(instructions[0], ByteCode::Constant(0));
    assert_eq!(instructions[1], ByteCode::Negate);
    assert_eq!(instructions[2], ByteCode::Pop);
    assert_eq!(instructions[3], ByteCode::Return);
  }

  #[test]
  fn op_add() {
    let example = "10 + 4;".to_string();

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 5);
    assert_eq!(instructions[0], ByteCode::Constant(0));
    assert_eq!(instructions[1], ByteCode::Constant(1));
    assert_eq!(instructions[2], ByteCode::Add);
    assert_eq!(instructions[3], ByteCode::Pop);
    assert_eq!(instructions[4], ByteCode::Return);
  }

  #[test]
  fn op_subtract() {
    let example = "10 - 4;".to_string();

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 5);
    assert_eq!(instructions[0], ByteCode::Constant(0));
    assert_eq!(instructions[1], ByteCode::Constant(1));
    assert_eq!(instructions[2], ByteCode::Subtract);
    assert_eq!(instructions[3], ByteCode::Pop);
    assert_eq!(instructions[4], ByteCode::Return);
  }

  #[test]
  fn op_divide() {
    let example = "10 / 4;".to_string();

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 5);
    assert_eq!(instructions[0], ByteCode::Constant(0));
    assert_eq!(instructions[1], ByteCode::Constant(1));
    assert_eq!(instructions[2], ByteCode::Divide);
    assert_eq!(instructions[3], ByteCode::Pop);
    assert_eq!(instructions[4], ByteCode::Return);
  }

  #[test]
  fn op_multi() {
    let example = "10 * 4;".to_string();

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 5);
    assert_eq!(instructions[0], ByteCode::Constant(0));
    assert_eq!(instructions[1], ByteCode::Constant(1));
    assert_eq!(instructions[2], ByteCode::Multiply);
    assert_eq!(instructions[3], ByteCode::Pop);
    assert_eq!(instructions[4], ByteCode::Return);
  }

  #[test]
  fn op_equal() {
    let example = "true == nil;".to_string();

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 5);
    assert_eq!(instructions[0], ByteCode::True);
    assert_eq!(instructions[1], ByteCode::Nil);
    assert_eq!(instructions[2], ByteCode::Equal);
    assert_eq!(instructions[3], ByteCode::Pop);
    assert_eq!(instructions[4], ByteCode::Return);
  }

  #[test]
  fn op_not_equal() {
    let example = "true != nil;".to_string();

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 6);
    assert_eq!(instructions[0], ByteCode::True);
    assert_eq!(instructions[1], ByteCode::Nil);
    assert_eq!(instructions[2], ByteCode::Equal);
    assert_eq!(instructions[3], ByteCode::Not);
    assert_eq!(instructions[4], ByteCode::Pop);
    assert_eq!(instructions[5], ByteCode::Return);
  }

  #[test]
  fn op_less() {
    let example = "3 < 5;".to_string();

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 5);
    assert_eq!(instructions[0], ByteCode::Constant(0));
    assert_eq!(instructions[1], ByteCode::Constant(1));
    assert_eq!(instructions[2], ByteCode::Less);
    assert_eq!(instructions[3], ByteCode::Pop);
    assert_eq!(instructions[4], ByteCode::Return);
  }

  #[test]
  fn op_less_equal() {
    let example = "3 <= 5;".to_string();

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 6);
    assert_eq!(instructions[0], ByteCode::Constant(0));
    assert_eq!(instructions[1], ByteCode::Constant(1));
    assert_eq!(instructions[2], ByteCode::Greater);
    assert_eq!(instructions[3], ByteCode::Not);
    assert_eq!(instructions[4], ByteCode::Pop);
    assert_eq!(instructions[5], ByteCode::Return);
  }

  #[test]
  fn op_greater() {
    let example = "3 > 5;".to_string();

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 5);
    assert_eq!(instructions[0], ByteCode::Constant(0));
    assert_eq!(instructions[1], ByteCode::Constant(1));
    assert_eq!(instructions[2], ByteCode::Greater);
    assert_eq!(instructions[3], ByteCode::Pop);
    assert_eq!(instructions[4], ByteCode::Return);
  }

  #[test]
  fn op_greater_equal() {
    let example = "3 >= 5;".to_string();

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 6);
    assert_eq!(instructions[0], ByteCode::Constant(0));
    assert_eq!(instructions[1], ByteCode::Constant(1));
    assert_eq!(instructions[2], ByteCode::Less);
    assert_eq!(instructions[3], ByteCode::Not);
    assert_eq!(instructions[4], ByteCode::Pop);
    assert_eq!(instructions[5], ByteCode::Return);
  }
}
