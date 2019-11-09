use crate::scanner::{Scanner, TokenKind, Token};
use crate::chunk::{Chunk, OpCode};
use crate::debug::{disassemble_chunk};
use crate::value::{Value};
use crate::object::{Obj, copy_string};

/// The spacelox compiler for converting tokens to bytecode
pub struct Compiler {
  
  /// The parser in charge incrementing the scanner and 
  /// expecting / consuming tokens
  parser: Parser,

  /// The current chunk this compiler points to
  chunk: Chunk,
}

impl Compiler {
  /// Create a new instance of the spacelox compiler.
  /// The compiler write a sequence of op codes to the chunk
  /// to be executed
  /// 
  /// # Examples
  /// ```
  /// use lox_runtime::chunk::{Chunk};
  /// use lox_runtime::compiler::{Compiler};
  /// 
  /// // an expression
  /// let source = String::from("10 + 3");
  /// let chunk = Chunk::new();
  /// 
  /// let compiler = Compiler::new(source, chunk);
  /// ```
  pub fn new(source: String, chunk: Chunk) -> Self {
    Self { 
      chunk,
      parser: Parser::new(source),
    }
  }

  /// Compile the provided source code onto the chunk
  /// returns true if the compiler ran without parser errors
  /// 
  /// # Examples
  /// ```
  /// use lox_runtime::chunk::{Chunk};
  /// use lox_runtime::compiler::{Compiler};
  /// 
  /// // an expression
  /// let source = String::from("3 / 2 + 10");
  /// let chunk = Chunk::new();
  /// 
  /// let compiler = Compiler::new(source, chunk);
  /// let (success, chunk) = compiler.compile();
  /// assert_eq!(success, true);
  /// ```
  pub fn compile(mut self) -> (bool, Chunk) {
    self.parser.advance();
    
    match self.parser.current.kind {
      TokenKind::Eof => {
        self.end_compiler();
        return (!self.parser.had_error, self.chunk)
      },
      _ => self.expression(),
    }

    self.parser.consume(TokenKind::Eof, "Expected end of expression.");
    self.end_compiler();
    (!self.parser.had_error, self.chunk)
  }

  /// Parse an expression
  fn expression(&mut self) {
    self.parse_precendence(Precedence::Assignment)
  }

  /// End the complation at eof
  fn end_compiler(&mut self) {
    self.emit_return();

    #[cfg(debug_assertions)]
    self.print_chunk()
  }

  /// Print the chunk if debug and an error occurred
  fn print_chunk(&self) {
    if self.parser.had_error {
      disassemble_chunk(&self.chunk, "code")
    }
  }

  /// Compile a binary expression
  pub fn binary(&mut self) {
    // Remember the operator
    let operator_kind = self.parser.previous.kind.clone();
  
    let precedence = get_rule(operator_kind.clone()).precedence.higher();
    self.parse_precendence(precedence);

    match operator_kind {
      TokenKind::BangEqual => self.emit_bytes(OpCode::Equal, OpCode::Not),
      TokenKind::EqualEqual => self.emit_byte(OpCode::Equal),
      TokenKind::Greater => self.emit_byte(OpCode::Greater),
      TokenKind::GreaterEqual => self.emit_bytes(OpCode::Less, OpCode::Not),
      TokenKind::Less => self.emit_byte(OpCode::Less),
      TokenKind::LessEqual => self.emit_bytes(OpCode::Greater, OpCode::Not),
      TokenKind::Plus => self.emit_byte(OpCode::Add),
      TokenKind::Minus => self.emit_byte(OpCode::Subtract),
      TokenKind::Star => self.emit_byte(OpCode::Multiply),
      TokenKind::Slash => self.emit_byte(OpCode::Divide),
      _ => panic!("Invalid operator")
    }
  }
    
  /// Compile a unary expression
  fn unary(&mut self) {
    let operator_kind = self.parser.previous.kind.clone();

    // Compile the operand
    self.parse_precendence(Precedence::Unary);

    // Emit the operator instruction
    match operator_kind {
      TokenKind::Minus => self.emit_byte(OpCode::Negate),
      TokenKind::Bang => self.emit_byte(OpCode::Not),
      _ => panic!(),
    }
  }

  /// Compile a grouping expression
  fn grouping(&mut self) {
    self.expression();

    self.parser.consume(TokenKind::RightParen, "Expected ')' after expression")
  }

  /// Compile a number literal
  fn number(&mut self) {
    let value = Value::Number(self.parser.previous.lexeme.parse::<f64>().expect("Unable to parse float"));
    self.emit_constant(value);
  }

  fn string(&mut self) {
    let obj = Obj::String(copy_string(&self.parser.previous));
    let value = Value::Obj(obj);
    self.emit_constant(value)
  }

  /// Compile a literal
  fn literal(&mut self) {
    match self.parser.previous.kind {
      TokenKind::True => self.emit_byte(OpCode::True),
      TokenKind::False => self.emit_byte(OpCode::False),
      TokenKind::Nil => self.emit_byte(OpCode::Nil),
      _ => panic!(format!("Unexpected token kind {:?}", self.parser.previous.kind))
    }
  }

  /// Compile an expression an a provided precedences
  fn parse_precendence(&mut self, precedence: Precedence) {
    self.parser.advance();

    let prefix_fn = get_rule(self.parser.previous.kind.clone()).prefix.clone();
    self.execute_action(prefix_fn.expect("Expected expression."));

    while precedence <= get_rule(self.parser.current.kind.clone()).precedence {
      self.parser.advance();
      let infix_fn = get_rule(self.parser.previous.kind.clone()).infix.clone();

      self.execute_action(infix_fn.expect("Failure"));
    }
  }

  /// Execute a provided action
  fn execute_action(&mut self, action: Act) {
    match action {
      Act::Literal => self.literal(),
      Act::Binary => self.binary(), 
      Act::Unary => self.unary(), 
      Act::Grouping => self.grouping(), 
      Act::Number => self.number(),
      Act::String => self.string(),
    }
  }

  /// Emit byte code for a return
  fn emit_return(&mut self) {
    self.emit_byte(OpCode::Return)
  }

  /// Add a constant to the current chunk
  fn make_constant(&mut self, value: Value) -> u8 {
    let index = self.chunk.add_constant(value);
    if index > std::u8::MAX as usize {
      self.parser.error("Too many constants in one chunk.");
      return 0;
    }

    index as u8
  }

  /// Emit byte code for a constant
  fn emit_constant(&mut self, value: Value) {
    let index = self.make_constant(value);
    self.emit_byte(OpCode::Constant(index));
  }

  /// Emit two provided instruction
  fn emit_bytes(&mut self, op_code1: OpCode, op_code2: OpCode) {
    let line = self.parser.current.line.clone();
    self.chunk.write_instruction(op_code1, line);
    self.chunk.write_instruction(op_code2, line);
  }

  /// Emit a provided instruction
  fn emit_byte(&mut self, op_code: OpCode) {
    let line = self.parser.current.line.clone();
    self.chunk.write_instruction(op_code, line);
  }
}

/// The rules for infix and prefix operators
const RULES_TABLE: [ParseRule; 40] = [
  ParseRule::new(
    Some(Act::Grouping),
    None,
    Precedence::None),       
    // TOKEN_LEFT_PAREN

  ParseRule::new(
    None, 
    None, 
    Precedence::None),       
    // TOKEN_RIGHT_PAREN

  ParseRule::new(
    None,
    None,
    Precedence::None),       
    // TOKEN_LEFT_BRACE

  ParseRule::new(
    None,
    None,
    Precedence::None),
    // TOKEN_RIGHT_BRACE

  ParseRule::new(
    None,
    None,
    Precedence::None),
    // TOKEN_COMMA

  ParseRule::new(
    None,
    None,
    Precedence::None),
    // TOKEN_DOT
             
  ParseRule::new(
    Some(Act::Unary),
    Some(Act::Binary),
    Precedence::Term),
    // TOKEN_MINUS
           
  ParseRule::new(
    None,
    Some(Act::Binary),
    Precedence::Term),
    // TOKEN_PLUS
            
  ParseRule::new(
    None,
    None,
    Precedence::None),       
    // TOKEN_SEMICOLON
       
  ParseRule::new(
    None,
    Some(Act::Binary),
    Precedence::Factor),
    // TOKEN_SLASH
           
  ParseRule::new(
    None,
    Some(Act::Binary),
    Precedence::Factor),     
    // TOKEN_STAR
            
  ParseRule::new(
    Some(Act::Unary),
    None,
    Precedence::None),       
    // TOKEN_BANG
            
  ParseRule::new(
    None,
    Some(Act::Binary),
    Precedence::Equality),       
    // TOKEN_BANG_EQUAL
      
  ParseRule::new(
    None,
    None,
    Precedence::None),       
    // TOKEN_EQUAL
           
  ParseRule::new(
    None,
    Some(Act::Binary),
    Precedence::Equality),       
    // TOKEN_EQUAL_EQUAL
     
  ParseRule::new(
    None,
    Some(Act::Binary),
    Precedence::Comparison),       
    // TOKEN_GREATER
         
  ParseRule::new(
    None,
    Some(Act::Binary),
    Precedence::Comparison),       
    // TOKEN_GREATER_EQUAL
   
  ParseRule::new(
    None,
    Some(Act::Binary),
    Precedence::Comparison),       
    // TOKEN_LESS
            
  ParseRule::new(
    None,
    Some(Act::Binary),
    Precedence::Comparison),       
    // TOKEN_LESS_EQUAL
      
  ParseRule::new(
    None,
    None,
    Precedence::None),       
    // TOKEN_IDENTIFIER
      
  ParseRule::new(
    Some(Act::String),
    None,
    Precedence::None),       
    // TOKEN_STRING
          
  ParseRule::new(
    Some(Act::Number),
    None,
    Precedence::None),       
    // TOKEN_NUMBER
          
  ParseRule::new(
    None,
    None,
    Precedence::None),       
    // TOKEN_AND
             
  ParseRule::new(
    None,
    None,
    Precedence::None),       
    // TOKEN_CLASS
           
  ParseRule::new(
    None,
    None,
    Precedence::None),       
    // TOKEN_ELSE
            
  ParseRule::new(
    Some(Act::Literal),
    None,
    Precedence::None),       
    // TOKEN_FALSE
           
  ParseRule::new(
    None,
    None,
    Precedence::None),       
    // TOKEN_FOR
             
  ParseRule::new(
    None,
    None,
    Precedence::None),       
    // TOKEN_FUN
             
  ParseRule::new(
    None,
    None,
    Precedence::None),       
    // TOKEN_IF
              
  ParseRule::new(
    Some(Act::Literal),
    None,
    Precedence::None),       
    // TOKEN_NIL
             
  ParseRule::new(
    None,
    None,
    Precedence::None),       
    // TOKEN_OR
              
  ParseRule::new(
    None,
    None,
    Precedence::None),       
    // TOKEN_PRINT
           
  ParseRule::new(
    None,
    None,
    Precedence::None),       
    // TOKEN_RETURN
          
  ParseRule::new(
    None,
    None,
    Precedence::None),       
    // TOKEN_SUPER
           
  ParseRule::new(
    None,
    None,
    Precedence::None),       
    // TOKEN_THIS
            
  ParseRule::new(
    Some(Act::Literal),
    None,
    Precedence::None),
    // TOKEN_TRUE
            
  ParseRule::new(
    None,
    None,
    Precedence::None),       
    // TOKEN_VAR
             
  ParseRule::new(
    None,
    None,
    Precedence::None),       
    // TOKEN_WHILE
           
  ParseRule::new(
    None,
    None,
    Precedence::None),       
    // TOKEN_ERROR
           
  ParseRule::new(
    None,
    None,
    Precedence::None)        
    // TOKEN_EOF 
];

/// Get a rule from the rules table
const fn get_rule(kind: TokenKind) -> &'static ParseRule {
  return &RULES_TABLE[kind as usize];
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
  scanner: Scanner
}

impl Parser {

  /// Create a new instance of the parser from a source str
  pub fn new(source: String) -> Self {
    Self { 
      scanner: Scanner::new(source),
      had_error: false,
      panic_mode: false,
      previous: Token { lexeme: String::from("error"), line: 0, kind: TokenKind::Error },
      current: Token { lexeme: String::from("error"), line: 0, kind: TokenKind::Error },
    }
  }

  /// Advance the parser a token forward
  pub fn advance(&mut self) -> () {
    self.previous = self.current.clone();

    loop {
      println!("here");
      self.current = self.scanner.scan_token();
      if self.current.kind != TokenKind::Error {
        break;
      }

      self.error_at_current(&self.current.lexeme.to_string())
    }
  }

  /// Consume a token and advance the current token index
  fn consume(&mut self, kind: TokenKind, message: &str) -> () {
    // println!("current {:?}, expected {:?}", self.current, kind);

    if self.current.kind == kind {
      self.advance();
      return;
    }

    self.error_at_current(message)
  }

  /// Indicate an error occurred at he current index
  fn error_at_current(&mut self, message: &str) -> () {
    let token = self.current.clone();
    self.error_at(token, message);
  }

  /// Indicate an error occurred at the previous index
  pub fn error(&mut self, message: &str) -> () {
    let token = self.previous.clone();
    self.error_at(token, message);
  }

  /// Print an error to the console for a user to address
  fn error_at(&mut self, token: Token, message: &str) -> () {
    if self.panic_mode {
      return;
    }

    self.panic_mode = true;
    eprint!("[line {}] Error", token.line);

    match token.kind {
      TokenKind::Eof => print!(" at end"),
      TokenKind::Error => (),
      _ => eprint!(" at {}", token.lexeme)
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
  Primary
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
  precedence: Precedence
}

impl ParseRule {
  const fn new(
    prefix: Option<Act>,
    infix: Option<Act>,
    precedence: Precedence) -> Self {
    Self {
      prefix,
      infix,
      precedence
    }
  }
}

#[derive(Clone)]
enum Act {
  Grouping,
  Binary,
  Unary,
  Number,
  String,
  Literal,
}

#[cfg(test)]
mod test {
  use super::*;

  fn test_compile(src: String) -> Vec<OpCode> {
    let compiler = Compiler::new(src, Chunk::new());
    let (success, chunk) = compiler.compile();
    assert_eq!(success, true);

    chunk.instructions
  }

  #[test]
  fn op_return() {
    let example = String::from("");

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 1);
    assert_eq!(instructions[0], OpCode::Return);
  }

  #[test]
  fn op_number() {
    let example = String::from("5.18");

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 2);
    assert_eq!(instructions[0].clone(), OpCode::Constant(0));
    assert_eq!(instructions[1].clone(), OpCode::Return);
  }

  #[test]
  fn op_false() {
    let example = String::from("false");

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 2);
    assert_eq!(instructions[0], OpCode::False);
    assert_eq!(instructions[1], OpCode::Return);
  }

  #[test]
  fn op_true() {
    let example = String::from("true");

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 2);
    assert_eq!(instructions[0], OpCode::True);
    assert_eq!(instructions[1], OpCode::Return);
  }

  #[test]
  fn op_nil() {
    let example = String::from("nil");

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 2);
    assert_eq!(instructions[0], OpCode::Nil);
    assert_eq!(instructions[1], OpCode::Return);
  }

  #[test]
  fn op_not() {
    let example = String::from("!false");

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 3);
    assert_eq!(instructions[0], OpCode::False);
    assert_eq!(instructions[1], OpCode::Not);
    assert_eq!(instructions[2], OpCode::Return);
  }

  #[test]
  fn op_negate() {
    let example = String::from("-15");

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 3);
    assert_eq!(instructions[0], OpCode::Constant(0));
    assert_eq!(instructions[1], OpCode::Negate);
    assert_eq!(instructions[2], OpCode::Return);
  }


  #[test]
  fn op_add() {
    let example = String::from("10 + 4");

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 4);
    assert_eq!(instructions[0], OpCode::Constant(0));
    assert_eq!(instructions[1], OpCode::Constant(1));
    assert_eq!(instructions[2], OpCode::Add);
    assert_eq!(instructions[3], OpCode::Return);
  }

  #[test]
  fn op_subtract() {
    let example = String::from("10 - 4");

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 4);
    assert_eq!(instructions[0], OpCode::Constant(0));
    assert_eq!(instructions[1], OpCode::Constant(1));
    assert_eq!(instructions[2], OpCode::Subtract);
    assert_eq!(instructions[3], OpCode::Return);
  }

  #[test]
  fn op_divide() {
    let example = String::from("10 / 4");

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 4);
    assert_eq!(instructions[0], OpCode::Constant(0));
    assert_eq!(instructions[1], OpCode::Constant(1));
    assert_eq!(instructions[2], OpCode::Divide);
    assert_eq!(instructions[3], OpCode::Return);
  }

  #[test]
  fn op_multi() {
    let example = String::from("10 * 4");

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 4);
    assert_eq!(instructions[0], OpCode::Constant(0));
    assert_eq!(instructions[1], OpCode::Constant(1));
    assert_eq!(instructions[2], OpCode::Multiply);
    assert_eq!(instructions[3], OpCode::Return);
  }


  #[test]
  fn op_equal() {
    let example = String::from("true == nil");

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 4);
    assert_eq!(instructions[0], OpCode::True);
    assert_eq!(instructions[1], OpCode::Nil);
    assert_eq!(instructions[2], OpCode::Equal);
    assert_eq!(instructions[3], OpCode::Return);
  }

  #[test]
  fn op_not_equal() {
    let example = String::from("true != nil");

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 5);
    assert_eq!(instructions[0], OpCode::True);
    assert_eq!(instructions[1], OpCode::Nil);
    assert_eq!(instructions[2], OpCode::Equal);
    assert_eq!(instructions[3], OpCode::Not);
    assert_eq!(instructions[4], OpCode::Return);
  }

  #[test]
  fn op_less() {
    let example = String::from("3 < 5");

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 4);
    assert_eq!(instructions[0], OpCode::Constant(0));
    assert_eq!(instructions[1], OpCode::Constant(1));
    assert_eq!(instructions[2], OpCode::Less);
    assert_eq!(instructions[3], OpCode::Return);
  }

  #[test]
  fn op_less_equal() {
    let example = String::from("3 <= 5");

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 5);
    assert_eq!(instructions[0], OpCode::Constant(0));
    assert_eq!(instructions[1], OpCode::Constant(1));
    assert_eq!(instructions[2], OpCode::Greater);
    assert_eq!(instructions[3], OpCode::Not);
    assert_eq!(instructions[4], OpCode::Return);
  }

  #[test]
  fn op_greater() {
    let example = String::from("3 > 5");

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 4);
    assert_eq!(instructions[0], OpCode::Constant(0));
    assert_eq!(instructions[1], OpCode::Constant(1));
    assert_eq!(instructions[2], OpCode::Greater);
    assert_eq!(instructions[3], OpCode::Return);
  }

  #[test]
  fn op_greater_equal() {
    let example = String::from("3 >= 5");

    let instructions = test_compile(example);

    assert_eq!(instructions.len(), 5);
    assert_eq!(instructions[0], OpCode::Constant(0));
    assert_eq!(instructions[1], OpCode::Constant(1));
    assert_eq!(instructions[2], OpCode::Less);
    assert_eq!(instructions[3], OpCode::Not);
    assert_eq!(instructions[4], OpCode::Return);
  }


    // OpCode::Add => self.op_add(),
    // OpCode::Subtract => self.op_sub(),
    // OpCode::Multiply => self.op_mul(),
    // OpCode::Divide => self.op_div(),
    // OpCode::Equal => self.op_equal(),
    // OpCode::Greater => self.op_greater(),
    // OpCode::Less => self.op_less(),
}